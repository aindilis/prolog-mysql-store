% ============================================================================
% Advanced Variable Support: Cross-Clause Unification
% ============================================================================
%
% This module implements sophisticated variable handling that allows
% unification across different stored clauses. This is useful for
% storing rule-like structures where variables in one term should
% potentially unify with terms in another clause.
%
% WARNING: This is computationally expensive and should be used
% judiciously. For most use cases, the simpler template storage
% is more appropriate.
%
% Example Usage:
%   % Store rules with shared variables
%   ?- store_clause(mydb, kb:edge(A, B) :- path(A, B)).
%   ?- store_clause(mydb, kb:edge(A, C) :- edge(A, B), edge(B, C)).
%   
%   % Query with resolution
%   ?- store_resolve(mydb, kb:edge(node1, node3)).
%

:- module(mysql_store_clauses, [
    % Clause storage
    store_clause/2,
    store_clause/3,
    store_fact_with_vars/2,
    
    % Clause querying with resolution
    store_resolve/2,
    store_resolve/3,
    
    % Clause management
    store_list_clauses/3,
    store_retract_clause/2,
    
    % Variable tracking
    store_variable_bindings/3,
    store_variable_usage/3
]).

:- use_module(mysql_store).
:- use_module(mysql_store_templates).
:- use_module(library(odbc)).
:- use_module(library(sha)).

% ============================================================================
% Clause Table Schema
% ============================================================================

ensure_clause_tables(ConnectionId) :-
    % Clauses table (stores both facts and rules)
    ClausesQuery = 'CREATE TABLE IF NOT EXISTS formula_clauses (
        clause_id INT PRIMARY KEY AUTO_INCREMENT,
        context_id INT NOT NULL,
        head_functor VARCHAR(255) NOT NULL,
        head_arity INT NOT NULL,
        head_repr TEXT NOT NULL,
        body_repr TEXT,
        clause_type ENUM("fact", "rule") NOT NULL,
        var_count INT NOT NULL DEFAULT 0,
        clause_hash VARCHAR(64) NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE KEY unique_clause (context_id, clause_hash),
        KEY idx_head (context_id, head_functor, head_arity),
        KEY idx_type (context_id, clause_type),
        FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
    )',
    odbc_query(ConnectionId, ClausesQuery, _),
    
    % Variable correlation table
    VarCorrelationQuery = 'CREATE TABLE IF NOT EXISTS clause_variables (
        clause_id INT NOT NULL,
        var_index INT NOT NULL,
        var_name VARCHAR(50),
        appears_in_head BOOLEAN DEFAULT FALSE,
        appears_in_body BOOLEAN DEFAULT FALSE,
        positions TEXT,
        PRIMARY KEY (clause_id, var_index),
        FOREIGN KEY (clause_id) REFERENCES formula_clauses(clause_id) ON DELETE CASCADE
    )',
    odbc_query(ConnectionId, VarCorrelationQuery, _),
    
    % Variable binding cache (for optimization)
    BindingCacheQuery = 'CREATE TABLE IF NOT EXISTS variable_bindings (
        binding_id INT PRIMARY KEY AUTO_INCREMENT,
        clause_id INT NOT NULL,
        var_index INT NOT NULL,
        bound_term TEXT,
        binding_context TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        KEY idx_clause_var (clause_id, var_index),
        FOREIGN KEY (clause_id) REFERENCES formula_clauses(clause_id) ON DELETE CASCADE
    )',
    odbc_query(ConnectionId, BindingCacheQuery, _).

% ============================================================================
% Clause Storage
% ============================================================================

%! store_clause(+ConnectionId, +ContextualClause) is det.
%! store_clause(+ConnectionId, +ContextualClause, -ClauseId) is det.
%
% Store a clause (fact or rule) with variables.
% Clauses can be:
%   - Facts: Context:head
%   - Rules: Context:(head :- body)
%
store_clause(ConnectionId, ContextualClause) :-
    store_clause(ConnectionId, ContextualClause, _).

store_clause(ConnectionId, Context:Clause, ClauseId) :-
    must_be(atom, Context),
    
    ensure_clause_tables(ConnectionId),
    
    % Get context ID
    mysql_store:store_ensure_context(ConnectionId, Context),
    mysql_store:context_mapping(Context, ContextId),
    
    % Parse clause
    (Clause = (Head :- Body) ->
        ClauseType = rule,
        parse_clause(Head, Body, HeadFunctor, HeadArity, HeadRepr, BodyRepr, VarInfo, VarCount)
    ;
        ClauseType = fact,
        parse_fact(Clause, HeadFunctor, HeadArity, HeadRepr, VarInfo, VarCount),
        BodyRepr = null
    ),
    
    % Generate hash and convert to hex string
    (ClauseType = rule ->
        format(atom(ClauseStr), '~k :- ~k', [Head, Body])
    ;
        format(atom(ClauseStr), '~k', [Clause])
    ),
    sha_hash(ClauseStr, HashList, [algorithm(sha256)]),
    hash_list_to_hex(HashList, HashHex),
    
    % Store clause
    store_clause_record(ConnectionId, ContextId, HeadFunctor, HeadArity,
                       HeadRepr, BodyRepr, ClauseType, VarCount, HashHex,
                       VarInfo, ClauseId).

%! hash_list_to_hex(+HashList, -HexString) is det.
%
% Convert a list of bytes to a hexadecimal string.
%
hash_list_to_hex(HashList, HexString) :-
    maplist(byte_to_hex, HashList, HexChars),
    atomic_list_concat(HexChars, HexString).

byte_to_hex(Byte, Hex) :-
    format(atom(Hex), '~|~`0t~16r~2+', [Byte]).

%! parse_clause(+Head, +Body, -Functor, -Arity, -HeadRepr, -BodyRepr, -VarInfo, -VarCount) is det.
%
parse_clause(Head, Body, Functor, Arity, HeadRepr, BodyRepr, VarInfo, VarCount) :-
    functor(Head, Functor, Arity),
    
    % Collect all variables from both head and body
    term_variables((Head, Body), AllVars),
    length(AllVars, VarCount),
    
    % Analyze where each variable appears
    term_variables(Head, HeadVars),
    term_variables(Body, BodyVars),
    
    % Create variable info
    maplist(analyze_var(HeadVars, BodyVars), AllVars, VarInfo),
    
    % Serialize
    format(atom(HeadRepr), '~k', [Head]),
    format(atom(BodyRepr), '~k', [Body]).

%! parse_fact(+Fact, -Functor, -Arity, -Repr, -VarInfo, -VarCount) is det.
%
parse_fact(Fact, Functor, Arity, Repr, VarInfo, VarCount) :-
    functor(Fact, Functor, Arity),
    term_variables(Fact, Vars),
    length(Vars, VarCount),
    
    % All variables appear in head only
    maplist(fact_var_info, Vars, VarInfo),
    
    format(atom(Repr), '~k', [Fact]).

analyze_var(HeadVars, BodyVars, Var, var_info(Var, Name, InHead, InBody)) :-
    (var_property(Var, name(Name)) -> true ; Name = '_'),
    (memberchk(Var, HeadVars) -> InHead = true ; InHead = false),
    (memberchk(Var, BodyVars) -> InBody = true ; InBody = false).

fact_var_info(Var, var_info(Var, Name, true, false)) :-
    (var_property(Var, name(Name)) -> true ; Name = '_').

%! store_clause_record(...) is det.
%
store_clause_record(ConnectionId, ContextId, Functor, Arity, HeadRepr, BodyRepr,
                    ClauseType, VarCount, Hash, VarInfo, ClauseId) :-
    InsertQuery = 'INSERT INTO formula_clauses
                   (context_id, head_functor, head_arity, head_repr, body_repr,
                    clause_type, var_count, clause_hash)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    
    Result = catch(
        (odbc_prepare(ConnectionId, InsertQuery,
                     [integer, default, integer, default, default, default, integer, default],
                     Stmt),
         odbc_execute(Stmt, [ContextId, Functor, Arity, HeadRepr, BodyRepr,
                            ClauseType, VarCount, Hash]),
         odbc_query(ConnectionId, 'SELECT LAST_INSERT_ID()', row(ClauseId)),
         insert),  % Tag as successful insert
        error(odbc(_, duplicate_key, _), _),
        duplicate  % Tag as duplicate
    ),
    
    (Result = insert ->
        % New insert - store variable information
        store_clause_var_info(ConnectionId, ClauseId, VarInfo)
    ;
        % Duplicate - set placeholder ID and skip var_info
        ClauseId = -1
    ).

store_clause_var_info(_, _, []).
store_clause_var_info(ConnectionId, ClauseId, [var_info(_Var, Name, InHead, InBody)|Rest]) :-
    length(Rest, VarIndex),
    
    InsertQuery = 'INSERT IGNORE INTO clause_variables
                   (clause_id, var_index, var_name, appears_in_head, appears_in_body)
                   VALUES (?, ?, ?, ?, ?)',
    odbc_prepare(ConnectionId, InsertQuery,
                [integer, integer, default, integer, integer], Stmt),
    
    (InHead = true -> HeadFlag = 1 ; HeadFlag = 0),
    (InBody = true -> BodyFlag = 1 ; BodyFlag = 0),
    
    odbc_execute(Stmt, [ClauseId, VarIndex, Name, HeadFlag, BodyFlag]),
    
    store_clause_var_info(ConnectionId, ClauseId, Rest).

%! store_fact_with_vars(+ConnectionId, +ContextualFact) is det.
%
% Convenience predicate for storing facts with variables.
%
store_fact_with_vars(ConnectionId, ContextualFact) :-
    store_clause(ConnectionId, ContextualFact).

% ============================================================================
% Resolution-Based Querying
% ============================================================================

%! store_resolve(+ConnectionId, +ContextualGoal) is nondet.
%! store_resolve(+ConnectionId, +ContextualGoal, +MaxDepth) is nondet.
%
% Query using SLD resolution. Attempts to prove the goal by
% unifying with stored clauses and recursively proving subgoals.
%
% WARNING: This can be slow for complex queries. Use with caution.
%
store_resolve(ConnectionId, ContextualGoal) :-
    store_resolve(ConnectionId, ContextualGoal, 10).

store_resolve(ConnectionId, Context:Goal, MaxDepth) :-
    MaxDepth > 0,
    
    % Try to match goal with clause heads
    mysql_store:context_mapping(Context, ContextId),
    functor(Goal, Functor, Arity),
    
    % Fetch matching clauses
    fetch_clauses(ConnectionId, ContextId, Functor, Arity, Clauses),
    
    % Try each clause
    member(clause(ClauseType, HeadRepr, BodyRepr), Clauses),
    
    % Instantiate clause with fresh variables
    read_term_from_atom(HeadRepr, Head, []),
    
    % Unify goal with head
    Head = Goal,
    
    % If it's a rule, prove the body
    (ClauseType = rule ->
        read_term_from_atom(BodyRepr, Body, []),
        NextDepth is MaxDepth - 1,
        prove_body(ConnectionId, Context, Body, NextDepth)
    ;
        true  % Fact - we're done
    ).

%! prove_body(+ConnectionId, +Context, +Body, +MaxDepth) is nondet.
%
prove_body(_, _, true, _) :- !.

prove_body(ConnectionId, Context, (Goal1, Goal2), MaxDepth) :- !,
    prove_goal(ConnectionId, Context, Goal1, MaxDepth),
    prove_body(ConnectionId, Context, Goal2, MaxDepth).

prove_body(ConnectionId, Context, Goal, MaxDepth) :-
    prove_goal(ConnectionId, Context, Goal, MaxDepth).

%! prove_goal(+ConnectionId, +Context, +Goal, +MaxDepth) is nondet.
%
prove_goal(ConnectionId, Context, Goal, MaxDepth) :-
    % Try resolution
    store_resolve(ConnectionId, Context:Goal, MaxDepth).

prove_goal(ConnectionId, Context, Goal, _MaxDepth) :-
    % Try ground facts
    mysql_store:store_call(ConnectionId, Context:Goal).

prove_goal(ConnectionId, Context, Goal, _MaxDepth) :-
    % Try templates
    mysql_store_templates:store_call_template(ConnectionId, Context:Goal).

%! fetch_clauses(+ConnectionId, +ContextId, +Functor, +Arity, -Clauses) is det.
%
fetch_clauses(ConnectionId, ContextId, Functor, Arity, Clauses) :-
    Query = 'SELECT clause_type, head_repr, body_repr
             FROM formula_clauses
             WHERE context_id = ? AND head_functor = ? AND head_arity = ?
             ORDER BY clause_id',
    
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    
    findall(clause(Type, HeadRepr, BodyRepr),
            odbc_execute(Stmt, [ContextId, Functor, Arity],
                        row(Type, HeadRepr, BodyRepr)),
            Clauses).

% ============================================================================
% Clause Management
% ============================================================================

%! store_list_clauses(+ConnectionId, +Context, -Clauses) is det.
%
store_list_clauses(ConnectionId, Context, Clauses) :-
    mysql_store:context_mapping(Context, ContextId),
    
    Query = 'SELECT clause_id, clause_type, head_repr, body_repr, var_count
             FROM formula_clauses
             WHERE context_id = ?
             ORDER BY head_functor, head_arity',
    
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(clause(Id, Type, HeadRepr, BodyRepr, VarCount),
            odbc_execute(Stmt, [ContextId],
                        row(Id, Type, HeadRepr, BodyRepr, VarCount)),
            Clauses).

%! store_retract_clause(+ConnectionId, +ContextualClause) is nondet.
%
store_retract_clause(ConnectionId, Context:Clause) :-
    mysql_store:context_mapping(Context, ContextId),
    
    % Parse clause to find it
    (Clause = (Head :- Body) ->
        format(atom(ClauseStr), '~k :- ~k', [Head, Body])
    ;
        format(atom(ClauseStr), '~k', [Clause])
    ),
    sha_hash(ClauseStr, HashList, [algorithm(sha256)]),
    hash_list_to_hex(HashList, HashHex),
    
    % Delete
    Query = 'DELETE FROM formula_clauses
             WHERE context_id = ? AND clause_hash = ?',
    odbc_prepare(ConnectionId, Query, [integer, default], Stmt),
    odbc_execute(Stmt, [ContextId, HashHex]).

% ============================================================================
% Variable Analysis
% ============================================================================

%! store_variable_bindings(+ConnectionId, +ClauseId, -Bindings) is det.
%
% Retrieve stored variable bindings for a clause.
%
store_variable_bindings(ConnectionId, ClauseId, Bindings) :-
    Query = 'SELECT var_index, bound_term, binding_context
             FROM variable_bindings
             WHERE clause_id = ?
             ORDER BY var_index',
    
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(binding(VarIndex, BoundTerm, BindContext),
            odbc_execute(Stmt, [ClauseId],
                        row(VarIndex, BoundTerm, BindContext)),
            Bindings).

%! store_variable_usage(+ConnectionId, +Context, -Usage) is det.
%
% Analyze variable usage patterns across all clauses.
%
store_variable_usage(ConnectionId, Context, Usage) :-
    mysql_store:context_mapping(Context, ContextId),
    
    Query = 'SELECT 
                cv.var_name,
                COUNT(*) as occurrence_count,
                SUM(cv.appears_in_head) as head_count,
                SUM(cv.appears_in_body) as body_count,
                COUNT(DISTINCT fc.head_functor) as functor_count
             FROM clause_variables cv
             JOIN formula_clauses fc ON cv.clause_id = fc.clause_id
             WHERE fc.context_id = ?
             GROUP BY cv.var_name
             ORDER BY occurrence_count DESC',
    
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(usage(VarName, Occurrences, HeadCount, BodyCount, FunctorCount),
            odbc_execute(Stmt, [ContextId],
                        row(VarName, Occurrences, HeadCount, BodyCount, FunctorCount)),
            Usage).

% ============================================================================
% Examples
% ============================================================================

:- if(false).

test_clauses :-
    store_connect(testdb, 'localhost', 'prolog_store', 'prolog', 'password'),
    store_ensure_context(testdb, kb),
    
    % Store some facts with variables
    format('Storing facts with variables~n'),
    store_fact_with_vars(testdb, kb:edge(a, b)),
    store_fact_with_vars(testdb, kb:edge(b, c)),
    store_fact_with_vars(testdb, kb:edge(c, d)),
    
    % Store a rule
    format('Storing transitive closure rule~n'),
    store_clause(testdb, kb:(path(X, Y) :- edge(X, Y))),
    store_clause(testdb, kb:(path(X, Z) :- edge(X, Y), path(Y, Z))),
    
    % Query with resolution
    format('~nQuerying: path(a, d)?~n'),
    (store_resolve(testdb, kb:path(a, d)) ->
        format('  Yes!~n')
    ;
        format('  No.~n')
    ),
    
    % Find all paths from a
    format('~nFinding all paths from a:~n'),
    findall(Y, store_resolve(testdb, kb:path(a, Y)), Paths),
    format('  Paths: ~w~n', [Paths]),
    
    % Variable usage analysis
    format('~nVariable usage analysis:~n'),
    store_variable_usage(testdb, kb, Usage),
    maplist(writeln, Usage),
    
    store_disconnect(testdb).

:- endif.
