% ============================================================================
% MySQL Assertion Store - Core Module (CORRECTED)
% ============================================================================

:- module(mysql_store, [
    % Connection management
    store_connect/5,
    store_disconnect/1,
    store_ensure_context/2,
    
    % Core operations
    store_assert/2,
    store_retract/2,
    store_retractall/2,
    store_abolish/2,
    
    % Queries
    store_call/2,
    store_findall/4,
    
    % Cache management
    store_load_predicate/3,
    store_unload_predicate/3,
    store_sync/1,
    
    % Statistics
    store_stats/3,
    store_optimize/2
]).

:- use_module(library(odbc)).
:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% ============================================================================
% Dynamic predicates for runtime state
% ============================================================================

:- dynamic 
    connection/2,              % connection(Handle, ConnectionId)
    context_mapping/2,         % context_mapping(ContextName, ContextId)
    loaded_predicate/3,        % loaded_predicate(Context, Functor, Arity)
    predicate_cache/4,         % predicate_cache(Context, Functor, Arity, Facts)
    transaction_pending/1,     % transaction_pending(ConnectionId)
    index_config/4.            % index_config(Context, Functor, Arity, Positions)

% ============================================================================
% Configuration
% ============================================================================

% Which argument positions to index (by functor/arity)
default_index_positions(_, _, [1]).  % Default: index first argument only

% Should we index list elements?
index_list_elements(false).  % Set to true if you need list element queries

% Cache size limit (number of predicates kept in memory)
max_cached_predicates(100).

% Batch size for bulk operations
bulk_insert_threshold(50).

% ============================================================================
% Connection Management
% ============================================================================

%! store_connect(+ConnectionId, +Server, +Database, +User, +Password) is det.
%
% Establish connection to MySQL database.
%
store_connect(ConnectionId, _Server, _Database, _User, _Password) :-
    must_be(atom, ConnectionId),
    
    % Close existing connection if any
    (connection(_, ConnectionId) -> store_disconnect(ConnectionId) ; true),
    
    % Connect using DSN
    catch(
        odbc_connect(ConnectionId, _, [alias(ConnectionId), open(once)]),
        Error,
        throw(error(connection_failed(ConnectionId, Error), _))
    ),
    
    % Store connection handle
    assertz(connection(ConnectionId, ConnectionId)),
    
    % Set autocommit on by default (will be disabled during transactions)
    odbc_query(ConnectionId, 'SET autocommit=1', _),
    
    % Ensure schema exists
    ensure_schema(ConnectionId).

%! store_disconnect(+ConnectionId) is det.
%
% Close database connection and cleanup.
%
store_disconnect(ConnectionId) :-
    (connection(_, ConnectionId) ->
        (
            % Sync any pending changes
            store_sync(ConnectionId),
            
            % Clear cache
            retractall(loaded_predicate(_, _, _)),
            retractall(predicate_cache(_, _, _, _)),
            
            % Close connection
            catch(odbc_disconnect(ConnectionId), _, true),
            retractall(connection(_, ConnectionId))
        )
    ;   true
    ).

%! ensure_schema(+ConnectionId) is det.
%
% Verify database schema exists.
%
ensure_schema(ConnectionId) :-
    % Check if contexts table exists
    catch(
        odbc_query(ConnectionId, 'DESCRIBE contexts', _),
        _,
        throw(error(schema_missing, 
                   context('Please run the schema.sql script first')))
    ).

% ============================================================================
% Context Management
% ============================================================================

%! store_ensure_context(+ConnectionId, +ContextName) is det.
%
% Ensure context exists, create if necessary.
%
store_ensure_context(ConnectionId, ContextName) :-
    must_be(atom, ContextName),
    
    (context_mapping(ContextName, _) ->
        true
    ;
        % Try to get existing context
        format(atom(Query), 'SELECT context_id FROM contexts WHERE context_name = \'~w\'', [ContextName]),
        (odbc_query(ConnectionId, Query, row(ContextId)) ->
            assertz(context_mapping(ContextName, ContextId))
        ;
            % Create new context
            create_context(ConnectionId, ContextName)
        )
    ).

create_context(ConnectionId, ContextName) :-
    format(atom(Query), 'INSERT INTO contexts (context_name) VALUES (\'~w\')', [ContextName]),
    odbc_query(ConnectionId, Query, _),
    
    % Get the new ID
    odbc_query(ConnectionId, 'SELECT LAST_INSERT_ID()', row(ContextId)),
    assertz(context_mapping(ContextName, ContextId)).

% ============================================================================
% Core Assertion Operations
% ============================================================================

%! store_assert(+ConnectionId, +Term) is det.
%
% Assert a term into the database.
%
store_assert(ConnectionId, Term) :-
    must_be(ground, Term),
    
    % Extract context from term (assuming module-qualified)
    (Term = Context:ActualTerm ->
        true
    ;
        Context = user,
        ActualTerm = Term
    ),
    
    store_ensure_context(ConnectionId, Context),
    context_mapping(Context, ContextId),
    
    % Decompose term
    functor(ActualTerm, Functor, Arity),
    
    % Check for duplicates using hash
    term_to_canonical(ActualTerm, Canonical),
    sha_hash(Canonical, Hash, [algorithm(sha256)]),
    hash_atom(Hash, HashAtom),
    
    % Check if exists using prepared statement
    Query = 'SELECT formula_id FROM formulae WHERE context_id = ? AND term_hash = ?',
    odbc_prepare(ConnectionId, Query, [integer, varchar(64)], CheckStmt),
    
    (odbc_execute(CheckStmt, [ContextId, HashAtom], row(ExistingId)) ->
        % Duplicate - optionally update timestamp
        format(atom(UpdateQuery), 
               'UPDATE formulae SET updated_at = CURRENT_TIMESTAMP WHERE formula_id = ~w',
               [ExistingId]),
        odbc_query(ConnectionId, UpdateQuery, _)
    ;
        % New fact - insert
        insert_formula(ConnectionId, ContextId, Functor, Arity, 
                      ActualTerm, Canonical, HashAtom)
    ),
    
    % Update in-memory cache if loaded
    (loaded_predicate(Context, Functor, Arity) ->
        assertz(predicate_cache(Context, Functor, Arity, ActualTerm))
    ;
        true
    ),
    
    % Update statistics
    update_stats(ConnectionId, ContextId, Functor, Arity, assert).

%! insert_formula(+Conn, +CtxId, +Functor, +Arity, +Term, +Canonical, +Hash) is det.
%
% Insert formula and its indexed arguments.
%
insert_formula(ConnectionId, ContextId, Functor, Arity, Term, Canonical, Hash) :-
    % Pretty print for debugging
    with_output_to(string(Readable), write(Term)),
    
    % Insert main record using prepared statement
    InsertQuery = 'INSERT INTO formulae 
                   (context_id, functor, arity, term_canonical, term_readable, term_hash) 
                   VALUES (?, ?, ?, ?, ?, ?)',
    odbc_prepare(ConnectionId, InsertQuery, [integer, char(255), integer, longvarchar, longvarchar, char(64)], InsertStmt),
    odbc_execute(InsertStmt, [ContextId, Functor, Arity, Canonical, Readable, Hash]),
    
    % Get formula ID
    odbc_query(ConnectionId, 'SELECT LAST_INSERT_ID()', row(FormulaId)),
    
    % Index arguments if configured
    get_index_positions(ContextId, Functor, Arity, Positions),
    index_arguments(ConnectionId, FormulaId, Term, Positions),
    
    % Optionally index list elements
    (index_list_elements(true) ->
        index_list_args(ConnectionId, FormulaId, Term)
    ;
        true
    ).

%! term_to_canonical(+Term, -Canonical) is det.
%
% Convert term to canonical string form.
%
term_to_canonical(Term, Canonical) :-
    with_output_to(atom(Canonical), 
        write_canonical(Term)).

%! hash_atom(+HashBytes, -HashAtom) is det.
%
% Convert hash bytes to atom.
%
hash_atom(Hash, HashAtom) :-
    hash_atom(Hash, [], Codes),
    atom_codes(HashAtom, Codes).

hash_atom([], Acc, Codes) :-
    reverse(Acc, Codes).
hash_atom([H|T], Acc, Codes) :-
    format(codes(Hex), '~|~`0t~16r~2+', [H]),
    append(Hex, Acc, NewAcc),
    hash_atom(T, NewAcc, Codes).

% ============================================================================
% Argument Indexing
% ============================================================================

%! get_index_positions(+ContextId, +Functor, +Arity, -Positions) is det.
get_index_positions(ContextId, Functor, Arity, Positions) :-
    (index_config(ContextId, Functor, Arity, Positions) ->
        true
    ;
        default_index_positions(Functor, Arity, Positions)
    ).

%! index_arguments(+ConnectionId, +FormulaId, +Term, +Positions) is det.
index_arguments(_, _, _, []) :- !.
index_arguments(ConnectionId, FormulaId, Term, [Pos|Rest]) :-
    arg(Pos, Term, Arg),
    index_single_argument(ConnectionId, FormulaId, Pos, Arg),
    index_arguments(ConnectionId, FormulaId, Term, Rest).

%! index_single_argument(+ConnectionId, +FormulaId, +Position, +Arg) is det.
index_single_argument(ConnectionId, FormulaId, Position, Arg) :-
    (atomic(Arg) ->
        insert_indexed_arg(ConnectionId, FormulaId, Position, Arg)
    ;
        true  % Skip complex terms
    ).

%! insert_indexed_arg(+ConnectionId, +FormulaId, +Position, +Value) is det.
insert_indexed_arg(ConnectionId, FormulaId, Position, Value) :-
    % Determine type and appropriate column
    (atom(Value) ->
        ArgType = 'atom',
        atom_string(Value, ValueStr),
        Query = 'INSERT INTO arguments_indexed (formula_id, arg_position, arg_type, atom_value) VALUES (?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, char(10), char(255)], Stmt),
        odbc_execute(Stmt, [FormulaId, Position, ArgType, ValueStr])
    ; integer(Value) ->
        ArgType = 'integer',
        Query = 'INSERT INTO arguments_indexed (formula_id, arg_position, arg_type, int_value) VALUES (?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, char(10), integer], Stmt),
        odbc_execute(Stmt, [FormulaId, Position, ArgType, Value])
    ; float(Value) ->
        ArgType = 'float',
        Query = 'INSERT INTO arguments_indexed (formula_id, arg_position, arg_type, float_value) VALUES (?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, char(10), float], Stmt),
        odbc_execute(Stmt, [FormulaId, Position, ArgType, Value])
    ; string(Value) ->
        ArgType = 'string',
        Query = 'INSERT INTO arguments_indexed (formula_id, arg_position, arg_type, string_value) VALUES (?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, char(10), char(255)], Stmt),
        odbc_execute(Stmt, [FormulaId, Position, ArgType, Value])
    ;
        % Default: treat as atom
        atom_string(Value, ValueStr),
        ArgType = 'atom',
        Query = 'INSERT INTO arguments_indexed (formula_id, arg_position, arg_type, atom_value) VALUES (?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, char(10), char(255)], Stmt),
        odbc_execute(Stmt, [FormulaId, Position, ArgType, ValueStr])
    ).

%! index_list_args(+ConnectionId, +FormulaId, +Term) is det.
index_list_args(ConnectionId, FormulaId, Term) :-
    Term =.. [_|Args],
    index_list_args_helper(ConnectionId, FormulaId, Args, 1).

index_list_args_helper(_, _, [], _).
index_list_args_helper(ConnectionId, FormulaId, [Arg|Rest], Pos) :-
    (is_list(Arg) ->
        index_list_elements_internal(ConnectionId, FormulaId, Pos, Arg)
    ;
        true
    ),
    NextPos is Pos + 1,
    index_list_args_helper(ConnectionId, FormulaId, Rest, NextPos).

index_list_elements_internal(_, _, _, []).
index_list_elements_internal(ConnectionId, FormulaId, ArgPos, List) :-
    index_list_elements_internal(ConnectionId, FormulaId, ArgPos, List, 0).

index_list_elements_internal(_, _, _, [], _).
index_list_elements_internal(ConnectionId, FormulaId, ArgPos, [Elem|Rest], ElemPos) :-
    (atomic(Elem) ->
        NextElemPos is ElemPos + 1,
        (atom(Elem) ->
            ElemType = 'atom',
            atom_string(Elem, ElemStr)
        ; integer(Elem) ->
            ElemType = 'integer',
            atom_string(Elem, ElemStr)
        ; float(Elem) ->
            ElemType = 'float',
            atom_string(Elem, ElemStr)
        ; 
            ElemType = 'atom',
            atom_string(Elem, ElemStr)
        ),
        Query = 'INSERT INTO list_elements (formula_id, arg_position, element_position, element_type, element_value) VALUES (?, ?, ?, ?, ?)',
        odbc_prepare(ConnectionId, Query, [integer, integer, integer, char(10), char(255)], Stmt),
        odbc_execute(Stmt, [FormulaId, ArgPos, NextElemPos, ElemType, ElemStr]),
        index_list_elements_internal(ConnectionId, FormulaId, ArgPos, Rest, NextElemPos)
    ;
        % Skip compound terms
        NextElemPos is ElemPos + 1,
        index_list_elements_internal(ConnectionId, FormulaId, ArgPos, Rest, NextElemPos)
    ).

% ============================================================================
% Retraction Operations
% ============================================================================

%! store_retract(+ConnectionId, +Pattern) is nondet.
%
% Retract first matching term.
%
store_retract(ConnectionId, Pattern) :-
    (Pattern = Context:ActualPattern ->
        true
    ;
        Context = user,
        ActualPattern = Pattern
    ),
    
    context_mapping(Context, ContextId),
    functor(ActualPattern, Functor, Arity),
    
    % Ensure predicate is loaded so we can match against cache
    ensure_loaded(ConnectionId, Context, ContextId, Functor, Arity),
    
    % Find matching term in cache
    predicate_cache(Context, Functor, Arity, CachedTerm),
    subsumes_term(ActualPattern, CachedTerm),
    ActualPattern = CachedTerm,
    
    % Get the hash of the actual ground term
    term_to_canonical(CachedTerm, Canonical),
    sha_hash(Canonical, Hash, [algorithm(sha256)]),
    hash_atom(Hash, HashAtom),
    
    % Find and delete from database
    Query = 'SELECT formula_id FROM formulae WHERE context_id = ? AND term_hash = ?',
    odbc_prepare(ConnectionId, Query, [integer, varchar(64)], Stmt),
    odbc_execute(Stmt, [ContextId, HashAtom], row(FormulaId)),
    
    format(atom(DeleteQuery), 'DELETE FROM formulae WHERE formula_id = ~w', [FormulaId]),
    odbc_query(ConnectionId, DeleteQuery, _),
    
    % Remove from cache
    retractall(predicate_cache(Context, Functor, Arity, CachedTerm)),
    
    % Update stats
    update_stats(ConnectionId, ContextId, Functor, Arity, retract).
    update_stats(ConnectionId, ContextId, Functor, Arity, retract).

%! store_retractall(+ConnectionId, +Pattern) is det.
%
% Retract all matching terms.
%
store_retractall(ConnectionId, Pattern) :-
    forall(store_retract(ConnectionId, Pattern), true).

%! store_abolish(+ConnectionId, +Functor/Arity) is det.
%
% Remove all facts for a predicate.
%
store_abolish(ConnectionId, Context:Functor/Arity) :-
    !,
    store_abolish_impl(ConnectionId, Context, Functor, Arity).
store_abolish(ConnectionId, Functor/Arity) :-
    store_abolish_impl(ConnectionId, user, Functor, Arity).

store_abolish_impl(ConnectionId, Context, Functor, Arity) :-
    context_mapping(Context, ContextId),
    
    % Delete from database using prepared statement
    Query = 'DELETE FROM formulae WHERE context_id = ? AND functor = ? AND arity = ?',
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    odbc_execute(Stmt, [ContextId, Functor, Arity]),
    
    % Clear cache
    retractall(loaded_predicate(Context, Functor, Arity)),
    retractall(predicate_cache(Context, Functor, Arity, _)),
    
    % Clear stats
    StatsQuery = 'DELETE FROM predicate_stats WHERE context_id = ? AND functor = ? AND arity = ?',
    odbc_prepare(ConnectionId, StatsQuery, [integer, default, integer], StatsStmt),
    odbc_execute(StatsStmt, [ContextId, Functor, Arity]).

% ============================================================================
% Query Operations
% ============================================================================

%! store_call(+ConnectionId, +Goal) is nondet.
%
% Query the database.
%
store_call(ConnectionId, Goal) :-
    (Goal = Context:ActualGoal ->
        true
    ;
        Context = user,
        ActualGoal = Goal
    ),
    
    context_mapping(Context, ContextId),
    functor(ActualGoal, Functor, Arity),
    
    % Ensure predicate is loaded
    ensure_loaded(ConnectionId, Context, ContextId, Functor, Arity),
    
    % Query from cache
    predicate_cache(Context, Functor, Arity, CachedTerm),
    subsumes_term(ActualGoal, CachedTerm),
    ActualGoal = CachedTerm,
    
    % Update statistics
    update_stats(ConnectionId, ContextId, Functor, Arity, query).

%! ensure_loaded(+Conn, +Context, +CtxId, +Functor, +Arity) is det.
%
% Ensure predicate is loaded into memory.
%
ensure_loaded(ConnectionId, Context, ContextId, Functor, Arity) :-
    (loaded_predicate(Context, Functor, Arity) ->
        true
    ;
        load_predicate_internal(ConnectionId, Context, ContextId, Functor, Arity),
        assertz(loaded_predicate(Context, Functor, Arity))
    ).

%! load_predicate_internal(+Conn, +Context, +CtxId, +Functor, +Arity) is det.
%
% Load all facts for a predicate.
%
load_predicate_internal(ConnectionId, Context, ContextId, Functor, Arity) :-
    Query = 'SELECT term_canonical FROM formulae WHERE context_id = ? AND functor = ? AND arity = ?',
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    
    findall(
        Term,
        (odbc_execute(Stmt, [ContextId, Functor, Arity], row(Canonical)),
         atom_to_term(Canonical, Term, [])),
        Terms
    ),
    
    % Cache all terms
    forall(
        member(T, Terms),
        assertz(predicate_cache(Context, Functor, Arity, T))
    ),
    
    % Update cache status
    length(Terms, Count),
    get_time(Now),
    UpdateQuery = 'INSERT INTO cache_status 
                   (context_id, functor, arity, is_loaded, load_time, fact_count)
                   VALUES (?, ?, ?, TRUE, FROM_UNIXTIME(?), ?)
                   ON DUPLICATE KEY UPDATE 
                   is_loaded = TRUE, load_time = FROM_UNIXTIME(?), fact_count = ?',
    odbc_prepare(ConnectionId, UpdateQuery, [integer, default, integer, float, integer, float, integer], UpdateStmt),
    odbc_execute(UpdateStmt, [ContextId, Functor, Arity, Now, Count, Now, Count]).

%! store_findall(+ConnectionId, +Template, +Goal, -Results) is det.
%
% Findall via database.
%
store_findall(ConnectionId, Template, Goal, Results) :-
    findall(Template, store_call(ConnectionId, Goal), Results).

% ============================================================================
% Cache Management
% ============================================================================

%! store_load_predicate(+ConnectionId, +Context, +Functor/Arity) is det.
%
% Explicitly load a predicate into cache.
%
store_load_predicate(ConnectionId, Context, Functor/Arity) :-
    context_mapping(Context, ContextId),
    ensure_loaded(ConnectionId, Context, ContextId, Functor, Arity).

%! store_unload_predicate(+ConnectionId, +Context, +Functor/Arity) is det.
%
% Unload predicate from cache.
%
store_unload_predicate(_ConnectionId, Context, Functor/Arity) :-
    retractall(loaded_predicate(Context, Functor, Arity)),
    retractall(predicate_cache(Context, Functor, Arity, _)).

%! store_sync(+ConnectionId) is det.
%
% Synchronize in-memory changes to database.
%
store_sync(ConnectionId) :-
    % Currently all operations are write-through
    % This is a placeholder for future write-behind caching
    catch(odbc_query(ConnectionId, 'COMMIT', _), _, true).

% ============================================================================
% Statistics & Optimization
% ============================================================================

%! update_stats(+Conn, +CtxId, +Functor, +Arity, +Operation) is det.
%
% Update predicate statistics.
%
update_stats(ConnectionId, ContextId, Functor, Arity, Operation) :-
    operation_column(Operation, Column),
    format(atom(Query),
           'INSERT INTO predicate_stats (context_id, functor, arity, ~w) 
            VALUES (?, ?, ?, 1)
            ON DUPLICATE KEY UPDATE ~w = ~w + 1',
           [Column, Column, Column]),
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    odbc_execute(Stmt, [ContextId, Functor, Arity]).

operation_column(query, 'query_count').
operation_column(assert, 'assert_count').
operation_column(retract, 'retract_count').

%! store_stats(+ConnectionId, +Context, -Stats) is det.
%
% Get statistics for a context.
%
store_stats(ConnectionId, Context, Stats) :-
    context_mapping(Context, ContextId),
    Query = 'SELECT functor, arity, query_count, assert_count, retract_count
             FROM predicate_stats
             WHERE context_id = ?
             ORDER BY query_count DESC',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    findall(
        stats(Functor/Arity, Queries, Asserts, Retracts),
        odbc_execute(Stmt, [ContextId], row(Functor, Arity, Queries, Asserts, Retracts)),
        Stats
    ).

%! store_optimize(+ConnectionId, +Context) is det.
%
% Optimize database based on usage patterns.
%
store_optimize(ConnectionId, Context) :-
    context_mapping(Context, ContextId),
    
    % Find hot predicates
    Query = 'SELECT functor, arity FROM predicate_stats
             WHERE context_id = ? AND query_count > 1000
             ORDER BY query_count DESC LIMIT 10',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(
        Functor/Arity,
        odbc_execute(Stmt, [ContextId], row(Functor, Arity)),
        HotPredicates
    ),
    
    % Pre-load hot predicates
    forall(
        member(F/A, HotPredicates),
        (loaded_predicate(Context, F, A) ->
            true
        ;
            load_predicate_internal(ConnectionId, Context, ContextId, F, A)
        )
    ),
    
    % Analyze tables
    odbc_query(ConnectionId, 'ANALYZE TABLE formulae', _),
    odbc_query(ConnectionId, 'ANALYZE TABLE arguments_indexed', _).

% ============================================================================
% Utility Predicates
% ============================================================================

%! must_be(+Type, +Term) is det.
%
% Type checking utility.
%
must_be(ground, Term) :-
    (ground(Term) ->
        true
    ;
        throw(error(instantiation_error, _))
    ).
must_be(atom, Term) :-
    (atom(Term) ->
        true
    ;
        throw(error(type_error(atom, Term), _))
    ).
