% ============================================================================
% Utility Module: Inspection and Bulk Retrieval
% ============================================================================
%
% This module provides utility predicates for inspecting and retrieving
% stored terms across all storage backends (ground facts, templates, clauses).
%

:- module(mysql_store_utils, [
    % Bulk retrieval (both 2 and 3 argument versions)
    all_term_assertions/2,
    all_term_assertions/3,
    all_ground_assertions/2,
    all_ground_assertions/3,
    all_template_assertions/2,
    all_template_assertions/3,
    all_clause_assertions/2,
    all_clause_assertions/3,
    
    % Counting
    count_assertions/2,
    count_assertions/3,
    
    % Context inspection
    list_contexts/2,
    context_summary/3,
    
    % Predicate inspection
    list_predicates/3,
    predicate_info/4,
    
    % Search and filtering (both 3 and 4 argument versions)
    find_assertions_by_functor/3,
    find_assertions_by_functor/4,
    find_assertions_matching/3,
    find_assertions_matching/4,
    
    % Debugging and export
    dump_context/2,
    dump_context/3
]).

:- use_module(mysql_store).
:- use_module(library(odbc)).

% Try to load optional modules (may not always be present)
:- catch(use_module(mysql_store_templates), _, true).
:- catch(use_module(mysql_store_clauses), _, true).

% ============================================================================
% Bulk Retrieval
% ============================================================================

%! all_term_assertions(+ConnectionId, +Context, -Assertions) is det.
%
% Retrieve ALL assertions from a context, including:
% - Ground facts (from main formulae table)
% - Templates (from formula_templates table)
% - Clauses (from formula_clauses table)
%
% @arg ConnectionId The database connection
% @arg Context The context name (atom)
% @arg Assertions List of all terms in format Context:Term
%
all_term_assertions(ConnectionId, Context, Assertions) :-
    must_be(atom, Context),
    
    % Get context ID
    (mysql_store:context_mapping(Context, ContextId) ->
        true
    ;
        % Context doesn't exist
        Assertions = []
    ),
    
    % Collect from all sources
    all_ground_assertions_internal(ConnectionId, ContextId, Context, GroundFacts),
    all_template_assertions_internal(ConnectionId, ContextId, Context, Templates),
    all_clause_assertions_internal(ConnectionId, ContextId, Context, Clauses),
    
    % Combine all
    append(GroundFacts, Templates, Temp),
    append(Temp, Clauses, Assertions).

%! all_term_assertions(+Context, -Assertions) is det.
%
% Retrieve all assertions using the implicit connection.
% Note: This assumes a single active connection stored in connection/2.
%
all_term_assertions(Context, Assertions) :-
    % Find an active connection
    (mysql_store:connection(_, ConnectionId) ->
        all_term_assertions(ConnectionId, Context, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

% ============================================================================
% Retrieve Ground Facts
% ============================================================================

%! all_ground_assertions(+ConnectionId, +Context, -Assertions) is det.
%! all_ground_assertions(+Context, -Assertions) is det.
%
% Retrieve only ground facts (no variables).
%
all_ground_assertions(ConnectionId, Context, Assertions) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        all_ground_assertions_internal(ConnectionId, ContextId, Context, Assertions)
    ;
        Assertions = []
    ).

all_ground_assertions(Context, Assertions) :-
    (mysql_store:connection(_, ConnectionId) ->
        all_ground_assertions(ConnectionId, Context, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

all_ground_assertions_internal(ConnectionId, ContextId, Context, Assertions) :-
    Query = 'SELECT term_canonical FROM formulae WHERE context_id = ? ORDER BY formula_id',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(Context:Term,
            (odbc_execute(Stmt, [ContextId], row(Canonical)),
             atom_to_term(Canonical, Term, [])),
            Assertions).

% ============================================================================
% Retrieve Templates
% ============================================================================

%! all_template_assertions(+ConnectionId, +Context, -Assertions) is det.
%! all_template_assertions(+Context, -Assertions) is det.
%
% Retrieve only template assertions (terms with variables).
%
all_template_assertions(ConnectionId, Context, Assertions) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        all_template_assertions_internal(ConnectionId, ContextId, Context, Assertions)
    ;
        Assertions = []
    ).

all_template_assertions(Context, Assertions) :-
    (mysql_store:connection(_, ConnectionId) ->
        all_template_assertions(ConnectionId, Context, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

all_template_assertions_internal(ConnectionId, ContextId, Context, Assertions) :-
    % Check if templates table exists
    (table_exists(ConnectionId, 'formula_templates') ->
        Query = 'SELECT template_repr FROM formula_templates 
                 WHERE context_id = ? ORDER BY template_id',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        
        findall(Context:Term,
                (odbc_execute(Stmt, [ContextId], row(TemplateRepr)),
                 read_term_from_atom(TemplateRepr, Term, [])),
                Assertions)
    ;
        Assertions = []
    ).

% ============================================================================
% Retrieve Clauses
% ============================================================================

%! all_clause_assertions(+ConnectionId, +Context, -Assertions) is det.
%! all_clause_assertions(+Context, -Assertions) is det.
%
% Retrieve only clause assertions (facts and rules with variables).
%
all_clause_assertions(ConnectionId, Context, Assertions) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        all_clause_assertions_internal(ConnectionId, ContextId, Context, Assertions)
    ;
        Assertions = []
    ).

all_clause_assertions(Context, Assertions) :-
    (mysql_store:connection(_, ConnectionId) ->
        all_clause_assertions(ConnectionId, Context, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

all_clause_assertions_internal(ConnectionId, ContextId, Context, Assertions) :-
    % Check if clauses table exists
    (table_exists(ConnectionId, 'formula_clauses') ->
        Query = 'SELECT clause_type, head_repr, body_repr FROM formula_clauses
                 WHERE context_id = ? ORDER BY clause_id',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        
        findall(Context:Clause,
                (odbc_execute(Stmt, [ContextId], row(ClauseType, HeadRepr, BodyRepr)),
                 reconstruct_clause(ClauseType, HeadRepr, BodyRepr, Clause)),
                Assertions)
    ;
        Assertions = []
    ).

reconstruct_clause(fact, HeadRepr, _BodyRepr, Fact) :-
    read_term_from_atom(HeadRepr, Fact, []).

reconstruct_clause(rule, HeadRepr, BodyRepr, (Head :- Body)) :-
    read_term_from_atom(HeadRepr, Head, []),
    read_term_from_atom(BodyRepr, Body, []).

% ============================================================================
% Counting
% ============================================================================

%! count_assertions(+ConnectionId, +Context, -Count) is det.
%
% Count total number of assertions in a context.
%
count_assertions(ConnectionId, Context, Count) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        count_ground_facts(ConnectionId, ContextId, GroundCount),
        count_templates(ConnectionId, ContextId, TemplateCount),
        count_clauses(ConnectionId, ContextId, ClauseCount),
        Count is GroundCount + TemplateCount + ClauseCount
    ;
        Count = 0
    ).

%! count_assertions(+Context, -Count) is det.
%
count_assertions(Context, Count) :-
    (mysql_store:connection(_, ConnectionId) ->
        count_assertions(ConnectionId, Context, Count)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

count_ground_facts(ConnectionId, ContextId, Count) :-
    Query = 'SELECT COUNT(*) FROM formulae WHERE context_id = ?',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    (odbc_execute(Stmt, [ContextId], row(Count)) -> true ; Count = 0).

count_templates(ConnectionId, ContextId, Count) :-
    (table_exists(ConnectionId, 'formula_templates') ->
        Query = 'SELECT COUNT(*) FROM formula_templates WHERE context_id = ?',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        (odbc_execute(Stmt, [ContextId], row(Count)) -> true ; Count = 0)
    ;
        Count = 0
    ).

count_clauses(ConnectionId, ContextId, Count) :-
    (table_exists(ConnectionId, 'formula_clauses') ->
        Query = 'SELECT COUNT(*) FROM formula_clauses WHERE context_id = ?',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        (odbc_execute(Stmt, [ContextId], row(Count)) -> true ; Count = 0)
    ;
        Count = 0
    ).

% ============================================================================
% Context Inspection
% ============================================================================

%! list_contexts(+ConnectionId, -Contexts) is det.
%
% List all contexts in the database.
%
list_contexts(ConnectionId, Contexts) :-
    Query = 'SELECT context_name FROM contexts ORDER BY context_name',
    findall(ContextName,
            odbc_query(ConnectionId, Query, row(ContextName)),
            Contexts).

%! context_summary(+ConnectionId, +Context, -Summary) is det.
%
% Get summary statistics for a context.
%
context_summary(ConnectionId, Context, Summary) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        count_ground_facts(ConnectionId, ContextId, GroundCount),
        count_templates(ConnectionId, ContextId, TemplateCount),
        count_clauses(ConnectionId, ContextId, ClauseCount),
        TotalCount is GroundCount + TemplateCount + ClauseCount,
        
        % Get unique functors
        Query = 'SELECT COUNT(DISTINCT functor) FROM formulae WHERE context_id = ?',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        (odbc_execute(Stmt, [ContextId], row(UniqueFunctors)) -> true ; UniqueFunctors = 0),
        
        Summary = summary(
            context(Context),
            total(TotalCount),
            ground_facts(GroundCount),
            templates(TemplateCount),
            clauses(ClauseCount),
            unique_functors(UniqueFunctors)
        )
    ;
        Summary = summary(context(Context), total(0), ground_facts(0), 
                         templates(0), clauses(0), unique_functors(0))
    ).

% ============================================================================
% Predicate Inspection
% ============================================================================

%! list_predicates(+ConnectionId, +Context, -Predicates) is det.
%
% List all predicate functors and arities in a context.
%
list_predicates(ConnectionId, Context, Predicates) :-
    must_be(atom, Context),
    (mysql_store:context_mapping(Context, ContextId) ->
        Query = 'SELECT DISTINCT functor, arity FROM formulae 
                 WHERE context_id = ? 
                 ORDER BY functor, arity',
        odbc_prepare(ConnectionId, Query, [integer], Stmt),
        
        findall(Functor/Arity,
                odbc_execute(Stmt, [ContextId], row(Functor, Arity)),
                Predicates)
    ;
        Predicates = []
    ).

%! predicate_info(+ConnectionId, +Context, +Functor/Arity, -Info) is det.
%
% Get detailed information about a specific predicate.
%
predicate_info(ConnectionId, Context, Functor/Arity, Info) :-
    must_be(atom, Context),
    must_be(atom, Functor),
    must_be(integer, Arity),
    
    (mysql_store:context_mapping(Context, ContextId) ->
        % Count ground facts
        Query1 = 'SELECT COUNT(*) FROM formulae 
                  WHERE context_id = ? AND functor = ? AND arity = ?',
        odbc_prepare(ConnectionId, Query1, [integer, default, integer], Stmt1),
        (odbc_execute(Stmt1, [ContextId, Functor, Arity], row(GroundCount)) -> true ; GroundCount = 0),
        
        % Count templates
        (table_exists(ConnectionId, 'formula_templates') ->
            Query2 = 'SELECT COUNT(*) FROM formula_templates 
                      WHERE context_id = ? AND functor = ? AND arity = ?',
            odbc_prepare(ConnectionId, Query2, [integer, default, integer], Stmt2),
            (odbc_execute(Stmt2, [ContextId, Functor, Arity], row(TemplateCount)) -> true ; TemplateCount = 0)
        ;
            TemplateCount = 0
        ),
        
        TotalCount is GroundCount + TemplateCount,
        
        Info = info(
            predicate(Functor/Arity),
            total(TotalCount),
            ground(GroundCount),
            templates(TemplateCount)
        )
    ;
        Info = info(predicate(Functor/Arity), total(0), ground(0), templates(0))
    ).

% ============================================================================
% Search and Filtering
% ============================================================================

%! find_assertions_by_functor(+ConnectionId, +Context, +Functor, -Assertions) is det.
%! find_assertions_by_functor(+Context, +Functor, -Assertions) is det.
%
% Find all assertions with a specific functor (any arity).
%
find_assertions_by_functor(ConnectionId, Context, Functor, Assertions) :-
    must_be(atom, Context),
    must_be(atom, Functor),
    
    all_term_assertions(ConnectionId, Context, AllAssertions),
    findall(Assertion,
            (member(Assertion, AllAssertions),
             Assertion = _:Term,
             functor(Term, Functor, _)),
            Assertions).

find_assertions_by_functor(Context, Functor, Assertions) :-
    (mysql_store:connection(_, ConnectionId) ->
        find_assertions_by_functor(ConnectionId, Context, Functor, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

%! find_assertions_matching(+ConnectionId, +Context, +Pattern, -Assertions) is det.
%! find_assertions_matching(+Context, +Pattern, -Assertions) is det.
%
% Find all assertions that unify with the given pattern.
%
find_assertions_matching(ConnectionId, Context, Pattern, Assertions) :-
    must_be(atom, Context),
    
    all_term_assertions(ConnectionId, Context, AllAssertions),
    findall(Assertion,
            (member(Assertion, AllAssertions),
             Assertion = Context:Term,
             \+ \+ (Term = Pattern)),  % Check if unifiable without binding
            Assertions).

find_assertions_matching(Context, Pattern, Assertions) :-
    (mysql_store:connection(_, ConnectionId) ->
        find_assertions_matching(ConnectionId, Context, Pattern, Assertions)
    ;
        throw(error(no_connection, 'No active database connection found'))
    ).

% ============================================================================
% Debugging and Export
% ============================================================================

%! dump_context(+ConnectionId, +Context) is det.
%
% Print all assertions in a context to stdout.
%
dump_context(ConnectionId, Context) :-
    dump_context(ConnectionId, Context, user_output).

%! dump_context(+ConnectionId, +Context, +Stream) is det.
%
% Write all assertions in a context to a stream.
%
dump_context(ConnectionId, Context, Stream) :-
    must_be(atom, Context),
    
    % Print header
    format(Stream, '~n% ======================================~n', []),
    format(Stream, '% Context: ~w~n', [Context]),
    format(Stream, '% ======================================~n~n', []),
    
    % Get and print summary
    context_summary(ConnectionId, Context, Summary),
    format(Stream, '% Summary: ~w~n~n', [Summary]),
    
    % Print ground facts
    all_ground_assertions(ConnectionId, Context, GroundFacts),
    (GroundFacts \= [] ->
        format(Stream, '% --- Ground Facts ---~n', []),
        forall(member(Assertion, GroundFacts),
               format(Stream, '~q.~n', [Assertion])),
        format(Stream, '~n', [])
    ;
        true
    ),
    
    % Print templates
    all_template_assertions(ConnectionId, Context, Templates),
    (Templates \= [] ->
        format(Stream, '% --- Templates ---~n', []),
        forall(member(Assertion, Templates),
               format(Stream, '~q.~n', [Assertion])),
        format(Stream, '~n', [])
    ;
        true
    ),
    
    % Print clauses
    all_clause_assertions(ConnectionId, Context, Clauses),
    (Clauses \= [] ->
        format(Stream, '% --- Clauses ---~n', []),
        forall(member(Assertion, Clauses),
               format(Stream, '~q.~n', [Assertion])),
        format(Stream, '~n', [])
    ;
        true
    ).

% ============================================================================
% Helper Predicates
% ============================================================================

%! table_exists(+ConnectionId, +TableName) is semidet.
%
% Check if a table exists in the database.
%
table_exists(ConnectionId, TableName) :-
    % SHOW TABLES doesn't work well with parameters in some ODBC drivers
    % So we build the query directly (TableName is trusted, it's a constant atom)
    format(atom(Query), 'SHOW TABLES LIKE \'~w\'', [TableName]),
    catch(
        odbc_query(ConnectionId, Query, row(_)),
        _,
        fail
    ).

% ============================================================================
% Examples and Tests
% ============================================================================

:- if(false).

test_all_assertions :-
    % Setup
    store_connect(testdb, 'localhost', 'prolog_store', 'prolog', 'password'),
    store_ensure_context(testdb, test),
    
    % Add some ground facts
    format('Adding ground facts...~n'),
    store_assert(testdb, test:person(john, 30)),
    store_assert(testdb, test:person(jane, 25)),
    store_assert(testdb, test:parent(john, alice)),
    
    % Add some templates (if module available)
    format('Adding templates...~n'),
    catch(
        (store_template(testdb, test:hasPrecondition(_Op, _Conds)),
         store_template(testdb, test:hasPostcondition(_Op2, _Results))),
        error(existence_error(procedure, store_template/2), _),
        format('Template module not available~n')
    ),
    
    % Test all_term_assertions/2
    format('~nRetrieving all assertions:~n'),
    all_term_assertions(test, Assertions),
    length(Assertions, Count),
    format('Found ~w assertions~n', [Count]),
    forall(member(A, Assertions), format('  ~q~n', [A])),
    
    % Test counting
    format('~nCounting assertions:~n'),
    count_assertions(test, Total),
    format('Total: ~w~n', [Total]),
    
    % Test context summary
    format('~nContext summary:~n'),
    context_summary(testdb, test, Summary),
    format('~w~n', [Summary]),
    
    % Test predicate listing
    format('~nPredicates in context:~n'),
    list_predicates(testdb, test, Predicates),
    forall(member(P, Predicates), format('  ~w~n', [P])),
    
    % Test find by functor
    format('~nFinding all person/2 assertions:~n'),
    find_assertions_by_functor(testdb, test, person, PersonAssertions),
    forall(member(P, PersonAssertions), format('  ~q~n', [P])),
    
    % Test dump
    format('~nDumping context:~n'),
    dump_context(testdb, test),
    
    % Cleanup
    store_disconnect(testdb).

:- endif.
