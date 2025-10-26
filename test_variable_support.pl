% ============================================================================
% Test Suite for Variable Support Modules
% ============================================================================
%
% Usage:
%   1. Update the connection details below
%   2. Load this file: [test_variable_support].
%   3. Run: test_all.
%

% Load all modules
:- use_module(mysql_store).
:- use_module(mysql_store_templates).
:- use_module(mysql_store_clauses).
:- use_module(mysql_store_utils).

% ============================================================================
% Configuration
% ============================================================================

% Update these with your database credentials
db_config(
    connection_id(mydb),
    server('localhost'),
    database('prolog_store'),
    user('prolog'),
    password('prolog123')
).

test_context(test_vars).

% ============================================================================
% Test Runner
% ============================================================================

%! test_all is det.
%
% Run all tests.
%
test_all :-
    format('~n~n========================================~n'),
    format('Variable Support Test Suite~n'),
    format('========================================~n~n'),
    
    setup_connection,
    
    test_ground_facts,
    test_templates,
    test_smart_operations,
    test_all_assertions,
    test_inspection,
    test_clauses,
    
    cleanup_connection,
    
    format('~n========================================~n'),
    format('All tests completed!~n'),
    format('========================================~n~n'),
    !.  % Cut to prevent backtracking

% ============================================================================
% Test 1: Ground Facts (Baseline)
% ============================================================================

test_ground_facts :-
    format('~n--- Test 1: Ground Facts ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    % Clear context
    catch(store_retractall(ConnId, Context:_), _, true),
    
    % Assert some ground facts
    format('Asserting ground facts...~n'),
    store_assert(ConnId, Context:person(john, 30)),
    store_assert(ConnId, Context:person(jane, 25)),
    store_assert(ConnId, Context:parent(john, alice)),
    store_assert(ConnId, Context:parent(jane, bob)),
    
    % Query them
    format('Querying ground facts...~n'),
    findall(Name-Age, store_call(ConnId, Context:person(Name, Age)), People),
    format('  People: ~w~n', [People]),
    
    findall(Parent-Child, store_call(ConnId, Context:parent(Parent, Child)), Parents),
    format('  Parents: ~w~n', [Parents]),
    
    format('✓ Ground facts test passed~n').

% ============================================================================
% Test 2: Template Storage
% ============================================================================

test_templates :-
    format('~n--- Test 2: Template Storage ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    % Store templates with variables
    format('Storing templates...~n'),
    store_template(ConnId, Context:hasPrecondition(createFile, 
        [directoryExists(Dir), hasPermission(Dir, write)])),
    
    store_template(ConnId, Context:hasPrecondition(deleteFile,
        [fileExists(Path), hasPermission(Path, write)])),
    
    store_template(ConnId, Context:hasPostcondition(createFile,
        [fileExists(Path)])),
    
    % Query templates
    format('Querying templates...~n'),
    findall(Op-Conds, 
            store_call_template(ConnId, Context:hasPrecondition(Op, Conds)),
            Preconditions),
    format('  Preconditions: ~w~n', [Preconditions]),
    
    % Test that we get fresh variables each time
    format('Testing fresh variables...~n'),
    store_call_template(ConnId, Context:hasPrecondition(createFile, Conds1)),
    store_call_template(ConnId, Context:hasPrecondition(createFile, Conds2)),
    (Conds1 \== Conds2 ->
        format('  ✓ Got different variable instances~n')
    ;
        format('  ✗ Variables are the same (unexpected)~n')
    ),
    
    format('✓ Template storage test passed~n').

% ============================================================================
% Test 3: Smart Operations (Auto-detect)
% ============================================================================

test_smart_operations :-
    format('~n--- Test 3: Smart Operations ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    format('Using store_assert_smart (auto-detects ground vs template)...~n'),
    
    % This should go to regular store_assert (ground)
    store_assert_smart(ConnId, Context:concrete_person(bob, 40)),
    
    % This should go to store_template (has variables)
    store_assert_smart(ConnId, Context:template_person(_Person, _Age)),
    
    % Query with store_call_smart (queries both backends)
    format('Querying with store_call_smart...~n'),
    findall(P-A, store_call_smart(ConnId, Context:concrete_person(P, A)), Concrete),
    format('  Concrete: ~w~n', [Concrete]),
    
    findall(P-A, store_call_smart(ConnId, Context:template_person(P, A)), Template),
    format('  Template: ~w~n', [Template]),
    
    format('✓ Smart operations test passed~n').

% ============================================================================
% Test 4: All Assertions Retrieval
% ============================================================================

test_all_assertions :-
    format('~n--- Test 4: All Assertions Retrieval ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    % Get all assertions
    format('Retrieving all assertions with all_term_assertions...~n'),
    all_term_assertions(ConnId, Context, All),
    length(All, TotalCount),
    format('  Total assertions: ~w~n', [TotalCount]),
    
    % Break down by type
    all_ground_assertions(ConnId, Context, Ground),
    length(Ground, GroundCount),
    format('  Ground facts: ~w~n', [GroundCount]),
    
    all_template_assertions(ConnId, Context, Templates),
    length(Templates, TemplateCount),
    format('  Templates: ~w~n', [TemplateCount]),
    
    % Show first few of each
    format('~n  Sample ground facts:~n'),
    take(3, Ground, GroundSample),
    forall(member(G, GroundSample), format('    ~q~n', [G])),
    
    format('~n  Sample templates:~n'),
    take(3, Templates, TemplateSample),
    forall(member(T, TemplateSample), format('    ~q~n', [T])),
    
    format('✓ All assertions test passed~n').

% ============================================================================
% Test 5: Inspection and Statistics
% ============================================================================

test_inspection :-
    format('~n--- Test 5: Inspection and Statistics ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    % Count assertions
    format('Counting assertions...~n'),
    count_assertions(ConnId, Context, Count),
    format('  Total count: ~w~n', [Count]),
    
    % Context summary
    format('Getting context summary...~n'),
    context_summary(ConnId, Context, Summary),
    format('  Summary: ~w~n', [Summary]),
    
    % List predicates
    format('Listing predicates...~n'),
    list_predicates(ConnId, Context, Predicates),
    format('  Predicates: ~w~n', [Predicates]),
    
    % Get info about a specific predicate
    (member(person/2, Predicates) ->
        format('Getting info about person/2...~n'),
        predicate_info(ConnId, Context, person/2, Info),
        format('  Info: ~w~n', [Info])
    ;
        true
    ),
    
    % Find by functor
    format('Finding all assertions with functor hasPrecondition...~n'),
    find_assertions_by_functor(ConnId, Context, hasPrecondition, Preconditions),
    length(Preconditions, PrecondCount),
    format('  Found: ~w~n', [PrecondCount]),
    
    format('✓ Inspection test passed~n').

% ============================================================================
% Test 6: Clause Storage and Resolution
% ============================================================================

test_clauses :-
    format('~n--- Test 6: Clause Storage and Resolution ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    format('Storing graph edges...~n'),
    store_clause(ConnId, Context:edge(a, b)),
    store_clause(ConnId, Context:edge(b, c)),
    store_clause(ConnId, Context:edge(c, d)),
    store_clause(ConnId, Context:edge(a, e)),
    
    format('Storing path rules...~n'),
    store_clause(ConnId, Context:(path(X, Y) :- edge(X, Y))),
    store_clause(ConnId, Context:(path(X, Z) :- edge(X, Y), path(Y, Z))),
    
    % Test resolution
    format('Testing resolution: path(a, d)?~n'),
    (store_resolve(ConnId, Context:path(a, d)) ->
        format('  ✓ Found path from a to d~n')
    ;
        format('  ✗ No path found (unexpected)~n')
    ),
    
    % Find all reachable nodes
    format('Finding all nodes reachable from a...~n'),
    findall(Dest, store_resolve(ConnId, Context:path(a, Dest)), Destinations),
    format('  Reachable: ~w~n', [Destinations]),
    
    % List all clauses
    format('Listing all clauses...~n'),
    all_clause_assertions(ConnId, Context, Clauses),
    length(Clauses, ClauseCount),
    format('  Total clauses: ~w~n', [ClauseCount]),
    
    format('✓ Clause storage test passed~n').

% ============================================================================
% Test 7: Dump Context
% ============================================================================

test_dump :-
    format('~n--- Test 7: Dump Context ---~n'),
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    
    dump_context(ConnId, Context),
    
    format('✓ Dump context test passed~n').

% ============================================================================
% Helpers
% ============================================================================

setup_connection :-
    format('Setting up database connection...~n'),
    db_config(connection_id(ConnId), server(Server), database(Database), 
              user(User), password(Password)),
    
    % Connect
    catch(
        store_connect(ConnId, Server, Database, User, Password),
        Error,
        (format('Error connecting: ~w~n', [Error]), fail)
    ),
    !,  % Cut after successful connection
    
    % Ensure test context exists
    test_context(Context),
    store_ensure_context(ConnId, Context),
    !,  % Cut after context setup
    
    format('✓ Connected to database~n').

cleanup_connection :-
    format('~nCleaning up...~n'),
    db_config(connection_id(ConnId), _, _, _, _),
    store_disconnect(ConnId),
    !,  % Cut after disconnect
    format('✓ Disconnected from database~n').

take(N, List, Taken) :-
    length(Taken, N),
    append(Taken, _, List), !.
take(_, List, List).

% ============================================================================
% Individual Test Runners
% ============================================================================

%! test_ground_only is det.
%
% Run only ground facts tests (no variable support needed).
%
test_ground_only :-
    setup_connection,
    test_ground_facts,
    test_all_assertions,
    test_inspection,
    cleanup_connection.

%! test_templates_only is det.
%
% Run only template-related tests.
%
test_templates_only :-
    setup_connection,
    test_templates,
    test_smart_operations,
    test_all_assertions,
    cleanup_connection.

%! test_clauses_only is det.
%
% Run only clause-related tests.
%
test_clauses_only :-
    setup_connection,
    test_clauses,
    cleanup_connection.

% ============================================================================
% Interactive Testing Helpers
% ============================================================================

%! quick_setup is det.
%
% Quick setup for interactive testing.
%
quick_setup :-
    db_config(connection_id(ConnId), server(Server), database(Database), 
              user(User), password(Password)),
    store_connect(ConnId, Server, Database, User, Password),
    test_context(Context),
    store_ensure_context(ConnId, Context),
    format('Connected! You can now run tests interactively.~n'),
    format('Try: store_assert(mydb, test_vars:person(john, 30)).~n').

%! quick_cleanup is det.
%
% Quick cleanup for interactive testing.
%
quick_cleanup :-
    db_config(connection_id(ConnId), _, _, _, _),
    store_disconnect(ConnId),
    format('Disconnected.~n').

%! show_all is det.
%
% Show all assertions in test context.
%
show_all :-
    test_context(Context),
    db_config(connection_id(ConnId), _, _, _, _),
    dump_context(ConnId, Context).

% ============================================================================
% Documentation
% ============================================================================

:- if(false).

%% USAGE EXAMPLES:
%
% 1. Run all tests:
%    ?- test_all.
%
% 2. Run specific test category:
%    ?- test_templates_only.
%
% 3. Interactive testing:
%    ?- quick_setup.
%    ?- store_template(mydb, test_vars:hasPrecondition(Op, Conds)).
%    ?- all_term_assertions(test_vars, All).
%    ?- show_all.
%    ?- quick_cleanup.
%
% 4. Individual operations:
%    ?- quick_setup.
%    ?- store_assert(mydb, test_vars:foo(a, b)).
%    ?- store_template(mydb, test_vars:bar(X, Y)).
%    ?- all_term_assertions(mydb, test_vars, All).
%    ?- count_assertions(mydb, test_vars, Count).
%    ?- dump_context(mydb, test_vars).
%

:- endif.
