% ============================================================================
% File System Operations Example
% ============================================================================
%
% This example shows how to use the mysql_store variable support features
% to manage file system operations with preconditions and postconditions.
%
% Use Case: You want to store operation specifications (what conditions must
% be true before/after) and check them against actual system state.
%

:- use_module(mysql_store).
:- use_module(mysql_store_templates).
:- use_module(mysql_store_utils).

% ============================================================================
% Configuration
% ============================================================================

% Update these with your database details
example_config(
    connection_id(mydb),
    server('localhost'),
    database('prolog_store'),
    user('prolog'),
    password('prolog123')  % UPDATE THIS!
).

% ============================================================================
% Setup and Teardown
% ============================================================================

%! setup_example is det.
%
% Connect to database and set up contexts.
%
setup_example :-
    format('~n=== Setting up File System Operations Example ===~n~n'),
    example_config(connection_id(ConnId), server(Server), database(DB), 
                   user(User), password(Pass)),
    
    % Connect
    format('Connecting to database...~n'),
    store_connect(ConnId, Server, DB, User, Pass),
    format('✓ Connected~n~n'),
    
    % Create contexts for different purposes
    format('Creating contexts...~n'),
    store_ensure_context(ConnId, operations),  % Operation specifications
    store_ensure_context(ConnId, system_state), % Actual system state
    format('✓ Contexts created~n~n').

%! teardown_example is det.
%
% Clean up and disconnect.
%
teardown_example :-
    format('~n=== Cleaning up ===~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    store_disconnect(ConnId),
    format('✓ Disconnected~n~n').

% ============================================================================
% Example 1: Storing Operation Specifications
% ============================================================================

%! example1_store_operations is det.
%
% Store operation specifications with preconditions and postconditions.
% These use TEMPLATES (terms with variables) so we can query them later.
%
example1_store_operations :-
    format('~n--- Example 1: Storing Operation Specifications ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    format('Storing file operation specs...~n'),
    
    % CreateFile operation
    % Preconditions: directory exists, has write permission
    % Postconditions: file exists
    store_template(ConnId, operations:hasPrecondition(createFile, 
        [directoryExists(Dir), hasPermission(Dir, write)])),
    store_template(ConnId, operations:hasPostcondition(createFile,
        [fileExists(Path)])),
    format('  ✓ createFile operation stored~n'),
    
    % DeleteFile operation
    % Preconditions: file exists, has permission
    % Postconditions: file does not exist
    store_template(ConnId, operations:hasPrecondition(deleteFile,
        [fileExists(Path), hasPermission(Path, write)])),
    store_template(ConnId, operations:hasPostcondition(deleteFile,
        [not(fileExists(Path))])),
    format('  ✓ deleteFile operation stored~n'),
    
    % CreateSymlink operation
    % Preconditions: target exists, link doesn't exist
    % Postconditions: symlink exists
    store_template(ConnId, operations:hasPrecondition(createSymlink,
        [fileExists(Target), not(fileExists(Link))])),
    store_template(ConnId, operations:hasPostcondition(createSymlink,
        [hasSymlink(Link, Target)])),
    format('  ✓ createSymlink operation stored~n'),
    
    % MoveFile operation  
    % Preconditions: source exists, dest doesn't exist
    % Postconditions: file at new location
    store_template(ConnId, operations:hasPrecondition(moveFile,
        [fileExists(Source), not(fileExists(Dest))])),
    store_template(ConnId, operations:hasPostcondition(moveFile,
        [fileExists(Dest), not(fileExists(Source))])),
    format('  ✓ moveFile operation stored~n'),
    
    format('~nAll operation specs stored successfully!~n').

% ============================================================================
% Example 2: Querying Operation Specifications
% ============================================================================

%! example2_query_operations is det.
%
% Query stored operations to see what preconditions they require.
%
example2_query_operations :-
    format('~n--- Example 2: Querying Operation Specifications ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    % Query preconditions for createFile
    format('What are the preconditions for createFile?~n'),
    store_call_template(ConnId, operations:hasPrecondition(createFile, Conds)),
    format('  Preconditions: ~w~n~n', [Conds]),
    
    % Query postconditions for createFile
    format('What are the postconditions for createFile?~n'),
    store_call_template(ConnId, operations:hasPostcondition(createFile, Results)),
    format('  Postconditions: ~w~n~n', [Results]),
    
    % Find all operations
    format('What operations do we have?~n'),
    findall(Op, 
            store_call_template(ConnId, operations:hasPrecondition(Op, _)),
            Operations),
    format('  Operations: ~w~n~n', [Operations]),
    
    % Get all operation specifications at once
    format('All operation specifications:~n'),
    all_term_assertions(ConnId, operations, AllSpecs),
    forall(member(Spec, AllSpecs),
           format('  ~q~n', [Spec])).

% ============================================================================
% Example 3: Storing Current System State
% ============================================================================

%! example3_store_system_state is det.
%
% Store facts about the actual system state.
% These are GROUND FACTS (no variables) representing reality.
%
example3_store_system_state :-
    format('~n--- Example 3: Storing System State ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    format('Recording current file system state...~n'),
    
    % Files that exist
    store_assert(ConnId, system_state:fileExists('/home/user/document.txt')),
    store_assert(ConnId, system_state:fileExists('/tmp/tempfile.log')),
    store_assert(ConnId, system_state:fileExists('/etc/config.conf')),
    format('  ✓ Stored file existence facts~n'),
    
    % Directories that exist
    store_assert(ConnId, system_state:directoryExists('/home/user')),
    store_assert(ConnId, system_state:directoryExists('/tmp')),
    store_assert(ConnId, system_state:directoryExists('/etc')),
    format('  ✓ Stored directory existence facts~n'),
    
    % Permissions
    store_assert(ConnId, system_state:hasPermission('/home/user', write)),
    store_assert(ConnId, system_state:hasPermission('/tmp', write)),
    store_assert(ConnId, system_state:hasPermission('/etc', read)),
    format('  ✓ Stored permission facts~n'),
    
    % Symlinks
    store_assert(ConnId, system_state:hasSymlink('/home/user/link.txt', '/tmp/target.txt')),
    format('  ✓ Stored symlink facts~n'),
    
    format('~nSystem state recorded!~n').

% ============================================================================
% Example 4: Checking Preconditions
% ============================================================================

%! example4_check_preconditions is det.
%
% Check if preconditions for an operation are satisfied.
%
example4_check_preconditions :-
    format('~n--- Example 4: Checking Preconditions ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    % Check if we can create a file in /home/user
    format('Can we create a file in /home/user?~n'),
    
    % Get preconditions for createFile
    store_call_template(ConnId, operations:hasPrecondition(createFile, Preconditions)),
    format('  Required: ~w~n', [Preconditions]),
    
    % Instantiate with actual path
    Preconditions = [directoryExists(Dir), hasPermission(Dir, write)],
    Dir = '/home/user',
    
    % Check each precondition against system state
    (check_all_conditions(ConnId, Preconditions) ->
        format('  ✓ YES - All preconditions satisfied!~n~n')
    ;
        format('  ✗ NO - Some preconditions not met~n~n')
    ),
    
    % Check if we can create a file in /etc (should fail - only read permission)
    format('Can we create a file in /etc?~n'),
    store_call_template(ConnId, operations:hasPrecondition(createFile, Preconditions2)),
    Preconditions2 = [directoryExists(Dir2), hasPermission(Dir2, write)],
    Dir2 = '/etc',
    
    (check_all_conditions(ConnId, Preconditions2) ->
        format('  ✓ YES - All preconditions satisfied!~n~n')
    ;
        format('  ✗ NO - Some preconditions not met (expected - no write permission)~n~n')
    ).

%! check_all_conditions(+ConnId, +Conditions) is semidet.
%
% Check if all conditions are true in the system state.
%
check_all_conditions(_, []).
check_all_conditions(ConnId, [not(Cond)|Rest]) :-
    !,
    \+ store_call(ConnId, system_state:Cond),
    check_all_conditions(ConnId, Rest).
check_all_conditions(ConnId, [Cond|Rest]) :-
    store_call(ConnId, system_state:Cond),
    check_all_conditions(ConnId, Rest).

% ============================================================================
% Example 5: Smart Operations (Auto-detect Ground vs Template)
% ============================================================================

%! example5_smart_operations is det.
%
% Use store_assert_smart which automatically detects if you're storing
% ground facts or templates.
%
example5_smart_operations :-
    format('~n--- Example 5: Smart Operations ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    format('Using store_assert_smart (auto-detects ground vs template)...~n'),
    
    % This is ground (no variables) - goes to regular store_assert
    store_assert_smart(ConnId, system_state:fileExists('/var/log/system.log')),
    format('  ✓ Stored ground fact~n'),
    
    % This has variables - goes to store_template
    store_assert_smart(ConnId, operations:requires(backup, [fileExists(Source), hasSpace(Dest, Size)])),
    format('  ✓ Stored template~n'),
    
    % Query with store_call_smart (queries both ground facts and templates)
    format('~nQuerying with store_call_smart...~n'),
    
    findall(File, 
            store_call_smart(ConnId, system_state:fileExists(File)),
            Files),
    length(Files, NumFiles),
    format('  Found ~w files in system state~n', [NumFiles]),
    
    findall(Op-Reqs,
            store_call_smart(ConnId, operations:requires(Op, Reqs)),
            Requirements),
    format('  Found ~w operation requirements~n', [Requirements]).

% ============================================================================
% Example 6: Inspecting and Analyzing Storage
% ============================================================================

%! example6_inspection is det.
%
% Use utility predicates to inspect what's stored.
%
example6_inspection :-
    format('~n--- Example 6: Inspection and Analysis ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    % Count assertions
    format('Statistics:~n'),
    count_assertions(ConnId, operations, OpCount),
    count_assertions(ConnId, system_state, StateCount),
    format('  Operations context: ~w assertions~n', [OpCount]),
    format('  System state context: ~w assertions~n~n', [StateCount]),
    
    % Get context summary
    format('Context summary for operations:~n'),
    context_summary(ConnId, operations, Summary),
    format('  ~w~n~n', [Summary]),
    
    % List all predicates in system_state
    format('Predicates in system_state:~n'),
    list_predicates(ConnId, system_state, Predicates),
    forall(member(Pred, Predicates),
           format('  - ~w~n', [Pred])),
    
    % Get all ground facts vs all templates
    format('~nGround facts in system_state:~n'),
    all_ground_assertions(ConnId, system_state, GroundFacts),
    length(GroundFacts, NumGround),
    format('  Total: ~w ground facts~n', [NumGround]),
    
    format('~nTemplates in operations:~n'),
    all_template_assertions(ConnId, operations, Templates),
    length(Templates, NumTemplates),
    format('  Total: ~w templates~n', [NumTemplates]),
    
    % Find all operations that have preconditions
    format('~nOperations with preconditions:~n'),
    find_assertions_by_functor(ConnId, operations, hasPrecondition, PrecondAssertions),
    forall(member(operations:hasPrecondition(Op, _), PrecondAssertions),
           format('  - ~w~n', [Op])).

% ============================================================================
% Example 7: Updating System State
% ============================================================================

%! example7_update_state is det.
%
% Simulate executing an operation and updating state.
%
example7_update_state :-
    format('~n--- Example 7: Simulating Operation Execution ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    format('Simulating: Create file /home/user/newfile.txt~n~n'),
    
    % Step 1: Check preconditions
    format('Step 1: Check preconditions...~n'),
    store_call_template(ConnId, operations:hasPrecondition(createFile, Preconditions)),
    Preconditions = [directoryExists(Dir), hasPermission(Dir, write)],
    Dir = '/home/user',
    
    (check_all_conditions(ConnId, Preconditions) ->
        format('  ✓ Preconditions satisfied~n~n')
    ;
        format('  ✗ Preconditions not satisfied - aborting~n~n'),
        fail
    ),
    
    % Step 2: Execute operation (simulated)
    format('Step 2: Execute operation...~n'),
    format('  [Simulating file creation...]~n'),
    format('  ✓ Operation completed~n~n'),
    
    % Step 3: Update system state with postconditions
    format('Step 3: Update system state...~n'),
    store_assert(ConnId, system_state:fileExists('/home/user/newfile.txt')),
    format('  ✓ System state updated~n~n'),
    
    % Step 4: Verify new state
    format('Step 4: Verify state...~n'),
    (store_call(ConnId, system_state:fileExists('/home/user/newfile.txt')) ->
        format('  ✓ File exists in system state~n~n')
    ;
        format('  ✗ File not found in system state~n~n')
    ).

% ============================================================================
% Example 8: Complex Query Patterns
% ============================================================================

%! example8_complex_queries is det.
%
% Advanced querying patterns.
%
example8_complex_queries :-
    format('~n--- Example 8: Complex Query Patterns ---~n~n'),
    example_config(connection_id(ConnId), _, _, _, _),
    
    % Find all files in /tmp
    format('Files in /tmp:~n'),
    findall(Path,
            (store_call(ConnId, system_state:fileExists(Path)),
             atom_concat('/tmp/', _, Path)),
            TmpFiles),
    forall(member(File, TmpFiles),
           format('  - ~w~n', [File])),
    
    % Find all directories we can write to
    format('~nDirectories with write permission:~n'),
    findall(Dir,
            store_call(ConnId, system_state:hasPermission(Dir, write)),
            WritableDirs),
    forall(member(Dir, WritableDirs),
           format('  - ~w~n', [Dir])),
    
    % Find operations that require a specific condition
    format('~nOperations requiring fileExists condition:~n'),
    findall(Op,
            (store_call_template(ConnId, operations:hasPrecondition(Op, Conds)),
             member(fileExists(_), Conds)),
            OpsWithFileCheck),
    forall(member(Op, OpsWithFileCheck),
           format('  - ~w~n', [Op])),
    
    % Get everything at once
    format('~nAll assertions in operations context:~n'),
    all_term_assertions(ConnId, operations, AllOps),
    length(AllOps, NumAllOps),
    format('  Total: ~w assertions~n', [NumAllOps]).

% ============================================================================
% Main Example Runner
% ============================================================================

%! run_all_examples is det.
%
% Run all examples in sequence.
%
run_all_examples :-
    setup_example,
    
    example1_store_operations,
    example2_query_operations,
    example3_store_system_state,
    example4_check_preconditions,
    example5_smart_operations,
    example6_inspection,
    example7_update_state,
    example8_complex_queries,
    
    teardown_example,
    
    format('~n=== All Examples Completed! ===~n~n').

% ============================================================================
% Individual Example Runners
% ============================================================================

%! run_example(+N) is det.
%
% Run a specific example by number.
%
run_example(1) :- setup_example, example1_store_operations, teardown_example.
run_example(2) :- setup_example, example1_store_operations, example2_query_operations, teardown_example.
run_example(3) :- setup_example, example3_store_system_state, teardown_example.
run_example(4) :- setup_example, example1_store_operations, example3_store_system_state, 
                  example4_check_preconditions, teardown_example.
run_example(5) :- setup_example, example5_smart_operations, teardown_example.
run_example(6) :- setup_example, example1_store_operations, example3_store_system_state,
                  example6_inspection, teardown_example.
run_example(7) :- setup_example, example1_store_operations, example3_store_system_state,
                  example7_update_state, teardown_example.
run_example(8) :- setup_example, example1_store_operations, example3_store_system_state,
                  example8_complex_queries, teardown_example.

% ============================================================================
% Usage Instructions
% ============================================================================

:- if(false).

%% QUICK START:
%
% 1. Update the password in example_config/5 (line 19)
%
% 2. Load this file:
%    ?- [example_filesystem_ops].
%
% 3. Run all examples:
%    ?- run_all_examples.
%
% 4. Or run individual examples:
%    ?- run_example(1).  % Just store operations
%    ?- run_example(4).  % Check preconditions
%    ?- run_example(6).  % Inspection tools
%
%% WHAT YOU'LL LEARN:
%
% Example 1: How to store operation specifications with variables
% Example 2: How to query those specifications
% Example 3: How to store actual system state (ground facts)
% Example 4: How to check if preconditions are met
% Example 5: Using smart operations (auto-detect ground vs template)
% Example 6: Using inspection utilities to see what's stored
% Example 7: Simulating operation execution and state updates
% Example 8: Advanced query patterns
%
%% KEY CONCEPTS:
%
% - Templates (with variables): For specifications/patterns
% - Ground facts (no variables): For actual state
% - store_template: Store patterns
% - store_assert: Store facts
% - store_assert_smart: Auto-detect which to use
% - store_call_template: Query templates
% - store_call: Query ground facts
% - all_term_assertions: Get everything at once

:- endif.

% Print usage on load
:- format('~n=== File System Operations Example Loaded ===~n~n').
:- format('QUICK START:~n').
:- format('1. Update password in example_config/5 (line 19)~n').
:- format('2. Run: ?- run_all_examples.~n').
:- format('3. Or run individual: ?- run_example(N). where N is 1-8~n~n').
:- format('Type ?- run_all_examples. to start!~n~n').
