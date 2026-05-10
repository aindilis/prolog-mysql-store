% ============================================================================
% Template Storage Module: Support for terms with variables
% ============================================================================
%
% This module extends mysql_store to support storing and querying terms
% that contain unbound variables. Templates are stored as serialized
% Prolog terms and instantiated with fresh variables on each query.
%
% Example Usage:
%   ?- store_template(mydb, rules:hasPrecondition(operation1, [hasSymlink(A, B)])).
%   ?- store_call_template(mydb, rules:hasPrecondition(Op, [Cond])).
%   Op = operation1, Cond = hasSymlink(_G123, _G124).
%

:- module(mysql_store_templates, [
    % Template operations
    store_template/2,
    store_template/3,
    store_call_template/2,
    store_retract_template/2,
    store_retractall_template/2,
    
    % Template management
    store_list_templates/3,
    store_template_info/3,
    
    % Hybrid operations (ground + templates)
    store_assert_smart/2,
    store_retract_smart/2,
    store_call_smart/2,

    var_info/2,
    byte_to_hex/2
]).

:- use_module(mysql_store).
:- use_module(library(odbc)).
:- use_module(library(sha)).

% ============================================================================
% Template Table Schema
% ============================================================================

%! ensure_template_tables(+ConnectionId) is det.
%
% Create template storage tables if they don't exist.
%
ensure_template_tables(ConnectionId) :-
    % Templates table
    TemplatesQuery = 'CREATE TABLE IF NOT EXISTS formula_templates (
        template_id INT PRIMARY KEY AUTO_INCREMENT,
        context_id INT NOT NULL,
        functor VARCHAR(255) NOT NULL,
        arity INT NOT NULL,
        template_repr TEXT NOT NULL,
        template_hash VARCHAR(64) NOT NULL,
        var_count INT NOT NULL DEFAULT 0,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        UNIQUE KEY unique_template (context_id, template_hash),
        KEY idx_functor_arity (context_id, functor, arity),
        KEY idx_var_count (context_id, var_count),
        FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
    )',
    odbc_query(ConnectionId, TemplatesQuery, _),
    
    % Variable info table (optional, for analysis)
    VarInfoQuery = 'CREATE TABLE IF NOT EXISTS template_variables (
        template_id INT NOT NULL,
        var_index INT NOT NULL,
        var_name VARCHAR(50),
        positions TEXT,
        PRIMARY KEY (template_id, var_index),
        FOREIGN KEY (template_id) REFERENCES formula_templates(template_id) ON DELETE CASCADE
    )',
    odbc_query(ConnectionId, VarInfoQuery, _).

% ============================================================================
% Template Storage
% ============================================================================

%! store_template(+ConnectionId, +ContextualTerm) is det.
%! store_template(+ConnectionId, +ContextualTerm, -TemplateId) is det.
%
% Store a term with variables as a template.
% Variables will be preserved and instantiated fresh on each query.
%
% @arg ContextualTerm Term in format Context:Term
%
store_template(ConnectionId, ContextualTerm) :-
    store_template(ConnectionId, ContextualTerm, _).

store_template(ConnectionId, Context:Term, TemplateId) :-
    must_be(atom, Context),
    must_be(nonvar, Term),
    
    % Ensure tables exist
    ensure_template_tables(ConnectionId),
    
    % Get context ID
    mysql_store:store_ensure_context(ConnectionId, Context),
    mysql_store:context_mapping(Context, ContextId),
    
    % Serialize term preserving variables
    serialize_template(Term, TemplateRepr, VarCount, VarInfo),
    
    % Extract metadata
    functor(Term, Functor, Arity),
    
    % Generate hash and convert to hex string
    sha_hash(TemplateRepr, HashList, [algorithm(sha256)]),
    hash_list_to_hex(HashList, HashHex),
    
    % Store template
    store_template_record(ConnectionId, ContextId, Functor, Arity, 
                         TemplateRepr, HashHex, VarCount, VarInfo, TemplateId).

%! hash_list_to_hex(+HashList, -HexString) is det.
%
% Convert a list of bytes to a hexadecimal string.
%
hash_list_to_hex(HashList, HexString) :-
    maplist(byte_to_hex, HashList, HexChars),
    atomic_list_concat(HexChars, HexString).

byte_to_hex(Byte, Hex) :-
    format(atom(Hex), '~|~`0t~16r~2+', [Byte]).

%! serialize_template(+Term, -TemplateRepr, -VarCount, -VarInfo) is det.
%
% Serialize a term preserving variable structure.
%
serialize_template(Term, TemplateRepr, VarCount, VarInfo) :-
    % Get all variables
    term_variables(Term, Vars),
    length(Vars, VarCount),
    
    % Create variable info
    maplist(var_info, Vars, VarInfo),
    
    % Serialize with write_canonical format
    format(atom(TemplateRepr), '~k', [Term]).

var_info(Var, info(Var, Name, Positions)) :-
    % Try to get variable name
    (var_property(Var, name(Name)) -> true ; Name = '_'),
    % Variable positions would require deeper analysis
    Positions = [].

%! store_template_record(+ConnectionId, +ContextId, +Functor, +Arity,
%                        +TemplateRepr, +Hash, +VarCount, +VarInfo, -TemplateId) is det.
%
store_template_record(ConnectionId, ContextId, Functor, Arity, 
                      TemplateRepr, Hash, VarCount, VarInfo, TemplateId) :-
    % Try to insert
    InsertQuery = 'INSERT INTO formula_templates 
                   (context_id, functor, arity, template_repr, template_hash, var_count)
                   VALUES (?, ?, ?, ?, ?, ?)',
    
    catch(
        (odbc_prepare(ConnectionId, InsertQuery, 
                     [integer, default, integer, default, default, integer], 
                     Stmt),
         odbc_execute(Stmt, [ContextId, Functor, Arity, TemplateRepr, Hash, VarCount]),
         odbc_query(ConnectionId, 'SELECT LAST_INSERT_ID()', row(TemplateId)),
         Result = insert),  % Tag as successful insert
        Error,
        (% Extract State and Code
         Error = error(odbc(State, Code, _), _),
         % Check if it's duplicate key error (State='23000', Code=1062)
         (   (State == '23000', Code == 1062)
         ->  Result = duplicate
         ;   throw(Error)  % Re-throw other errors
         ))
    ),
    
    (Result = insert ->
        % New insert - store variable info
        store_var_info(ConnectionId, TemplateId, VarInfo)
    ;
        % Duplicate - set placeholder ID and skip var_info
        TemplateId = -1
    ).

store_var_info(_, _, []).
store_var_info(ConnectionId, TemplateId, [info(_Var, Name, Positions)|Rest]) :-
    length([_|Rest], VarIndex),
    format(atom(PosStr), '~w', [Positions]),
    
    InsertQuery = 'INSERT IGNORE INTO template_variables 
                   (template_id, var_index, var_name, positions)
                   VALUES (?, ?, ?, ?)',
    odbc_prepare(ConnectionId, InsertQuery, [integer, integer, default, default], Stmt),
    odbc_execute(Stmt, [TemplateId, VarIndex, Name, PosStr]),
    
    store_var_info(ConnectionId, TemplateId, Rest).

% ============================================================================
% Template Querying
% ============================================================================

%! store_call_template(+ConnectionId, +ContextualPattern) is nondet.
%
% Query templates, instantiating fresh variables for each solution.
%
store_call_template(ConnectionId, Context:Pattern) :-
    must_be(atom, Context),
    must_be(nonvar, Pattern),
    
    % Get context ID
    mysql_store:context_mapping(Context, ContextId),
    
    % Extract functor/arity for efficient lookup
    functor(Pattern, Functor, Arity),
    
    % Fetch matching templates
    fetch_templates(ConnectionId, ContextId, Functor, Arity, Templates),
    
    % Try each template
    member(TemplateRepr, Templates),
    instantiate_and_unify(TemplateRepr, Pattern).

%! fetch_templates(+ConnectionId, +ContextId, +Functor, +Arity, -Templates) is det.
%
fetch_templates(ConnectionId, ContextId, Functor, Arity, Templates) :-
    Query = 'SELECT template_repr FROM formula_templates
             WHERE context_id = ? AND functor = ? AND arity = ?
             ORDER BY updated_at DESC',
    
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    
    findall(Repr,
            odbc_execute(Stmt, [ContextId, Functor, Arity], row(Repr)),
            Templates).

%! instantiate_and_unify(+TemplateRepr, +Pattern) is nondet.
%
% Instantiate a template with fresh variables and unify with pattern.
%
instantiate_and_unify(TemplateRepr, Pattern) :-
    % Read term from serialized form (creates fresh variables)
    read_term_from_atom(TemplateRepr, Instance, []),
    
    % Attempt unification
    Instance = Pattern.

% ============================================================================
% Template Retraction
% ============================================================================

%! store_retract_template(+ConnectionId, +ContextualPattern) is nondet.
%
% Retract first matching template.
%
store_retract_template(ConnectionId, Context:Pattern) :-
    must_be(atom, Context),
    must_be(nonvar, Pattern),
    
    % Find matching template
    mysql_store:context_mapping(Context, ContextId),
    functor(Pattern, Functor, Arity),
    
    % Get templates
    fetch_templates(ConnectionId, ContextId, Functor, Arity, Templates),
    
    % Find first match
    member(TemplateRepr, Templates),
    instantiate_and_unify(TemplateRepr, Pattern),
    
    % Delete it
    sha_hash(TemplateRepr, HashList, [algorithm(sha256)]),
    hash_list_to_hex(HashList, HashHex),
    delete_template_by_hash(ConnectionId, ContextId, HashHex),
    
    !. % Cut after first retraction

%! store_retractall_template(+ConnectionId, +ContextualPattern) is det.
%
% Retract all matching templates.
%
store_retractall_template(ConnectionId, Context:Pattern) :-
    must_be(atom, Context),
    must_be(nonvar, Pattern),
    
    % Collect all matching template hashes
    mysql_store:context_mapping(Context, ContextId),
    functor(Pattern, Functor, Arity),
    fetch_templates(ConnectionId, ContextId, Functor, Arity, Templates),
    
    % Find all matches
    findall(Hash,
            (member(TemplateRepr, Templates),
             instantiate_and_unify(TemplateRepr, Pattern),
             sha_hash(TemplateRepr, HashList, [algorithm(sha256)]),
             hash_list_to_hex(HashList, Hash)),
            Hashes),
    
    % Delete all
    forall(member(Hash, Hashes),
           delete_template_by_hash(ConnectionId, ContextId, Hash)).

delete_template_by_hash(ConnectionId, ContextId, Hash) :-
    Query = 'DELETE FROM formula_templates 
             WHERE context_id = ? AND template_hash = ?',
    odbc_prepare(ConnectionId, Query, [integer, default], Stmt),
    odbc_execute(Stmt, [ContextId, Hash]).

% ============================================================================
% Template Management
% ============================================================================

%! store_list_templates(+ConnectionId, +Context, -Templates) is det.
%
% List all templates in a context.
%
store_list_templates(ConnectionId, Context, Templates) :-
    mysql_store:context_mapping(Context, ContextId),
    
    Query = 'SELECT functor, arity, template_repr, var_count, updated_at
             FROM formula_templates
             WHERE context_id = ?
             ORDER BY functor, arity',
    
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    findall(template(Functor, Arity, Repr, VarCount, UpdatedAt),
            odbc_execute(Stmt, [ContextId], 
                        row(Functor, Arity, Repr, VarCount, UpdatedAt)),
            Templates).

%! store_template_info(+ConnectionId, +Context, -Info) is det.
%
% Get statistics about templates in a context.
%
store_template_info(ConnectionId, Context, Info) :-
    mysql_store:context_mapping(Context, ContextId),
    
    Query = 'SELECT 
                COUNT(*) as template_count,
                AVG(var_count) as avg_vars,
                MAX(var_count) as max_vars,
                COUNT(DISTINCT functor) as unique_functors
             FROM formula_templates
             WHERE context_id = ?',
    
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    odbc_execute(Stmt, [ContextId], 
                row(Count, AvgVars, MaxVars, UniqueFunctors)),
    
    Info = info(templates(Count), avg_vars(AvgVars), 
                max_vars(MaxVars), unique_functors(UniqueFunctors)).

% ============================================================================
% Smart Operations (Auto-detect ground vs. template)
% ============================================================================

%! store_assert_smart(+ConnectionId, +ContextualTerm) is det.
%
% Automatically choose between store_assert (ground) and store_template (with vars).
%
store_assert_smart(ConnectionId, ContextualTerm) :-
    ContextualTerm = _:Term,
    term_variables(Term, Vars),
    (Vars = [] ->
        % Ground term - use regular assertion
        mysql_store:store_assert(ConnectionId, ContextualTerm)
    ;
        % Has variables - use template storage
        store_template(ConnectionId, ContextualTerm)
    ).

%! store_retract_smart(+ConnectionId, +ContextualTerm) is det.
%
% Smart retraction - automatically detects if term is ground or has variables.
% Routes to appropriate retraction method.
%
% @arg ConnectionId Database connection identifier
% @arg ContextualTerm Term in format Context:Term
%
% @example Ground fact retraction
%   ?- store_retract_smart(mydb, ctx:person(john, 30)).
%
% @example Template retraction  
%   ?- store_retract_smart(mydb, ctx:person(Name, Age)).
%
store_retract_smart(ConnectionId, Context:Term) :-
    must_be(atom, Context),
    must_be(nonvar, Term),
    
    % Check if term is ground (no variables)
    (ground(Term) ->
        % Ground term - use regular retract
        mysql_store:store_retract(ConnectionId, Context:Term)
    ;
        % Has variables - use template retract
        store_retract_template(ConnectionId, Context:Term)
    ).

%! store_call_smart(+ConnectionId, +ContextualPattern) is nondet.
%
% Query both ground facts and templates.
%
store_call_smart(ConnectionId, ContextualPattern) :-
    % Try ground facts first
    mysql_store:store_call(ConnectionId, ContextualPattern).

store_call_smart(ConnectionId, ContextualPattern) :-
    % Then try templates
    store_call_template(ConnectionId, ContextualPattern).

% ============================================================================
% Utility Predicates
% ============================================================================

%! template_vars(+TemplateRepr, -VarNames) is det.
%
% Extract variable names from a template representation.
%
template_vars(TemplateRepr, VarNames) :-
    read_term_from_atom(TemplateRepr, _Term, [variable_names(VarNames)]).

% ============================================================================
% Examples and Tests
% ============================================================================

:- if(false).  % Disable when loading as module

test_templates :-
    % Setup
    store_connect(testdb, 'localhost', 'prolog_store', 'prolog', 'password'),
    store_ensure_context(testdb, test),
    
    % Test 1: Store a template with variables
    format('Test 1: Storing template with variables~n'),
    store_template(testdb, test:hasPrecondition(operation1, [hasSymlink(_A, _B)])),
    
    % Test 2: Query the template
    format('Test 2: Querying template~n'),
    findall(Op-Conds, 
            store_call_template(testdb, test:hasPrecondition(Op, Conds)),
            Results),
    format('  Results: ~w~n', [Results]),
    
    % Test 3: Store another template
    format('Test 3: Storing multiple templates~n'),
    store_template(testdb, test:hasPostcondition(operation1, [fileExists(_F)])),
    store_template(testdb, test:hasPrecondition(operation2, [fileExists(_X), hasPermission(_X, read)])),
    
    % Test 4: List all templates
    format('Test 4: Listing templates~n'),
    store_list_templates(testdb, test, Templates),
    maplist(writeln, Templates),
    
    % Test 5: Template info
    format('Test 5: Template statistics~n'),
    store_template_info(testdb, test, Info),
    format('  Info: ~w~n', [Info]),
    
    % Test 6: Smart operations
    format('Test 6: Smart operations (ground + template)~n'),
    store_assert_smart(testdb, test:hasSymlink('/tmp/a', '/tmp/b')),  % Ground
    store_assert_smart(testdb, test:operation(op1, _Params)),  % Template
    
    findall(R, store_call_smart(testdb, test:hasSymlink(_Y, _Z)), R1),
    format('  Symlinks: ~w~n', [R1]),
    
    findall(R, store_call_smart(testdb, test:operation(_O, _P)), R2),
    format('  Operations: ~w~n', [R2]),
    
    % Cleanup
    store_disconnect(testdb).

:- endif.
