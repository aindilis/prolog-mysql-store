% ============================================================================
% Advanced Features: Transactions, Batch Operations, Indexing Configuration
% ============================================================================

:- module(mysql_store_advanced, [
    % Transactions
    store_transaction/2,
    store_begin_transaction/1,
    store_commit/1,
    store_rollback/1,
    
    % Batch operations
    store_assert_batch/2,
    store_retract_batch/2,
    
    % Index configuration
    store_configure_index/4,
    store_rebuild_indices/2,
    
    % Import/Export
    store_export_context/3,
    store_import_context/3
]).

:- use_module(mysql_store).
:- use_module(library(odbc)).

% ============================================================================
% Transactions
% ============================================================================

%! store_transaction(+ConnectionId, :Goal) is semidet.
%
% Execute goal within a transaction.
%
store_transaction(ConnectionId, Goal) :-
    setup_call_cleanup(
        store_begin_transaction(ConnectionId),
        (catch(Goal, Error, (store_rollback(ConnectionId), throw(Error))) -> store_commit(ConnectionId) ; (store_rollback(ConnectionId), fail)),
        % Cleanup: ensure we rollback if still pending (in case of abnormal exit)
        (retract(mysql_store:transaction_pending(ConnectionId)) -> 
            (odbc_query(ConnectionId, 'ROLLBACK', _), odbc_query(ConnectionId, 'SET autocommit=1', _))
        ; 
            true)
    ).

store_begin_transaction(ConnectionId) :-
    odbc_query(ConnectionId, 'SET autocommit=0', _),
    odbc_query(ConnectionId, 'BEGIN', _),
    assertz(mysql_store:transaction_pending(ConnectionId)).

store_commit(ConnectionId) :-
    odbc_query(ConnectionId, 'COMMIT', _),
    odbc_query(ConnectionId, 'SET autocommit=1', _),
    retractall(mysql_store:transaction_pending(ConnectionId)).

store_rollback(ConnectionId) :-
    odbc_query(ConnectionId, 'ROLLBACK', _),
    odbc_query(ConnectionId, 'SET autocommit=1', _),
    retractall(mysql_store:transaction_pending(ConnectionId)),
    % Clear all loaded predicates since rollback invalidates cache
    retractall(mysql_store:loaded_predicate(_, _, _)),
    retractall(mysql_store:predicate_cache(_, _, _, _)).

% ============================================================================
% Batch Operations
% ============================================================================

%! store_assert_batch(+ConnectionId, +Terms) is det.
%
% Assert multiple terms efficiently.
%
store_assert_batch(ConnectionId, Terms) :-
    store_transaction(ConnectionId,
        forall(member(Term, Terms), mysql_store:store_assert(ConnectionId, Term))
    ).

%! store_retract_batch(+ConnectionId, +Patterns) is det.
%
% Retract multiple patterns efficiently.
%
store_retract_batch(ConnectionId, Patterns) :-
    store_transaction(ConnectionId,
        forall(member(Pattern, Patterns), 
               mysql_store:store_retractall(ConnectionId, Pattern))
    ).

% ============================================================================
% Index Configuration
% ============================================================================

%! store_configure_index(+Context, +Functor, +Arity, +Positions) is det.
%
% Configure which argument positions to index.
%
store_configure_index(Context, Functor, Arity, Positions) :-
    mysql_store:context_mapping(Context, ContextId),
    retractall(mysql_store:index_config(ContextId, Functor, Arity, _)),
    assertz(mysql_store:index_config(ContextId, Functor, Arity, Positions)).

%! store_rebuild_indices(+ConnectionId, +Context) is det.
%
% Rebuild all indices for a context.
%
store_rebuild_indices(ConnectionId, Context) :-
    mysql_store:context_mapping(Context, ContextId),
    
    % Clear existing indexed arguments
    Query = 'DELETE ai FROM arguments_indexed ai
             JOIN formulae f ON ai.formula_id = f.formula_id
             WHERE f.context_id = ?',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    odbc_execute(Stmt, [ContextId]),
    
    % Rebuild for each predicate
    forall(
        (mysql_store:index_config(ContextId, Functor, Arity, Positions),
         Positions \= []),
        rebuild_predicate_index(ConnectionId, ContextId, Functor, Arity, Positions)
    ).

rebuild_predicate_index(ConnectionId, ContextId, Functor, Arity, Positions) :-
    Query = 'SELECT formula_id, term_canonical FROM formulae
             WHERE context_id = ? AND functor = ? AND arity = ?',
    odbc_prepare(ConnectionId, Query, [integer, default, integer], Stmt),
    
    forall(
        odbc_execute(Stmt, [ContextId, Functor, Arity], row(FormulaId, Canonical)),
        (atom_to_term(Canonical, Term, []),
         mysql_store:index_arguments(ConnectionId, FormulaId, Term, Positions))
    ).

% ============================================================================
% Import/Export
% ============================================================================

%! store_export_context(+ConnectionId, +Context, +File) is det.
%
% Export context to a Prolog file.
%
store_export_context(ConnectionId, Context, File) :-
    mysql_store:context_mapping(Context, ContextId),
    
    open(File, write, Stream),
    
    % Write header
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    format(Stream, '% Exported from context: ~w~n', [Context]),
    format(Stream, '% Export date: ~w~n~n', [DateTime]),
    
    % Export all formulae
    Query = 'SELECT functor, arity, term_canonical FROM formulae
             WHERE context_id = ?
             ORDER BY functor, arity',
    odbc_prepare(ConnectionId, Query, [integer], Stmt),
    
    forall(
        odbc_execute(Stmt, [ContextId], row(_Functor, _Arity, Canonical)),
        (atom_to_term(Canonical, Term, []),
         format(Stream, '~w:~q.~n', [Context, Term]))
    ),
    
    close(Stream).

%! store_import_context(+ConnectionId, +Context, +File) is det.
%
% Import context from a Prolog file.
%
store_import_context(ConnectionId, Context, File) :-
    mysql_store:store_ensure_context(ConnectionId, Context),
    
    open(File, read, Stream),
    
    read_terms_from_stream(Stream, Terms),
    
    close(Stream),
    
    % Import all terms in a transaction
    store_assert_batch(ConnectionId, Terms).

read_terms_from_stream(Stream, Terms) :-
    read(Stream, Term),
    (Term == end_of_file ->
        Terms = []
    ;
        Terms = [Term|Rest],
        read_terms_from_stream(Stream, Rest)
    ).
