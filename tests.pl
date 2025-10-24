% ============================================================================
% Test Suite
% ============================================================================
:- use_module(mysql_store).
:- use_module(mysql_store_advanced).

run_my_tests :-
    write('=== MySQL Assertion Store Test Suite ==='), nl,
    test_connection,
    test_basic_operations,
    test_lists,
    test_complex_terms,
    test_transactions,
    test_batch_operations,
    test_statistics,
    write('=== All tests passed ==='), nl.

test_connection :-
    write('Test: Connection... '),
    store_connect(mydb, 'localhost', 'prolog_store', 'prolog', 'prolog123'),
    store_ensure_context(mydb, test_ctx),
    write('OK'), nl,
    !.

test_basic_operations :-
    write('Test: Basic operations... '),
    
    % Assert
    store_assert(mydb, test_ctx:foo(a, b, c)),
    store_assert(mydb, test_ctx:foo(x, y, z)),
    
    % Query
    findall(X-Y-Z, store_call(mydb, test_ctx:foo(X, Y, Z)), Results),
    length(Results, 2),
    
    % Retract
    store_retract(mydb, test_ctx:foo(a, _, _)),
    findall(_, store_call(mydb, test_ctx:foo(a, _, _)), []),
    
    write('OK'), nl,
    !.

test_lists :-
    write('Test: Lists... '),
    
    store_assert(mydb, test_ctx:path([room1, hall, room2])),
    store_assert(mydb, test_ctx:data([1, 2, 3, 4, 5])),
    
    store_call(mydb, test_ctx:path(P)),
    P = [room1, hall, room2],
    
    write('OK'), nl, !.

test_complex_terms :-
    write('Test: Complex nested terms... '),
    
    store_assert(mydb, test_ctx:person(john, 
                                       address(city(boston), zip(02101)),
                                       [hobby(chess), hobby(coding)])),
    
    store_call(mydb, test_ctx:person(john, address(city(C), _), _)),
    C = boston,
    
    write('OK'), nl, !.

test_transactions :-
    write('Test: Transactions... '),
    
    % Successful transaction
    store_transaction(mydb, (
        store_assert(mydb, test_ctx:trans(1)),
        store_assert(mydb, test_ctx:trans(2))
    )),
    
    findall(X, store_call(mydb, test_ctx:trans(X)), [1, 2]),
    
    % Failed transaction (should rollback)
    catch(
        store_transaction(mydb, (
            store_assert(mydb, test_ctx:trans(3)),
            throw(test_error)
        )),
        _,
        true
    ),
    
    \+ store_call(mydb, test_ctx:trans(3)),
    
    write('OK'), nl, !.

test_batch_operations :-
    write('Test: Batch operations... '),
    
    Terms = [
        test_ctx:batch(1, a),
        test_ctx:batch(2, b),
        test_ctx:batch(3, c)
    ],
    
    store_assert_batch(mydb, Terms),
    
    findall(X, store_call(mydb, test_ctx:batch(X, _)), Xs),
    length(Xs, 3),
    
    write('OK'), nl, !.

test_statistics :-
    write('Test: Statistics... '),
    
    % Generate some activity
    forall(between(1, 10, _), 
           store_call(mydb, test_ctx:foo(_, _, _))),
    
    store_stats(mydb, test_ctx, Stats),
    member(stats(foo/3, Queries, _, _), Stats),
    Queries >= 10,
    
    write('OK'), nl, !.
