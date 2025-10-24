# MySQL Assertion Store for SWI-Prolog

## Overview

The MySQL Assertion Store is a persistent storage system for Prolog facts that combines the declarative power of Prolog with the reliability and scalability of MySQL/MariaDB databases. It provides a seamless way to store, query, and manipulate Prolog terms while maintaining full ACID transaction support.

## Key Features

- **Persistent Storage**: Store Prolog facts in MySQL/MariaDB with full durability
- **Transparent Caching**: Automatic in-memory caching for frequently accessed predicates
- **Transaction Support**: Full ACID transactions with commit and rollback
- **Secure**: Uses parameterized SQL queries to prevent SQL injection
- **Hash-based Deduplication**: Automatic duplicate detection using SHA-256 hashing
- **Batch Operations**: Efficient bulk insert and delete operations
- **Context Management**: Organize facts into separate namespaces (contexts)
- **Statistics Tracking**: Monitor query patterns and optimize performance
- **Indexed Queries**: Configurable indexing on predicate arguments

## Architecture

### Core Components

1. **mysql_store.pl** - Core module providing:
   - Connection management
   - Basic CRUD operations (assert, retract, abolish)
   - Query operations with caching
   - Context management
   - Statistics tracking

2. **mysql_store_advanced.pl** - Advanced features:
   - Transaction support
   - Batch operations
   - Index configuration
   - Import/export functionality

3. **Database Schema** - MySQL tables:
   - `contexts` - Namespace management
   - `formulae` - Storage for Prolog terms
   - `arguments_indexed` - Indexed predicate arguments
   - `list_elements` - Indexed list elements
   - `predicate_stats` - Query statistics
   - `cache_status` - Cache metadata

### Data Flow

```
Application
    ↓
SWI-Prolog + mysql_store
    ↓
In-Memory Cache (predicate_cache)
    ↓
ODBC Layer
    ↓
MySQL/MariaDB Database
```

## Installation

### Prerequisites

1. **SWI-Prolog** (version 9.0.4 or higher)
2. **MySQL/MariaDB** server
3. **MySQL ODBC Driver** (8.0 or higher)
4. **unixODBC** library

### Installing Dependencies

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install swi-prolog mysql-server libmyodbc unixodbc
```

**Fedora/RHEL:**
```bash
sudo dnf install pl mysql-server mysql-connector-odbc unixodbc
```

### Database Setup

1. **Create Database and User:**
```sql
CREATE DATABASE prolog_store;
CREATE USER 'prolog'@'localhost' IDENTIFIED BY 'prolog123';
GRANT ALL PRIVILEGES ON prolog_store.* TO 'prolog'@'localhost';
FLUSH PRIVILEGES;
```

2. **Create Schema:**
```sql
USE prolog_store;

CREATE TABLE contexts (
    context_id INT AUTO_INCREMENT PRIMARY KEY,
    context_name VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE formulae (
    formula_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    term_canonical TEXT NOT NULL,
    term_readable TEXT NOT NULL,
    term_hash CHAR(64) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE,
    INDEX idx_functor_arity (functor, arity),
    INDEX idx_hash (term_hash),
    INDEX idx_context (context_id)
);

CREATE TABLE arguments_indexed (
    formula_id BIGINT NOT NULL,
    arg_position TINYINT UNSIGNED NOT NULL,
    arg_type ENUM('atom', 'integer', 'float', 'string') NOT NULL,
    atom_value VARCHAR(255),
    int_value BIGINT,
    float_value DOUBLE,
    string_value VARCHAR(255),
    PRIMARY KEY (formula_id, arg_position),
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id) ON DELETE CASCADE,
    INDEX idx_atom_value (atom_value),
    INDEX idx_int_value (int_value),
    INDEX idx_float_value (float_value),
    INDEX idx_string_value (string_value)
);

CREATE TABLE list_elements (
    formula_id BIGINT NOT NULL,
    arg_position TINYINT UNSIGNED NOT NULL,
    element_position SMALLINT UNSIGNED NOT NULL,
    element_type ENUM('atom', 'integer', 'float', 'compound') NOT NULL,
    element_value VARCHAR(255),
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id) ON DELETE CASCADE,
    INDEX idx_element_value (element_value)
);

CREATE TABLE predicate_stats (
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    query_count BIGINT DEFAULT 0,
    assert_count BIGINT DEFAULT 0,
    retract_count BIGINT DEFAULT 0,
    last_accessed TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (context_id, functor, arity),
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
);

CREATE TABLE cache_status (
    context_id INT NOT NULL,
    functor VARCHAR(255) NOT NULL,
    arity TINYINT UNSIGNED NOT NULL,
    is_loaded BOOLEAN DEFAULT FALSE,
    load_time TIMESTAMP NULL,
    fact_count INT DEFAULT 0,
    PRIMARY KEY (context_id, functor, arity),
    FOREIGN KEY (context_id) REFERENCES contexts(context_id) ON DELETE CASCADE
);
```

### ODBC Configuration

1. **Configure DSN** in `~/.odbc.ini` or `/etc/odbc.ini`:
```ini
[mydb]
Description = MySQL Prolog Store
Driver = MySQL ODBC 8.0 Driver
Server = localhost
Port = 3306
Database = prolog_store
User = prolog
Password = prolog123
```

2. **Verify Driver** in `/etc/odbcinst.ini`:
```ini
[MySQL ODBC 8.0 Driver]
Description = MySQL ODBC 8.0 Driver
Driver = /usr/lib/x86_64-linux-gnu/odbc/libmyodbc8w.so
Setup = /usr/lib/x86_64-linux-gnu/odbc/libmyodbc8S.so
UsageCount = 1
```

3. **Test Connection:**
```bash
isql -v mydb
```

## Usage Guide

### Basic Operations

#### Connecting to Database

```prolog
:- use_module(mysql_store).

% Connect to database
?- store_connect(mydb, 'localhost', 'prolog_store', 'prolog', 'prolog123').
```

#### Creating Contexts

Contexts provide namespaces for organizing facts:

```prolog
% Create or ensure context exists
?- store_ensure_context(mydb, my_context).
```

#### Asserting Facts

```prolog
% Assert a simple fact
?- store_assert(mydb, my_context:person(john, 30)).

% Assert complex terms
?- store_assert(mydb, my_context:
    employee(john, 
             department(engineering, building_a),
             [skill(prolog), skill(python)])).
```

#### Querying Facts

```prolog
% Query all matching facts
?- store_call(mydb, my_context:person(Name, Age)).
Name = john,
Age = 30.

% Use with findall
?- findall(Name-Age, 
           store_call(mydb, my_context:person(Name, Age)), 
           Results).
Results = [john-30, jane-25, bob-35].
```

#### Retracting Facts

```prolog
% Retract first matching fact
?- store_retract(mydb, my_context:person(john, _)).

% Retract all matching facts
?- store_retractall(mydb, my_context:person(_, Age)),
   Age > 60.

% Remove all facts for a predicate
?- store_abolish(mydb, my_context:person/2).
```

### Advanced Features

#### Transactions

```prolog
:- use_module(mysql_store_advanced).

% Successful transaction
?- store_transaction(mydb, (
    store_assert(mydb, my_context:account(acc1, 1000)),
    store_assert(mydb, my_context:account(acc2, 500))
)).

% Transaction with rollback on failure
?- store_transaction(mydb, (
    store_assert(mydb, my_context:transfer(acc1, acc2, 200)),
    (valid_transfer(acc1, acc2, 200) -> true ; throw(invalid_transfer))
)).
```

#### Batch Operations

```prolog
% Batch insert
?- Terms = [
    my_context:data(1, a),
    my_context:data(2, b),
    my_context:data(3, c)
],
store_assert_batch(mydb, Terms).

% Batch delete
?- Patterns = [
    my_context:temp(_),
    my_context:cache(_)
],
store_retract_batch(mydb, Patterns).
```

#### Statistics

```prolog
% Get usage statistics
?- store_stats(mydb, my_context, Stats).
Stats = [
    stats(person/2, 150, 50, 10),  % 150 queries, 50 asserts, 10 retracts
    stats(employee/3, 75, 25, 5)
].

% Optimize based on usage patterns
?- store_optimize(mydb, my_context).
```

#### Import/Export

```prolog
% Export context to file
?- store_export_context(mydb, my_context, 'backup.pl').

% Import context from file
?- store_import_context(mydb, my_context, 'backup.pl').
```

### Cache Management

```prolog
% Explicitly load predicate into cache
?- store_load_predicate(mydb, my_context, person/2).

% Unload predicate from cache
?- store_unload_predicate(mydb, my_context, person/2).

% Synchronize cache with database
?- store_sync(mydb).
```

## Performance Considerations

### Caching Strategy

The system uses a **lazy loading** strategy:
- Predicates are loaded into memory on first query
- Subsequent queries use the in-memory cache
- Cache is automatically updated on assert/retract
- Cache is cleared on transaction rollback

### Indexing

By default, the first argument of each predicate is indexed. Configure custom indexing:

```prolog
% Index arguments 1 and 3 of person/3
?- store_configure_index(my_context, person, 3, [1, 3]).

% Rebuild indices
?- store_rebuild_indices(mydb, my_context).
```

### Best Practices

1. **Use Transactions**: Batch multiple operations in transactions for consistency
2. **Monitor Statistics**: Use `store_stats/3` to identify hot predicates
3. **Configure Indexing**: Index frequently queried arguments
4. **Batch Operations**: Use `store_assert_batch/2` for bulk inserts
5. **Context Organization**: Separate unrelated data into different contexts
6. **Cache Management**: Unload rarely used predicates to save memory

## Security Features

### SQL Injection Prevention

All queries use **parameterized statements** via ODBC:

```prolog
% Secure - uses parameter binding
odbc_prepare(Conn, 'SELECT * FROM formulae WHERE context_id = ?', 
             [default], Stmt),
odbc_execute(Stmt, [ContextId], Results).
```

### Hash-based Deduplication

Terms are hashed using SHA-256 to prevent duplicates:

```prolog
% Automatic duplicate detection
?- store_assert(mydb, ctx:fact(a, b, c)).  % Inserted
?- store_assert(mydb, ctx:fact(a, b, c)).  % Duplicate - updates timestamp only
```

## Troubleshooting

### Common Issues

**Problem**: `Data source name not found`
```
Solution: Check ODBC DSN configuration in ~/.odbc.ini
Verify with: isql -v mydb
```

**Problem**: `Unknown column 'arg_value'`
```
Solution: Schema mismatch. Recreate database tables with correct schema.
```

**Problem**: `Transaction rollback not working`
```
Solution: Ensure autocommit is managed correctly. Check MySQL logs.
```

**Problem**: `Queries return stale data after rollback`
```
Solution: Cache is cleared automatically on rollback. If issue persists,
manually call store_sync/1.
```

### Debug Mode

Enable debug output:

```prolog
?- odbc_debug(1).  % Enable ODBC debugging
?- debug(mysql_store).  % Enable store debugging
```

### Performance Monitoring

```sql
-- Check table sizes
SELECT 
    table_name,
    table_rows,
    ROUND(data_length / 1024 / 1024, 2) AS data_mb
FROM information_schema.tables
WHERE table_schema = 'prolog_store';

-- Check query performance
SELECT functor, arity, query_count, assert_count
FROM predicate_stats
ORDER BY query_count DESC
LIMIT 10;
```

## API Reference

### Core Module (mysql_store)

#### Connection Management

- `store_connect(+ConnectionId, +Server, +Database, +User, +Password)`
  - Establish database connection
  
- `store_disconnect(+ConnectionId)`
  - Close database connection

- `store_ensure_context(+ConnectionId, +ContextName)`
  - Create or verify context exists

#### Assertion Operations

- `store_assert(+ConnectionId, +Term)`
  - Assert a ground term into database
  
- `store_retract(+ConnectionId, +Pattern)`
  - Retract first matching term (nondet)
  
- `store_retractall(+ConnectionId, +Pattern)`
  - Retract all matching terms
  
- `store_abolish(+ConnectionId, +Functor/Arity)`
  - Remove all facts for predicate

#### Query Operations

- `store_call(+ConnectionId, +Goal)`
  - Query database (nondet)
  
- `store_findall(+ConnectionId, +Template, +Goal, -Results)`
  - Collect all solutions

#### Cache Management

- `store_load_predicate(+ConnectionId, +Context, +Functor/Arity)`
  - Load predicate into cache
  
- `store_unload_predicate(+ConnectionId, +Context, +Functor/Arity)`
  - Unload predicate from cache
  
- `store_sync(+ConnectionId)`
  - Synchronize cache with database

#### Statistics

- `store_stats(+ConnectionId, +Context, -Stats)`
  - Get usage statistics
  
- `store_optimize(+ConnectionId, +Context)`
  - Optimize based on usage patterns

### Advanced Module (mysql_store_advanced)

#### Transactions

- `store_transaction(+ConnectionId, :Goal)`
  - Execute goal in transaction
  
- `store_begin_transaction(+ConnectionId)`
  - Start transaction manually
  
- `store_commit(+ConnectionId)`
  - Commit transaction
  
- `store_rollback(+ConnectionId)`
  - Rollback transaction

#### Batch Operations

- `store_assert_batch(+ConnectionId, +Terms)`
  - Assert multiple terms efficiently
  
- `store_retract_batch(+ConnectionId, +Patterns)`
  - Retract multiple patterns

#### Index Configuration

- `store_configure_index(+Context, +Functor, +Arity, +Positions)`
  - Configure argument indexing
  
- `store_rebuild_indices(+ConnectionId, +Context)`
  - Rebuild all indices

#### Import/Export

- `store_export_context(+ConnectionId, +Context, +File)`
  - Export context to Prolog file
  
- `store_import_context(+ConnectionId, +Context, +File)`
  - Import context from file

## Examples

### Example 1: Knowledge Base

```prolog
% Store a family tree
?- store_assert(mydb, kb:parent(tom, bob)).
?- store_assert(mydb, kb:parent(tom, alice)).
?- store_assert(mydb, kb:parent(bob, charlie)).

% Define rules (in Prolog, not stored)
grandparent(X, Z) :-
    store_call(mydb, kb:parent(X, Y)),
    store_call(mydb, kb:parent(Y, Z)).

% Query
?- grandparent(tom, charlie).
true.
```

### Example 2: Persistent Counter

```prolog
% Initialize counter
?- store_assert(mydb, app:counter(0)).

% Increment counter atomically
increment_counter(N) :-
    store_transaction(mydb, (
        store_call(mydb, app:counter(Old)),
        N is Old + 1,
        store_retract(mydb, app:counter(Old)),
        store_assert(mydb, app:counter(N))
    )).
```

### Example 3: Audit Log

```prolog
% Log events
log_event(Type, User, Data) :-
    get_time(Timestamp),
    store_assert(mydb, audit:event(Type, User, Timestamp, Data)).

% Query recent events
recent_events(N, Events) :-
    findall(event(Type, User, Time, Data),
            store_call(mydb, audit:event(Type, User, Time, Data)),
            AllEvents),
    length(AllEvents, Total),
    Skip is max(0, Total - N),
    length(Prefix, Skip),
    append(Prefix, Events, AllEvents).
```

## Limitations

1. **Ground Terms Only**: Only fully instantiated terms can be asserted
2. **No Backtracking on Assert**: Assert operations are immediate
3. **Cache Consistency**: Cache cleared on rollback (performance impact)
4. **Connection Overhead**: ODBC adds latency compared to in-memory
5. **Schema Changes**: Require manual migration

## Future Enhancements

- Support for incremental cache updates during transactions
- Distributed query optimization
- Replication support
- Compressed term storage
- Full-text search on term_readable
- GraphQL-style query interface

## License and Credits

This MySQL Assertion Store was developed to provide persistent storage for Prolog applications while maintaining the declarative nature of logic programming.

**Author**: Claude (Anthropic) with collaboration  
**Version**: 1.0  
**Date**: October 2025  
**SWI-Prolog Version**: 9.0.4+  
**MySQL Version**: 5.5+ / MariaDB 10.11+

## Acknowledgments

Special thanks to the SWI-Prolog and MySQL/MariaDB communities for providing the foundation that makes this system possible.

---

For questions, issues, or contributions, please refer to your project repository or contact information.
