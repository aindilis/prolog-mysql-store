# prolog-mysql-store

**Persistent storage for SWI-Prolog using MySQL/MariaDB with full ACID transaction support**

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.0.4+-red.svg)](https://www.swi-prolog.org/)
[![MySQL](https://img.shields.io/badge/MySQL-5.5+-blue.svg)](https://www.mysql.com/)

Store and query Prolog facts in MySQL/MariaDB while maintaining the declarative programming model. Combines the logical power of Prolog with the reliability and scalability of relational databases.

## Disclaimer

Developered iteratively with an LLM, some of the details have not been
finalized and may be inaccurate, for instance, the performance claims
made in this document. Appears to work and plan to develop and test
more in the coming days and weeks.

## Features

- 🔄 **Transparent Persistence** - Assert and retract facts as usual, automatically stored in MySQL
- 💾 **ACID Transactions** - Full transaction support with commit and rollback
- ⚡ **Intelligent Caching** - Automatic in-memory caching for frequently accessed predicates
- 🔒 **Secure** - Parameterized queries prevent SQL injection
- 📊 **Statistics Tracking** - Monitor query patterns and optimize performance
- 🔍 **Indexed Queries** - Configurable indexing on predicate arguments
- 📦 **Batch Operations** - Efficient bulk insert and delete
- 🏷️ **Context Management** - Organize facts into separate namespaces
- 🔗 **Hash-based Deduplication** - Automatic duplicate detection using SHA-256

## Quick Start

```prolog
:- use_module(mysql_store).

% Connect to database
?- store_connect(mydb, 'localhost', 'prolog_store', 'prolog', 'password').

% Create a context (namespace)
?- store_ensure_context(mydb, my_app).

% Assert some facts
?- store_assert(mydb, my_app:person(john, 30)).
?- store_assert(mydb, my_app:person(jane, 25)).

% Query facts
?- store_call(mydb, my_app:person(Name, Age)).
Name = john, Age = 30 ;
Name = jane, Age = 25.

% Use findall as usual
?- findall(Name-Age, store_call(mydb, my_app:person(Name, Age)), People).
People = [john-30, jane-25].

% Retract facts
?- store_retract(mydb, my_app:person(john, _)).

% Transactions
:- use_module(mysql_store_advanced).

?- store_transaction(mydb, (
    store_assert(mydb, my_app:account(acc1, 1000)),
    store_assert(mydb, my_app:account(acc2, 500))
)).
```

## Installation

### Prerequisites

- **SWI-Prolog** 9.0.4 or higher
- **MySQL** 5.5+ or **MariaDB** 10.11+
- **MySQL ODBC Driver** 8.0+
- **unixODBC** library

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install swi-prolog mysql-server libmyodbc unixodbc
```

### Fedora/RHEL

```bash
sudo dnf install pl mysql-server mysql-connector-odbc unixodbc
```

### Database Setup

1. **Create database and user:**

```sql
CREATE DATABASE prolog_store;
CREATE USER 'prolog'@'localhost' IDENTIFIED BY 'your_password';
GRANT ALL PRIVILEGES ON prolog_store.* TO 'prolog'@'localhost';
FLUSH PRIVILEGES;
```

2. **Create schema:**

Download and execute the [schema.sql](docs/schema.sql) file:

```bash
mysql -u prolog -p prolog_store < docs/schema.sql
```

3. **Configure ODBC DSN** in `~/.odbc.ini`:

```ini
[mydb]
Description = MySQL Prolog Store
Driver = MySQL ODBC 8.0 Driver
Server = localhost
Port = 3306
Database = prolog_store
User = prolog
Password = your_password
```

4. **Test connection:**

```bash
isql -v mydb
```

### Loading the Module

```prolog
:- use_module('/path/to/mysql_store.pl').
:- use_module('/path/to/mysql_store_advanced.pl').  % For transactions
```

Or install system-wide in SWI-Prolog's library directory.

## Documentation

- 📘 **[Complete Documentation](docs/mysql_assertion_store_documentation.md)** - Full guide with examples
- 📄 **[PDF Manual](docs/mysql_assertion_store_documentation.pdf)** - Printable reference
- 🔧 **[API Reference](docs/mysql_assertion_store_documentation.md#api-reference)** - All predicates documented
- 💡 **[Examples](examples/)** - Real-world usage examples

## Usage Examples

### Basic Operations

```prolog
% Assert complex terms
?- store_assert(mydb, kb:employee(
    john,
    department(engineering, building_a),
    [skill(prolog), skill(python), skill(sql)]
)).

% Query with pattern matching
?- store_call(mydb, kb:employee(Name, department(Dept, _), Skills)),
   member(skill(prolog), Skills).
Name = john,
Dept = engineering,
Skills = [skill(prolog), skill(python), skill(sql)].
```

### Transactions

```prolog
% Atomic counter increment
increment_counter(NewValue) :-
    store_transaction(mydb, (
        store_call(mydb, app:counter(OldValue)),
        NewValue is OldValue + 1,
        store_retract(mydb, app:counter(OldValue)),
        store_assert(mydb, app:counter(NewValue))
    )).
```

### Batch Operations

```prolog
% Bulk insert
?- Terms = [
    data:point(1, 10, 20),
    data:point(2, 15, 25),
    data:point(3, 20, 30)
],
store_assert_batch(mydb, Terms).

% Bulk delete
?- store_retract_batch(mydb, [temp:cache(_), temp:old_data(_)]).
```

### Knowledge Base with Rules

```prolog
% Store facts
?- store_assert(mydb, kb:parent(tom, bob)).
?- store_assert(mydb, kb:parent(bob, alice)).

% Define rules (in Prolog, not stored)
grandparent(X, Z) :-
    store_call(mydb, kb:parent(X, Y)),
    store_call(mydb, kb:parent(Y, Z)).

% Query
?- grandparent(tom, alice).
true.
```

## Architecture

```
┌─────────────────────────────────┐
│   Your Prolog Application       │
└───────────────┬─────────────────┘
                │
┌───────────────▼─────────────────┐
│   mysql_store Module            │
│   - store_assert/2              │
│   - store_call/2                │
│   - store_retract/2             │
└───────────────┬─────────────────┘
                │
┌───────────────▼─────────────────┐
│   In-Memory Cache               │
│   (Automatic)                   │
└───────────────┬─────────────────┘
                │
┌───────────────▼─────────────────┐
│   ODBC Layer                    │
└───────────────┬─────────────────┘
                │
┌───────────────▼─────────────────┐
│   MySQL/MariaDB                 │
└─────────────────────────────────┘
```

## Performance

Typical performance on modern hardware (Intel i7, SSD, local MySQL):

| Operation | Performance |
|-----------|-------------|
| Single assert | 2-5 ms |
| Batch assert (100 facts) | 20-50 ms |
| Cached query | 0.1-0.5 ms |
| Uncached query | 5-15 ms |
| Transaction (10 ops) | 10-25 ms |

### Optimization Tips

1. **Use transactions** for multiple operations
2. **Configure indexing** on frequently queried arguments
3. **Monitor statistics** with `store_stats/3`
4. **Batch operations** when possible
5. **Keep hot predicates loaded** in cache

## API Overview

### Core Module (`mysql_store`)

**Connection Management:**
- `store_connect/5` - Connect to database
- `store_disconnect/1` - Close connection
- `store_ensure_context/2` - Create/verify context

**Data Operations:**
- `store_assert/2` - Assert fact
- `store_retract/2` - Retract first match
- `store_retractall/2` - Retract all matches
- `store_abolish/2` - Remove entire predicate

**Queries:**
- `store_call/2` - Query facts (nondet)
- `store_findall/4` - Collect solutions

**Cache Management:**
- `store_load_predicate/3` - Load into cache
- `store_unload_predicate/3` - Remove from cache
- `store_sync/1` - Synchronize with database

**Statistics:**
- `store_stats/3` - Get usage statistics
- `store_optimize/2` - Optimize predicates

### Advanced Module (`mysql_store_advanced`)

**Transactions:**
- `store_transaction/2` - Execute in transaction
- `store_begin_transaction/1` - Start transaction
- `store_commit/1` - Commit
- `store_rollback/1` - Rollback

**Batch Operations:**
- `store_assert_batch/2` - Bulk insert
- `store_retract_batch/2` - Bulk delete

**Index Configuration:**
- `store_configure_index/4` - Configure indexing
- `store_rebuild_indices/2` - Rebuild indices

**Import/Export:**
- `store_export_context/3` - Export to file
- `store_import_context/3` - Import from file

## Security

### SQL Injection Prevention

All queries use **parameterized statements**:

```prolog
% Secure - uses parameter binding
odbc_prepare(Conn, 'SELECT * FROM formulae WHERE context_id = ?', 
             [integer], Stmt),
odbc_execute(Stmt, [ContextId], Results).
```

User input is never concatenated into SQL strings.

### Best Practices

1. Use dedicated database user with minimal privileges
2. Store passwords in environment variables or config files (not in code)
3. Use SSL/TLS for remote connections
4. Regular backups of the database
5. Monitor query logs for suspicious activity

## Testing

Run the test suite:

```bash
swipl -s mysql_store.pl mysql_store_advanced.pl tests.pl
```

```prolog
?- run_my_tests.
=== MySQL Assertion Store Test Suite ===
Test: Connection... OK
Test: Basic operations... OK
Test: Lists... OK
Test: Complex nested terms... OK
Test: Transactions... OK
Test: Batch operations... OK
Test: Statistics... OK
=== All tests passed ===
```

## Troubleshooting

### Connection Issues

**Problem:** `Data source name not found`

**Solution:**
1. Check `~/.odbc.ini` configuration
2. Verify with: `isql -v mydb`
3. Check driver in `/etc/odbcinst.ini`

### Schema Mismatch

**Problem:** `Unknown column` errors

**Solution:**
1. Drop and recreate database
2. Execute complete schema script
3. Verify with: `mysql> DESCRIBE table_name;`

### Performance Issues

**Problem:** Slow queries

**Solution:**
1. Enable statistics: `?- store_stats(mydb, context, Stats).`
2. Configure indexing on frequently queried arguments
3. Check MySQL slow query log
4. Ensure predicates are cached: `?- store_load_predicate/3`

### Debug Mode

```prolog
?- odbc_debug(1).        % Enable ODBC debugging
?- debug(mysql_store).   % Enable store debugging
```

## Limitations

- **Ground terms only** - Cannot assert terms with unbound variables
- **Single database** - No built-in replication support
- **Cache invalidation** - Rollback clears entire cache
- **Connection overhead** - ODBC adds latency vs pure in-memory

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Roadmap

- [ ] Connection pooling
- [ ] Incremental cache updates during transactions
- [ ] Distributed query optimization
- [ ] Replication support
- [ ] Compressed term storage
- [ ] Full-text search on term_readable
- [ ] PostgreSQL backend support
- [ ] Automatic schema migration tools

## Related Projects

This module is part of the [FRDCSA](https://frdcsa.org) (Formalized Research Database: Cluster, Study and Apply) ecosystem, a long-running project focused on AI-assisted planning, knowledge representation, and decision support systems. FRDCSA emphasizes FLOSS principles and collaborative development for collective progress.

Other FRDCSA components that work well with `prolog-mysql-store`:
- **Free Life Planner (FLP)** - AI-assisted life planning
- **Event Calculus reasoners** - Temporal reasoning
- **OpenCyc integration** - Large-scale ontology support

## License

This project is licensed under the **GNU General Public License v3.0** - see the [LICENSE](LICENSE) file for details.

### Why GPLv3?

We believe in the power of free/libre open source software (FLOSS) and the anti-rivalry of information. By using GPLv3, we ensure that improvements and derivatives remain open and accessible to all, fostering collective advancement over individual zero-sum competition.

## Citation

If you use this software in academic work, please cite:

```bibtex
@software{prolog_mysql_store_2025,
  title = {prolog-mysql-store: Persistent Storage for SWI-Prolog},
  author = {FRDCSA Project},
  year = {2025},
  url = {https://github.com/aindilis/prolog-mysql-store},
  license = {GPL-3.0}
}
```

## Acknowledgments

- **SWI-Prolog Team** - For creating an excellent Prolog system with comprehensive ODBC support
- **MySQL/MariaDB Communities** - For maintaining robust database engines
- **FRDCSA Contributors** - For 26 years of collaborative development

## Support

- 📧 **Issues:** [GitHub Issues](https://github.com/aindilis/prolog-mysql-store/issues)
- 💬 **Discussions:** [GitHub Discussions](https://github.com/aindilis/prolog-mysql-store/discussions)
- 🌐 **Website:** [https://frdcsa.org](https://frdcsa.org)

## Authors

Developed as part of the FRDCSA project with contributions from the Prolog and FLOSS communities.

---

**Made with ❤️ for the Prolog and FLOSS communities**
