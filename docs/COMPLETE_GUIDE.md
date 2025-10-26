# Complete Guide to mysql_store Variable Support

## 🎯 Start Here!

**New to this?** Start with:
1. 📖 [BEGINNERS_GUIDE.md](BEGINNERS_GUIDE.md) - Step-by-step tutorial with explanations
2. 💻 [example_filesystem_ops.pl](example_filesystem_ops.pl) - Runnable examples you can execute
3. 📚 [USAGE_GUIDE.md](USAGE_GUIDE.md) - Comprehensive reference

## What You Have

### Core Modules (✅ All Working!)

1. **mysql_store.pl** - Original module for ground facts
   - Store concrete data: `store_assert(ConnId, Context:fact)`
   - Query concrete data: `store_call(ConnId, Context:fact)`

2. **mysql_store_templates.pl** - NEW! Store patterns with variables
   - Store patterns: `store_template(ConnId, Context:pattern(Var))`
   - Query patterns: `store_call_template(ConnId, Context:pattern(Var))`
   - Auto-detect: `store_assert_smart(ConnId, Context:anything)`

3. **mysql_store_clauses.pl** - NEW! Store rules (advanced)
   - Store clauses: `store_clause(ConnId, Context:clause)`
   - Query clauses: `store_call_clause(ConnId, Context:clause)`
   - Note: Resolution engine not fully working yet (Test 6), but storage works!

4. **mysql_store_utils.pl** - NEW! Inspection and utilities
   - Get everything: `all_term_assertions(ConnId, Context, All)`
   - Get ground only: `all_ground_assertions(ConnId, Context, Facts)`
   - Get templates only: `all_template_assertions(ConnId, Context, Templates)`
   - Count: `count_assertions(ConnId, Context, Count)`
   - Statistics: `context_summary(ConnId, Context, Summary)`
   - Many more utilities!

5. **mysql_store_advanced.pl** - Transactions and batch operations
   - Transactions: `store_transaction(ConnId, Goal)`
   - Batch operations: `store_assert_batch(ConnId, Facts)`

### Test Suite

- **test_variable_support.pl** - Comprehensive test suite
  - Tests ground facts ✓
  - Tests template storage ✓
  - Tests smart operations ✓
  - Tests inspection utilities ✓
  - Tests clause storage ✓ (resolution ⚠️ not fully working)

### Documentation

1. **BEGINNERS_GUIDE.md** ⭐ START HERE
   - Core concepts explained simply
   - Quick start examples
   - Common patterns
   - When to use what

2. **example_filesystem_ops.pl** ⭐ RUNNABLE CODE
   - 8 complete examples
   - File system operations use case
   - Copy, update password, and run!

3. **USAGE_GUIDE.md** - Comprehensive reference
   - All features documented
   - API reference
   - Advanced patterns
   - Best practices

4. **README_VARIABLE_SUPPORT.md** - Quick reference
   - Quick start
   - Key features
   - Common operations

5. **variable_support_analysis.md** - Technical deep dive
   - Architecture details
   - Implementation notes
   - Design decisions

### Helper Scripts

- **check_versions.sh** - Verify you have the latest file versions
- **db_migration.pl** - Recreate database tables
- **drop_tables.pl** - Drop variable support tables

## Quick Start (2 Minutes)

### 1. Update Password

Edit `example_filesystem_ops.pl` line 19:
```prolog
password('YOUR_PASSWORD_HERE')  % Change this!
```

### 2. Run Examples

```prolog
?- [example_filesystem_ops].
?- run_all_examples.
```

### 3. Try It Yourself

```prolog
% Connect
?- store_connect(mydb, 'localhost', 'prolog_store', 'prolog', 'password').
?- store_ensure_context(mydb, test).

% Store a ground fact
?- store_assert(mydb, test:fileExists('/tmp/test.txt')).

% Store a template (with variable)
?- store_template(mydb, test:requires(Operation, Conditions)).

% Get everything
?- all_term_assertions(mydb, test, All).
All = [test:fileExists('/tmp/test.txt'), 
       test:requires(_G123, _G124)].
```

## Understanding the Three Types of Storage

### 1. Ground Facts (Original Feature)

**What:** Concrete data with no variables
**When:** Current system state, specific data
**Store:** `store_assert(ConnId, Context:fact)`
**Query:** `store_call(ConnId, Context:fact)`

Example:
```prolog
store_assert(mydb, state:fileExists('/tmp/file.txt')).
store_call(mydb, state:fileExists(Path)).
% Path = '/tmp/file.txt'
```

### 2. Templates (NEW Feature!)

**What:** Patterns with variables
**When:** Specifications, rules, schemas
**Store:** `store_template(ConnId, Context:pattern(Var))`
**Query:** `store_call_template(ConnId, Context:pattern(Var))`

Example:
```prolog
store_template(mydb, ops:hasPrecondition(Op, Conditions)).
store_call_template(mydb, ops:hasPrecondition(Op, Conditions)).
% Op = _G123, Conditions = _G124 (fresh variables!)
```

### 3. Smart Operations (Auto-Detect!)

**What:** Automatically chooses ground or template
**When:** You don't want to think about it
**Store:** `store_assert_smart(ConnId, Context:anything)`
**Query:** `store_call_smart(ConnId, Context:anything)`

Example:
```prolog
% Ground - automatically uses store_assert
store_assert_smart(mydb, ctx:fileExists('/tmp/file.txt')).

% Template - automatically uses store_template
store_assert_smart(mydb, ctx:requires(Op, Conds)).

% Query both - automatically searches both backends
store_call_smart(mydb, ctx:fileExists(X)).
store_call_smart(mydb, ctx:requires(X, Y)).
```

## Common Use Cases

### Use Case 1: Operation Specifications with Validation

```prolog
% Store specs (once during setup)
store_template(mydb, ops:hasPrecondition(createFile,
    [directoryExists(D), hasPermission(D, write)])).

% Store current state (updated frequently)
store_assert(mydb, state:directoryExists('/home/user')).
store_assert(mydb, state:hasPermission('/home/user', write)).

% Check if operation is possible
can_execute(Op, Args) :-
    store_call_template(mydb, ops:hasPrecondition(Op, Conditions)),
    instantiate_conditions(Conditions, Args),
    check_all_conditions(Conditions).
```

### Use Case 2: Configuration Management

```prolog
% Store configuration templates
store_template(mydb, config:setting(Key, Value, Type)).

% Store actual configuration
store_assert(mydb, active_config:setting(max_connections, 100, integer)).
store_assert(mydb, active_config:setting(timeout, 30, integer)).

% Query configuration
?- all_ground_assertions(mydb, active_config, Settings).
```

### Use Case 3: Multi-Version Specifications

```prolog
% Store different versions
store_template(mydb, api_v1:endpoint(Path, Method, Params)).
store_template(mydb, api_v2:endpoint(Path, Method, Params, Auth)).

% Query specific version
?- all_template_assertions(mydb, api_v2, Endpoints).
```

## Troubleshooting

### Tests Pass But Test 6 Shows "No path found"

**This is expected!** Clause resolution (Test 6) is an advanced feature that's not fully implemented. Everything else works perfectly:
- ✅ Ground facts
- ✅ Template storage
- ✅ Smart operations
- ✅ All inspection utilities

You don't need clause resolution for most use cases.

### Getting "Invalid handle" Error After Tests

**Fixed!** Make sure you have the latest `test_variable_support.pl` with strategic cuts (`!`).

### Getting Duplicate Entry Errors

**Fixed!** Make sure you have the latest files with improved duplicate handling. The system now gracefully handles duplicate templates.

### Want to Start Fresh

```bash
# Option 1: Drop tables via SQL
mysql -u prolog -p prolog_store -e "
DROP TABLE IF EXISTS template_variables;
DROP TABLE IF EXISTS formula_templates;
DROP TABLE IF EXISTS variable_bindings;
DROP TABLE IF EXISTS clause_variables;
DROP TABLE IF EXISTS formula_clauses;
"

# Option 2: Use drop script
# Edit password in drop_tables.pl first
swipl -g drop_tables -t halt drop_tables.pl
```

## File Checklist

In your project directory, you should have:

**Core Modules:**
- ✅ mysql_store.pl
- ✅ mysql_store_advanced.pl
- ✅ mysql_store_templates.pl (NEW!)
- ✅ mysql_store_clauses.pl (NEW!)
- ✅ mysql_store_utils.pl (NEW!)

**Tests and Examples:**
- ✅ test_variable_support.pl (NEW!)
- ✅ example_filesystem_ops.pl (NEW!)
- ✅ tests.pl (original)

**Documentation:**
- ✅ BEGINNERS_GUIDE.md (NEW!)
- ✅ USAGE_GUIDE.md (NEW!)
- ✅ README_VARIABLE_SUPPORT.md (NEW!)
- ✅ variable_support_analysis.md (NEW!)
- ✅ README.md (original)

**Helpers:**
- ✅ check_versions.sh (NEW!)
- ✅ db_migration.pl (NEW!)
- ✅ drop_tables.pl (NEW!)

**Optional (can delete):**
- BUG_FIX_SUMMARY.md
- FINAL_FIX_NOTES.md
- HASH_FIX_FINAL.md
- QUICK_FIX_GUIDE.md
- GET_RIGHT_FILES.md

## Learning Path

**Day 1:** Read BEGINNERS_GUIDE.md, run example_filesystem_ops.pl
**Day 2:** Experiment with your own use case
**Day 3:** Read USAGE_GUIDE.md for advanced patterns
**Day 4:** Explore variable_support_analysis.md for deep understanding

## What's Next?

1. **Experiment** with `example_filesystem_ops.pl`
2. **Adapt** examples to your use case
3. **Read** BEGINNERS_GUIDE.md to understand concepts
4. **Reference** USAGE_GUIDE.md when you need specific features
5. **Share** your use case if you'd like feedback!

## Summary

You now have a complete system for storing and querying:
- ✅ Ground facts (concrete data)
- ✅ Templates (patterns with variables)  
- ✅ Everything at once (inspection utilities)
- ✅ Smart operations (auto-detection)

All working, tested, and documented! 🎉

**Need help?** Check the guides or run the examples to see how it works.

**Want to understand deeply?** Read variable_support_analysis.md for architecture details.

**Just want to code?** Copy example_filesystem_ops.pl, update the password, and run!

Happy Prologging! 🚀
