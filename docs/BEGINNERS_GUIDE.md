# Beginner's Guide to Variable Support in mysql_store

## What Problem Does This Solve?

Imagine you want to store these two different kinds of information:

1. **Patterns/Specifications** (with variables):
   ```prolog
   hasPrecondition(createFile, [directoryExists(Dir), hasPermission(Dir, write)])
   ```
   This says "to create a file, some directory must exist and have write permission"
   The `Dir` is a variable - it can be any directory.

2. **Actual Facts** (no variables):
   ```prolog
   directoryExists('/home/user')
   hasPermission('/home/user', write)
   ```
   These are concrete facts about your system right now.

**The Problem:** The original `mysql_store` couldn't store patterns with variables!

**The Solution:** Three new modules that work together:
- `mysql_store_templates.pl` - Stores patterns with variables
- `mysql_store_clauses.pl` - Stores rules (advanced)
- `mysql_store_utils.pl` - Helpful utilities to query everything

---

## Core Concepts

### 1. Ground Facts (No Variables)

These are concrete, specific facts:
```prolog
fileExists('/tmp/file.txt')    % Specific file
hasPermission('/home', write)   % Specific permission
person(john, 30)                % Specific person
```

**Store with:** `store_assert/2` (the original function)

**Query with:** `store_call/2` (the original function)

### 2. Templates (With Variables)

These are patterns that can match many things:
```prolog
hasPrecondition(Op, Conditions)           % Op and Conditions are variables
person(Name, Age)                         % Name and Age are variables
fileExists(Path)                          % Path is a variable
```

**Store with:** `store_template/2` (NEW!)

**Query with:** `store_call_template/2` (NEW!)

**Get everything:** `all_term_assertions/2` (NEW!)

---

## Quick Start Example

### Step 1: Connect to Database

```prolog
?- store_connect(mydb, 'localhost', 'prolog_store', 'prolog', 'password').
?- store_ensure_context(mydb, my_context).
```

### Step 2: Store Some Ground Facts

```prolog
% Store concrete facts about your system
?- store_assert(mydb, my_context:fileExists('/home/user/doc.txt')).
?- store_assert(mydb, my_context:fileExists('/tmp/temp.log')).
?- store_assert(mydb, my_context:hasPermission('/home/user', write)).
```

### Step 3: Store Some Templates

```prolog
% Store operation specifications (with variables)
?- store_template(mydb, my_context:hasPrecondition(createFile, [directoryExists(D), hasPermission(D, write)])).
?- store_template(mydb, my_context:hasPrecondition(deleteFile, [fileExists(F), hasPermission(F, write)])).
```

### Step 4: Query Ground Facts

```prolog
% Find specific files
?- store_call(mydb, my_context:fileExists(Path)).
Path = '/home/user/doc.txt' ;
Path = '/tmp/temp.log'.

% Check if a specific file exists
?- store_call(mydb, my_context:fileExists('/home/user/doc.txt')).
true.
```

### Step 5: Query Templates

```prolog
% Get preconditions for createFile (with fresh variables)
?- store_call_template(mydb, my_context:hasPrecondition(createFile, Conditions)).
Conditions = [directoryExists(_G123), hasPermission(_G123, write)].

% Find all operations that have preconditions
?- store_call_template(mydb, my_context:hasPrecondition(Op, _)).
Op = createFile ;
Op = deleteFile.
```

### Step 6: Get Everything at Once

```prolog
% Get all assertions (both ground facts AND templates)
?- all_term_assertions(mydb, my_context, All).
All = [
    my_context:fileExists('/home/user/doc.txt'),
    my_context:fileExists('/tmp/temp.log'),
    my_context:hasPermission('/home/user', write),
    my_context:hasPrecondition(createFile, [directoryExists(_G123), hasPermission(_G123, write)]),
    my_context:hasPrecondition(deleteFile, [fileExists(_G456), hasPermission(_G456, write)])
].
```

---

## Understanding Variables in Templates

When you store a template like:
```prolog
store_template(mydb, ops:hasPrecondition(createFile, [directoryExists(D)]))
```

The variable `D` is captured and stored. When you query it later:
```prolog
?- store_call_template(mydb, ops:hasPrecondition(createFile, Conds)).
```

You get **fresh variables** back:
```prolog
Conds = [directoryExists(_G123)]
```

This `_G123` is a NEW variable that you can then use in your program!

---

## Practical Use Case: Checking Preconditions

Here's a complete workflow:

```prolog
% 1. Store operation specs (once, during setup)
store_template(mydb, ops:hasPrecondition(createFile, 
    [directoryExists(Dir), hasPermission(Dir, write)])).

% 2. Store current system state (updated as system changes)
store_assert(mydb, state:directoryExists('/home/user')).
store_assert(mydb, state:hasPermission('/home/user', write)).

% 3. Check if we can execute createFile in /home/user
can_create_file(Dir) :-
    % Get the preconditions template
    store_call_template(mydb, ops:hasPrecondition(createFile, Conditions)),
    
    % Instantiate the variables with our specific directory
    Conditions = [directoryExists(Dir), hasPermission(Dir, write)],
    
    % Check if both conditions are true in system state
    store_call(mydb, state:directoryExists(Dir)),
    store_call(mydb, state:hasPermission(Dir, write)).

% Usage:
?- can_create_file('/home/user').
true.  % Yes, we can!

?- can_create_file('/root').
false. % No, we can't (not in our system state)
```

---

## When to Use What?

### Use `store_assert` (ground facts) for:
- ✅ Current system state
- ✅ Specific file paths
- ✅ Concrete data
- ✅ Things that change frequently
- ✅ Example: `fileExists('/tmp/file.txt')`

### Use `store_template` (with variables) for:
- ✅ Operation specifications
- ✅ Rules and patterns
- ✅ Schemas or types
- ✅ Things that rarely change
- ✅ Example: `hasPrecondition(Op, Conditions)`

### Use `store_assert_smart` when:
- ✅ You don't want to think about it
- ✅ It auto-detects: ground → store_assert, variables → store_template
- ✅ Example: Both of these work automatically:
  ```prolog
  store_assert_smart(mydb, ctx:fileExists('/tmp/file.txt')).  % Goes to store_assert
  store_assert_smart(mydb, ctx:hasCondition(Op, Conds)).      % Goes to store_template
  ```

---

## Inspection Utilities

Once you have data stored, use these to explore it:

```prolog
% Count how many assertions you have
?- count_assertions(mydb, my_context, Count).
Count = 42.

% Get just the ground facts
?- all_ground_assertions(mydb, my_context, Facts).
Facts = [my_context:fileExists('/tmp/file.txt'), ...].

% Get just the templates
?- all_template_assertions(mydb, my_context, Templates).
Templates = [my_context:hasPrecondition(createFile, ...), ...].

% Get summary statistics
?- context_summary(mydb, my_context, Summary).
Summary = summary(
    context(my_context),
    total(42),
    ground_facts(30),
    templates(12),
    clauses(0),
    unique_functors(5)
).

% Find all assertions with a specific functor
?- find_assertions_by_functor(mydb, my_context, fileExists, Assertions).
Assertions = [...].
```

---

## Common Patterns

### Pattern 1: Store Specs, Check Against Reality

```prolog
% Setup (once)
store_template(mydb, specs:requires(backup, [hasSpace(Dest, Size), fileExists(Source)])).

% Add current state (frequently updated)
store_assert(mydb, state:hasSpace('/backup', 1000000)).
store_assert(mydb, state:fileExists('/data/important.db')).

% Check if backup is possible
can_backup :-
    store_call_template(mydb, specs:requires(backup, Requirements)),
    Requirements = [hasSpace(Dest, Size), fileExists(Source)],
    Dest = '/backup', Size = 1000000, Source = '/data/important.db',
    store_call(mydb, state:hasSpace(Dest, Size)),
    store_call(mydb, state:fileExists(Source)).
```

### Pattern 2: Version Control Your Specs

```prolog
% Store multiple versions of specifications
store_template(mydb, specs_v1:hasPrecondition(createFile, [directoryExists(D)])).
store_template(mydb, specs_v2:hasPrecondition(createFile, [directoryExists(D), hasPermission(D, write)])).

% Use the version you need
?- store_call_template(mydb, specs_v2:hasPrecondition(createFile, Conds)).
```

### Pattern 3: Multiple Contexts for Organization

```prolog
% Organize by purpose
store_template(mydb, file_ops:hasPrecondition(...)).
store_template(mydb, network_ops:hasPrecondition(...)).
store_template(mydb, db_ops:hasPrecondition(...)).

store_assert(mydb, current_state:fileExists(...)).
store_assert(mydb, cached_state:fileExists(...)).

% Query specific contexts
?- all_term_assertions(mydb, file_ops, FileOps).
?- all_term_assertions(mydb, current_state, CurrentState).
```

---

## Debugging Tips

### See what's stored:
```prolog
?- all_term_assertions(mydb, my_context, All), maplist(writeln, All).
```

### Count your data:
```prolog
?- count_assertions(mydb, my_context, Count), 
   format('You have ~w assertions stored~n', [Count]).
```

### Check if something exists:
```prolog
?- store_call(mydb, my_context:fileExists('/tmp/test.txt')).
true.  % It exists

?- store_call(mydb, my_context:fileExists('/nonexistent')).
false. % It doesn't exist
```

### List all contexts:
```prolog
?- list_contexts(mydb, Contexts).
Contexts = [my_context, other_context, ...].
```

---

## What About Clause Resolution (Test 6)?

You saw "No path found" in Test 6. That test uses `mysql_store_clauses.pl` to store rules like:

```prolog
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

This is for **automatic inference** - where Prolog chains through multiple rules to prove something.

**Do you need it?** Probably not! For most use cases:
- ✅ Use templates to store specifications
- ✅ Use ground facts to store state
- ✅ Write the logic in your Prolog code

Clause resolution is an advanced feature for when you want the database itself to do logical inference.

---

## Summary

**Three ways to store data:**

1. **Ground facts** → `store_assert(ConnId, Context:fact)`
   - For concrete data that changes
   
2. **Templates** → `store_template(ConnId, Context:pattern(Var))`
   - For specifications with variables
   
3. **Smart** → `store_assert_smart(ConnId, Context:anything)`
   - Auto-detects which method to use

**Three ways to query data:**

1. **Ground facts** → `store_call(ConnId, Context:fact)`
   - Match specific values
   
2. **Templates** → `store_call_template(ConnId, Context:pattern(Var))`
   - Get fresh variables
   
3. **Everything** → `all_term_assertions(ConnId, Context, All)`
   - Get all assertions at once

**Use cases:**
- Operation specifications (templates)
- System state (ground facts)
- Checking preconditions
- Configuration management
- Validation rules
- Any pattern matching scenario

---

## Next Steps

1. **Read** `example_filesystem_ops.pl` - Complete runnable examples
2. **Try** the Quick Start example above
3. **Experiment** with your own use case
4. **Check** `USAGE_GUIDE.md` for more advanced patterns

Happy coding! 🚀
