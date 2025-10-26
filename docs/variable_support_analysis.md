# Variable Support for prolog-mysql-store: Design Exploration

## The Challenge

Currently, the system only supports **ground terms** (fully instantiated). You want to store terms like:
```prolog
hasPrecondition(operation1, [hasSymlink(A, B)])
```

This requires:
1. **Storage**: How to represent unbound variables in MySQL
2. **Unification**: How to match patterns with variables during queries
3. **Variable scoping**: Ensuring variables in different clauses are distinct

## Approach 1: Simple Variable Naming (Your Initial Idea)

### Storage Strategy
```prolog
% Original term
hasPrecondition(operation1, [hasSymlink(A, B)])

% Stored as (using term_variables/2)
hasPrecondition(operation1, [hasSymlink('$VAR'(0), '$VAR'(1))])
```

### Implementation
```prolog
normalize_term_for_storage(Term, Normalized) :-
    copy_term(Term, Normalized),
    term_variables(Normalized, Vars),
    numbervars(Vars, 0, _).
```

### Pros
- Simple to implement
- Variables become ground terms (can store immediately)
- Preserves variable structure

### Cons
- **Lost unification semantics**: `'$VAR'(0)` is just an atom, not a real variable
- **No cross-clause unification**: Can't unify variables across different stored facts
- **Variable renaming issues**: Two facts with same variable name treated as identical

### When to use
Only if you need to store "variable-shaped" data for documentation or templates, but don't need true logical unification.

---

## Approach 2: Variable Substitution Tables

### Storage Strategy
Store variable bindings separately:

**Main table (formulae):**
```sql
formula_id | context_id | functor | arity | term_canonical
1          | 1          | hasPrecondition | 2 | hasPrecondition(operation1,[hasSymlink(_G123,_G124)])
```

**Variable table:**
```sql
CREATE TABLE term_variables (
    formula_id INT,
    var_name VARCHAR(50),
    var_position TEXT,  -- e.g., "2.1.1" for nested position
    FOREIGN KEY (formula_id) REFERENCES formulae(formula_id)
);
```

### Implementation Sketch
```prolog
store_assert_with_vars(ConnectionId, Term) :-
    term_variables(Term, Vars),
    (Vars = [] ->
        % Ground term, use existing logic
        store_assert(ConnectionId, Term)
    ;
        % Has variables, store with variable table
        store_term_and_variables(ConnectionId, Term, Vars)
    ).

store_term_and_variables(ConnectionId, Term, Vars) :-
    % Generate unique variable names
    maplist(gensym('_G'), Vars),
    
    % Store the term with named variables
    term_string(Term, TermStr),
    % ... insert into formulae ...
    
    % Store variable positions
    forall(
        (nth1(N, Vars, Var),
         variable_positions(Term, Var, Positions)),
        store_variable_info(ConnectionId, FormulaId, Var, Positions)
    ).
```

### Query Strategy
```prolog
store_call_with_vars(ConnectionId, Goal) :-
    % 1. Retrieve candidate terms from DB
    % 2. For each term, reconstruct variables
    % 3. Attempt unification with Goal
    % 4. If unification succeeds, yield solution
    ...
```

### Pros
- Can store true variable information
- Enables variable tracking across queries
- Potential for sophisticated variable analysis

### Cons
- Complex implementation
- Query performance overhead (need to reconstruct terms)
- **Still doesn't solve unification across stored clauses**

---

## Approach 3: Skolemization (Convert Variables to Functions)

### Core Idea
Replace variables with unique function terms (Skolem constants):

```prolog
% Original
hasPrecondition(operation1, [hasSymlink(A, B)])

% Skolemized
hasPrecondition(operation1, [hasSymlink(skolem(formula_1, var_1), 
                                        skolem(formula_1, var_2))])
```

### Implementation
```prolog
skolemize_term(Term, FormulaId, Skolemized) :-
    copy_term(Term, Skolemized),
    term_variables(Skolemized, Vars),
    skolemize_vars(Vars, FormulaId, 1).

skolemize_vars([], _, _).
skolemize_vars([Var|Vars], FormulaId, N) :-
    Var = skolem(FormulaId, N),
    N1 is N + 1,
    skolemize_vars(Vars, FormulaId, N1).
```

### Query Strategy
During query, recognize skolem/2 terms and treat them as unifiable:

```prolog
unify_with_skolem(skolem(_, _), _) :- !.
unify_with_skolem(X, Y) :- X = Y.
```

### Pros
- Variables become ground terms (storable)
- Each variable has unique identity
- Can implement custom unification semantics

### Cons
- Not true Prolog unification
- Skolem constants clutter the database
- **Still limited**: Can't share variables between clauses

---

## Approach 4: Template Storage with On-Demand Instantiation

### Core Idea
Store terms as **templates** that get instantiated when queried:

```prolog
% Store as template
store_template(ConnectionId, TemplateId, Term) :-
    % Store term with variables intact (as string or special format)
    term_string(Term, TemplateStr),
    % ... store TemplateStr in database ...
    
% Query instantiates templates
store_call_template(ConnectionId, Pattern) :-
    % 1. Fetch matching templates
    % 2. read_term_from_atom to recreate with fresh variables
    % 3. Unify with Pattern
    % 4. Yield solutions
    ...
```

### Implementation
```prolog
store_template(ConnectionId, Context:Term) :-
    % Serialize term preserving variables
    format(atom(TemplateAtom), '~k', [Term]),
    
    % Store in database
    functor(Term, Functor, Arity),
    sha_hash(TemplateAtom, Hash, [algorithm(sha256)]),
    % ... INSERT with template representation ...
    
store_call_template(ConnectionId, Context:Pattern) :-
    % Fetch matching templates
    functor(Pattern, Functor, Arity),
    fetch_templates(ConnectionId, Context, Functor, Arity, Templates),
    
    % For each template, instantiate and unify
    member(TemplateAtom, Templates),
    read_term_from_atom(TemplateAtom, Instance, [variable_names(VarNames)]),
    Instance = Pattern.  % Unification happens here!
```

### Pros
- **Preserves true Prolog variables**
- Natural unification behavior
- Each query gets fresh variable instances

### Cons
- Can't index on variable positions efficiently
- Serialization/deserialization overhead
- **Still isolated**: Each stored term is independent

---

## Approach 5: Clause-Based Storage (The "Right" Way)

### Core Insight
Your request suggests you want something like:

```prolog
% Store
store_assert(mydb, kb:hasPrecondition(operation1, [hasSymlink(A, B)])).
store_assert(mydb, kb:hasSymlink('/tmp/link', '/tmp/target')).

% Query (unification across clauses!)
?- store_call(mydb, kb:hasPrecondition(operation1, [Cond])),
   store_call(mydb, kb:Cond).
```

This requires **cross-clause variable binding**, which is fundamentally different from storing individual facts.

### The Problem
This is asking for a **theorem prover** or **clause database**, not just fact storage:

1. **Variable scope**: Variables in one clause must potentially unify with terms in another
2. **Substitution tracking**: Need to maintain bindings across multiple queries
3. **Occurs check**: Need proper unification algorithm
4. **Performance**: This is computationally expensive

### Possible Implementation
```prolog
% Store clauses with explicit variable representation
store_clause(ConnectionId, Head :- Body) :-
    % Separate head and body
    % Store each with variable correlation table
    store_clause_components(ConnectionId, Head, Body, ClauseId),
    store_variable_links(ConnectionId, ClauseId, Head, Body).

% Query with resolution
store_resolve(ConnectionId, Goal) :-
    % Fetch clauses where head unifies with Goal
    fetch_matching_clauses(ConnectionId, Goal, Clauses),
    
    % For each clause, attempt resolution
    member(clause(Head, Body), Clauses),
    copy_term(clause(Head, Body), clause(HeadFresh, BodyFresh)),
    Head = Goal,  % Unification
    resolve_body(ConnectionId, BodyFresh).

resolve_body(ConnectionId, (Goal1, Goal2)) :- !,
    store_resolve(ConnectionId, Goal1),
    resolve_body(ConnectionId, Goal2).
resolve_body(ConnectionId, Goal) :-
    store_resolve(ConnectionId, Goal).
```

### Reality Check
This is implementing **SLD resolution** over a database-backed knowledge base. It's a research-level project!

---

## Recommended Pragmatic Approaches

### Option A: Template Storage (Approach 4)
**Best for**: Storing term patterns that will be instantiated independently

```prolog
:- use_module(mysql_store_templates).

% Store a template
?- store_template(mydb, rules:hasPrecondition(operation1, [hasSymlink(A, B)])).

% Query - gets fresh variables each time
?- store_call_template(mydb, rules:hasPrecondition(Op, Conds)).
Op = operation1,
Conds = [hasSymlink(_G123, _G124)].
```

### Option B: Hybrid Approach
Store ground facts + in-memory rules:

```prolog
% Store ground facts in MySQL
?- store_assert(mydb, kb:hasSymlink('/tmp/a', '/tmp/b')).
?- store_assert(mydb, kb:operation(operation1)).

% Define rules in Prolog
hasPrecondition(Op, [hasSymlink(A, B)]) :-
    store_call(mydb, kb:operation(Op)),
    Op = operation1,
    store_call(mydb, kb:hasSymlink(A, B)).

% Query
?- hasPrecondition(operation1, Conds).
Conds = [hasSymlink('/tmp/a', '/tmp/b')].
```

This is actually how most Prolog systems work: facts in database, rules in code.

---

## Implementation Priority

### Phase 1: Template Storage (Feasible)
Implement Approach 4 - allows storing terms with variables as patterns.

### Phase 2: Pattern Matching Optimization
Add indexing for common patterns, variable position tracking.

### Phase 3: (Optional) Clause Storage
If you really need cross-clause unification, implement simplified clause database.

---

## Key Questions to Guide Design

1. **Do you need cross-clause unification?**
   - If NO → Template storage (Approach 4)
   - If YES → Clause storage (Approach 5) or hybrid

2. **Are variables just placeholders or do they need to unify across queries?**
   - Placeholders → Skolemization (Approach 3)
   - True unification → Template storage (Approach 4)

3. **Is this for preconditions/postconditions of operations?**
   - Consider storing condition *templates* separately from instances
   - Match templates to instances at query time

4. **Performance vs. Expressiveness?**
   - Fast reads → Skolemization or simple naming
   - Full Prolog semantics → Template or clause storage

---

## Code Skeleton for Template Storage

```prolog
:- module(mysql_store_templates, [
    store_template/2,
    store_call_template/2,
    store_retract_template/2
]).

:- use_module(mysql_store).

% Store a term with variables as a template
store_template(ConnectionId, Context:Term) :-
    % Serialize preserving variables
    format(atom(TemplateAtom), '~k', [Term]),
    
    % Extract metadata
    functor(Term, Functor, Arity),
    
    % Generate hash (templates with same structure should hash same)
    term_hash(Term, Hash),
    
    % Store in special templates table
    ensure_templates_table(ConnectionId),
    store_template_record(ConnectionId, Context, Functor, Arity, 
                          TemplateAtom, Hash).

% Query templates
store_call_template(ConnectionId, Context:Pattern) :-
    functor(Pattern, Functor, Arity),
    fetch_templates(ConnectionId, Context, Functor, Arity, Templates),
    member(TemplateAtom, Templates),
    
    % Recreate term with fresh variables
    read_term_from_atom(TemplateAtom, Instance, []),
    
    % Unification!
    Instance = Pattern.

% Helper to create templates table
ensure_templates_table(ConnectionId) :-
    Query = 'CREATE TABLE IF NOT EXISTS formula_templates (
        template_id INT PRIMARY KEY AUTO_INCREMENT,
        context_id INT NOT NULL,
        functor VARCHAR(255) NOT NULL,
        arity INT NOT NULL,
        template_repr TEXT NOT NULL,
        template_hash BINARY(32) NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE KEY unique_template (context_id, template_hash),
        KEY idx_functor_arity (context_id, functor, arity),
        FOREIGN KEY (context_id) REFERENCES contexts(context_id)
    )',
    odbc_query(ConnectionId, Query, _).
```

Would you like me to implement a complete working version of template storage, or would you prefer to explore one of the other approaches first?
