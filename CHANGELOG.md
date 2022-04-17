# Meeting room
https://ucph-ku.zoom.us/j/63343496731?pwd=Z3RiWUJ2ZmwvQ3haMTJmMmpkN1RCdz09

# Todo
- [ ] Look into whether non optimizing should add assertions for DLocal
- [ ] Fix that array modification only works for 1D arrays
- [ ] Communicate onwards whether invariant is provable correct
- [x] Actually implement array initialization both for globals and locals
  - [x] 1D
  - [x] nD
- [x] Import and rewrite optimizer to work in new and fancy way
  - [x] Make a global store used for every procedure
  - [x] Implement call/uncall
    - [x] Renaming to unique names
    - [x] Implementing a AST reverser for uncall, and other stuff
  - [x] Implement for loops
      - [x] Invariants (INVARIANTS CANNOT BE USED TO REMOVE THE ADDED ASSERTION SEE EXAMPLE BELOW)
        - [x] Make unrollable invariant loops invalidate variables modified within that is not part of invariant
      - [x] Constants
        - [x] Make unrollable for loops invalidate variables modified within!
- [x] Write translator from AST to C++
- [ ] Write playground website to use
- [ ] Write benchmark tests
- [ ] Write report
- [ ] profit

## Example why invariant cannot be used to prove the assertion away
for local int i = 0, invariant(i != 0) {
  x1 += 1
} i += 1, until (dealloc int i = 4)

 | |
  v

int i = 0
invariant(i != 0)
while (!(i == 4)) {
    body
    i += 1
    assert(!(i == 0))
}

# Nice to have TODO
- [ ] loop optimization
  - [ ] Size n should also be seen as constant, and not variable
    * Need some virtual table mapping array names to size
    * Wait with this for later
  - [ ] Maybe allow modification of increment value within body?
    * Either do not allow unrolling, or use extensive analysis to determine loop bound
      * Should definitely just start with the first


# Revision history for janus-like

## 0.1.0.0 -- 26/03-2022

* Parser to parse Japa programs as String
* Syntax describing the abstract syntax of a Japa program


##### shit save
                      -- {-
                      --   Must prove invariant at:
                      --     1. Initialization of loop.
                      --     2. Maintenance - if true before an iteration also true after.
                      --     3. Termination of loop! Otherwise the whole analysis does not matter.
                      -- -}
                      -- processExpr scope' inv' >>= \case
                      --   Right z3inv -> do
                      --     validated <- validate stmt z3inv
                      --     case validated of
                      --       Right True -> do
                      --         -- invariant true at initialization
                      --         Z3.push
                      --         Z3.assert z3inv

                      --         -- end ite on return () to make both branches have same return type
                      --         (scope''') <-
                      --           if (forward) then do
                      --             (_, scope'', _, _) <-
                      --               processStatements body' False ast scope' state warnings
                      --             (scope''', _, _) <- mod (Mod m Nothing pos) scope'' warnings
                      --             return scope'''
                      --           else do
                      --             (scope'', _, _) <- mod (Mod m Nothing pos) scope' warnings
                      --             (_, scope''', _, _) <-
                      --               processStatements body' False ast scope'' state warnings
                      --             return scope'''

                      --         validated <- validate stmt z3inv
                      --         case validated of
                      --           Right True -> do
                      --             {-
                      --               invariant true at initialization+maintenance.
                      --               This ALWAYS assumes that the decreasing/increasing variable is the loop variable
                                  
                      --               Based on whether <id> starts higher or lower than its end value,
                      --               I can determine whether it's increasing or decreasing.
                      --               Now all that is needed is:
                      --                 iteration start
                      --                 (get value of <id>)
                      --                 loop body
                      --                 (assert <id> being either larger or less than start)
                      --             -}
                      --             case tryGetVar name scope''' of
                      --               Just z3var' -> do
                      --                 z3ast <-
                      --                   case op of
                      --                     PlusEq -> Z3.mkBvugt z3var' z3var
                      --                     _      -> Z3.mkBvult z3var' z3var
                                      
                      --                 validate stmt z3ast >>= \case
                      --                   Right True ->
                      --                     -- also true at termination. So create scope, invalidate changed variables not part of
                      --                     -- invariant.
                      --                     -- Optimizing the assertion away is not possible only with invariant.
                      --                     Z3.pop 1
                      --                     >> invalidateVars (modifiedVars body' \\ exprVars inv') scope
                      --                     >>= (\scope' -> return (scope', False, body', warnings'))
                      --                   _ ->
                      --                     Z3.pop 1
                      --                     >> invalidateVars (modifiedVars body) scope
                      --                     >>= (\scope' -> return (scope', False, body',
                      --                           ("Loop invariant not true at termination " ++ show pos) : warnings'))

                      --               _ ->
                      --                 Z3.pop 1 >>
                      --                 return (scope, False, body',
                      --                   ("Loop variable " ++ show name ++ " has been invalidated") : warnings')


                      --           _ ->
                      --             Z3.pop 1
                      --             >> invalidateVars (modifiedVars body) scope
                      --             >>= (\scope' -> return (scope', False, body',
                      --                   ("Loop invariant not true at maintenance " ++ show pos) : warnings'))

                            
                      --       Right False ->
                      --         invalidateVars (modifiedVars body) scope
                      --         >>= (\scope' -> return (scope', False, body',
                      --               ("Loop invariant not true at initialization " ++ show pos) : warnings'))
                      --       Left errmsg ->
                      --         invalidateVars (modifiedVars body) scope
                      --         >>= (\scope' -> return (scope', False, body', errmsg : warnings'))

                      --   Left errvar ->
                      --     invalidateVars (modifiedVars body) scope
                      --       >>= (\scope' -> return (scope', False, body',
                      --               ("Loop invariant used invalidated variables at " ++ show pos) : warnings'))
