# Meeting room
https://ucph-ku.zoom.us/j/63343496731?pwd=Z3RiWUJ2ZmwvQ3haMTJmMmpkN1RCdz09
Meeting ID: 633 4349 6731
Passcode: 276031

# Todo
- [x] Add bool to playground editor syntax
- [x] Still need to debug loop optimizations in regards to perm-to-code.japa
- [x] Add the new loopDescsending to invariant function to determine termination
- [x] Make invalidation not remove var, but generalize it into a variable of
      which we only know its existence.
- [x] Make invariant maintenance work on general assumptions only (so only invalidated vars).
- [x] Look into whether non optimizing should add assertions for DLocal
- [x] Fix that array modification only works for 1D arrays
- [x] Fix arrays in expressions
- [x] Communicate onwards whether invariant is provable correct
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
- [x] Write playground website to use
- [x] Write benchmark tests
- [ ] Write execution test; testing runtime behavior of optimized and unoptimized.
- [ ] Write report
  - [ ] Maybe add some more in-depth profiling analysis of execution time for benchmark
        section.
- [ ] profit


## Report
intro:
* Also what I'm missing e.g. recursive procedures.
* Why do this and also goal of project
* Overview of method used (using theorem provers, simple language)
* Maybe a teaser with a small japa example
* Catch the reader with an intuition
* Limitation is that we are not looking at any other optimization!!

2:
* small examples things showing the syntax
* for semantics reference Robert's paper on Janus,
  and show the changes from janus
* focus on extentions such as invariants and for-loops.
  Remember to show examples as there exists none.

3:
* Remember to talk about SSA and stuff

4:
* Gather theory considerations (what we ask the theorem prover,
  how it proves things, why did we choose haskell) into section 3.
  to avoid overloading this section.
* 4 should focus on the engineering of the compiler.
  This is more on the how
* Move Lexer Parser into a front-end section
* Use Torbens way of showing translation in the report.
* Use diagrams to show the overview!!!

5:
* Remember to show the lackings og the optimizer
  e.g. it cannot optimize something, but then
  assertions of invariants can be set to make it
  optimize it.
  Also just things that does not work.
  So this way we work us into what it cannot do

6:
* Clever optimization programs that advertises what the
  optimization can do (so that it removes non obvious things)
* e.g. in nested loops.

7:
* The haskell code consists of # lines of code divided into these
  modules

Until next time:
Flesh out the report, fix problems in compiler,
and think more about the test programs and testing!!
We will look at draft report and benchmark tests


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
- [ ] negative integers
- [x] reverse assertionremoval should not analyze main function!
- [ ] Fix perm-to-code.japa reverse loop stating inner loop is ascending, as it performs
      underflow in z3 at the moment!!
- [ ] Proper error information on the playground
- [ ] Boolean arithmetic. Z3 does not support it!!
- [ ] loop optimization
  - [x] make a small unroll to see if loop is optimizable
    - Before processing unrollable loops, try and remove any prior known information
      and then process the loop, to see if it's always provable correct.
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
