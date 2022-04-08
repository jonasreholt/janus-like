# Meeting room
https://ucph-ku.zoom.us/j/63343496731?pwd=Z3RiWUJ2ZmwvQ3haMTJmMmpkN1RCdz09

# Revision history for janus-like

## 0.1.0.0 -- 26/03-2022

* Parser to parse Japa programs as String
* Syntax describing the abstract syntax of a Japa program

### Todo
* Actually implement array initialization both for globals and locals
  - [ ] 1D
  - [ ] nD
* Import and rewrite optimizer to work in new and fancy way
  - [x] Make a global store used for every procedure
  - [x] Implement call/uncall
    - [x] Renaming to unique names
    - [x] Implementing a AST reverser for uncall, and other stuff
  - [ ] Implement for loops
      - [ ] Invariants
        - [ ] Make unrollable invariant loops invalidate variables modified within that is not part of invariant
      - [x] Constants
        - [x] Make unrollable for loops invalidate variables modified within!
      - [ ] Size n should also be seen as constant, and not variable
        * Need some virtual table mapping array names to size
      - [ ] Maybe allow modification of increment value within body?
        * Either do not allow unrolling, or use extensive analysis to determine loop bound
          * Should definately just start with the first
* Write translator from AST to C++
* Write playground website to use
* profit
