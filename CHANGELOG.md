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
  - [ ] Implement call/uncall
    - [x] Renaming to unique names
    - [ ] Implementing a AST reverser for uncall, and other stuff
  - [ ] Implement for loops
      - [ ] Invariants
      - [ ] Constants
* Write translator from AST to C++
* Write playground website to use
* profit
