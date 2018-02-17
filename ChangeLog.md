# Revision history for milc

## 0.2.1.0  -- 2018-02-16

* Implement a simble optimizer
    * enabled using `-O1`
    * does:
        * basic block merging
        * local copy propagation
        * constant folding
        * simple strength reduction

## 0.2.0.0  -- 2018-02-10

* Changed name of the project from "mcomp" to "milc"
* Added a recursive descent parser and AST implementation
* Added the MIL intermediate language implementation
    * includes MIL generation from the AST
* Added a code generator targeting Robin's Stack Machine (RSM)
    * includes support for the csh implementation

## 0.1.0.0  -- 2018-01-18

* First version. Released on an unsuspecting world.
