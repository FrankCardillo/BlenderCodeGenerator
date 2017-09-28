# CS 394P: Pattern Matching Exercise

The goal of this exercise is to gain experience in the use of pattern-based transformations for optimization, specialization of generic programs, language translation, code expansion, and partial evaluation.

Examine the code in the file ./patmatch.lsp and ./patm.lsp . A set of example patterns for use with this code is given in the file ./pats.lsp . Try the examples that are shown as comments in ./pats.lsp .

* Add to the optimization patterns given in ./pats.lsp in order to optimize the examples of code given in the file ./patex.lsp. Most of these examples are code produced by the GLISP compiler prior to optimization.

* Add to the loop patterns given in ./pats.lsp to implement the generic sequence functions min, max, and member-of. Demonstrate that these patterns work for linked lists and arrays.

* Add to the C translation patterns given in ./pats.lsp to allow the array functions in item 3 above to be translated into C.

* Write optimization patterns to unroll short loops. Test these on the function mxmult in the file ./patex.lsp. Write additional optimization patterns to optimize the results of loop unrolling.

-- It is necessary to remove old patterns if they turn out to be incorrect. You can add your patterns to the file ./pats.lsp and reload this file each time you make a change; this will remove the old patterns.

Files:

./patmatch.lsp    Simple matcher, as in lecture slides.
./patm.lsp        More powerful matcher
./pats.lsp        Starter set of patterns
./patex.lsp       Examples for use in optimization exercise
