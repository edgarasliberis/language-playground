# language-playground

Expression manipulation (in OCaml, see Compilers course):
* Implement expression evaluator for arithmetic ops, add operator precedence and variables;
* Implement compiling into machine instructions
* Implement a stack-based machine with memory to run these instructions
* For lambda expressions, determine equivalence
* Given some grammar, determine longest expression generable (if finite)

Functional primitives (OCaml) [see functional_primitives.ml]:
* Memoisation wrapper. Optionally cache control.
* Map, foldl, foldr, filter + tail recursive where appropriate; explain, write types

Data structures (that functional data structures book will be useful here) [see data.ml]:
* Priority queue using a stack
* Linked list. 
* Array heap, making invariants hold, all operations.
* Binary (+search) tree and all operations (insert, delete, traverse, to list, length, depth, k-th child) 

Combinatorics (OCaml) [see combinatorics.ml]:
* Generate all permutations of a string
* Given a list of N people. On the first day, divide them into N/2 groups, each group contains two people. On the day 2, divide them into groups of two again... Do this every day, until day N-1. In a way such that all pairs of people has been groupmates once.
* Generate the combinations of K distinct objects chosen from the N elements of a list. (+ other from OCaml 99 problems)

Misc [see misc.ml]:
* Given a set of strings, return a set of [lists of anagrams; pairs of palindromes; lists of strings with same acronyms]
* Rotate array n times in constant space and linear time
* Find nth element in merged sorted arrays
* Given a collection of lists, return one with minimum length / minimum sum of elements.

Python:
* Sieve of Erathostenes
