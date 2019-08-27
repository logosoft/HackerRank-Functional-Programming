# HackerRank Functional Programming 

[![HackerRank logo](https://hrcdn.net/fcore/assets/brand/h_mark_sm-966d2b45e3.svg)](https://www.hackerrank.com) 

Collection of solutions to [HackerRank](https://www.hackerrank.com) practice problems in [Functional Programming](https://www.hackerrank.com/domains/fp).

All the code tested with Scala version 2.13.1.

All test cases passed. 


![Scala version](https://img.shields.io/badge/scala-2.13.1-brightgreen)
![Test passing](https://img.shields.io/badge/test-passing-brightgreen)




## Index

- [Introduction](#Introduction)
- [Recursion](#Recursion)
- [Functional Structures](#Functional-Structures)
- [Memoization and DP](#Memoization-and-DP)
- [Persistent Structures](#Persistent-Structures)
- [Ad Hoc](#Ad-Hoc)
- [Parsers](#Parsers)
- [Interpreter and Compilers](#Interpreter-and-Compilers)

### Introduction

| Problem | Solution |
| :--- | :---: |
|  [Solve Me First FP](https://www.hackerrank.com/challenges/fp-solve-me-first/problem) | [Scala](src/main/introduction/fp_solve_me_first/Solution.scala) |
|  [Hello World](https://www.hackerrank.com/challenges/fp-hello-world/problem) | [Scala worksheet](src/main/introduction/hello_world/Solution.sc) |
|  [Hello World N Times](https://www.hackerrank.com/challenges/fp-hello-world-n-times/problem) | [Scala worksheet](src/main/introduction/hello_world_n_times/Solution.sc) |
|  [List Replication](https://www.hackerrank.com/challenges/fp-list-replication/problem) | [Scala worksheet](src/main/introduction/fp_list_replication/Solution.sc) |
|  [Filter Array](https://www.hackerrank.com/challenges/fp-filter-array/problem) | [Scala worksheet](src/main/introduction/fp_filter_array/Solution.sc) |
|  [Filter Positions in a List](https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem) | [Scala worksheet](src/main/introduction/fp_filter_positions_in_a_list/Solution.sc) |
|  [Array Of N Elements](https://www.hackerrank.com/challenges/fp-array-of-n-elements/problem) | [Scala worksheet](src/main/introduction/fp_array_of_n_elements/Solution.sc) |
|  [Reverse a List](https://www.hackerrank.com/challenges/fp-reverse-a-list/problem) | [Scala worksheet](src/main/introduction/fp_reverse_a_list/Solution.sc) |
|  [Sum of Odd Elements](https://www.hackerrank.com/challenges/fp-sum-of-odd-elements/problem) | [Scala worksheet](src/main/introduction/fp_sum_of_odd_elements/Solution.sc) |
|  [List Length](https://www.hackerrank.com/challenges/fp-list-length/problem) | [Scala worksheet](src/main/introduction/fp_list_length/Solution.sc) |
|  [Update List](https://www.hackerrank.com/challenges/fp-update-list/problem) | [Scala worksheet](src/main/introduction/fp_update_list/Solution.sc) |
|  [Evaluating e^x](https://www.hackerrank.com/challenges/eval-ex/problem) | [Scala](src/main/introduction/eval_ex/Solution.scala) |
|  [Area Under Curves and Volume of Revolving a Curve](https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv/problem) | [Scala worksheet](src/main/introduction/area_under_curves_and_volume_of_revolving_a_curv/Solution.sc) |
|  [Lambda Calculus - Reductions #1](https://www.hackerrank.com/challenges/lambda-calculus-reductions-1/problem) | [Text](src/main/introduction/lambda_calculus_reductions_1/Solution.txt) |
|  [Lambda Calculus - Reductions #2](https://www.hackerrank.com/challenges/lambda-calculus-reductions-2/problem) | [Text](src/main/introduction/lambda_calculus_reductions_2/Solution.txt) |
|  [Lambda Calculus - Reductions #3](https://www.hackerrank.com/challenges/lambda-calculus-reductions-3/problem) | [Text](src/main/introduction/lambda_calculus_reductions_3/Solution.txt) |
|  [Lambda Calculus - Reductions #4](https://www.hackerrank.com/challenges/lambda-calculus-reductions-4/problem) | [Text](src/main/introduction/lambda_calculus_reductions_4/Solution.txt) |
|  [Lambda Calculus - Evaluating Expressions #1](https://www.hackerrank.com/challenges/lambda-calculus-getting-started/problem) | [Text](src/main/introduction/lambda-calculus-getting_started/Solution.txt) |
|  [Lambda Calculus - Evaluating Expressions #2](https://www.hackerrank.com/challenges/lambda-calculus-understanding-the-syntax/problem) | [Text](src/main/introduction/lambda_calculus_understanding_the_syntax/Solution.txt) |
|  [Lambda Calculus - Evaluating Expressions #3](https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression/problem) | [Text](src/main/introduction/lambda_calculus_evaluate-the_expression/Solution.txt) |
|  [Lambda Calculus - Evaluating Expressions #4](https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression-1/problem) | [Text](src/main/introduction/lambda_calculus_evaluate_the_expression_1/Solution.txt) |
|  [Lambda Calculus - Evaluating Expressions #5](https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression-2/problem) | [Text](src/main/introduction/lambda_calculus_evaluate_the_expression_2/Solution.txt) |
|  [Functions or Not?](https://www.hackerrank.com/challenges/functions-or-not/problem) | [Scala](src/main/introduction/functions_or_not/Solution.scala) |
|  [Compute the Perimeter of a Polygon](https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon/problem) | [Scala](src/main/introduction/lambda_march_compute_the_perimeter_of_a_polygon/Solution.scala) |
|  [Compute the Area of a Polygon](https://www.hackerrank.com/challenges/lambda-march-compute-the-area-of-a-polygon/problem) | [Scala](src/main/introduction/lambda_march_compute_the_area_of_a_polygon/Solution.scala) |
  
### Recursion

| Problem | Solution |
| :--- | :---: |
|  [Computing the GCD](https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd/problem) | [Scala](src/main/recursion/functional_programming_warmups_in_recursion___gcd/Solution.scala) |
|  [Fibonacci Numbers](https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---fibonacci-numbers/problem) | [Scala](src/main/recursion/functional_programming_warmups_in_recursion___fibonacci_numbers/Solution.scala) |
|  [Pascal's Triangle](https://www.hackerrank.com/challenges/pascals-triangle/problem) | [Scala](src/main/recursion/pascals_triangle/Solution.scala) |
|  [Functions and Fractals: Sierpinski triangles](https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles/problem) | [Scala](src/main/recursion/functions_and_fractals_sierpinski_triangles/Solution.scala) |
|  [String Mingling](https://www.hackerrank.com/challenges/string-mingling/problem) | [Scala](src/main/recursion/string_mingling/Solution.scala) |
|  [String-o-Permute](https://www.hackerrank.com/challenges/string-o-permute/problem) | [Scala](src/main/recursion/string_o_permute/Solution.scala) |
|  [Functions and Fractals - Recursive Trees](https://www.hackerrank.com/challenges/fractal-trees/problem) | [Scala](src/main/recursion/fractal_trees/Solution.scala) |
|  [Convex Hull](https://www.hackerrank.com/challenges/convex-hull-fp/problem) | [Scala](src/main/recursion/convex_hull_fp/Solution.scala) |
|  [String Compression](https://www.hackerrank.com/challenges/string-compression/problem) | [Scala](src/main/recursion/string_compression/Solution.scala) |
|  [Crosswords-101](https://www.hackerrank.com/challenges/crosswords-101/problem) | [Scala](src/main/recursion/crosswords_101/Solution.scala) |
|  [Prefix Compression](https://www.hackerrank.com/challenges/prefix-compression/problem) | [Scala](src/main/recursion/prefix_compression/Solution.scala) |
|  [String Reductions](https://www.hackerrank.com/challenges/string-reductions/problem) | [Scala](src/main/recursion/string_reductions/Solution.scala) |
|  [Super-Queens on a Chessboard](https://www.hackerrank.com/challenges/super-queens-on-a-chessboard/problem) | [Scala](src/main/recursion/super_queens_on_a_chessboard/Solution.scala) |
|  [The Sums of Powers](https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers/problem) | [Scala](src/main/recursion/functional_programming_the_sums_of_powers/Solution.scala) |
|  [Sequence full of colors](https://www.hackerrank.com/challenges/sequence-full-of-colors/problem) | [Scala](src/main/recursion/sequence_full_of_colors/Solution.scala) |
|  [Filter Elements](https://www.hackerrank.com/challenges/filter-elements/problem) | [Scala](src/main/recursion/filter_elements/Solution.scala) |
|  [Super Digit](https://www.hackerrank.com/challenges/super-digit/problem) | [Scala](src/main/recursion/super_digit/Solution.scala) |
|  [The Tree Of Life](https://www.hackerrank.com/challenges/the-tree-of-life/problem) | [Scala](src/main/recursion/the_tree_of_life/Solution.scala) |

### Functional Structures
### Memoization and DP
### Persistent Structures
### Ad Hoc
### Parsers
### Interpreter and Compilers

## Author
Oleg Oleshchuk

## License

[![License](http://img.shields.io/:license-mit-blue.svg?style=flat-square)](http://badges.mit-license.org)

- [MIT license](http://opensource.org/licenses/mit-license.php)
- Copyright 2019 © <a href="https://www.linkedin.com/in/oleg-oleshchuk/" target="_blank">Oleg Oleshchuk</a>.