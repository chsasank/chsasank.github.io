---
layout: post
title: "Neural Networks are Differentiable Interpreters"
author: Sasank Chilamkurthy
twitter_image: "https://media.geeksforgeeks.org/wp-content/cdn-uploads/20230602113310/Neural-Networks-Architecture.png"
---

I have been reading up on old computer science theory especially around programming languages and computer architecture. I have come to discover an interesting analogy between neural networks and programming languages and computers. To understand this analogy, we should dig a bit into history of computer science starting from Alan Turing's computable numbers.

## Turing's Computable Numbers

Alan Turing wanted to solve Hilbert's [Decision Problem](https://en.wikipedia.org/wiki/Entscheidungsproblem). The problem asks for an algorithm that takes in a statement/theorem and decide whether the statement is true or false. In other words, can we mechanically prove any statement as true of false? He proved that such an algorithm doesn't exist and that there are statements that can never be proven true or false. His approach to the problem is very interesting.

He needed to first define what 'mechanical' meant or in other words, what an algorithm is. He invents Turing Machine for this by modelling pen/paper computation of a mathematician. He then shows that any 'mechanical' computation is equivalent to a Turing Machine. Therefore, if something is 'computable' by Turing Machine, it's computable by any other method and vice versa. Thus, it is enough to work with Turing Machine abstraction to tackle Hilbert's Decision Problem.

A computable number is defined as a number whose nth bit can be determined by a Turing Machine in finite number of steps. He shows that all integers are computable by simply working with Peano's axioms which basically say that every natural number has a next one. Since integers are computable, rational numbers are also computable by creating program for division. And so on. Thus computable numbers have a program associated with them. Since the program for Turing Machine can also be encoded into 0/1s, there exist a mapping from every integer to a computable number. He then uses [diagonalization trick](https://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument) to show that there are numbers which do not correspond to any possible program.

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Cantor's Diagonalization Argument. Construct a number by flipping bits of diagonal elements of every computable number $s_i$. This number can't be computable because it differs from every possible computable number.
Figure from Wikipedia.</span>
<img src="/assets/images/random/diagonal.png" alt="Cantor's Diagonalization Argument">
</figure>

If Hilbert's Decision Problem had a solution, we should be able to construct a machine that can determine whether a program halts or not. Since 


Mathematicians start with axioms which they take for granted as true (i.e. self-evident). They then 'mechanically' apply rules of inference to generate true statements called theorems. Any statement that leads to contradiction of a true statement is a false statement.
Therefore, there are statements that can not be arrived by following a mechanical process of manipulating finite axioms.