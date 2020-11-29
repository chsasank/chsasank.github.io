---
layout: post
title: Form and Content in Computer Science
author: Marvin Minsky
category: project_turing
description: 
published: 1970-04-01
twitter_image: https://upload.wikimedia.org/wikipedia/en/5/59/Alan_Perlis.jpg
---

## Abstract

An excessive preoccupation with formalism is impeding the development of
computer science. Form-content confusion is discussed relative to three
areas: theory of computation, programming languages, and education.

The trouble with computer science today is an obsessive concern with form
instead of content.

No, that is the wrong way to begin. By any previous standard the vitality of
computer science is enormous; what other intellectual area ever advanced so
far in twenty years? Besides, the theory of computation perhaps encloses, in
some way, the science of form, so that the concern is not so badly
misplaced. Still, <mark>I will argue that an excessive preoccupation with
formalism is impeding our development.</mark>

Before entering the discussion proper, I want to record the satisfaction my
colleagues, students, and I derive from this Turing award. The cluster of
questions, once philosophical but now scientific, surrounding the
understanding of intelligence was of paramount concern to Alan Turing, and
he along with a few other thinkers – notably Warren S. McCulloch and his
young associate, Walter Pitts –  made many of the early analyses that led
both to the computer itself and to the new technology of artificial
intelligence. In recognizing this area, this award should focus attention on
other work of my own scientific family –   especially Ray Solomonoff, Oliver
Selfridge, John McCarthy, Allen Newell, Herbert Simon, and Seymour Papert,
my closest associates in a decade of work. Papert's views pervade this
essay.

This essay has three parts, suggesting form-content confusion in *theory of
computation*, in *programming languages*, and in *education*.

## 1. Theory of Computation

To build a theory, one needs to know a lot about the basic phenomena of the
subject matter. We simply do not know enough about these, in the theory of
computation, to teach the subject very abstractly. Instead, we ought to
teach more about the particular examples we now understand thoroughly, and
hope that from this we will be able to guess and prove more general
principles. I am not saying this just to be conservative about things
probably true that haven't been proved yet. I think that many of our beliefs
that seem to be common sense are false. <mark>We have bad misconceptions about the
possible exchanges between time and memory, tradeoffs between time and
program complexity, software and hardware, digital and analog circuits,
serial and parallel computations, associative and addressed memory, and so
on.</mark>

It is instructive to consider the analogy with physics, in which one can
organize much of the basic knowledge as a collection of rather compact
conservation laws. This, of course, is just one kind of description; one
could use differential equations, minimum principles, equilibrium laws, etc.
Conservation of energy, for example, can be interpreted as defining
exchanges between various forms of potential and kinetic energies, such as
between height and velocity squared, or between temperature and
pressure-volume. One can base a development of quantum theory on trade-off
between certainties of position and momentum, or between time and energy.
There is nothing extraordinary about this; any equation with reasonably
smooth solutions can be regarded as defining some kind of trade-off among
its variable quantities. But there are many ways to formulate things and it
is risky to become too attached to one particular form or law and come to
believe that it is the *real* basic principle. See Feynman's [1]
dissertation on this.



Nonetheless, the recognition of exchanges is often the conception of a
science, if quantifying them is its birth. What do we have, in the
computation field, of this character? In the theory of recursive functions,
we have the observation by Shannon [2] that any Turing machine with $Q$ states
and $R$ symbols is equivalent to one with 2 states and $nQR$ symbols, and to one
with 2 symbols and $n'QR$ states, where $n$ and $n'$ are small numbers. Thus the
state-symbol product $QR$ has an almost invariant quality in classifying
machines. Unfortunately, one cannot identify the product with a useful
measure of machine complexity because this, in turn, has a trade-off with
the complexity of the encoding process for the machines – and that trade-off
seems too inscrutable for useful application.


Let us consider a more elementary, but still puzzling, trade-off, that
between addition and multiplication. How many multiplications does it take
to evaluate the $3 \times 3$ determinant? If we write out the expansion as
six triple-products, we need twelve multiplications. If we collect factors,
using the distributive law, this reduces to nine. What is the minimum
number, and how does one prove it, in this and in the $n \times n$ case? The
important point is not that we need the answer. It is that we do not know
how to tell or prove that proposed answers are correct! For a particular
formula, one could perhaps use some sort of exhaustive search, but that
wouldn't establish a general rule. <mark>One of our prime research goals should be
to develop methods to prove that particular procedures are computationally
minimal, in various senses.</mark>


A startling discovery was made about multiplication itself in the thesis of Cook [3], which uses a result of Toom; it is discussed in Knuth [4]. Consider the ordinary algorithm for multiplying decimal numbers: for two $n$-digit numbers this employs $n^2$ one-digit products. It is usually supposed that this is minimal. But suppose we write the numbers in two halves, so that the product is $N = (@A + B)(@C + D)$,
where $@$ stands for multiplying by $10^{n/2}$. (The left-shift operation is considered to have negligible cost.) Then one can verify that

$$ N=@@ AC + BD + @(A + B)(C + D)- @(AC + BD).$$

This involves only three half-length multiplications, instead of the four that one might suppose were needed. For large $n$, the reduction can obviously be reapplied over and over to the smaller numbers. The price is a growing number of additions. By compounding this and other ideas, Cook showed that for any $\epsilon$ and large enough $n$, multiplication requires less than $n^{1+\epsilon}$ products, instead of the expected $n^2$. Similarly, V. Strassen showed recently that to multiply two m X m matrices, the number of products could be reduced to the order of $m^{\log_27}$, when it was always believed that the number must be cubic because there are $m^2$ terms in the result and each would seem to need a separate inner product with $m$ multiplications. In both cases ordinary intuition has been wrong for a long time, so wrong that apparently no one looked for better methods. We still do not have a set of proof methods adequate for establishing exactly what is the minimum trade-off exchange, in the matrix ease, between multiplying and adding.

The multiply-add exchange may not seem vitally important in itself, but if we cannot thoroughly understand something so simple, we can expect serious trouble with anything more complicated.

Consider another trade-off, that between memory size and computation time. In our book [5], Papert and I have posed a simple question: given an arbitrary collection of $n$-bit words, how many references to memory are required to tell which of those words is nearest<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">
For identifying an *exact* match, one can use hash-coding and the problem is reasonably well understood.
</span> (in number of bits that agree) to an arbitrary given word? Since there are many ways to encode the "library" collection, some using more memory than others, the question stated more precisely is: how must the memory size grow to achieve a given reduction in the number of memory references? This much is trivial: if memory is large enough, only one reference is required, for we can use the question itself as address, and store the answer in the register so addressed. But if the memory is just large enough to store the information in the library, then one has to search all of it--*and we do not know any intermediate results of any value*. It is surely a fundamental theoretical problem of information retrieval, yet no one seems to have any idea about how to set a good lower bound on this basic trade-off.

Another is the serial-parallel exchange. Suppose that we had $n$ computers instead of just one. How much can we speed up what kinds of calculations? For some, we can surely gain a factor of $n$. But these are rare. For others, we can gain $\log n$, but it is hard to find any or to prove what are their properties. And for most, I think, we can gain hardly anything; this is the case in which there are many highly branched conditionals, so that look-ahead on possible branches will usually be wasted. We know almost nothing about this; most people think, with surely incorrect optimism, that parallelism is usually a profitable way to speed up most computations.

These are just a few of the poorly understood questions about computational trade-offs. There is no space to discuss others, such as the digital-analog question. (Some problems about local versus global computations are outlined in [5].) And we know very little about trades between numerical and symbolic calculations.

There is, in today's computer science curricula, very little attention to what is known about such questions; almost all their time is devoted to formal classifications of syntactic language types, defeatist unsolvability theories, folklore about systems programming, and generally trivial fragments of "optimization of logic design"--the latter often in situations where the art of heuristic programming has far outreached the special-case "theories" so grimly taught and tested--and invocations about programming style almost sure to be outmoded before the student graduates. Even the most seemingly abstract courses on recursive function theory and formal logic seem to ignore the few known useful results on proving facts about compilers or equivalence of programs. Most courses treat the results of work in artificial intelligence, some now fifteen years old, as a peripheral collection of special applications, whereas they in fact represent one of the largest bodies of empirical and theoretical exploration of real computational questions. <mark>Until all this preoccupation with form is replaced by attention to the substantial issues in computation, a young student might be well advised to avoid much of the computer science curricula, learn to program, acquire as much mathematics and other science as he can, and study the current literature in artificial intelligence, complexity, and optimization theories.</mark>

## 2. Programming Languages

Even in the field of programming languages and compilers, there is too much concern with form. I say "even" because one might feel that this is one area in which form ought to be the chief concern. But let us consider two assertions: (1) languages are getting so they have too much syntax, and (2) languages are being described with too much syntax.

Compilers are not concerned enough with the meanings of expressions, assertions, and descriptions. The use of context-free grammars for describing fragments of languages led to important advances in uniformity, both in specification and in implementation. But although this works well in simple eases, attempts to use it may be retarding development in more complicated areas. There are serious problems in using grammars to describe self-modifying or self-extending languages that involve executing, as well as specifying, processes. One cannot describe syntactically--that is, statically--the valid expressions of a language that is changing. Syntax extension mechanisms must be described, to be sure, but if these are given in terms of a modern pattern-matching language such as Snobol, Convert [6], or Matchless[7], there need be no distinction between the parsing program and the language description itself. Computer languages of the future will be more concerned with goals and less with procedures specified by the programmer. The following arguments are a little on the extreme side but, in view of today's preoccupation with form, this overstepping will do no harm. (Some of the ideas are due to C. Hewitt and T. Winograd.)

### 2.1. Syntax is Often Unnecessary

One can survive with much less syntax than is generally realized. Much of programming syntax is concerned with suppression of parentheses or with emphasis of scope markers. There are alternatives that have been much underused.

Please do not think that I am against the use, at the human interface, of such devices as infixes and operator precedence. They have their place. But their importance to computer science as a whole has been so exaggerated that it is beginning to corrupt the youth.

Consider the familiar algorithm for the square root, as it might be written in a modern algebraic language, ignoring such matters as declarations of data types. One asks for the square root of A, given an initial estimate X and an error limit E.

```
DEFINE SQRT(A,X,E) :
    if ABS(A - X * X) < E then X else SQRT(A, (X + A ÷ X) ÷ 2, E).
```

In an imaginary but recognizable version of Lisp, (see Levin [8] or Weissman [9]), this same procedure might be written:

```lisp
(DEFINE (SQRT A X E)
    (IF (LESS (ABS (- A (* X X))) E) THEN X
    ELSE (SQRT A (÷ (+ X (÷ A X)) 2) E)))
```

Here, the function names come immediately *inside* their parentheses. The clumsiness, for humans, of writing all the parentheses is evident; the advantages of not having to learn all the conventions, such as that `(X + A ÷ X)` is `(+ X (÷ A X))` and not `(÷ (+ X A) X)`, is often overlooked.

It remains to be seen whether a syntax with explicit delimiters is reactionary, or whether it is the wave of the future. It has important advantages for editing, interpreting, and *for creation of programs by other programs*. The complete syntax of Lisp can be learned in an hour or so; the interpreter is compact and not exceedingly complicated, and students often can answer questions about the system by reading the interpreter program itself. Of course, this will not answer *all* questions about a real, practical implementation, but neither would any feasible set of syntax rules. Furthermore, despite the language's clumsiness, many frontier workers consider it to have outstanding expressive power. Nearly all work on procedures that solve problems by building and modifying hypotheses have been written in this or related languages. Unfortunately, language designers are generally unfamiliar with this area, and tend to dismiss it as a specialized body of "symbol-manipulation techniques."

Much can be done to clarify the structure of expressions in such a "syntax-weak" language by using indentation and other layout devices that are outside the language proper. For example, one can use a "postponement" symbol that belongs to an input preprocessor to rewrite the above as

```
DEFINE (SQRT A X E) ↓.
    IF ↓ THEN X ELSE ↓.
        LESS (ABS ↓) E.
                    -
```