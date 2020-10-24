---
layout: post
title: "The FORTRAN Automatic Coding System "
author: "J. W. Backus, R. J. Beeber, S. Best, R. Goldberg, L. M. Haibt, H. L. Herrick, R. A. Nelson, D. Sayre, P. B. Sheridan, H.Stern, I. Ziller, R. A. Hughes, And R. Nutt"
category: classic_papers
description: 
published: 1957-02-01
twitter_image: https://miro.medium.com/max/930/0*3jGv1es40NHPcTE3.jpg
---


# Introduction

The FORTRAN project was begun in the summer of 1954. Its purpose was to reduce
by a large factor the task of preparing scientific problems for IBM's next large
computer, the 704. If it were possible for the 704 to code problems for itself
and produce as good programs as human coders (but without the errors), it was
clear that large benefits could be achieved. For it was known that about
two-thirds of the cost of solving most scientific and engineering problems on
large computers was that of problem preparation. Furthermore, more than 90 per
cent of the elapsed time for a problem was usually devoted to planning, writing,
and debugging the program. In many cases the development of a general plan for
solving a problem was a small job in comparison to the task of devising and
coding machine procedures to carry out the plan. The goal of the FORTRAN project
was to enable the programmer to specify a numerical procedure using a concise
language like that of mathematics and obtain automatically from this
specification an efficient 704 program to carry out the procedure. It was
expected that such a system would reduce the coding and debugging task to less
than one-fifth of the job it had been.

Two and one-half years and 18 man years have elapsed since the beginning of the
project. The FORTRAN system is now complete. It has two components: the FORTRAN
language, in which programs are written, and the translator or executive routine
for the 704 which effects the translation of FORTRAN language programs into 704
programs. Descriptions of the FORTRAN language and the translator form the
principal sections of this paper.

The experience of the FORTRAN group in using the system has confirmed the
original expectations concerning reduction of the task of problem preparation
and the efficiency of output programs. A brief case history of one job done
with a system seldom gives a good measure of its usefulness, particularly when
the selection  is made by the authors of the system. Nevertheless, here are
the facts about a rather simple but sizable job. The programmer attended a
one-day course on FORTRAN and spent some more time referring to the manual. He
then programmed the job in four hours, using 47 FORTRAN statements. These were
compiled by the 704 in six minutes, producing about 1000 instructions. He ran
the program and found the output incorrect. He studied the output (no tracing or
memory dumps were used) and was able to localize his error in a FORTRAN
statement he had written. He rewrote the offending statement, recompiled, and
found that the resulting program was correct. He estimated that it might have
taken three days to code this job by hand, plus an unknown time to debug it, and
that no appreciable increase in speed of execution would have been achieved
thereby.

## The FORTRAN Language

The FORTRAN language is most easily described by reviewing some examples.

### Arithmetic Statements 

Example 1: Compute:

$$ root = \frac{ - (B/2) + \sqrt{(B/2)^2 - AC} }{A} $$ 

FORTRAN Program:

```
ROOT = (-(B/2.0) + SQRTF((B/2.0) ** 2 - A*C))/A
```

Notice that the desired program is a single FORTRAN statement, an arithmetic
formula. Its meaning is: "Evaluate the expression on the right of the `=` sign
and make this the value of the variable on the left" The symbol `*` denotes
multiplication and `**` denotes exponentiation (i.e., `A ** B` means $A^B$). The
program which is generated from this statement effects the computation in
floating point arithmetic, avoids computing `(B/2.0)` twice and computes
`(B/2.0) * * 2` by a multiplication rather than by an exponentiation routine.
[Had `(B/2.O) ** 2.01` appeared instead, an exponentiation routine would
necessarily be used, requiring more time than the multiplication.]

The programmer can refer to quantities in both floating point and integer form.
Integer quantities are somewhat restricted in their use and serve primarily as
subscripts or exponents. Integer constants are written without a decimal point.
Example: `2` (integer form) vs `2.0` (floating point form). Integer variables
begin with `I`, `J`, `K`, `L`, `M`, or `N`. Any meaningful arithmetic expression
may appear on the right-hand side of an arithmetic statement, provided the
following restriction is observed: an integer quantity can appear in a floating
point expression only as a subscript or as an exponent or as the argument of
certain functions. The functions which the programmer may refer to are limited
only by those available on the library tape at the time, such as `SQRTF`, plus
those simple functions which he has defined for the given problem by means of
function statements. An example will serve to describe the latter.

### Function Statements

Example 1: Define a function of three variables to be used throughout a  given
problem, as follows:

```
ROOTF(A, B, C) = (-(B/2.0) + SQRTF((B/2.0) ** 2 - A * C))/A
```

Function statements must precede the rest of the program. They are composed of
the desired function name (ending in `F`) followed by any desired arguments
which appear in the arithmetic expression on the right of the `=` sign. The
definition of a  function may employ any previously defined functions. Having defined `ROOTF` as above, the programmer may apply it to any set of arguments in any subsequent arithmetic statements. For example, a  later arithmetic statement might be 

```
THETA = 1.0 + GAMMA * ROOTF(P1, 3.2 * Y + 14.0, 7.63)
```

### DO Statements, DIMENSION Statements, and Subscripted  Variables

Example 3: Set $Q_{max}$, equal to the largest quantity $P(a_i + b_i)/ P(a_i-
b_i)$ for  some i between 1 and 1000 where $P(x) =c_0 + c_1 x + c_2 x^2 +c_3 x^3
$

FORTRAN Program:

```
1) POLYF(X) =CO + X * (Cl + X * (C2 + X * C3))
2) DIMENSION A(1000), B(1000)
3) QMAX = - 1.0 E20
4) DO 5 I = 1, 1000
5) QMAX = MAXF(QMAX, POLYF(A(I)+B(I))/POLYF(A(I)-B(I)))
6) STOP
```

The program above is complete except for input and output statements which will
be described later. The first statement is not executed; it defines the desired
polynomial  (in factored form for efficient output program). Similarly, the
second statement merely informs the executive routine that the vectors `A` and
`B` each have 1000 elements. Statement 3  assigns a large negative initial value
to `QMAX`, $- 1.0 \times 10^{20}$, using a special concise form  for writing
floating-point constants. Statement 4 says "`DO` the following sequence of
statements down to and including the statement numbered 5 for successive values
of `I` from 1 to 1000." In this case there is only one statement 5 to be
repeated. It is executed 1000 times; the first time reference is made to `A(l)`
and `B(1)`, the second time to `A(2)` and `B(2)`, etc. After the 1000th
execution of statement 5, statement 6-STOP is finally encountered. In statement
5, the function `MAXF` appears. `MAXF` may  have two or more arguments and its
value, by definition, is the value of its largest argument. Thus on  each
repetition of statement 5  the old value of `QMAX` is replaced by itself or by
the value of `POLYF(A(I)+B(I))/POLYF(A(I)-B(I))`, whichever  is larger. The
value of `QMAX` after the 1000th repetition is therefore the desired maximum.

Example 4: Multiply the $n \times n$ matrix $a_{ij} (n \leq 20) $ by its
transpose, obtaining the product elements on or below the main diagonal by the
relation

$$ c_{i,j} = \sum_{k=1}^{n} a_{i,k} a_{j,k} \text{ for } (j \leq i) $$

and the remaining elements by the relation

$$c_{j_i} = c_{i, j}$$

<figure>
<img src='/assets/images/classic_papers/fortran/prog1.png'>
</figure>

As in the preceding  example, the `DIMENSION` statement says that there are two matrices of maximum size 20 x 20 named `A` and `C`. For explanatory purposes only, the three boxes around the program show the sequence of statements controlled by each `DO` statement. The first `DO` statement says that procedure P, i.e., the following statements down to statement 2 (outer box) is to be carried out for  `I = 1` then for `I = 2` and so on up to `I = N`. The first statement of procedure `P(DO 2 J = 1, I)` directs that procedure Q be done for `J = 1` to `J = I`. And of course each execution of procedure Q involves N executions of procedure R for `K = l, 2, ... , N`.

Consider procedure Q. Each  time its last statement is completed the "index" J of its controlling `DO` statement is increased by 1 and control goes to the first statement of Q, until finally its last statement is reached and `J = I`. Since this is also the last statement of P and P has not been repeated  until `I = N`, I  will be increased and control will then pass to the first statement of P. This statement (DO 2 J = 1, I) causes the repetition of Q to begin again. Finally, the last statement~f Q and P (statement 2) will be reached with J =I and I = M, meaning that both Q and P have been repeated the required number of times.  Control will then go to the next statement, STOP. Each time R is executed a  new term is added to a  product  element. Each time Q is executed a  new product  element and its mate are ob: tained. Each time P is executed a  product row (over to the diagonal) and the corresponding  column (down to the diagonal) are obtained. 