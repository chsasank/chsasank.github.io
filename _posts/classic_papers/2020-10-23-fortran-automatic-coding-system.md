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
the selection is made by the authors of the system. Nevertheless, here are
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
multiplication and `**` denotes exponentiation (i.e., A ** B means $A^B$). The
program which is generated from this statement effects the computation in
floating point arithmetic, avoids computing (B/2.0) twice and computes
(B/2.0) ** 2 by a multiplication rather than by an exponentiation routine.
[Had (B/2.O) ** 2.01 appeared instead, an exponentiation routine would
necessarily be used, requiring more time than the multiplication.]

The programmer can refer to quantities in both floating point and integer form.
Integer quantities are somewhat restricted in their use and serve primarily as
subscripts or exponents. Integer constants are written without a decimal point.
Example: 2 (integer form) vs 2.0 (floating point form). Integer variables
begin with I, J, K, L, M, or N. Any meaningful arithmetic expression
may appear on the right-hand side of an arithmetic statement, provided the
following restriction is observed: an integer quantity can appear in a floating
point expression only as a subscript or as an exponent or as the argument of
certain functions. The functions which the programmer may refer to are limited
only by those available on the library tape at the time, such as SQRTF, plus
those simple functions which he has defined for the given problem by means of
function statements. An example will serve to describe the latter.

### Function Statements

Example 1: Define a function of three variables to be used throughout a given
problem, as follows:

```
ROOTF(A, B, C) = (-(B/2.0) + SQRTF((B/2.0) ** 2 - A * C))/A
```

Function statements must precede the rest of the program. They are composed
of the desired function name (ending in F) followed by any desired arguments
which appear in the arithmetic expression on the right of the = sign. The
definition of a function may employ any previously defined functions. Having
defined ROOTF as above, the programmer may apply it to any set of arguments
in any subsequent arithmetic statements. For example, a later arithmetic
statement might be 

```
THETA = 1.0 + GAMMA * ROOTF(P1, 3.2 * Y + 14.0, 7.63)
```

### `DO` Statements, `DIMENSION` Statements, and Subscripted Variables

Example 3: Set $Q_{max}$, equal to the largest quantity $P(a_i + b_i)/ P(a_i-
b_i)$ for some i between 1 and 1000 where $P(x) =c_0 + c_1 x + c_2 x^2 +c_3 x^3
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
polynomial (in factored form for efficient output program). Similarly, the
second statement merely informs the executive routine that the vectors A and
B each have 1000 elements. Statement 3 assigns a large negative initial value
to QMAX, $- 1.0 \times 10^{20}$, using a special concise form for writing
floating-point constants. Statement 4 says "DO the following sequence of
statements down to and including the statement numbered 5 for successive values
of I from 1 to 1000." In this case there is only one statement 5 to be
repeated. It is executed 1000 times; the first time reference is made to A(1)
and B(1), the second time to A(2) and B(2), etc. After the 1000th
execution of statement 5, statement 6-STOP is finally encountered. In statement
5, the function MAXF appears. MAXF may have two or more arguments and its
value, by definition, is the value of its largest argument. Thus on each
repetition of statement 5 the old value of QMAX is replaced by itself or by
the value of POLYF(A(I)+B(I))/POLYF(A(I)-B(I)), whichever is larger. The
value of QMAX after the 1000th repetition is therefore the desired maximum.

Example 4: Multiply the $n \times n$ matrix $a_{ij} (n \leq 20) $ by its
transpose, obtaining the product elements on or below the main diagonal by the
relation

$$ c_{i,j} = \sum_{k=1}^{n} a_{i,k} a_{j,k} \text{ for } (j \leq i) $$

and the remaining elements by the relation

$$c_{j_i} = c_{i, j}$$

<figure>
<img src='/assets/images/classic_papers/fortran/prog1.png'>
</figure>

As in the preceding example, the DIMENSION statement says that there are
two matrices of maximum size 20 x 20 named A and C. For explanatory
purposes only, the three boxes around the program show the sequence of
statements controlled by each DO statement. The first DO statement says
that procedure P, i.e., the following statements down to statement 2 (outer
box) is to be carried out for I = 1 then for I = 2 and so on up to I =
N. The first statement of procedure P(DO 2 J = 1, I) directs that
procedure Q be done for J = 1 to J = I. And of course each execution of
procedure Q involves N executions of procedure R for K = 1, 2, ... , N.

Consider procedure Q. Each time its last statement is completed the "index"
J of its controlling DO statement is increased by 1 and control goes to
the first statement of Q, until finally its last statement is reached and J
= I. Since this is also the last statement of P and P has not been repeated
until I = N, I will be increased and control will then pass to the first
statement of P. This statement (DO 2 J = 1, I) causes the repetition of Q
to begin again. Finally, the last statement of Q and P (statement 2)
will be reached with J = I and I = N, meaning that both Q and P have
been repeated the required number of times. Control will then go to the
next statement, STOP. Each time R is executed a new term is added to a
product element. Each time Q is executed a new product element and its
mate are obtained. Each time P is executed a product row (over to the
diagonal) and the corresponding column (down to the diagonal) are obtained.

The last example contains a "nest" of DO statements, meaning that the
sequence of statements controlled by one DO statement contains other DO
statements. Another example of such a nest is shown in the next column, on
the left. Nests of the type shown on the right are not permitted, since they
would usually be meaningless.

<figure>
<img src='/assets/images/classic_papers/fortran/fig2.png'>
</figure>

Although not illustrated in the examples given, the programmer may also
employ subscripted variables having three independent subscripts.

### READ, PRINT, FORMAT, IF and GO TO Statements

Example 5: For each case, read from cards two vectors, ALPHA and RHO, and
the number ARG. ALPHA and RHO each have 25 elements and ALPHA(I) ≤
ALPHA(I+1), I = 1 to 24. Find the SUM of all the elements of ALPHA from the
beginning to the last one which is less than or equal to ARG [assume
ALPHA(1) ≤ ARG ≤ ALPHA(25)] . If this last element is the Nth, set VALUE =
3.14159 * RHO(N). Print a line for each case with ARG, SUM, and VALUE.

FORTRAN Program:

```
 DIMENSION ALPHA(25), RHO(25)
1) FORMAT(5F12.4)
2) READ 1, ALPHA, RHO, ARG
 SUM = 0.0
 DO 3 I = 1, 25
 IF (ARG-ALPMA(1)) 4, 3, 3
3) SUM = SUM + ALPHA(I)
4) VALUE = 3.14159 * RHO(I - 1)
 PRINT 1, ARG, SUM, VALUE
 GO TO 2.
```

The FORMAT statement says that numbers are to be found (or printed) 5 per
card (or line), that each number is in fixed point form, that each number
occupies a field 12 columns wide and that the decimal point is located 4
digits from the right,The FORMAT statement is not executed; it is referred
to by the READ and PRINT statements to describe the desired arrangement of
data in the external medium.

The READ statement says "READ cards in the card reader which are arranged
according to FORMAT statement 1, and assign the successive numbers obtained
as values of ALPHA(1) I = 1, 25 and RHO(I) I = 1, 25 and ARG." Thus "ALPHA,
RHO, ARC" is a description of a list of 51 quantities (the size of ALPHA and
RHO being obtained from the DIMENSION statement). Reading of cards proceeds
until these 51 quantities have been obtained each card having five numbers,
as per the FORMAT description, except the last which has the value ARG only.
Since ARG terminated the list, the remaining four fields on the last card
are not read. The PRINT statement is similar to READ except that it
specifies a list of only three quantities. Thus each execution of PRINT
causes a single line to be printed with ARG, SUM, VALUE printed in the first
three of the five fields described by FORMAT statement 1.

The IF statement says "If ARG-ALPHA(I) is negative go to statement 4, if it
is zero go to statement 3, and if it is positive go to 3." Thus the
repetition of the two statements controlled by the DO consists normally of
computing ARG-ALPHA(1), finding it zero or positive, and going to statement
3 followed by the next repetition. However, when I has been increased to the
extent that the first ALPHA exceeding ARG is encountered, control will pass
to statement 4. Note that this statement does not belong to the sequence
controlled by the DO. In such cases, the repetition specified by the DO is
terminated and the value of the index (in this ease I) is preserved. Thus if
the first ALPHA exceeding ARG were ALPHA (20), then RHO (19) would be
obtained in statement 4.

The GO TO statement, of course, passes control to statement 2, which
initiates reading the 11 cards for the next case. The process will continue
until there are no more cards in the reader. The above program is entirely
complete. When punched in cards as shown, and compiled, the translator will
produce a ready-to-run 704 program which will perform the job specified.

### Other Types of FORTRAN Statements

In the above examples the following types of FORTRAN statements have been
exhibited.

* Arithmetic statements
* Function statements
* DO statements
* IF statements
* GO TO statements
* READ statements
* PRINT statements
* STOP statements
* DIMENSION statements
* FORMAT statements

The explanations accompanying each example have attempted to show some of
the possible applications and variations of these statements. It is felt
that these examples give a representative picture of the FORTRAN language;
however, many of its features have had to be omitted. There are 23 other
types of statements in the language, many of them completely analogous to
some of those described here. They provide facilities for referring to other
input-output and auxiliary storage devices (tapes, drums, and card punch),
for specifying preset and computed branching of control, for detecting
various conditions which may arise such as an attempt to divide by zero, and
for providing various information about a program to the translator. A
complete description of the language is to be found in *Programmer's
Reference Manual, the FORTRAN Automatic Coding System for the IBM 704*.

### Preparation of a Program for Translation

The translator accepts statements punched one per card (continuation cards
may be used for very long statements). There is a separate key on the
keypunching device for each character used in FORTRAN statements and each
character is represented in the card by several holes in a single column of
the card. Five columns are reserved for a statement number (if present) and
66 are available for the statement. Keypunching a FORTRAN program is
therefore a process similar to that of typing the program.

### Translation

The deck of cards obtained by keypunching may then be put in the card reader
of a 704 equipped with the translator program. When the load button is
pressed one gets either 1) a list of input statements which fail to conform
to specifications of the FORTRAN language accompanied by remarks which
indicate the type of error in each case; 2) a deck of binary cards
representing the desired 704 program, 3) a binary tape of the program which
can either be preserved or loaded and executed immediately after translation
is complete, or 4) a tape containing the output program in symbolic form
suitable for alteration and later assembly. (Some of these outputs may be
unavailable at the time of publication.)

<<<<<<< HEAD
## The FORTRAN Translator

### General Organization of the System

The FORTRAN translator consists of six successive sections, as follows.

*Section 1:* Reads in and classifies statements. For arithmetic formulas,
compiles the object (output) instructions. For nonarithmetic statements
including input-output, does a partial compilation, and records the
remaining information in tables. All instructions compiled in this section
are in the COMPAIL file.

*Section 2:* Compiles the instructions associated with indexing, which
result from DO statements and the occurrence of subscripted variables, These
instructions are placed in the COMPDO file.

*Section 3:* Merges the COMPAIL and COMPDO files into a single file,
meanwhile completing the compilation of nonarithmetic statements begun in
Section 1. The object program is now complete, but assumes an object machine
with a large number of index registers.

*Section 4:* Carries out an analysis of the flow of the object program, to
be used by Section 5.

*Section 5:* Converts the object program to one which involves only the
three index registers of the 704.

*Section 6:* Assembles the object program, producing a relocatable binary
program ready for running. Also on demand produces the object program in
SHARE symbolic language.

(*Note:* Section 3 is of internal importance only; Section 6 is a fairly
conventional assembly program. These sections will be treated only briefly
in what follows.)

Within the translator, information is passed from section to section in two
principal forms: as compiled instructions, and as tables. The compiled
instructions (e.g., the COMPAIL and COMPDO files, and later their merged
result) exist in a four-word format which contains all the elements of a
symbolic 704 instruction; ie., symbolic location, three-letter operation
code, symbolic address with relative absolute part, symbolic tag, and
absolute decrement. (Instructions which refer to quantities given symbolic
names by the programmer have those same names in their addresses.) This
symbolic format is retained until section 6. Throughout, the order of the
compiled instructions is maintained by means of the symbolic locations
(internal statement numbers), which are assigned in sequential fashion by
section 1 as each new statement is encountered.

The tables contain all information which cannot yet be embodied in compiled
instructions. For this reason the translator requires only the single scan
of the source program performed in section 1.

A final observation should be made about the organization of the system.
Basically, it is simple, and most of the complexities which it does possess
arise from the effort to cause it to produce object programs which can
compete in efficiency with hand-written programs. Some of these complexities
will be found within the individual sections; but also, in the system as a
whole, the sometimes complicated interplay between compiled instructions and
tables is a consequence of the desire to postpone compiling until the
analysis necessary to produce high object-program efficiency has been
performed.

## Section 1 (Beeber, Herrick, Nutt, Sheridan, and Stern)

The overall flow of section 1 is

<figure>
<img src='/assets/images/classic_papers/fortran/fig3.png'>
</figure>

For an input-output statement, section 1 compiles the appropriate read or
write select (RDS or WRS) instruction, and the necessary copy (CPY)
instructions (for binary operations) or transfer instructions to pre-written
input-output routines which perform conversion between decimal and binary
and govern format (for decimal operations). When the list of the
input-output statement is repetitive, table entries are made which will
cause section 2 to generate the indexing instructions necessary to make the
appropriate loops.

The treatment of statements which are neither input-output nor arithmetic is
similar; i.e., those instructions which can be compiled are compiled, and
the remaining information is extracted and placed in one or more of the
appropriate tables.

In contrast, arithmetic formulas are completely treated in section 1, except
for open (built-in) subroutines, which are added in section 3; a complete
set of compiled instructions is produced in the COMPAIL file. This
compilation involves two principal tasks: 1) the generation of an
appropriate sequence of arithmetic instructions to carry out the computation
specified by the formula, and 2) the generation of (symbolic) tags for those
arithmetic instructions which refer to subscripted variables (variables
which denote arrays) which in combination with the indexing instructions to
be compiled in section 2 will refer correctly to the individual members of
those arrays. Both these tasks are accomplished in the course of a single
scan of the formula.

Task 2) can be quickly disposed of. When a subscripted variable is
encountered in the scan, its subscript(s) are examined to determine the
symbols used in the subscripts, their multiplicative coefficients, and the
dimensions of the array. These items of information are placed in tables
where they will be available to section 2 ; also from them is generated a
subscript combination name which is used as the symbolic tag of those
instructions which refer to the subscripted variable.


The difficulty in carrying out task 1) is one of *level* ; there is implicit
in every arithmetic formula an order of computation, which arises from the
control over ordering assigned by convention to the various symbols
(parentheses, +, - , * , /, etc.) which can appear, and this implicit
ordering must be made explicit before compilation of the instructions can be
done. This explicitness is achieved, during the formula scan, by associating
with each operation required, by the formula a *level number*, such that if
the operations are carried out in the order of increasing level number the
correct sequence of arithmetic instructions will be obtained. The sequence
of level numbers is obtained by means of a set of rules, which specify for
each possible pair formed of operation type and symbol type the increment to
be added to or subtracted from the level number of the preceding pair.

In fact, the compilation is not carried out with the raw set of level
numbers produced during the scan. After the scan, but before the
compilation, the levels are examined for empty sections which can be
deleted, for permutations of operations on the same level, which will reduce
the number of accesses to memory, and for redundant computation (arising
from the existence of common subexpressions) which can be eliminated.

An example will serve to show (somewhat inaccurately) some of the principles
employed in the level-analysis process. Consider the following arithmetic
expression:

```
A + B * * C * (E + F)
```

In the level analysis of this expression parentheses are in effect inserted
which define the proper order in which the operations are to be performed.
If only three implied levels are recognized (corresponding to +, * and * *)
the expression obtains the following:

```
+(* (* * A)) + (* (* * B * * C) * [+ (* (* * E)) + (*(* * F))
```

The brackets represent the parentheses appearing in the original expression.
(The level-analysis routine actually recognizes an additional level
corresponding to functions.) Given the above expression the level-analysis
routine proceeds to define a sequence of new dependent variables the first
of which represents the value of the entire expression. Each new variable is
generated whenever a left parenthesis is encountered and its definition is
entered on another line. In the single scan of the expression it is often
necessary to begin the definition of one new variable before the definition
of another has been completed. The subscripts of the u's in the following
sets of definitions indicate the order in which they were defined.

* $u_0 = + u_1 + u_3$
* $u_1 = * u_2$
* $u_2 = * * A$
* $u_3 = * u_ 5 * u_5$
* $u_4 = * * B * * C$
* $u_5 = + u_6 + u_8$
* $u_6 = * u_7$
* $u_7 = * * E$
* $u_8 = * u_9$
* $u_9 = * * F$

This is the point reached at the end of the formula scan. What follows
illustrates the further processing applied to the set of levels. Notice that
$u_9$, for example, is defined as $* * F$. Since there are not two or more
operands to be combined the $* *$ serves only as a level indication and no
further purpose is served by having defined us. The procedure therefore
substitutes F for $u_9$ wherever $u_9$ appears and the line $u_9 = * * F$ is
deleted. Similarly, F is then substituted for $u_8$ and $u_8 = * F$ is
deleted. This elimination of "redundant" u's is carried to completion and
results in the following:

* $u_0 = + A + u_3$
* $u_3 = * u_4 * u_5$
* $u_4 = ** B ** C$
* $u_5 = + E + F$

These definitions, read up, describe a legitimate a procedure for obtaining
the value of the original expression. The number of $u$'s remaining at this
point (in this case four) determines the number of intermediate quantities
which *may* need to be stored. However, further examination of this case
reveals that the result of $u_3$ is in the accumulator, ready for $u_0$;
therefore the store and load instructions which would usually be compiled
between $u_3$ and $u_0$ are omitted.

### Section 2 (Nelson and Ziller)

Throughout the object program will appear instructions which refer to subscripted variables. Each of these instructions will (until section 5) be tagged with a symbolic index register corresponding to the particu- l b subscript combination of the subscripts of the varia- ble [e.g., (I, K , J) and (K, I , J) are two different sub- script combinations]. If the object program is to work correctly, every symbolic index register must be so governed that it will have the appropriate contents at every instant that it is being used. It is the source pro- gram, of course, which determines what these appro- priate contents must be, primarily through its DO statements, but also through arithmetic formulas (e.g. I =N+1) which may define the values of variables ap- pearing in subscripts, or input formulas which may read such values in at object time. Moreover, in the case of DO statements, which are designed to produce loops in the object program, it is necessary to provide tests for loop exit. It is these two tasks, the governing of symbolic index registers and the testing of their contents, which section 2 must carry out.
=======
>>>>>>> fortran
