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
misplaced. Still, I will argue that an excessive preoccupation with
formalism is impeding our development.

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
that seem to be common sense are false. We have bad misconceptions about the
possible exchanges between time and memory, tradeoffs between time and
program complexity, software and hardware, digital and analog circuits,
serial and parallel computations, associative and addressed memory, and so
on.

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
we have the observation by Shannon [2] that any Turing machine with Q states
and R symbols is equivalent to one with 2 states and nQR symbols, and to one
with 2 symbols and n'QR states, where n and n' are small numbers. Thus the
state-symbol product QR has an almost invariant quality in classifying
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
wouldn't establish a general rule. One of our prime research goals should be
to develop methods to prove that particular procedures are computationally
minimal, in various senses.


A startling discovery was made about multiplication itself in the thesis of Cook [3], which uses a result of Toom; it is discussed in Knuth [4]. Consider the ordinary algorithm for multiplying decimal numbers: for two n-digit numbers this employs $n^2$ one-digit products. It is usually supposed that this is minimal. But suppose we write the numbers in two halves, so that the product is N = (~A + B)((~C + D),
where ~ stands for multiplying by 10~/~. (The left-shift operation is considered to have negligible cost.) Then one can verify that