---
layout: post
title: "Aspect-Oriented Programming"
author: "Gregor Kiczales, John Lamping, Anurag Mendhekar, Chris Maeda, Cristina Lopes, Jean-Marc Loingtier and John Irwin"
category: classic_papers
description: My problem is that I have been persecuted by an integer. For seven years this number has followed me around, has intruded in my most private data, and has assaulted me from the pages of our most public journals. This number assumes a variety of disguises, being sometimes a little larger and sometimes a little smaller than usual
published: 1956-03-01
twitter_image: 
---

## Abstract

We have found many programming problems for which neither procedural nor object-oriented programming techniques are sufficient to clearly capture some of the important design decisions the program must implement. This forces the implementation of those design decisions to be scattered throughout the code, resulting in "tangled" code that is excessively difficult to develop and maintain. We present an analysis of why certain design decisions have been so difficult to clearly capture in actual code. We call the properties these decisions address *aspects*, and show that the reason they have been hard to capture is that they *cross-cut* the system’s basic functionality. We present the basis for a new programming technique, called aspect-oriented programming, that makes it possible to clearly express programs involving such aspects, including appropriate isolation, composition and reuse of the aspect code. The discussion is rooted in systems we have built using aspect-oriented programming.

## 1. Introduction

Object-oriented programming (OOP) has been presented as a technology that can fundamentally aid software engineering, because the underlying object model provides a better fit with real domain problems. But we have found many programming problems where OOP techniques are not sufficient to clearly capture all the important design decisions the program must implement. Instead, it seems that there are some programming problems that fit neither the OOP approach nor the procedural approach it replaces.

This paper reports on our work developing programming techniques that make it possible to clearly express those programs that OOP (and POP) fail to support. We present an analysis of why some design decisions have been so difficult to cleanly capture in actual code. We call the issues these decisions address *aspects*, and show that the reason they have been hard to capture is that they *cross-cut* the system’s basic functionality. We present the basis for a new programming technique, called aspect-oriented programming (AOP), that makes it possible to clearly express programs involving such aspects, including appropriate isolation, composition and reuse of the aspect code.

We think of the current state of AOP research as analogous to that of OOP 20 years ago. The basic concepts are beginning to take form, and an expanding group of researchers are using them in their work [1, 4, 13, 28]. Furthermore, while AOP qua AOP is a new idea, there are existing systems that have AOP-like properties. The contribution of this paper is an analysis of the problems AOP is intended to solve, as well as an initial set of terms and concepts that support explicit AOP-based system design.

The paper presents AOP in an example-driven way — the generalizations and definitions are all derived from examples, rather than presented in advance. Section 3 uses a medium-scale example to present the aspect-tangling problem AOP solves; the section culminates with a definition of the term aspect. Section 4 presents several more small examples of aspects. Sections 5 and 6 each provide an example of a complete AOP system. The remaining sections present future work, related work and conclusions.

## 2. Background Assumptions

This section outlines important assumptions about the relationship between programming languages and software design processes that underlie the rest of the paper.

Software design processes and programming languages exist in a mutually supporting relationship. Design processes break a system down into smaller and smaller units. Programming languages provide mechanisms that allow the programmer to define abstractions of system sub-units, and then compose those abstractions in different ways to produce the overall system. A design process and a programming language work well together when the programming language provides abstraction and composition mechanisms that cleanly support the kinds of units the design process breaks the system into.

From this perspective, many existing programming languages, including object-oriented languages, procedural languages and functional languages, can be seen as having a common root in that their key abstraction and composition mechanisms are all rooted in some form of generalized procedure. For the purpose of this paper we will refer to these as generalized-procedure (GP) languages. (This is not to say that we are ignorant of the many important advantages of OOP languages! It is only to say that for the purposes of the discussion in this paper, it is simpler to focus on what is common across all GP languages.)

The design methods that have evolved to work with GP languages tend to break systems down into units of behavior or function. This style has been called functional decomposition
[25-27].<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">
In some communities this term connotes the use of functional programming languages (i.e. side-effect free functions), but we do not use the term in that sense.
</span>
The exact nature of the decomposition differs between the language paradigms of course, but each unit is encapsulated in a procedure/function/object, and in each case, it feels comfortable to talk about what is encapsulated as a *functional* unit of the overall system. This last point may be so familiar that it feels somewhat redundant. But it is important that we give it explicit attention now, because in the course of this paper will be considering units of system decomposition that are not functional.

## 3. What Are Aspects?

To better understand the origins of tangling problems, and how AOP works to solve them, this section is organized around a detailed example, that is based on a real application we have been working with [18, 22]. There are three implementations of the real application: easy to understand but inefficient, efficient but difficult to understand, and an AOP-based implementation that is both easy to understand and efficient. The presentation here will be based on three analogous but simplified implementations.

Consider the implementation of a black-and-white image processing system, in which the desired domain model is one of images passing through a series of filters to produce some desired output. Assume that important goals for the system are that it be easy to develop and maintain, and that it make efficient use of memory. The former because of the need to quickly develop bug-free enhancements to the system. The latter because the images are large, so that in order for the system to be efficient, it must minimize both memory references and overall storage requirements.

### 3.1 Basic Functionality

Achieving the first goal is relatively easy. Good old-fashioned procedural programming can be used to implement the system clearly, concisely, and in good alignment with the domain model. In such an approach the filters can be defined as procedures that take several input images and produce a single output image. A set of primitive procedures would implement the basic filters, and higher level filters would be defined in terms of the primitive ones. For example, a primitive `or!` filter, which takes two images and returns their pixelwise logical or, might be implemented as:<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/>
<span class="sidenote">
We have chosen Common Lisp syntax for this presentation, but this could be written fairly easily in any other Algol-like language.</span>

<!-- TODO: put code here -->

Starting from `or!` and other primitive filters, the programmer could work up to the definition of a filter that selects just those black pixels on a horizontal edge, returning a new image consisting of just those boundary pixels.

|functionality |implementation |
|---|-------|
|pixelwise logical operations | written using loop primitive as above|
|shift image up, down | written using loop primitive; slightly different loop structure |
|difference of two images |`(defun remove! (a b) (and! a (not! b)))`|
|pixels at top edge of a region | `(defun top-edge! (a) (remove! a (down! a)))`|
|pixels at bottom edge of a region | `(defun bottom-edge! (a) (remove! a (up! a)))`|
|horizontal edge pixels| `(defun horizontal-edge! (a) (or! (top-edge! a) (bottom-edge! a)))` |

Note that only the primitive filters deal explicitly with looping over the pixels in the images. The higher level filters, such as `horizontal-edge!`, are expressed clearly in terms of primitive ones. The resulting code is easy to read, reason about, debug, and extend — in short, it meets the first goal.

### 3.2 Optimizing Memory Usage

But this simple implementation doesn't address the second goal of optimizing memory usage. When each procedure is called, it loops over a number of input images and produces a new output image. Output images are created frequently, often existing only briefly before they are consumed by some other loop. This results in excessively frequent memory references and storage allocation, which in turn leads to cache misses, page faults, and terrible performance.

The familiar solution to the problem is to take a more global perspective of the program, map out what intermediate results end up being inputs to what other filters, and then code up a version of the program that fuses loops appropriately to implement the original functionality while creating as few intermediate images as possible. The revised code for `horizontal-edge!` would look something like:

<!-- TODO: code here -->

Compared to the original, this code is all tangled up. It incorporates all the different filters that `horizontal-edge!` is defined in terms of, and fuses many, but not all, of their loops together. (The loops for `up!` and `down!` are not fused because those operations have a different looping structure.)
<label for="sn-3" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-3" class="margin-toggle"/>
<span class="sidenote">
Our AOP-based re-implementation of the full application fuses these other loops as well. We chose not to show that code here because it is so tangled that it is distractingly difficult to understand.</span>
In short, revising the code to make more efficient use of memory has destroyed the original clean component structure.

Of course, this is a very simple example, and it is not so difficult to deal with such a small amount of tangled code. But in real programs the complexity due to such tangling quickly expands to become a major obstacle to ease of code development and maintenance. The real system this example was drawn from is an important sub-component of an optical character recognition system. The clean implementation of the real system, similar to the first code shown above, is only 768 lines of code; but the tangled implementation, which does the fusion optimization as well as memoization of intermediate results, compile-time memory allocation and specialized intermediate datastructures, is 35213 lines. The tangled code is extremely difficult to maintain, since small changes to the functionality require mentally untangling and then re-tangling it.


<!-- TODO: figure here -->

### 3.3 Cross-Cutting

Returning to the example code, Figure 1 provides a different basis for understanding the tangling in it. On the left there is the hierarchical structure of the filtering functionality. On the right there is a data flow diagram for the original, un-optimized version of `horizontal-edge!`. In this diagram, the boxes and lines show the primitive filters and data flow between them. The dashed oval shows the boundary of what is fused into a single loop in the optimized version of `horizontal-edge!`.

Notice that the fusion oval does not incorporate all of `horizontal-edge!` In fact, it doesn’t align with any of the hierarchical units on the left. While the two properties being implemented— the functionality and the loop fusion — both originate in the same primitive filters, they must compose differently as filters are composed. The functionality composes hierarchically in the traditional way. But the loop fusion composes by fusing the loops of those primitive filters that have the same loop structure and that are direct neighbors in the data flow graph. Each of these composition rules is easy to understand when looking at its own appropriate picture. But the two composition relationships cut each other so fundamentally that each is very difficult to see in the other’s picture.

This cross-cutting phenomena is directly responsible for the tangling in the code. The single composition mechanism the language provides us — procedure calling — is very well suited to building up the un-optimized functional units. But it can’t help us compose the functional units and the loop fusion simultaneously, because they follow such different composition rules and yet must co-compose. This breakdown forces us to combine the properties entirely by hand — that’s what happening in the tangled code above.

In general, whenever two properties being programmed must compose differently and yet be coordinated, we say that they cross-cut each other. Because GP languages provide only one composition mechanism, the programmer must do the co-composition manually, leading to complexity and tangling in the code.

We can now define two important terms more precisely:


> With respect to a system and its implementation using a GP-based language, a property that must be implemented is:
> 
> **A component, if it can be cleanly encapsulated in a generalized procedure** (i.e. object, method, procedure, API). By cleanly, we mean well-localized, and easily accessed and composed as necessary. Components tend to be units of the system’s functional decomposition, such as image filters, bank accounts and GUI widgets.
> 
> **An aspect, if it can not be cleanly encapsulated in a generalized procedure**. Aspects tend not to be units of the system’s functional decomposition, but rather to be properties that affect the performance or semantics of the components in systemic ways. Examples of aspects include memory access patterns and synchronization of concurrent objects. (Section 4 provides more examples of aspects.)


Using these terms it is now possible to clearly state the goal of AOP: To support the programmer in cleanly separating components and aspects from each other,<label for="sn-4" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-4" class="margin-toggle"/>
<span class="sidenote">
Components from each other, aspects from each other, and components from aspects.
</span>
by providing mechanisms that make it possible to abstract and compose them to produce the overall system. This is in contrast to GP-based programming, which supports programmers in separating only components from each other by providing mechanisms that make it possible to abstract and compose them to produce the overall system.<label for="sn-5" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-5" class="margin-toggle"/>
<span class="sidenote">
Our analysis of aspects as system properties that cross-cut components helps explain the persistent popularity of mechanisms like dynamic scoping, catch and throw in otherwise purely GP languages. These mechanisms provide a different composition mechanism, that helps programmers implement certain aspects in their systems.</span>
