---
layout: post
title: "Can Quantum-Mechanical Description of Physical Reality Be Considered Complete?"
author: "A. Einstein, B. Podolsky and N. Rosen"
category: classic_papers
description: "In a complete theory there is an element corresponding to each element of reality. A sufficient condition for the reality of a physical quantity is the possibility of predicting it with certainty, without disturbing the system. In quantum mechanics in the case of two physical quantities described by non-commuting operators, the knowledge of one precludes the knowledge of the other. Then either (1) the description of reality given by the wave function in quantum mechanics is not complete or (2) these two quantities cannot have simultaneous reality. Consideration of the problem of making predictions concerning a system on the basis of measurements made on another system that had previously interacted with it leads to the result that if (1) is false then (2) is also false. One is thus led to conclude that the description of reality as given by a wave function is not complete."
published: 1935-05-15
twitter_image: "https://en.wikipedia.org/wiki/File:Einstein.jpg"
tag: Physics
notes: "This paper introduces the famous [Einstein-Podolsky-Rosen(EPR) Paradox paradox](https://en.wikipedia.org/wiki/EPR_paradox). Right after the mainstream acceptance of quantum mechanics in the late 1920s, EPR discovered the problem of [quantum entanglement and teleportation](https://en.wikipedia.org/wiki/Quantum_entanglement) implied by the formulation of quantum mechanics. The problem is that measurement of one system can mysteriously affect another system even if they are not interacting any more. This counter-intuitive prediction of quantum mechanics is actually [verified to be true](https://www.nature.com/news/quantum-teleportation-achieved-over-record-distances-1.11163)! In fact, concept of entanglement remains central to quantum computing."
---

## Abstract

In a complete theory there is an element corresponding to each element of reality. A sufficient condition for the reality of a physical quantity is the possibility of predicting it with certainty, without disturbing the system. In quantum mechanics in the case of two physical quantities described by non-commuting operators, the knowledge of one precludes the knowledge of the other. Then either (1) the description of reality given by the wave function in quantum mechanics is not complete or (2) these two quantities cannot have simultaneous reality. Consideration of the problem of making predictions concerning a system on the basis of measurements made on another system that had previously interacted with it leads to the result that if (1) is false then (2) is also false. One is thus led to conclude that the description of reality as given by a wave function is not complete.

## 1.

Any serious consideration of a physical theory must take into account the distinction between the objective reality, which is independent of any theory, and the physical concepts with which the theory operates. These concepts are intended to correspond with the objective reality, and by means of these concepts we picture this reality to ourselves.

In attempting to judge the success of a physical theory, we may ask ourselves two questions: (1) "Is the theory correct?" and (2) "Is the description given by the theory complete?" It is only in the case in which positive answers may be given to both of these questions, that the concepts of the theory may be said to be satisfactory. The correctness of the theory is judged by the degree of agreement between the conclusions of the theory and human experience. This experience, which alone enables us to make inferences about reality, in physics takes the form of experiment and measurement. It is the second question that we wish to consider here, as applied to quantum mechanics.

<span class="mark">Whatever the meaning assigned to the term *complete*, the following requirement for a complete theory seems to be a necessary one: *every element of the physical reality must have a counterpart in the physical theory.*</span> We shall call this the condition of completeness. The second question is thus easily answered, as soon as we are able to decide what are the elements of the physical reality.

The elements of the physical reality cannot be determined by *a priori* philosophical considerations, but must be found by an appeal to results of experiments and measurements. A comprehensive definition of reality is, however, unnecessary for our purpose. <span class="mark">We shall be satisfied with the following criterion, which we regard as reasonable. *If, without in any way disturbing a system, we can predict with certainty (i.e., with probability equal to unity) the value of a physical quantity, then there exists an element of physical reality corresponding to this physical quantity.*</span> It seems to us that this criterion, while far from exhausting all possible ways of recognizing a physical reality, at least provides us with one such way, whenever the conditions set down in it occur. Regarded not as a necessary, but merely as a sufficient, condition of reality, this criterion is in agreement with classical as well as quantum-mechanical ideas of reality.

To illustrate the ideas involved let us consider the quantum-mechanical description of the behavior of a particle having a single degree of freedom. The fundamental concept of the theory is the concept of *state*, which is supposed to be completely characterized by the wave function $\psi$, which is a function of the variables chosen to describe the particle’s behavior. Corresponding to each physically observable quantity $A$ there is an operator, which may be designated by the same letter.

If $\psi$ is an eigenfunction of the operator $A$, that is, if

$$\psi' \equiv A\psi = a\psi \tag{1},$$

where $a$ is a number, then the physical quantity $A$ has with certainty the value $a$ whenever the particle is in the state given by $\psi$. In accordance with our criterion of reality, for a particle in the state given by $\psi$ for which Eq. (1) holds, there is an element of physical reality corresponding to the physical quantity $A$. Let, for example, 

$$\psi = e^{(2\pi i/h)p_0 x} \tag{2},$$

where $h$ is Planck’s constant, $p_0$ is some constant number, and $x$ the independent variable. Since the operator corresponding to the momentum of the particle is

$$ p = (h/2\pi i)\partial / \partial x, \tag{3}$$

we obtain

$$\psi' = p\psi = (h/2\pi i) \partial \psi/ \partial x = p_o \psi. \tag{4}$$

Thus, in the state given by Eq. (2), the momentum has certainly the value $p_0$. It thus has meaning to say that the momentum of the particle in the state given by Eq. (2) is real.

On the other hand if Eq. (1) does not hold, we can no longer speak of the physical quantity $A$ having a particular value. This is the case, for example, with the coordinate of the particle. The operator corresponding to it, say $q$, is the operator of multiplication by the independent variable. Thus,

$$ q\psi = x\psi \neq a\psi. \tag{5}$$

In accordance with quantum mechanics we can only say that the relative probability that a measurement of the coordinate will give a result lying between $a$ and $b$ is

$$ P(a, b) = \int_a^b \bar{\psi}\psi dx = \int_a^b dx = b -a.   \tag{6}$$

Since this probability is independent of $a$, but depends only upon the difference $b—a$, we see that all values of the coordinate are equally probable.

A definite value of the coordinate, for a particle in the state given by Eq. (2), is thus not predictable, but may be obtained only by a direct measurement. Such a measurement however disturbs the particle and thus alters its state. After the coordinate is determined, the particle will no longer be in the state given by Eq. (2). <span class="mark">The usual conclusion from this in quantum mechanics is that *when the momentum of a particle is known, its coordinate has no physical reality*.</span>

More generally, it is shown in quantum mechanics that, if the operators corresponding to two physical quantities, say $A$ and $B$, do not commute, that is, if $AB \neq BA$, then the precise knowledge of one of them precludes such a knowledge of the other. Furthermore, any attempt to determine the latter experimentally will alter the state of the system in such a way as to destroy the knowledge of the first.

From this follows that either *(1) the quantum-mechanical description of reality given by the wave function is not complete or (2) when the operators corresponding to two physical quantities do not commute the two quantities cannot have simultaneous reality.* For if both of them had simultaneous reality--and thus definite values--these values would enter into the complete description, according to the condition of completeness. If then the wave function provided such a complete description of reality, it would contain these values; these would then be predictable. This not being the case, we are left with the alternatives stated.

<span class="mark">In quantum mechanics it is usually assumed that the wave function *does* contain a complete description of the physical reality of the system in the state to which it corresponds. At first sight this assumption is entirely reasonable, for the information obtainable from a wave function seems to correspond exactly to what can be measured without altering the state of the system. We shall show, however, that this assumption, together with the criterion of reality given above, leads to a contradiction.</span>

## 2.

<span class="mark">For this purpose let us suppose that we have two systems, I and II, which we permit to interact from the time $t=0$ to $t=T$, after which time we suppose that there is no longer any interaction between the two parts.</span> We suppose further that the states of the two systems before $t=0$ were known. We can then calculate with the help of Schrodinger's equation the state of the combined system I+II at any subsequent time; in particular, for any $t>T$. Let us designate the corresponding wave function by $\psi$. We cannot, however, calculate the state in which either one of the two systems is left after the interaction. This, according to quantum mechanics, can be done only with the help of further measurements, by a process known as the *reduction of the wave packet*. Let us consider the essentials of this process.

Let $a_1, a_2, a_3, \cdots$ be the eigenvalues of some physical quantity $A$ pertaining to system I and $u_1(x_1), u_2(x_2), u_3(x_3)$, the corresponding eigenfunctions, where $x_1$ stands for the variables used to describe the first system. Then $\psi$, considered as a function of $x_1$, can be expressed as

$$\psi(x_1, x_2) = \sum_{n=1}^{\infty} \psi_n(x_2) u_n(x_1), \tag{7}$$

where $x_2$ stands for the variables used to describe the second system. Here $\psi_n(x_2)$ are to be regarded merely as the coefficients of the expansion of $\psi$ into a series of orthogonal functions $u_n(x_1)$. Suppose now that the quantity $A$ is measured and it is found that it has the value $a_k$. It is then
concluded that after the measurement the first system is left in the state given by the wave function $u_k(x_1)$, and that the second system is left in the state given by the wave function $\psi_k(x_2)$. This is the process of reduction of the wave packet; the wave packet given by the infinite series (7) is reduced to a single term
$ \psi_k(x_2) u_k(x_1)$.

The set of functions $u_n(x_1)$ is determined by the choice of the physical quantity $A$. If, instead of this, we had chosen another quantity, say $B$, having the eigenvalues $b_1, b_2, b_3, \cdots$ and eigenfunctions $v_1(x_1), v_2(x_1), v_3(x_1), \cdots$, we should have obtained, instead of Eq. (7), the expansion

$$\psi(x1, x2) = \sum_{s=1}^{\infty} \varphi_s(x_2) v_s(x_2), \tag{8}$$

Where $\varphi_s$'s are the new coefficients. If now the quantity $B$ is measured and is found to have the value $b_r$, we conclude that after the measurement the first system is left in the state given by $v_r(x_1)$ and the second system is left in the state given by $\varphi_r(x_2)$.

<span class="mark">We see therefore that, as a consequence of two different measurements performed upon the first system, the second system may be left in states with two different wave functions. On the other hand, since at the time of measurement the two systems no longer interact, no real change can take place in the second system in consequence of anything that may be done to the first system.</span> This is, of course, merely a statement of what is meant by the absence of an interaction between the two systems. Thus, *it is possible to assign two different wave functions* (in our example $\psi_k$ and $\varphi_r$) to the same reality (the second system after the interaction with the first).

Now, it may happen that the two wave functions, $\psi_k$ and $\varphi_r$, are eigenfunctions of two noncommuting operators corresponding to some physical quantities $P$ and $Q$, respectively. That this may actually be the case can best be shown by an example. Let us suppose that the two systems are two particles, and that

$$\psi(x_1, x_2) = \int_{-\infty}^{\infty} e^{(2\pi i / h) (x_1 - x_2 + x_0 )p} dp, \tag{9} $$

where $x_0$ is some constant. Let $A$ be the momentum of the first particle; then, as we have seen in Eq. (4), its eigenfunctions will be

$$u_p(x_1) =  e^{(2\pi i / h) p x_1} \tag{10}$$

corresponding to the eigenvalue $p$. Since we have here the case of a continuous spectrum, Eq. (7) will now be written

$$\psi(x_1, x_2) = \int_{-\infty}^{\infty} \psi_p(x_2)u_p(x_1) dp, \tag{11}$$

where

$$\psi_p(x_2) = e^{-(2\pi i / h) (x_2-x_0)p}. \tag{12}$$

This $\psi_p$ however is the eigenfunction of the operator

$$P = (h/2\pi i) \partial/\partial x_2, \tag{13}$$

corresponding to the eigenvalue $-p$ of the momentum of the second particle. On the other hand, if $B$ is the coordinate of the first particle, it has for eigenfunctions

$$v_x(x_1) = \delta(x_1 - x), \tag{14}$$

corresponding to the eigenvalue $x$, where $\delta(x_1—x)$ is the well-known Dirac delta-function. Eq. (8) in this case becomes 

$$\psi(x1, x2) = \int_{-\infty}^{\infty} \varphi_x(x_2) v_x(x_1) dx, \tag{15}$$

where

$$ \varphi_x(x_2) =  \int_{-\infty}^{\infty} e^{(2\pi i / h) (x - x_2 + x_0)p} dp = h\delta(x-x_2+x_0) .\tag{16}$$

This $\varphi_x$, however, is the eigenfunction of the operator

$$Q = x_2 \tag{17}$$

corresponding to the eigenvalue $x+x_0$ of the coordinate of the second particle. Since

$$PQ - QP = h/2\pi i, \tag{18}$$

we have shown that it is in general possible for $\psi_k$, and $\varphi_r$, to be eigenfunctions of two noncommuting operators, corresponding to physical quantities.

Returning now to the general case contemplated in Eqs. (7) and (8), we assume that  $\psi_k$ and $\varphi_r$, are indeed eigenfunctions of some noncommuting operators $P$ and $Q$, corresponding to the eigenvalues $p_k$ and $q_r$, respectively. <span class="mark">Thus, by measuring either $A$ or $B$ we are in a position to predict with certainty, and without in any way disturbing the second system, either the value of the quantity $P$ (that is $p_k$) or the value of the quantity $Q$ (that is $q_r$). In accordance with our criterion of reality, in the first case we must consider the quantity $P$ as being an element of reality, in the second case the quantity $Q$ is an element of reality.</span> But, as we have seen, both wave functions $\psi_k$ and $\varphi_r$, belong to the same reality.

Previously we proved that either (1) the quantum-mechanical description of reality given by the wave function is not complete or (2) when the operators corresponding to two physical quantities do not commute the two quantities cannot have simultaneous reality. <span class="mark">Starting then with the assumption that the wave function does give a complete description of the physical reality, we arrived at the conclusion that two physical quantities, with noncommuting operators, can have simultaneous reality.</span> Thus the negation of (1) leads to the negation of the only other alternative (2). We are thus forced to conclude that the quantum-mechanical description of physical reality given by wave functions is not complete.

One could object to this conclusion on the grounds that our criterion of reality is not sufficiently restrictive. Indeed, one would not arrive at our conclusion if one insisted that two or more physical quantities can be regarded as simultaneous elements of reality *only when they can be simultaneously measured or predicted*. On this point of view, since either one or the other, but not both simultaneously, of the quantities $P$ and $Q$ can be predicted, they are not simultaneously real. <span class="mark">This makes the reality of $P$ and $Q$ depend upon the process of measurement carried out on the first system, which does not disturb the second system in any way. No reasonable definition of reality could be expected to permit this.</span>

While we have thus shown that the wave function does not provide a complete description of the physical reality, we left open the question of whether or not such a description exists. We believe, however, that such a theory is possible.