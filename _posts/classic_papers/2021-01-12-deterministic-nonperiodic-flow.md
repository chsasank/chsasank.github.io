---
layout: post
title: "Deterministic Nonperiodic Flow"
author: "Edward N. Lorenz"
category: classic_papers
description: 
published: 1963-01-07
twitter_image: 
tag: "Math"
notes: 
---

> Finite systems of deterministic ordinary nonlinear differential equations may be designed to represent forced dissipative hydrodynamic flow. Solutions of these equations can be identified with trajectories in phase space. For those systems with bounded solutions, it is found that nonperiodic solutions are ordinarily unstable with respect to small modifications, so that slightly differing initial states can evolve into considerably different states. Systems with bounded solutions are shown to possess bounded numerical solutions.
>
> A simple system representing cellular convection is solved numerically, All of the solutions are found to be unstable, and almost all of them are nonperiodic.
>
> The feasibility of very-long-range weather prediction is examined in the light of these results.

## Introduction

Certain hydrodynamical systems exhibit steady-state flow patterns, while others oscillate in a regular periodic fashion. Still others vary in an irregular, seemingly haphazard manner, and, even when observed for long periods of time, do not appear to repeat their previous history.

These modes of behavior may all be observed in the familiar rotating-basin experiments, described by Fultz, ef al, (1959) and Hide (1958). In these experiments, a cylindrical vessel containing water is rotated about its axis, and is heated near its rim and cooled near its center in a steady symmetrical fashion. Under certain conditions the resulting flow is as symmetric and steady as the heating which gives rise to it. Under different conditions a system of regularly spaced waves develops, and progresses at a uniform speed without changing its shape. Under still different conditions an irregular flow pattern forms, and moves and changes its shape in an irregular nonperiodic manner.

Lack of periodicity is very common in natural systems, and is one of the distinguishing features of turbulent flow. Because instantaneous turbulent flow patterns are so irregular, attention is often confined to the statistics of turbulence, which, in contrast to the details of turbulence, often behave in a regular well-organized manner. The short-range weather forecaster, however, is forced willy-nilly to predict the details of the large-scale turbulent eddies--the cyclones and anticyclones--which continually arrange themselves into new patterns. Thus there are occasions when more than the statistics of irregular flow are of very real concern.

In this study we shall work with systems of deterministic equations which are idealizations of hydrodynamical systems. We shall be interested principally in nonperiodic solutions, i.e., solutions which never repeat their past history exactly, and where all approximate repetitions are of finite duration. Thus we shall be involved with the ultimate behavior of the solutions, as opposed to the transient behavior associated with arbitrary initial conditions.

A closed hydrodynamical system of finite mass may ostensibly be treated mathematically as a finite collection of molecules--usually a very large finite collection--in which case the governing laws are expressible as a finite set of ordinary differential equations. These equations are generally highly intractable, and the set of molecules is usually approximated by a continuous distribution of mass. The governing laws are then expressed as a set of partial differential equations, containing such quantities as velocity, density, and pressure as dependent variables.

It is sometimes possible to obtain particular solutions of these equations analytically, especially when the solutions are periodic or invariant with time, and, indeed, much work has been devoted to obtaining such solutions by one scheme or another. Ordinarily, however, nonperiodic solutions cannot readily be determined except by numerical procedures. Such procedures involve replacing the continuous variables by a new finite set of functions of time, which may perhaps be the values of the continuous variables at a chosen grid of points, or the coefficients in the expansions of these variables in series of orthogonal functions. The governing laws then become a finite set of ordinary differential equations again, although a far simpler set than the one which governs individual molecular motions.

In any real hydrodynamical system, viscous dissipation is always occurring, unless the system is moving as a solid, and thermal dissipation is always occurring, unless the system is at constant temperature. For certain purposes many systems may be treated as conservative systems, in which the total energy, or some other quantity, does not vary with time. In seeking the ultimate behavior of a system, the use of conservative equations is unsatisfactory, since the ultimate value of any conservative quantity would then have to equal the arbitrarily chosen initial value. This difficulty may be obviated by including the dissipative processes, thereby making the equations nonconservative, and also including external mechanical or thermal forcing, thus preventing the system from ultimately reaching a state of rest. If the system is to be deterministic, the forcing functions, if not constant with time, must themselves vary according to some deterministic rule.

In this work, then, we shall deal specifically with finite systems of deterministic ordinary differential equations, designed to represent forced dissipative hydrodynamical systems. We shall study the properties of nonperiodic solutions of these equations. 

It is not obvious that such solutions can exist at all. Indeed, in dissipative systems governed by finite sets of linear equations, a constant forcing leads ultimately to a constant response, while a periodic forcing leads to a periodic response. Hence, nonperiodic flow has sometimes been regarded as the result of nonperiodic or random forcing.

The reasoning leading to these conclusions is not applicable when the governing equations are nonlinear. Hf the equations contain terms representing advection--the transport of some property of a fluid by the motion of the fluid itself--a constant forcing can lead to a variable response. In the rotating-basin experiments already mentioned, both periodic and nonperiodic flow result from thermal forcing which, within the limits of experimental control, is constant. Exact periodic solutions of simplified systems of equations, representing dissipative flow with constant thermal forcing, have been obtained analytically by the writer (1962a). The writer (1962b) has also found nonperiodic solutions of similar systems of equations by numerical means.

