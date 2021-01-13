---
layout: post
title: "Deterministic Nonperiodic Flow"
author: "Edward N. Lorenz"
category: classic_papers
description: 
published: 1963-01-07
twitter_image: 
tag: "Physics"
notes: 
---

> Finite systems of deterministic ordinary nonlinear differential equations may be designed to represent forced dissipative hydrodynamic flow. Solutions of these equations can be identified with trajectories in phase space. For those systems with bounded solutions, it is found that nonperiodic solutions are ordinarily unstable with respect to small modifications, so that slightly differing initial states can evolve into considerably different states. Systems with bounded solutions are shown to possess bounded numerical solutions.
>
> A simple system representing cellular convection is solved numerically, All of the solutions are found to be unstable, and almost all of them are nonperiodic.
>
> The feasibility of very-long-range weather prediction is examined in the light of these results.

## 1. Introduction

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

## 2. Phase space

Consider a system whose state may be described by $M$ variables $X_1, \cdots, X_M$. Let the system be governed by the set of equations

$$dX_i/dt = F_i(X_1, \cdots, X_M), i=1 \quad \cdots, M \tag{1}$$

where time $t$ is the single independent variable, and the functions $F_i$ possess continuous first partial derivatives. Such a system may be studied by means of *phase space*--an $M$-dimensional Euclidean space $\Gamma$ whose coordinates are $X_1, \cdots, X_M$. Each *point* in phase space represents a possible instantaneous state of the system. A state which is varying in accordance with (1) is represented by a moving *particle* in phase space, traveling along a *trajectory* in phase space. For completeness, the position of a stationary particle, representing a steady state, is included as a trajectory.

Phase space has been a useful concept in treating finite systems, and has been used by such mathematicians as Gibbs (1902) in his development of statistical mechanics, Poincaré (1881) in his treatment of the solutions of differential equations, and Birkhoff (1927) in his treatise on dynamical systems.

From the theory of differential equations (e,g., Ford 1933, ch. 6), it follows, since the partial derivatives $\partial F_i/\partial X_j$, are continuous, that if $t_0$ is any time, and if $X_{10}, \cdots X_{M0}$ is any point in $\Gamma$, equations (1) possess a unique solution

$$X_i = f_i(X_{10}, \cdots, X_{M0}, t), \quad i=1, \cdots, M, \tag{2}$$

valid throughout some time interval containing $t_0$, and satisfying the condition

$$f_i(X_{10}, \cdots, X_{M0}, t_0) = X_{i0}, \quad i=1, \cdots, M, \tag{3}$$

The functions $f_i$, are continuous in $X_{10}, \cdots, X_{M0}$ and $t$. Hence there is a unique trajectory through each point of $\Gamma$. Two or more trajectories may, however, approach the same point or the same curve asymptotically as $t \to \infty$ or as  $t \to -\infty$, Moreover, since the functions $f_i$ are continuous, the passage of time defines a continuous deformation of any region of $\Gamma$ into another region.

In the familiar case of a conservative system, where some positive definite quantity $Q$, which may represent some form of energy, is invariant with time, each trajectory is confined to one or another of the surfaces of constant $Q$. These surfaces may take the form of closed concentric shells.

If, on the other hand, there is dissipation and forcing, and if, whenever $Q$ equals or exceeds some fixed value $Q_1$, the dissipation acts to diminish $Q$ more rapidly then the forcing can increase $Q$, then $(—dQ/dt)$ has a positive lower bound where $Q \ge Q_1$, and each trajectory must ultimately become trapped in the region where $Q < Q_1$. Trajectories representing forced dissipative flow may therefore differ considerably from those representing conservative flow.

Forced dissipative systems of this sort are typified by the system

$$dX_i/dt = \sum_{j,k} a_{ijk}X_j X_k - \sum_{j}b_{ij}X_j + c_{i}, \tag{4} $$

where $\sum a_{ijk}X_i X_j X_k$ vanishes identically, $\sum b_{ij} X_i X_j$ is
positive definite, and $c_1, \cdots, c_M$ are constants. If

$$Q = \frac{1}{2} \sum_{i} X_i^2, \tag{5}$$

and if $e_i, \cdots, e_M$ are the roots of the equations

$$\sum_j (b_{ij} + b_{ji}) e_j = c_i,  \tag{6}$$

it follows from (4) that

$$ dQ/dt = \sum_{i,j} b_{ij}e_i e_j - \sum_{i,j} b_ij (X_i - e_i)(X_j-e_j). \tag{7}$$

The right side of (7) vanishes only on the surface of an ellipsoid $E$, and is positive only in the interior of $E$. The surfaces of constant $Q$ are concentric spheres. If $S$ denotes a particular one of these spheres whose interior $R$ contains the ellipsoid $E$, it is evident that each trajectory eventually becomes trapped within $R$.

## 3. The instability of nonperiodic flow

In this section we shall establish one of the most important properties of deterministic nonperiodic flow, namely, its instability with respect to modifications of small amplitude. We shall find it convenient to do this by identifying the solutions of the governing equations with trajectories in phase space. We shall use such symbols as $P(t)$ (variable argument) to denote trajectories, and such symbols as $P$ or $P(t_0)$ (no argument or constant argument) to denote points, the latter symbol denoting the specific point through which $P(t)$ passes at time $t_0$.

We shall deal with a phase space $\Gamma$ in which a unique trajectory passes through each point, and where the passage of time defines a continuous deformation of any region of $\Gamma$ into another region, so that if the points $P_1(t_0), P_2(t_0), \cdots $ approach $P_0(t_o)$ as a limit, the points $P_1(t_0 + \tau), P_2(t_0 + \tau), \cdots $ must approach $P_0(t_0+ \tau)$ as a limit. We shall furthermore require that the trajectories be uniformly bounded as $t \to \infty$; that is, there must be a bounded region $R$, such that every trajectory ultimately remains with $R$. Our procedure is influenced by the work of Birkhoff (1927) on dynamical systems, but differs in that Birkhoff was concerned mainly with conservative systems. A rather detailed treatment of dynamical systems has been given by Nemytskii and Stepanov (1960), and rigorous proofs of some of the theorems which we shall present are to be found in that source.

We shall first classify the trajectories in three different manners, namely, according to the absence or presence of transient properties, according to the stability or instability of the trajectories with respect to small modifications, and according to the presence or absence of periodic behavior.

Since any trajectory $P(t)$ is bounded, it must possess at least one *limit point* $P_0$, a point which it approaches arbitrarily closely arbitrarily often. More precisely, $P_o$ is a limit point of $P(t)$ if for any $\epsilon>0$ and any time $t_1$ there exists a time $t_2(\epsilon,t_1) > t_1$, such that $ \| P(t_2) — P_0 \| <\epsilon$. Here absolute-value signs denote distance in phase space. Because $\Gamma$ is continuously deformed as $t$ varies, every point on the trajectory through $P_0$ is also a limit point of $P(t)$, and the set of limit points of $P(t)$ forms a trajectory, or a set of trajectories, called the *limiting trajectories* of $P(t)$. A limiting trajectory is obviously contained within $R$ in its entirety.

If a trajectory is contained among its own limiting trajectories, it will be called *central*; otherwise it will be called *noncentral*. A central trajectory passes arbitrarily closely arbitrarily often to any point through which it has previously passed, and, in this sense at least, separate sufficiently long segments of a central trajectory are statistically similar. A noncentral trajectory remains a certain distance away from any point through which it has previously passed. It must approach its entire set of limit points asymptotically, although it need not approach any particular limiting trajectory asymptotically. Its instantaneous distance from its closest limit point is therefore a transient quantity, which becomes arbitrarily small as $t \to \infty$.

A trajectory $P(t)$ will be called *stable at a point* $P(t_1)$ if any other trajectory passing sufficiently close to $P(t_1)$ at time $t_1$ remains close to $P(t)$ as $t \to \infty$; i.e., $P(t)$ is stable at $P(t_1)$ if for any $\epsilon > 0$ there exists a $\delta(\epsilon,t_1)>0$ such that if $\|P_1(t_1)—P(t_1)\| < \delta$ and $t_2>t_1$, $\|P_1(t_2) — P_2(t_2)\| < \epsilon $. Otherwise $P(t)$ will be called *unstable* at $P(t_1)$. Because $\Gamma$ is continuously deformed as $t$ varies, a trajectory which is stable at one point is stable at every point, and will be called a *stable* trajectory. A trajectory unstable at one point is unstable at every point, and will be called an *unstable* trajectory. In the special case that $P(t)$ is confined to one point, this definition of stability coincides with the familiar concept of stability of steady flow.

A stable trajectory $P(t)$ will be called uniformly stable if the distance within which a neighboring trajectory must approach a point $P(t_1)$, in order to be certain of remaining close to $P(t)$ as $t \to \infty$, itself possesses a positive lower bound as $t_1 \to \infty$ i.e., $P(t)$ is uniformly stable if for any $ \epsilon > O$ there exists a $\delta(\epsilon) > 0$ and a time $t_0(\epsilon)$ such that if $t_1 > t_0$ and $\| P_1(t_1) - P(t_1) \| < \delta $ and $t_2 > t_1$, $\| P_1(t_2)— P(t_2) < \epsilon$. A limiting trajectory $P_0(t)$ of a uniformly stable trajectory $P(t)$ must be uniformly stable itself, since all trajectories passing sufficiently close to $P_0(t)$ must pass arbitrarily close to some point of $P(t)$ and so must remain close to $P(t)$, and hence to $P_0(t)$, as $t \to \infty$.

Since each point lies on a unique trajectory, any trajectory passing through a point through which it has previously passed must continue to repeat its past behavior, and so must be *periodic*. A trajectory $P(t)$ will be called *quasi-periodic* if for some arbitrarily large time interval $\tau$, $P(T+\tau)$ ultimately remains arbitrarily close to $P(t)$, i.e., $P(t)$ is quasi-periodic if for any $\epsilon>0$ and for any time interval $\tau_0$, there exists a $\tau(\epsilon,\tau_0) > \tau_0$ and a time $t_1(\epsilon,\tau_0)$ such that if $t_2 > t_1$, $\|P(t_2 + \tau) — P(t_2) \| < \epsilon$. Periodic trajectories are special cases of quasi-periodic trajectories.