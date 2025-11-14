---
layout: post
title: "Elastically Deformable Models"
author: "Linus Torvalds"
category: classic_papers
description: "The intellectual property debate is so hot these days that I can’t wander into a restroom without running across graffiti supporting one side or the other. Some people think that patents and other forms of intellectual property law are the bane of the free universe, and that these laws are not just misguided but actually evil and should be struck down as soon as possible. Others are convinced that pretty much the whole world economy is driven by intellectual property. And those people want to do anything to strengthen the legal status of IP rights."
published: 2001-05-15
twitter_image: https://en.wikipedia.org/wiki/File:LinuxCon_Europe_Linus_Torvalds_03_(cropped).jpg
---


## Abstract
The theory of elasticity describes deformable materials such as rubber, cloth, paper, and flexible metals. We employ elasticity theory to construct differential equations that model the behavior of non-rigid curves, surfaces, and solids as a function of time. Elastically deformable models are active: they respond in a natural way to applied forces, constraints, ambient media, and impenetrable obstacles. The models are fundamentally dynamic and realistic animation is created by numerically solving their underlying differential equations. Thus, the description of shape and the description of motion are unified.

## Introduction

Methods to formulate and represent instantaneous shapes of objects are central to computer graphics modeling. These methods have been particularly successful for modeling rigid objects whose shapes do not change over time. This paper develops an approach to modeling which incorporates the physically-based dynamics of flexible materials into the purely geometric models which have been used traditionally. We propose models based on elasticity theory which conveniently represent the shape and motion of deformable materials, especially when these materials interact with other physically-based computer graphics objects.

### 1.1. Physical Models versus Kinematic Models

Most traditional methods for computer graphics modeling are kinematic; that is, the shapes are compositions of geometrically or algebraically defined primitives. Kinematic models are passive because they do not interact with each other or with external forces. The models are either stationary or are subjected to motion according to prescribed trajectories. Expertise is required to create natural and pleasing dynamics with passive models.

As an alternative, we advocate the use of active models in computer graphics. Active models are based on principles of mathematical physics [5]. They react to applied forces (such as gravity), to constraints (such as linkages), to ambient media (such as viscous fluids), or to impenetrable obstacles (such as supporting surfaces) as one would expect real, physical objects to react.

This paper develops models of deformable curves, surfaces, and solids which are based on simplifications of elasticity theory. By simulating physical properties such as tension and rigidity, we can model static shapes exhibited by a wide range of deformable objects, including string, rubber, cloth, paper, and flexible metals. Furthermore, by including physical properties such as mass and damping, we can simulate the dynamics of these objects. The simulation involves numerically solving the partial differential equations that govern the evolving shape of the deformable object and its motion through space.

The dynamic behavior inherent to our deformable models significantly simplifies the animation of complex objects. Consider the graphical representation of a coiled telephone cord. The traditional approach has been to represent the instantaneous shape of the cord as a mesh assembly of bicubic spline patches or polygons. Making the cord move plausibly is a nontrivial task. In contrast, our deformable models can provide a physical representation of the cord which exhibits natural dynamics as it is subjected to external forces and constraints.

### 1.2 Outline

The remainder of the paper develops as follows: Section 2 discusses the connections of our work to other physical models in computer graphics. Section 3 gives differential equations of motion describing the dynamic behavior of deformable models under the influence of external forces. Section 4 contains an analysis of deformation and defines deformation energies for curve, surface, and solid models. Section 5 lists various external forces that can be applied to deformable models to produce animation. Section 6 describes our implementation of deformable models. Section 7 presents simulations illustrating the application of deformable models. Section 8 discusses our work in progress.

## 2. Related Graphics Models


Interestingly, the classical spline representations of shape have characterizations based in elasticity theory [7]. However, in adopting splines as a representation of curve and surface shape, the graphics literature has deemphasized the physical basis of splines. The cubic interpolating spline, for instance, is an abstraction of the shape exhibited by a thin elastic beam (the elastica used in boat construction) whose minimal bending energy configuration may be characterized by a fourth-order differential equation. The elasticity theory perspective leads to generalized spline representations of curves, surfaces, and solids. Our work in this paper can be viewed as an extension, including physically-based dynamics, of the mixed-order generalized splines employed in computer vision by Terzopoulos [24].

Special purpose physical models have begun to capture the attention of the computer graphics community. Fluid mechanics was used by Peachey [20] and Fournier and Reeves [11] to model water waves, as well as Kajiya and yon Herzen [17] and Yaeger et al [28] for cloud simulation. Also, the physics of imaging has been applied to rendering [16, 15]. Weil [26] used catenaries to approximate cloth, while Feynman [10] used a more sophisticated thin plate flexure model for the same purpose.

Terzopoulos [23] employed deformable models based on variational principles to reconstruct surfaces from scattered visual constraints. To create deformable models, Barr [3] subjected solid primitives to prescribed deformations using Jacobian matrices. Sederberg and Parry [21] imposed similar deformations to solids modeled as freeform surfaces. We extend these approaches by adding equations governing the evolution of deformations.

Our models are compatible with and complementary to the constraint-based modeling approach for rigid primitives proposed by Barzel and Barr [4], as well as with the dynamics-based approaches of Wilhelms and Barsky [27] and Armstrong and Green [1] to animating articulated rigid bodies. Finally, since computer vision is the inverse problem of computer graphics, the models presented in this paper are of value for reconstructing mathematical representations of non-rigid objects from their images [25].

## 3. Dynamics of Deformable Models

We begin the mathematical development by giving the equations of motion governing the dynamics of our deformable models under the influence of applied forces. The equations of motion are obtained from Newtonian mechanics and balance the externally applied forces with the forces due to the deformable model.

Let $a$ be the intrinsic or material coordinates of a point in a body $\Omega$. For a solid body, $\mathbf{a}$ has three components: $[a_1, a_2, a_3]$. Similarly, for a surface $\mathbf{a} = [a_1, a_2]$, and a curve $\mathbf{a} = [a_1]$. The Euclidean 3-space positions of points in the body are given by a time-varying vector valued function of the material coordinates $\mathbf{r}(\mathbf{a}, t) = [r_1(a, t), r_2(a, t), r_3(a, t)]$. The body in its natural rest state (see Figure 1) is specified by 
$\mathbf{r}^0(a) = [r_1^0(a), r_2^0(a), r_3^0(a)]$.

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">
Figure 1. Coordinate Systems
</span>
<img src='/assets/images/classic_papers/elastic_deformable_models/fig1.png'>
</figure>

The equations governing a deformable model's motion can be written in Lagrange's form [14] as follows:

