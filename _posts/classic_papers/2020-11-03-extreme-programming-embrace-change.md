---
layout: post
title: "Embracing Change with Extreme Programming"
author: "Kent Beck"
category: classic_papers
description: 
published: 1999-10-01
twitter_image: https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Kent_Beck_no_Workshop_Mapping_XP.jpg/220px-Kent_Beck_no_Workshop_Mapping_XP.jpg
---

> Extreme Programming turns the conventional software process sideways. Rather than planning, analyzing, and designing for the far-flung future, XP programmers do all of these activities -- a little at a time -- throughout development.

In the beginning was the waterfall (Figure 1a). We would get the users to tell us once and for all exactly what they wanted. We would design the system  that  would  deliver  those  features.  We would code it. We would test to make sure the features were delivered. All would be well.

All was not well. The users didn't tell us once and for all exactly what they wanted. They didn’t know. They contradicted themselves. They changed their minds. And the users weren't the only problem. We programmers  could  think  we  were  making  great progress only to discover three-fourths of the way through that we were one-third of the way through.

If long development cycles were bad because they couldn’t adapt to changes, perhaps what we needed was to make shorter development cycles. As Figure 1b shows, the waterfall begat iterations.

The waterfall model didn't just appear. It was a rational reaction to the shocking measurement that the cost of changing a piece of software rose dramatically over time. If that’s true, then you want to make the biggest, most far-reaching decisions as early in the life cycle as possible to avoid paying big bucks for them.

The  academic  software  engineering  community took the high cost of changing software as a challenge, creating technologies like relational databases, modular programming, and information hiding. What if all that hard work paid off? What if we got good at reducing the costs of ongoing changes? What if we didn’t have to settle for taking a cleaver to the waterfall? What if we could throw it in a blender?

We might get a picture like the one shown in Figure1c. It’s called Extreme Programming.

## Anatomy of XP

XP turns the conventional software process side-ways. Rather than planning, analyzing, and designing for the far-flung future, XP exploits the reduction in the cost of changing software to do all of these activities a little at a time, throughout software development. (The "XP Practices" sidebar will give you a quick grasp of the practices and philosophy underlying XP. These practices are designed to work together, and trying to examine any one soon leads you to the rest. The "Roots of XP" sidebar on page 73 traces the historical antecedents of these practices.)

<div class="box">

### XP Practices

Here is a quick summary of each of the major practices in XP.

**Planning game**. Customers decide the scope and timing of releases based on estimates provided by programmers. Programmers implement only the functionality demanded by the stories in this iteration.

**Small releases.** The system is put into production in a few months, before solving  the  whole  problem.  New releases are made often -- anywhere from daily to monthly. 

**Metaphor.** The shape of the system is defined  by  a  metaphor  or  set  of metaphors shared between the customer and programmers. 

**Simple design.** At every moment, the design runs all the tests, communicates everything the programmers want to communicate, contains no duplicate code, and has the fewest possible classes and methods. This rule  can  be  summarized  as,  “Say everything once and only once.”

**Tests.**  Programmers  write  unit  tests minute by minute. These tests are collected and they must all run correctly. Customers write functional tests for the stories in an iteration. These tests should also all run, although practically speaking, sometimes a business decision must be made comparing the cost of shipping a known defect and the cost of delay.

**Refactoring.** The design of the system is evolved through transformations of the existing design that keep all the tests running.

**Pair  programming.**  All  production code is written by two people at onescreen/keyboard/mouse.

**Continuous integration.** New code is integrated with the current system after  no  more  than  a  few  hours. When integrating, the system is built from scratch and all tests must pass or the changes are discarded. 

**Collective ownership.** Every programmer improves any code anywhere in the system at any time if they see the opportunity.

**On-site customer.** A customer sits with the team full-time.

**40-hour weeks.** No one can work a second  consecutive  week  of  over-time. Even isolated overtime used too  frequently  is  a  sign  of  deeper problems that must be addressed.

**Open workspace.** The team works in a large room with small cubicles around the periphery. Pair programmers work on computers set up in the center.

**Just rules.** By being part of an Extreme team, you sign up to follow the rules. But they’re just the rules. The team can change the rules at any time as long as they agree on how they will assess the effects of the change.
</div>