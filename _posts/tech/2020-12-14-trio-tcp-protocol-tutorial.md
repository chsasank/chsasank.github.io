---
layout: post
title: Concurrent Programming and Trio 
author: Sasank Chilamkurthy
---

Concurrent programming is unintuitive and hard. Woefully hard but quite important. People do PhDs, spend careers and win Turing awards doing research on concurrent programming and distributed computing. Yet, most programmers *never* have to deal with it.<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">Super consciously anyway. Node.js is all about async programming.</span> Why is that? Because most of the hard work has been done for you by the libraries and programs you use.

Let's say you're writing a simple web app i.e. it communicates to the world using `http` protocol. Your app has to serve multiple requests at the same time. Your app inturn makes multiple database queries at the same time. You're likely using a web framework, say Django, to write your app and host it behind nginx. Django and Nginx likely abstracts out all these concurrencies for you and database itself can handle concurrencies. You therefore end up writing a sequential code.

What if you're now asked to write your app in a completely new protocol?<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/><span class="sidenote">In my case, this protocol is DICOM.</span> Unlike `http`, may be your protocol is asynchronous by definition -- say WebSocket. You can't rely on your favorite web framework and its abstractions to make your life simple. You have to deal with the complexity of concurrent programming. Add to that additional issues that raw threading, reference counting and error propagation brings. Not the best place to be in.

There are a lot of libraries out there to make concurrent programming 'easy'. There are a lot of models -- threads, tasks, callbacks and so on. As shown in [this beautiful article by Nathaniel](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful), they all boil down to `goto` statement that Dijkstra so [vehemently opposed](https://chsasank.github.io/classic_papers/goto-statement-considered-harmful.html). No wonder I don't find these models very intuitive. In the same post, Nathaniel proposed an alternative to these models, `nursery` and a library called `trio` which implements it.

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Nursery illustrated. Observe the `with` statement that destroys the nursery after completing the tasks async. Arrows represent the control flow. [Source](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful)</span>
<img src="/assets/images/random/nursery.svg" alt="nursery">
</figure>

In this post, I show how to implement a simple TCP protocol server using the concepts and `trio` library. I intentionally pick the protocols from [twisted's tutorial](https://twistedmatrix.com/documents/current/core/howto/servers.html) here so as to illustrate the differences with twisted, another popular async python library.