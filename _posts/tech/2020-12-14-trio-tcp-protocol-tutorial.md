---
layout: post
title: Concurrent Programming and Trio 
author: Sasank Chilamkurthy
---

Concurrent programming is unintuitive and hard. Pretty hard but quite important. People do PhDs, spend careers and win Turing awards doing research on concurrent programming and distributed computing. Yet, most programmers *never* have to deal with it.<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">Super consciously anyway. Node.js is all about async programming.</span> Why is that? Because most of the hard work has been done for you by the libraries and programs you use.

Let's say you're writing a simple web app i.e. it communicates to the world using `http` protocol. Your app has to serve multiple requests at the same time. Your app inturn makes multiple database queries at the same time. You're likely using a web framework, say Django, to write your app and host it behind nginx. Django and Nginx likely abstracts out all these concurrencies for you and database itself can handle concurrencies. You therefore end up writing a sequential code.

What if you're now asked to write your app in a completely new protocol?<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/><span class="sidenote">In my case, this protocol is DICOM.</span> Unlike `http`, may be your protocol is asynchronous by definition -- say WebSocket. You can't rely on your favorite web framework and its abstractions to make your life simple. You have to deal with the complexity of concurrent programming. Add to that additional issues that raw threading, reference counting and error propagation brings. Not the best place to be in.

## Trio and Nursery

There are a lot of libraries out there to make concurrent programming 'easy'. There are a lot of models -- threads, tasks, callbacks and so on. As shown in [this beautiful article by Nathaniel](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful), they all boil down to `goto` statement that Dijkstra so [vehemently opposed](https://chsasank.github.io/classic_papers/goto-statement-considered-harmful.html). No wonder I don't find these models very intuitive. In the same post, Nathaniel proposed an alternative to these models, `nursery` and a library called `trio` which implements it.

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Nursery illustrated. Observe the `with` statement that destroys the nursery after completing the tasks async. Arrows represent the control flow. [Source](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful)</span>
<img src="/assets/images/random/nursery.svg" alt="nursery">
</figure>

In this post, I show how to implement a simple TCP protocol server using these concepts and illustrate `trio` library. I intentionally pick the protocols from [twisted's tutorial](https://twistedmatrix.com/documents/current/core/howto/servers.html) here so as to illustrate the differences with twisted, another popular async python library. Start with installing trio:

```bash
$ pip install -U trio
```

Let's write a very simple program which sleeps asynchronously to see how `nursery` works in `trio`<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/>
<span class="sidenote">
Code snippet taken directly from [trio's tutorial](https://trio.readthedocs.io/en/stable/tutorial.html)</span>:

```python
# tasks-intro.py
import trio

async def child1():
    print("  child1: started! sleeping now...")
    # do not forget the await!
    await trio.sleep(1)
    print("  child1: exiting!")

async def child2():
    print("  child2: started! sleeping now...")
    await trio.sleep(1)
    print("  child2: exiting!")

async def parent():
    print("parent: started!")
    async with trio.open_nursery() as nursery:
        print("parent: spawning child1...")
        nursery.start_soon(child1)

        print("parent: spawning child2...")
        nursery.start_soon(child2)

        print("parent: waiting for children to finish...")
        # -- we exit the nursery block here --
    print("parent: all done!")

trio.run(parent)
```

What this does should be fairly obvious: `parent` is running `child1` and `child2` asynchronously. Let's run this:

```
$ python tasks-intro.py
parent: started!
parent: spawning child1...
parent: spawning child2...
parent: waiting for children to finish...
  child2: started! sleeping now...
  child1: started! sleeping now...
    [... 1 second passes ...]
  child1: exiting!
  child2: exiting!
parent: all done!
```

Note how parent waits for its children to finish before exiting the nursery. 

## Echo Server

Let's write very simple echo server: receive a message and write the same message back to the TCP stream. 

```python
# echo-server.py
import trio

async def echo_server(server_stream):
    print("echo_server: started")
    async for data in server_stream:
        print("echo_server: received data {!r}".format(data))
        await server_stream.send_all(data)

    print("echo_server: connection closed")

async def main():
    await trio.serve_tcp(echo_server, port=12345)

trio.run(main)
```

`trio.serve_tcp` creates a nursery internally and listens to TCP connections indefinitely on the specified port and forward them to `echo_server` function. Run the server using 

```
$ python echo-server.py
```

Use telnet in another terminal to talk to the server. Type 'hello' or 'hi', press enter and you should see that server responds back with 'hello' or 'hi'.

```
$ telnet localhost 12345
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
hello
hi
hi
^]

telnet> Connection closed.
```

And the log of your server should look like:

```
echo_server: started
echo_server: received data b'hello\r\n'
echo_server: received data b'hi\r\n'
echo_server: connection closed
```

You can try spinning up multiple telnet clients and verify that our server handles multiple requests at the same time.

## Chat Server

That was very simple. Let's write a more complicated protocol -- a chat server like IRC. Our server should allow user to login with a name and all the logged in users should be able to participate in a group chat.