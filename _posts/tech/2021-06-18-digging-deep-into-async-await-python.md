---
layout: post
title: Demystifying Asynchronous Programming in Python
author: Sasank Chilamkurthy
twitter_image: "https://www.modernescpp.com/images/blog/Cpp20/co_return/FunctionsVersusCoroutines.png"
---

`asyncio` is all the rage these days in Python because we can write high performance io code in a single thread. While there are a lot of tutorials on `asyncio` library itself, there is little explanation on how it works internally. Although  `asyncio` library uses the `async`/`await` syntax, the syntax itself is independent of the library.

It's commonly said that `async`/`await` is just syntactic sugar for coroutines, but I never really understood what this means. So, I will try to dig deep into coroutines and write my own simple network io program with coroutines. With this, idea behind the syntax and asyncio library should become apparent.

## Coroutines

[Python Enhancement Proposal (PEP) 492](https://www.python.org/dev/peps/pep-0492/) introduced `async/await` syntax into the language. Here's the relevant passage from the abstract:

> It is proposed to make _coroutines_ a proper standalone concept in Python, and introduce new supporting syntax. The ultimate goal is to help establish a common, easily approachable, mental model of asynchronous programming in Python and make it as close to synchronous programming as possible.

So the syntax is about _coroutines_. What the hell are coroutines? Let's try to figure that out. Here's [wikipedia definition of coroutine](https://en.wikipedia.org/wiki/Coroutine)

> Coroutines are computer program components that generalize subroutines for non-preemptive multitasking, by allowing execution to be suspended and resumed. Coroutines are well-suited for implementing familiar program components such as cooperative tasks, exceptions, event loops, iterators, infinite lists and pipes.

That sounds scary. Here's an image representation of coroutine compared to traditional subroutine or function ([source](https://www.modernescpp.com/index.php/implementing-futures-with-coroutines))

![](https://www.modernescpp.com/images/blog/Cpp20/co_return/FunctionsVersusCoroutines.png)


Turns out, we've been using coroutines all along when we used Python's lazy generators like `range`. Let's write a simple generator using Python's `yield` syntax to illustrate the point:

```python
import time

def coroutine():
    for x in [1, 2, 3]:
        print("resumed coroutine")
        time.sleep(0.5)  # to represent some computation
        yield x
    
    print("destroyed coroutine")


def caller():
    context = coroutine()
    for x in context:
        print("suspended coroutine")
        time.sleep(0.25) # to represent some computation

caller()
```

This prints:

```
resumed coroutine
suspended coroutine
resumed coroutine
suspended coroutine
resumed coroutine
suspended coroutine
destroyed coroutine
```

Huh, coroutines are not so scary after all!


## Network IO with Coroutines

Network IO makes for a perfect use case of coroutines because we can suspend process while we're waiting for new connections or input. So, let's explore how we can use coroutines to write a echo server. There are many ways to do this but let's start by [reviewing](https://realpython.com/python-sockets/) low level unix `socket` API.

Here's an ultra simple echo server which accepts only one connection. Since we're working with only one connection, we can write standard sequential code. Connect to the server using `$ telnet localhost 5432`.

```python
import socket

HOST = '127.0.0.1'
PORT = 5432

def create_server():
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind((HOST, PORT))
    server.listen()
    print(f'Listening at {HOST}:{PORT}')
    return server


def accept_connection(server):
    # this blocks until we get new connection
    conn, addr = server.accept()
    print(f'Connected by {addr}')
    return conn


def handle_connection(conn):
    addr = conn.getpeername()
    with conn:
        while True:
            # this blocks until we get new data
            data = conn.recv(1024)
            if not data:
                break

            print(f'echoing {data} to {addr}')
            conn.sendall(data)

    print(f'Disconnected by {addr}')


def main():
    with create_server() as server:
        conn = accept_connection(server)
        handle_connection(conn)


main()
```

That was not hard but how do we handle multiple connections? One option is to create a thread of each connection. However, we'll use coroutines as light weight 'threads' or as I call them below, `tasks`. At any point of time, only one socket is doing its job, but sockets do not block the loop while waiting. Instead other sockets can do their jobs. This is called [co-operative multitasking](https://en.wikipedia.org/wiki/Cooperative_multitasking).

Let's use [Unix's `select`](https://man7.org/linux/man-pages/man2/select.2.html) to get the right task to resume/execute and `yield` as checkpoint to know where to resume a task from.


```python
import socket
import select

HOST = '127.0.0.1'
PORT = 5433

tasks = {}


def create_server():
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # Instead of blocking if trying to read or accept stuff,
    # this will raise error if socket is not ready
    server.setblocking(False)
    server.bind((HOST, PORT))
    server.listen()
    print(f'Listening at {HOST}:{PORT}')

    # we create a task for the server socket which looks
    # for new connections in 'background'.
    tasks[server] = accept_connections(server)

    return server


def accept_connections(server):
    while True:
        # we wait until server socket has new connection
        yield
        conn, addr = server.accept()
        conn.setblocking(False)
        print(f'Connected by {addr}')

        # we create a task for the client socket which 
        # handles the connection in 'background'.
        tasks[conn] = handle_connection(conn)


def handle_connection(conn):
    addr = conn.getpeername()
    with conn:
        while True:
            # we wait until client socket has some data
            yield
            data = conn.recv(1024)
            if not data:
                break

            print(f'echoing {data} to {addr}')
            conn.sendall(data)

    print(f'Disconnected by {addr}')


def main():
    with create_server():
        # event loop
        while True:
            # select.select will give us the sockets which are
            # ready to be read.
            ready, _, _ = select.select(tasks.keys(), [], [])
            for sock in ready:
                try:
                    # resume the task. send asks the generator
                    # to move by a step
                    tasks[sock].send(None)
                except StopIteration:
                    # task is over. let's delete it.
                    del tasks[sock]


main()
```

You can open `$ telnet localhost 5433` in multiple terminals and verify that everything is working as expected. Same code when written without coroutines doesn't exactly [look pretty](https://gist.github.com/ls0f/86d7a8b903fb7028a3a9).


## Nesting Coroutines: Syntactic Sugar

This is pretty cool but this particular construct of `yield` is limited because a coroutine can only yield to its caller. Let me illustrate why this can be limiting by taking following code:

```python
def power_coroutine():
    for z in range(3):
        print('==> restart')

        for x in [1, 2, 3]:
            yield x ** 2

        for y in [1, 2, 3]:
            yield y ** 3


for x in power_coroutine():
    print(x)
```

We can *not* refactor this code to the following:


```python
def squares():
    for x in [1, 2, 3]:
        yield x ** 2

def cubes():
    for y in [1, 2, 3]:
        yield y ** 3


def power_coroutine():
    for z in range(3):
        print('==> restart')
        squares()
        cubes()

for x in power_coroutine():
    print(x)
```

This code will raise an error because `power_coroutine()` doesn't have any yield statements. To allow refactoring of generators, [PEP 380](https://www.python.org/dev/peps/pep-0380/) introduced `yield from` so that can rewrite `power_coroutine` as

```python
def power_coroutine():
    for z in range(3):
        print('==> restart')
        yield from squares()
        yield from cubes()
```

Therefore, `yield from` allows for nesting for coroutines. Another feature is that it will capture `return` of the nested coroutines.

```python
def squares():
    for x in [1, 2, 3]:
        yield x ** 2
    
    return '-> squares done'

def cubes():
    for y in [1, 2, 3]:
        yield y ** 3

    return '-> cubes done'


def power_coroutine():
    for z in range(3):
        print('==> restart')
        msg_from_squares = yield from squares()
        print(msg_from_squares)
        msg_from_cubes = yield from cubes()
        print(msg_from_cubes)

for x in power_coroutine():
    print(x)
```

This will print

```
==> restart
1
4
9
-> squares done
1
8
27
-> cubes done
==> restart
1
4
9
-> squares done
1
8
27
-> cubes done
==> restart
1
4
9
-> squares done
1
8
27
-> cubes done
```

Another issue with the above construct is that it's not obvious whether a function is coroutine or traditional subroutine. So, we need some syntax sugar to mark the coroutines explicitly. Can you smell the `async`/`await` already? Because we're super close! Replace `yield from` with `await` and stick `async` keyword before `def` and we have python native coroutine. Therefore `binary` and `abinary` defined below are equivalent.

```python
# generator based coroutine
def binary(n):
    if n <= 0:
        return 1
    l = yield from binary(n - 1)
    r = yield from binary(n - 1)
    return l + 1 + r


# python native coroutine
async def abinary(n):
    if n <= 0:
        return 1
    l = await abinary(n - 1)
    r = await abinary(n - 1)
    return l + 1 + r


# you can run both using `send`
for cor in [binary(5), abinary(5)]:
    try:
        while True:
            cor.send(None)
    except StopIteration as e:
        print(cor, e.value)
```

which prints

```
<generator object binary at 0x7fc9580977b0> 63
<coroutine object abinary at 0x7fc958076b40> 63
```

That was a long post. I hope this gives you an idea about asynchronous programming and async/await syntax of python. In a future post, we'll examine internals of asyncio by trying to write synchronizing primitives like futures and events and implement a simple event loop.

### References:

1. [From yield to async/await](https://mleue.com/posts/yield-to-async-await/)
2. [Making sense of generators, coroutines, and “yield from” in Python](https://lerner.co.il/2020/05/08/making-sense-of-generators-coroutines-and-yield-from-in-python/)
3. [python generator “send” function purpose?](https://stackoverflow.com/a/19302700)
4. [Socket Programming in Python (Guide)](https://realpython.com/python-sockets/#handling-multiple-connections)
5. [select – Wait for I/O Efficiently](https://pymotw.com/2/select/)
6. [Does `await` in Python yield to the event loop?](https://stackoverflow.com/a/59780868)