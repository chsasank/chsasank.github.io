---
layout: post
title: Docker is Object Oriented Programming (OOP) for Systems
author: Sasank Chilamkurthy
---

I was explaining about docker to my colleagues and I was struck by an epiphany: [Docker](https://en.wikipedia.org/wiki/Docker_(software)) is essentially object oriented programming (OOP) for systems programming. For the uninitiated Docker is OS-level virtualization that allow you to package softwares and make the installations easy and reproducible across systems. No more *but it worked on my computer* problems. Follow this [tutorial from Keval](https://kevalnagda.github.io/getting-started-with-docker) to get a better idea of how it works. In this post, I'll quickly review the docker concepts so that we can make the connection to OOP.

In docker, you build an *image* by specifying instructions in `Dockerfile` on how to install your application. You can think of an image as packaged file system containing your programs. For example, here's how you build an image.

```Dockerfile
# Dockerfile
FROM ubuntu:18.04
RUN apt-get update && apt-get install -y python3 python3-pip
RUN pip3 install numpy
CMD python3 -m http.server
```

This `Dockerfile` starts with `FROM ubuntu:18.04` which means you're using ubuntu 18.04 as base image and `RUN`s the next two commands in this image. `CMD` specifies the default command when you run this image. You build the image by running: 

```bash
$ docker build -t numpy .
```

Once built, you can use this image to spin up an *container* in which you can run whatever you want. Let's spin up a container and run python.

```
$ docker run -it numpy python3
Python 3.6.9 (default, Oct  8 2020, 12:12:24) 
[GCC 8.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import numpy
>>> 
```

Obviously numpy is already installed in this container. How do we get input and output to this container? We can use file system and/or networking to interact with the container. Here's an example:

```bash
$ mkdir input_dir
$ touch input_dir/hi
$ echo "hello sasank" > input_dir/hi
$ docker run -p 8001:8000 -v `pwd`/input_dir:/root/input numpy 
```

`-p` publishes port 8001 from docker and maps it to port number 8000 (the default python server port) inside docker. `-v` mounts input_dir in the present directory to `/root/input` of the container. In a different terminal, you can verify that the filesystem and network I/O worked with curl.

```bash
$ curl localhost:8001/root/input/hi
hello sasank
```

Now why am I calling this object oriented programming? Let's start with clarifying what OOP is. In OOP, we have a *class* which encapsulates the *data* and *methods* of a particular functionality. For example, `Human` can be class whose data is the name and method is `what_is_your_name()`. You'll have to create an *instance* of the `Human` class (say `sasank`) to interact with it. Once instantiated, the only way to interact with this instance is through the method *interface* (e.g., `sasank.what_is_your_name()`). What the class does internally is treated like a black box. A class can *inherit* data and methods from another class (say `Animal`)

Docker image is exactly like a class in which you define instructions to install your app. Data of this class is the code to run your app. Methods are what your app does. `FROM` is inheritance where you can inherit data and methods from a previously built image. You instantiate an image by spinning up a container. You interact with your container through filesystem and networking. What the container does internally is not your problem. I summarized the argument below:


| Docker        | Object Oriented Programming  |
| ------------- |:-------------:|
| Image         | Class  |
| Installed packages and code   |  Data |
| `CMD` and what your code does | Methods |
| `FROM`        | Inheritance |
| Container         | Instance  |
| Filesystem mounting and networking | Interface |

You usually make different classes interact with each other by *composition* where each interact with other through methods. This is exactly the use case of `docker-compose` where you can make different container interact with each other. For example your backend and front end can be two containers which can be composed in the following way:

```
version: "3"
services:
  backend:
    image: jodogne/orthanc-plugins
    ports:
      - 8042:8042

  frontend:
    image: ohif/viewer
    ports:
      - 3000:80

volumes: 
  orthanc_db:
```

All in all, docker is just OOP for systems programming. No wonder we all love docker and its abstractions. 