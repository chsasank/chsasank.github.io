---
layout: post
title: Exposing C functions to Lua
---

Lua has a very simple C API. You can run Lua code and access lua objects from C. Similarly, you can access C functions and libraries from Lua. In this post, we'll primarily look at how to expose C functions to lua.

### Install Lua

Let's start off with the installation of lua. 
You can install lua using system package managers as 

Mac:

```bash
$ brew install lua
```

Ubuntu:

```bash
$ sudo apt-get install lua5.2 liblua5.2-dev 
```

You can also build lua from source. However, you have to build lua as a shared library and this can be a bit tricky.

### Lua Stack

Communication between lua and C is held through stacks with each element in stack representing lua value (nil, number, string, etc.). 

For example, a C wrapped function `c_swap` is called in lua as:

```
x, y = c_swap(4, 5)
```

In C, you will receive arguments in a stack as 

```
lua_State *L:

|     5     |  <--- 2
+-----------+
|     4     |  <--- 1
+===========+
```

You will then read the arguments from the stack as (indexing for this stack starts with 1 like in lua): 

```C
double arg1 = lua_tonumber (L, 1);
double arg2 = lua_tonumber (L, 2);
```

This is actually unsafe because these calls will fail if value in the stack at the specified index is not a number. 
Following code uses auxiliary library `lauxlib.h` and is safer; it will raise errors instead of segfaulting.

```C
double arg1 = luaL_checknumber (L, 1);
double arg2 = luaL_checknumber (L, 2);
```

Then, you will process the arguments and push the results to stack.

```
lua_pushnumber(L, arg2);
lua_pushnumber(L, arg1);
```

Stack will now look like

```
|     4     |  <--- 4
+-----------+
|     5     |  <--- 3
+-----------+
|     5     |  <--- 2
+-----------+
|     4     |  <--- 1
+===========+
```

Since you need to communicate the number of return value to lua, you will `return 2`. Lua will then know that top 2 values in the stack are the returned values. 

The stack we've described is *not* global. Each function has its own stack. 

### Code

Let's put all this together into `main.c`. `main()` function illustrates how to run lua code from C.

```c
#ifdef __cplusplus
  #include "lua.hpp"
#else
  #include "lua.h"
  #include "lualib.h"
  #include "lauxlib.h"
#endif

//so that name mangling doesn't mess up function names
#ifdef __cplusplus
extern "C"{
#endif

static int c_swap (lua_State *L) {
    //check and fetch the arguments
    double arg1 = luaL_checknumber (L, 1);
    double arg2 = luaL_checknumber (L, 2);

    //push the results
    lua_pushnumber(L, arg2);
    lua_pushnumber(L, arg1);

    //return number of results
    return 2;
}

#ifdef __cplusplus
}
#endif


int main(){
    // Create new Lua state and load the lua libraries
    lua_State *L = luaL_newstate();
    luaL_openlibs(L);

    //Expose the c_swap function to the lua environment
    lua_pushcfunction(L, c_swap);
    lua_setglobal(L, "c_swap");

    // Tell Lua to execute a lua command
    luaL_dostring(L, "print(c_swap(4, 5))");
    return 0;
}
```

And build and execute as 
<span class="marginnote" margin-bottom='100px' >
    If you have installed lua with apt-get, build using `gcc main.c -o swap -llua5.2 -I/usr/include/lua5.2/`
</span>

```bash
$ gcc main.c -o swap -llua
$ ./swap
```

which should print

```
5   4
```

### Library

In the above code, you've not really used lua interpretor; you've run the lua code in C itself.
You might rather want to create a module which you can load into a lua interpretor using 

```
> mylib = require "mylib"
```

To do this, you'll have to register your functions by creating a array of `luaL_Reg` and a function `luaopen_mylib`. 

Let's do this right away by editing `main.c`. We will also add additional `mysin` function.

<span class="marginnote" margin-bottom='100px' >
    This code works only for lua 5.2. For lua 5.1, please use
    `luaL_register(L, "mylib", mylib);`
    instead of `luaL_newlib(L, mylib);`
</span>

```c
#ifdef __cplusplus
  #include "lua.hpp"
#else
  #include "lua.h"
  #include "lualib.h"
  #include "lauxlib.h"
#endif
#include <math.h>

//so that name mangling doesn't mess up function names
#ifdef __cplusplus
extern "C"{
#endif

static int c_swap (lua_State *L) {
    //check and fetch the arguments
    double arg1 = luaL_checknumber (L, 1);
    double arg2 = luaL_checknumber (L, 2);

    //push the results
    lua_pushnumber(L, arg2);
    lua_pushnumber(L, arg1);

    //return number of results
    return 2;
}

static int my_sin (lua_State *L) {
    double arg = luaL_checknumber (L, 1);
    lua_pushnumber(L, sin(arg));
    return 1;
}

//library to be registered
static const struct luaL_Reg mylib [] = {
      {"c_swap", c_swap},
      {"mysin", my_sin}, /* names can be different */
      {NULL, NULL}  /* sentinel */
    };

//name of this function is not flexible
int luaopen_mylib (lua_State *L){
    luaL_newlib(L, mylib);
    return 1;
}

#ifdef __cplusplus
}
#endif
```

Create a loadable `.so` file with 
<span class="marginnote" margin-bottom='100px' >
    If you have installed lua with apt-get, build using `gcc main.c -shared -o mylib.so -fPIC  -llua5.2 -I/usr/include/lua5.2/`
</span>

```
$ gcc main.c -shared -o mylib.so -fPIC  -llua
```

You can now load this module in lua.

```
$ lua
Lua 5.2.4  Copyright (C) 1994-2015 Lua.org, PUC-Rio
> mylib = require 'mylib'
> print(mylib.c_swap(2, 4))
4   2
> print(mylib.mysin(2))
0.90929742682568
```

Now, you should be able to wrap any C library by writing a lua wrapper.
We still have some caveats: 

1. We will have to write a wrapping function for each of the functions in your C library to 'parse' the arguments(`luaL_checknumber` etc.). This is repetitive and can be a potential source for bugs. 
2. C doesn't have classes. What if your library is in C++? You cannot easily wrap classes like above. You will have to mess with metatables and so on.

A solution to both of these is to use [Swig](http://www.swig.org/). Swig allows you to wrap C/C++ classes/functions into many languages like python, lua, java quite easily. In a later post, we will see how to use swig and wrap a simple library.