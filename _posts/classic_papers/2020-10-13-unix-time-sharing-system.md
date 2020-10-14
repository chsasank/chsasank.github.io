---
layout: post
title: "The UNIX Time-Sharing System"
author: "Dennis M. Ritchie and Ken Thompson"
category: classic_papers
description: 
published: 1974-07-01
twitter_image: https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/Ken_Thompson_%28sitting%29_and_Dennis_Ritchie_at_PDP-11_%282876612463%29.jpg/1280px-Ken_Thompson_%28sitting%29_and_Dennis_Ritchie_at_PDP-11_%282876612463%29.jpg
---

> **NOTE**: This is *not* my article. This is a classic paper originally published in *Communications of the ACM*, 1974 by Dennis M. Ritchie and Ken Thompson. Blue highlights/annotations are my own.

## Abstract

UNIX is a general-purpose, multi-user, interactive operating system for the Digital Equipment Corporation PDP-11/40 and 11/45 computers. It offers a number of features seldom found even in larger operating systems, including: (1) a hierarchical file system incorporating demountable volumes; (2) compatible file, device, and inter-process I/O; (3) the ability to initiate asynchronous processes; (4) system command language select-able on a per-user basis; and (5) over 100 subsystems including a dozen languages. This paper discusses the nature and implementation of the file system and of the user command interface.

## 1. Introduction

There have been three versions of UNIX. The earliest version (circa 1969–70) ran on the Digital Equipment Corporation PDP-7 and -9 computers. The second version ran on the unprotected PDP-11/20 computer. This paper describes only the PDP-11/40 and /45 [l] system since it is more modern and many of the differences between it and older UNIX systems result from redesign of features found to be deficient or lacking.

Since PDP-11 UNIX became operational in February 1971, about 40 installations have been put into service; they are generally smaller than the system described here. Most of them are engaged in applications such as the preparation and formatting of patent applications and other textual material, the collection and processing of trouble data from various switching machines within the Bell System, and recording and checking telephone service orders. Our own installation is used mainly for research in operating systems, languages, computer networks, and other topics in computer science, and also for document preparation.

Perhaps the most important achievement of UNIX is to demonstrate that a powerful operating system for interactive use need not be expensive either in equipment or in human effort: UNIX can run on hardware costing as little as $40,000, and less than two man years were spent on the main system software. Yet UNIX contains a number of features seldom offered even in much larger systems. It is hoped, however, the users of UNIX will find that the most important characteristics of the system are its simplicity, elegance, and ease of use.

> The most important achievement of UNIX is to demonstrate that a powerful operating system need not be expensive either in equipment or in human effort.
> However, the users of UNIX will find that the most important characteristics of the system are its simplicity, elegance, and ease of use.

Besides the system proper, the major programs available under UNIX are: assembler, text editor based on QED[2], linking loader, symbolic debugger, compiler for a language resembling BCPL[3] with types and structures (C), interpreter for a dialect of BASIC, text formatting program, Fortran compiler, Snobol interpreter, top-down compiler-compiler (TMG) [4], bottom-up compiler-compiler (YACC), form letter generator, macro processor (M6) [5], and permuted index program.

There is also a host of maintenance, utility, recreation, and novelty programs. All of these programs were written locally. It is worth noting that the system is totally self-supporting. All UNIX software is maintained under UNIX; likewise, UNIX documents are generated and formatted by the UNIX editor and text formatting program.

## 2. Hardware and Software Environment

The PDP-11/45 on which our UNIX installation is implemented is a 16-bit word (8-bit byte) computer with 144 Kbytes of core memory; UNIX occupies 42K bytes. This system, however, includes a very large number of device drivers and enjoys a generous allotment of space for I/O buffers and system tables; a minimal system capable of running the software mentioned above can require as little as 50K bytes of core altogether.

The PDP-11 has a 1M byte fixed-head disk, used for filesystem storage and swapping, four moving-head disk drives which each provide 2.5M bytes on removable disk cartridges, and a single moving-head disk drive which uses removable 40M byte disk packs. There are also a high-speed paper tape reader-punch, nine-track magnetic tape,and D-tape (a variety of magnetic tape facility in which individual records may be addressed and rewritten). Besides the console typewriter, there are 14 variable-speed communications interfaces attached to 100-series datasets and a 201 dataset interface used primarily for spooling printout to a communal line printer. There are also several one-of-a-kind devices including a Picturephone® interface,a voice response unit, a voice synthesizer, a phototypesetter, a digital switching network, and a satellite PDP-11/20 which generates vectors, curves, and characters on a Tektronix 611 storage-tube display.

The greater part of UNIX software is written in the above-mentioned C language [6]. Early versions of the operating system were written in assembly language, but during the summer of 1973, it was rewritten in C. The size of the new system is about one third greater than the old. Since the new system is not only much easier to understand and to modify but also includes many functional improvements, including multiprogramming and the ability to share reentrant code among several user programs, we considered this increase in size quite acceptable.

## 3. The File System

<span class="marginnote">
 S: Note how the paper considers files important. Unix exposes all the functionality through files.
</span>

The most important job of UNIX is to provide a file system. From the point of view of the user, there are three kinds of files: ordinary disk files, directories, and special files

### 3.1 Ordinary Files

A file contains whatever information the user places on it, for example symbolic or binary (object) programs. No particular structuring is expected by the system. Files of text consist simply of a string of characters, with lines demarcated by the new-line character. Binary programs are sequences of words as they will appear in core memory when the program starts executing. A few user programs manipulate files with more structure: the assembler generates and the loader expects an object file in a particular format. However, the structure of files is controlled by the programs which use them, not by the system.

> A file contains whatever information the user places on it, for example symbolic or binary (object) programs. No particular structuring is expected by the system.

### 3.2 Directories

Directories provide the mapping between the names of files and the files themselves, and thus induce a structure on the file system as a whole. Each user has a directory of his own files; he may also create subdirectories to contain groups of files conveniently treated together. A directory behaves exactly like an ordinary file except that it cannot be written on by unprivileged programs, so that the system controls the contents of directories. However, anyone with appropriate permission may read a directory just like any other file.

> Directories provide the mapping between the names of files and the files themselves, and thus induce a structure on the file system as a whole.

The system maintains several directories for its own use. One of these is the *root* directory. All files in the system can be found by tracing a path through a chain of directories until the desired file is reached. The starting point for such searches is often the root. Another system directory contains all the programs provided for general use; that is, all the *commands*. As will be seen however, it is by no means necessary that a program reside in this directory for it to be executed.

<span class="marginnote">
 S: This is a succinct description of path system of unix and Linux
</span>

Files are named by sequences of 14 or fewer characters. When the name of a file is specified to the system, it may be in the form of a *path name*, which is a sequence of directory names separated by slashes `/` and ending in a file name. If the sequence begins with a slash, the search begins in the root directory. The name `/alpha/beta/gamma` causes the system to search the root for directory `alpha`, then to search `alpha` for `beta`, finally to find `gamma` in `beta`. `gamma` may be an ordinary file, a directory, or a special file. As a limiting case, the name `/` refers to the root itself.

A path name not starting with `/` causes the system to begin the search in the user’s current directory. Thus, the name `alpha/beta` specifies the file named `beta` in subdirectory `alpha` of the current directory. The simplest kind of name, for example `alpha`, refers to a file which itself is found in the current directory. As another limiting case, the null file name refers to the current directory.

The same nondirectory file may appear in several directories under possibly different names. This feature is called *linking*; a directory entry for a file is sometimes called a link. UNIX differs from other systems in which linking is permitted in that all links to a file have equal status. That is, a file does not exist within a particular directory; the directory entry for a file consists merely of its name and a pointer to the information actually describing the file. Thus a file exists independently of any directory entry, although in practice a file is made to disappear along with the last link to it.

> UNIX differs from other systems in which linking is permitted in that all links to a file have equal status.

Each directory always has at least two entries. The name in each directory refers to the directory itself. Thus a program may read the current directory under the name `.` without knowing its complete path name. The name `..` by convention refers to the parent of the directory in which it appears, that is, to the directory in which it was created.

The directory structure is constrained to have the form of a rooted tree. Except for the special entries `.` and `..`, each directory must appear as an entry in exactly one other,which is its parent. Te reason for this is to simplify the writing of programs which visit subtrees of the directory structure, and more important, to avoid the separation of portions of the hierarchy. If arbitrary links to directories were permitted, it would be quite difficult to detect when the last connection from the root to a directory was severed.

> The directory structure is constrained to have the form of a rooted tree.

### 3.3 Special Files

Special files constitute the most unusual feature of the UNIX file system. Each I/O device supported by UNIX is associated with at least one such file. Special files are read and written just like ordinary disk files, but requests to read or write result in activation of the associated device. An entry for each special file resides in directory `/dev`, although a link may be made to one of these files just like an ordinary file. Thus, for example, to punch paper tape, one may write on the file `/dev/ppt`. Special files exist for each communication line, each disk, each tape drive, and for physical core memory. Of course, the active disks and the core special file are protected from indiscriminate access.

> Special files constitute the most unusual feature of the UNIX file system. Each I/O device supported by UNIX is associated with at least one such file.

There is a threefold advantage in treating I/O devices this way: file and device I/O are as similar as possible; file and device names have the same syntax and meaning, so that a program expecting a file name as a parameter can be passed a device name; finally, special files are subject to the same protection mechanism as regular files.

### 3.4 Removable File Systems

Although the root of the file system is always stored on the same device, it is not necessary that the entire file system hierarchy reside on this device. There is a `mount` system request which has two arguments: the name of an existing ordinary file, and the name of a direct-access special file whose associated storage volume (e.g. disk pack) should have the structure of an independent file system containing its own directory hierarchy. The effect of `mount` is to cause references to the heretofore ordinary file to refer instead to the root directory of the file system on the removable volume. In effect, mount replaces a leaf of the hierarchy tree (the ordinary file) by a whole new subtree (the hierarchy stored on the removable volume). After the `mount`, there is virtually no distinction between files on the removable volume and those in the permanent file system. In our installation, for example, the root directory resides on the fixed-head disk, and the large disk drive, which contains user's files, is mounted by the system initialization program, the four smaller disk drives are available to users for mounting their own disk packs. A mountable file system is generated by writing on its corresponding special file. A utility program is available to create an empty file system, or one may simply copy an existing file system.

> The effect of `mount` is to cause references to the heretofore ordinary file to refer instead to the root directory of the file system on the removable volume.

There is only one exception to the rule of identical treatment of files on different devices: no link may exist between one file system hierarchy and another. This restriction is enforced so as to avoid the elaborate bookkeeping which would otherwise be required to assure removal of the links when the removable volume is finally dismounted. In particular, in the root directories of all file systems, removable or not, the name `..` refers to the directory itself instead of to its parent.

### 3.5 Protection

Although the access control scheme in UNIX is quite simple, it has some unusual features. Each user of the system is assigned a unique user identification number. When a file is created, it is marked with the user ID of its owner. Also given for new files is a set of seven protection bits. Six of these specify independently read, write, and execute permission for the owner of the file and for all other users.

If the seventh bit is on, the system will temporarily change the user identification of the current user to that of the creator of the file whenever the file is executed as a program. This change in user ID is effective only during the execution of the program which calls for it. The set-user-ID feature provides for privileged programs which may use files inaccessible to other users. For example, a program may keep an accounting file which should neither be read nor changed except by the program itself. If the set-user-identification bit is on for the program, it may access the file although this access might be forbidden to other programs invoked by the given program’s user. Since the actual user ID of the invoker of any program is always available, set-user-ID programs may take any measures desired to satisfy themselves as to their invoker's credentials. This mechanism is used to allow users to execute the carefully written commands which call privileged system entries. For example, there is a system entry invocable only by the “super-user” (below) which creates an empty directory. As indicated above, directories are expected to have entries for `.` and `..` . The command which creates a directory is owned by the superuser and has the set-user-ID bitset. After it checks its invoker’s authorization to create the specified directory, it creates it and makes the entries for `.` and `..` .

Since anyone may set the set-user-ID bit on one of his own files, this mechanism is generally available without administrative intervention. For example, this protection scheme easily solves the MOO accounting problem posed in [7].

The system recognizes one particular user ID (that of the “super-user”) as exempt from the usual constraints on file access; thus (for example) programs may be written to dump and reload the file system without unwanted interference from the protection system.

### 3.6 I/O Calls

The system calls to do I/O are designed to eliminate the differences between the various devices and styles of access. There is no distinction between "random" and sequential I/O, nor is any logical record size imposed by the system. The size of an ordinary file is determined by the highest byte written on it; no predetermination of the size of a file is necessary or possible.

> There is no distinction between "random" and sequential I/O, nor is any logical record size imposed by the system.

To illustrate the essentials of I/O in UNIX, Some of the basic calls are summarized below in an anonymous language which will indicate the required parameters without getting into the complexities of machine language programming. Each call to the system may potentially result in an error return, which for simplicity is not represented in the calling sequence.

To read or write a file assumed to exist already, it must be opened by the following call:

```C
filep = open(name, flag)
```

`name` indicates the name of the file. An arbitrary path name may be given. The `flag` argument indicates whether the file is to be read, written, or "updated", that is read and written simultaneously.

The returned value `filep` is called a file descriptor. It is a small integer used to identify the file in subsequent calls to read, write, or otherwise manipulate it.

To create a new file or completely rewrite an old one, there is a `create` system call which creates the given file if it does not exist, or truncates it to zero length if it does exist. `create` also opens the new file for writing and, like `open`, returns a file descriptor.

There are no user-visible locks in the file system, nor is there any restriction on the number of users who may have a file open for reading or writing; although it is possible for the contents of a file to become scrambled when two users write on it simultaneously, in practice, difficulties do not arise. We take the view that locks are neither necessary nor sufficient, in our environment, to prevent interference between users of the same file. They are unnecessary because we are not faced with large, single-file data bases maintained by independent processes. They are insufficient because locks in the ordinary sense, whereby one user is prevented from writing on a file which another user is reading, cannot prevent confusion when, for example, both users are editing a file with an editor which makes a copy of the file being edited.

> There are no user-visible locks in the file system, nor is there any restriction on the number of users who may have a file open for reading or writing

It should be said that the system has sufficient internal interlocks to maintain the logical consistency of the file system when two users engage simultaneously in such inconvenient activities as writing on the same file, creating files in the same directory or deleting each other’s open files.

Except as indicated below, reading and writing are sequential. This means that if a particular byte in the file was the last byte written (or read), the next I/O call implicitly refers to the first following byte. For each open file there is a pointer, maintained by the system, which indicates the next byte to be read or written. If n bytes are read or written, the pointer advances by n bytes.

Once a file is open, the following calls may be used:

```C
n = read(filep, buffer, count)
n = write(filep, buffer, count)
```

Up to `count` bytes are transmitted between the file specified by `filep` and the byte array specified by `buffer`. The returned value `n` is the number of bytes actually transmitted. In the `write` case, `n` is the same as `count` except under exceptional conditions like I/O errors or end of physical medium on special files; in a `read`, however, `n` may without error be less than `count`. If the read pointer is so near the end of the file that reading `count` characters would cause reading beyond the end, only sufficient bytes are transmitted to reach the end of the file; also, typewriter-like devices never return more than one line of input. When a `read` call returns with `n` equal to zero, it indicates the end of the file. For disk files this occurs when the read pointer becomes equal to the current size of the file. It is possible to generate an end-of-file from a typewriter by use of an escape sequence which depends on the device used.

Bytes written on a file affect only those implied by the position of the write pointer and the count; no other part of the file is changed. If the last byte lies beyond the end of the file, the file is grown as needed.

To do random (direct access) I/O, it is only necessary to move the read or write pointer to the appropriate location in the file:

```C
location = seek(filep, base, offset)
```

The pointer associated with `filep` is moved to a position `offset` bytes from the beginning of the file, from the current position of the pointer, or from the end of the file, depending on `base`. `offset` may be negative. For some devices (e.g. paper tape and typewriters) `seek` calls are ignored. The actual offset from the beginning of the file to which the pointer was moved is returned in `location`.

**3.6.1: Other I/O Calls**: There are several additional system entries having to do with I/O and with the file system which will not be discussed. For example: close a file, get the status of a file, change the protection mode or the owner of a file, create a directory, make a link to an existing file, delete a file.

## 4. Implementation of the File System

<span class="marginnote">
 S: Implementation of a toy file system (in rust?) makes for an illustrative exercise. Nice blog idea!
</span>
As mentioned in §3.2 above, a directory entry contains only a name for the associated file and a pointer to the file itself. This pointer is an integer called the `i-number` (for index number) of the file. When the file is accessed, its `i-number` is used as an index into a system table (the `i-list`) stored in a known part of the device on which the directory resides. The entry thereby found (the file’s `i-node`) contains the description of the file as follows.

1. Its owner.
2. Its protection bits.
3. The physical disk or tape addresses for the file contents.
4. Its size.
5. Time of last modification.
6. The number of links to the file, that is, the number of times it appears in a directory.
7. A bit indicating whether the file is a directory.
8. A bit indicating whether the file is a special file.
9. A bit indicating whether the file is "large" or "small."

The purpose of an `open` or `create` system call is to turn the path name given by the user into an `i-number` by searching the explicitly or implicitly named directories. Once a file is open, its device, `i-number`, and read/write pointer are stored in a system table indexed by the file descriptor returned by the open or create. Thus the file descriptor supplied during a subsequent call to read or write the file may be easily related to the information necessary to access the file.

> The purpose of an `open` or `create` system call is to turn the path name given by the user into an `i-number` by searching the explicitly or implicitly named directories

When a new file is created, an `i-node` is allocated for it and a directory entry is made which contains the name of the file and the `i-node` number. Making a link to an existing file involves creating a directory entry with the new name, copying the `i-number` from the original file entry, and incrementing the link-count field of the `i-node`. Removing (deleting) a file is done by decrementing the link-count of the `i-node` specified by its directory entry and erasing the directory entry. If the link-count drops to 0, any disk blocks in the file are freed and the `i-node` is deallocated.

The space on all fixed or removable disks which contain a file system is divided into a number of 512-byte blocks logically addressed from 0 up to a limit which depends on the device. There is space in the `i-node` of each file for eight device addresses. A *small* (nonspecial) file fits into eight or fewer blocks; in this case the addresses of the blocks themselves are stored. For *large* (nonspecial) files, each of the eight device addresses may point to an indirect block of 256 addresses of blocks constituting the file itself. These files may be as large as 8⋅256⋅512, or l,048,576 (220) bytes.

The foregoing discussion applies to ordinary files. When an I/O request is made to a file whose `i-node` indicates that it is special, the last seven device address words are immaterial, and the list is interpreted as a pair of bytes which constitute an internal *device* name. These bytes specify respectively a device type and subdevice number. The device type indicates which system routine will deal with I/O on that device; the subdevice number selects, for example, a disk drive attached to a particular controller or one of several similar typewriter interfaces.

In this environment, the implementation of the `mount` system call (§3.4) is quite straightforward. `mount` maintains a system table whose argument is the `i-number` and device name of the ordinary file specified during the mount, and whose corresponding value is the device name of the indicated special file. This table is searched for each (`i-number`, device)-pair which turns up while a path name is being scanned during an open or create; if a match is found, the `i-number` is replaced by 1 (which is the `i-number` of the root directory on all file systems), and the device name is replaced by the table value.

To the user, both reading and writing of files appear to be synchronous and unbuffered. That is immediately after return from a `read` call the data are available, and conversely after a `write` the user's workspace may be reused. In fact the system maintains a rather complicated buffering mechanism which reduces greatly the number of I/O operations required to access a file. Suppose a `write` call is made specifying transmission of a single byte.

> To the user, both reading and writing of files appear to be synchronous and unbuffered.


UNIX will search its buffers to see whether the affected disk block currently resides in core memory; if not, it will be read in from the device. Then the affected byte is replaced in the buffer, and an entry is made in a list of blocks to be written. The return from the `write` call may then take place, although the actual I/O may not be completed until a later time. Conversely, if a single byte is read, the system determines whether the secondary storage block in which the byte is located is already in one of the system's buffers; if so, the byte can be returned immediately. If not, the block is read into a buffer and the byte picked out.

A program which reads or writes files in units of 512 bytes has an advantage over a program which reads or writes a single byte at a time, but the gain is not immense; it comes mainly from the avoidance of system overhead. A program which is used rarely or which does no great volume of I/O may quite reasonably read and write in units as small as it wishes.

The notion of the `i-list` is an unusual feature of UNIX. In practice, this method of organizing the file system has proved quite reliable and easy to deal with. To the system itself, one of its strengths is the fact that each file has a short, unambiguous name which is related in a simple way to the protection, addressing, and other information needed to access the file. It also permits a quite simple and rapid algorithm for checking the consistency of a file system, for example verification that the portions of each device containing useful information and those free to be allocated are disjoint and together exhaust the space on the device. This algorithm is independent of the directory hierarchy, since it need only scan the linearly-organized `i-list`. At the same time the notion of the `i-list` induces certain peculiarities not found in other file system organizations. For example, there is the question of who is to be charged for the space a file occupies, since all directory entries for a file have equal status. Charging the owner of a file is unfair, in general, since one user may create a file, another may link to it, and the first user may delete the file. The first user is still the owner of the file, but it should be charged to the second user. The simplest reasonably fair algorithm seems to be to spread the charges equally among users who have links to a file. The current version of UNIX avoids the issue by not charging any fees at all.

### 4.1 Efficiency of the File System

To provide an indication of the overall efficiency of UNIX and of the file system in particular, timings were made of the assembly of a 7621-line program. The assembly was run alone on the machine; the total clock time was 35.9 sec, for a rate of 212 lines per sec. The time was divided as follows: 63.5 percent assembler execution time, 16.5 percent system overhead, 20.0 percent disk wait time. We will not attempt any interpretation of these figures nor any comparison with other systems, but merely note that we are generally satisfied with the overall performance of the system.

> We will not attempt any interpretation of these figures nor any comparison with other systems, but merely note that we are generally satisfied with the overall performance of the system.

## 5. Processes and Images

An *image* is a computer execution environment. It includes a core image, general register values, status of open files, current directory, and the like. An image is the current state of a pseudo computer.

A *process* is the execution of an image. While the processor is executing on behalf of a process, the image must reside in core; during the execution of other processes it remains in core unless the appearance of an active, higher-priority process forces it to be swapped out to the fixed-head disk.

The user-core part of an image is divided into three logical segments. The program text segment begins at location 0 in the virtual address space. During execution, this segment is write-protected and a single copy of it is shared among all processes executing the same program. At the first 8K byte boundary above the program text segment in the virtual address space begins a non-shared, writable data segment, the size of which may be extended by a system call. Starting at the highest address in the virtual address space is a stack segment, which automatically grows downward as the hardware's stack pointer fluctuates.

### 5.1 Processes

Except while UNIX is bootstrapping itself into operation, a new process can come into existence only by use of the `fork` system call:

```C
processid = fork(label)
```

When `fork` is executed by a process, it splits into two independently executing processes. The two processes have independent copies of the original core image, and share any open files. The new processes differ only in that one is considered the parent process: in the parent, control returns directly from the `fork`, while in the child, control is passed to location `label`. The `processid` returned by the `fork` call is the identification of the other process.

Because the return points in the parent and child process are not the same, each image existing after a `fork` may determine whether it is the parent or child process.

### 5.2 Pipes

Processes may communicate with related processes using the same system read and write calls that are used for file system I/O. The call

```
filep = pipe()
```

returns a file descriptor `filep` and creates an interprocess channel called a `pipe`. This channel, like other open flies, is passed from parent to child process in the image by the `fork` call. A `read` using a pipe file descriptor waits until another process writes using the file descriptor for the same pipe. At this point, data are passed between the images of the two processes. Neither process need know that a pipe, rather than an ordinary file, is involved.

Although  interprocess  communication  via  pipes  is  a quite valuable tool (see §6.2), it is not a completely general mechanism  since  the  pipe  must  be  set  up  by  a  common ancestor of the processes involved.

### 5.3  Execution of Programs

Another major system primitive is invoked by

```
execute(file, arg1, arg2, ..., argn)
```

which  requests  the  system  to  read  in  and  execute  the  program named by `file`, passing it string arguments `arg1`, `arg2`,..., `argn`. Ordinarily, `arg1`  should  be  the  same  string  as  `file`, so  that  the  program  may  determine  the  name  by  which  it was invoked. All the code and data in the process using `execute` is replaced from the file, but open files, current directory,  and  interprocess  relationships  are  unaltered.  Only if the call fails, for example because `file` could not be found or because its execute-permission bit was not set, does a return take place from the `execute` primitive; it resembles a "jump" machine instruction rather than a subroutine call.

### 5.4  Process Synchronization

Another process control system call

```C
processid = wait()
```

causes  its  caller  to  suspend  execution  until  one  of  its  children  has  completed  execution.  Then  `wait`  returns  the  `processid` of the terminated process. An error return is taken if the calling process has no descendants. Certain status from the  child  process  is  also  available.  `wait`  may  also  present status from a grandchild or more distant ancestor; see §5.5.

### 5.5 Termination

Lastly,

```
exit(status)
```

terminates  a  process,  destroys  its  image,  closes  its  open files,  and  generally  obliterates  it.  When  the  parent  is  notified through the `wait` primitive, the indicated `status` is available to the parent; if the parent has already terminated, the status is available to the grandparent, and so on. Processes may  also  terminate  as  a  result  of  various  illegal  actions  or user-generated signals (§7 below).

