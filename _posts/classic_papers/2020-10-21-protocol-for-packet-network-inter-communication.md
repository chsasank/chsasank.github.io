---
layout: post
title: "A Protocol For Packet Network Intercommunication"
author: "Vinton G. Cerf And Robert E. Icahn"
category: classic_papers
description: 
published: 1974-05-01
twitter_image: https://images.livemint.com/rf/Image-621x414/LiveMint/Period1/2014/01/02/Photos/Vinton-Cerf_Robert-Kahn2--621x414.jpg
---


## Abstract

A protocol that supports the sharing of resources that exist in different packet switching networks is presented. The protocol provides for variation in individual network packet sizes, transmission failures, sequencing, flow control, end-to-end error checking, and the creation and destruction of logical process-to-process connections. Some implementation issues are considered, and problems such as internetwork routing, accounting, and timeouts are exposed. 

## Introduction

In the last few years considerable effort has been expended on the design and implementation of packet switching networks [l]-[7],[14],[17]. A principle reason for developing such networks has been to facilitate the sharing of computer resources. A packet communication network includes a transportation mechanism for delivering data between computers or between computers and terminals. To make the data meaningful, computers and terminals share a common protocol (i.e., a set of agreed upon conventions). Several protocols have already been developed for this purpose [8]-[12],[16]. However, these protocols have addressed only the problem of communication on the same network. In this paper we present a protocol design and philosophy that supports the sharing of resources that exist in different packet switching networks.


After a brief introduction to internetwork protocol issues, we describe the function of a `gateway` as an interface between networks and discuss its role in the protocol. We then consider the various details of the protocol, including addressing, formatting, buffering, sequencing, flow control, error control, and so forth. Wc close with a description of an interprocess communication mechanism and show how it can be supported by the internetwork protocol.

Even though many different and complex problems must be solved in the design of an individual packet switching network, these problems are manifestly compounded when dissimilar networks are interconnected. Issues arise which may have no direct counterpart in an individual network and which strongly influence the way in which internetwork communication can take place.

A typical packet switching network is composed of a set of computer resources called `hosts`, a set of one or more *packet switches*, and a collection of communication media that interconnect the packet switches. Within each `host`, we assume that there exist *processes* which must communicate with processes in their own or other `hosts`. Any current definition of a process will be adequate for our purposes [13]. These processes are generally the ultimate source and destination of data in the network. Typically, within an individual network, there exists a protocol for communication between any source and destination process. Only the source and destination processes require knowledge of this convention for communication to take place. Processes in two distinct networks would ordinarily use different protocols for this purpose. The ensemble of packet switches and communication media is called the *packet switching subnet*. Fig. 1 illustrates these ideas. 


<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">
 Fig. 1. Typical packet switching network. 
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig1.png'>
</figure>

In a typical packet switching subnet, data of a fixed maximum size are accepted from a source `host`, together with a formatted destination address which is used to route the data in a store and forward fashion. The transmit time for this data is usually dependent upon internal network parameters such as communication media data rates, buffering and signaling strategies, routing, propagation delays, etc. In addition, some mechanism is generally present for error handling and determination of status of the networks components.

Individual packet switching networks may differ in their implementations as follows.

1. Each network may have distinct ways of addressing the receiver, thus requiring that a uniform addressing scheme be created which can be understood by each individual network.
2. Each network may accept data of different maximum size, thus requiring networks to deal in units of the smallest maximum size (which may he impractically small) or requiring procedures which allow data crossing a network boundary to be reformatted into smaller pieces. 
3. The success or failure of a transmission and its performance in each network is governed by different time delays in accepting, delivering, and transporting the data. This requires careful development of internetwork timing procedures to insure that data can be successfully delivered through the various networks.
4. Within each network, communication may be disrupted due to unrecoverable mutation of the data or missing data. End-to-end restoration procedures are desirable to allow complete recovery from these conditions. 
5. Status information, routing, fault detection, and isolation are typically different in each network. Thus, to obtain verification of certain conditions, such as an inaccessible or dead destination, various kinds of coordination must be invoked between the communicating networks. 

It would be extremely convenient if all the differences between networks could be economically  resolved by suitable interfacing at the network boundaries. For many of the differences, this objective can be achieved. However, both economic and technical considerations lead us to prefer that the interface be as simple and reliable as possible and deal primarily with passing data between networks that use different packet switching strategies.

The question now arises as to whether the interface ought to account for differences in `host` or process level protocols by transforming the source conventions into the corresponding destination conventions. We obviously want to allow  conversion between packet switching strategies at  the interface, to permit interconnection of existing and planned networks. However, the complexity and dissimilarity of the `host` or process level protocols makes it desirable to avoid having to transform between them at the interface, even if this transformation were always possible. Rather, compatible `host` and process level protocols must be developed to achieve effective internetwork resource sharing. The unacceptable alternative is for every `host` or process to implement every protocol (a potentially unbounded number) that may be needed to communicate with other networks. We therefore assume that a common protocol is to be used between `host`'s or processes in different networks and that the interface between networks should take as small a role as possible in this protocol.