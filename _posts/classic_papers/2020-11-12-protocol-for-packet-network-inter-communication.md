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

A protocol that supports the sharing of resources that exist in different
packet switching networks is presented. The protocol provides for variation
in individual network packet sizes, transmission failures, sequencing, flow
control, end-to-end error checking, and the creation and destruction of
logical process-to-process connections. Some implementation issues are
considered, and problems such as internetwork routing, accounting, and
timeouts are exposed. 

## Introduction

In the last few years considerable effort has been expended on the design
and implementation of packet switching networks [l]-[7],[14],[17]. A
principle reason for developing such networks has been to facilitate the
sharing of computer resources. A packet communication network includes a
transportation mechanism for delivering data between computers or between
computers and terminals. To make the data meaningful, computers and
terminals share a common protocol (i.e., a set of agreed upon conventions).
Several protocols have already been developed for this purpose
[8]-[12],[16]. However, these protocols have addressed only the problem of
communication on the same network. <span class="mark">In this paper we
present a protocol design and philosophy that supports the sharing of
resources that exist in different packet switching networks.</span>


After a brief introduction to internetwork protocol issues, we describe the
function of a *gateway* as an interface between networks and discuss its
role in the protocol. We then consider the various details of the protocol,
including addressing, formatting, buffering, sequencing, flow control, error
control, and so forth. We close with a description of an interprocess
communication mechanism and show how it can be supported by the internetwork
protocol.

Even though many different and complex problems must be solved in the design
of an individual packet switching network, these problems are manifestly
compounded when dissimilar networks are interconnected. Issues arise which
may have no direct counterpart in an individual network and which strongly
influence the way in which internetwork communication can take place.

<span class="mark">A typical packet switching network is composed of a set
of computer resources called  hosts*, a set of one or more *packet
switches*, and a collection of communication media that interconnect the
packet switches. Within each host, we assume that there exist *processes*
which must communicate with processes in their own or other hosts.</span>
Any current definition of a process will be adequate for our purposes [13].
These processes are generally the ultimate source and destination of data in
the network. Typically, within an individual network, there exists a
protocol for communication between any source and destination process. Only
the source and destination processes require knowledge of this convention
for communication to take place. Processes in two distinct networks would
ordinarily use different protocols for this purpose. The ensemble of packet
switches and communication media is called the *packet switching subnet*.
Fig. 1 illustrates these ideas. 


<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">
 Fig. 1. Typical packet switching network. 
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig1.png'>
</figure>

In a typical packet switching subnet, data of a fixed maximum size are
accepted from a source host, together with a formatted destination address
which is used to route the data in a store and forward fashion. The transmit
time for this data is usually dependent upon internal network parameters
such as communication media data rates, buffering and signaling strategies,
routing, propagation delays, etc. In addition, some mechanism is generally
present for error handling and determination of status of the networks
components.

Individual packet switching networks may differ in their implementations as follows.

1. Each network may have distinct ways of addressing the receiver, thus
   requiring that a uniform addressing scheme be created which can be
   understood by each individual network.
2. Each network may accept data of different maximum size, thus requiring
   networks to deal in units of the smallest maximum size (which may he
   impractically small) or requiring procedures which allow data crossing a
   network boundary to be reformatted into smaller pieces. 
3. The success or failure of a transmission and its performance in each
   network is governed by different time delays in accepting, delivering,
   and transporting the data. This requires careful development of
   internetwork timing procedures to insure that data can be successfully
   delivered through the various networks.
4. Within each network, communication may be disrupted due to unrecoverable
   mutation of the data or missing data. End-to-end restoration procedures
   are desirable to allow complete recovery from these conditions. 
5. Status information, routing, fault detection, and isolation are typically
   different in each network. Thus, to obtain verification of certain
   conditions, such as an inaccessible or dead destination, various kinds of
   coordination must be invoked between the communicating networks. 

<mark>It would be extremely convenient if all the differences between networks
could be economically  resolved by suitable interfacing at the network
boundaries.</mark> For many of the differences, this objective can be achieved.
However, both economic and technical considerations lead us to prefer that
the interface be as simple and reliable as possible and deal primarily with
passing data between networks that use different packet switching
strategies.

The question now arises as to whether the interface ought to account for
differences in host or process level protocols by transforming the source
conventions into the corresponding destination conventions. We obviously
want to allow  conversion between packet switching strategies at  the
interface, to permit interconnection of existing and planned networks.
However, <span class="mark">the complexity and dissimilarity of the host or process level
protocols makes it desirable to avoid having to transform between them at
the interface, even if this transformation were always possible.</span> Rather,
compatible host and process level protocols must be developed to achieve
effective internetwork resource sharing. The unacceptable alternative is for
every host or process to implement every protocol (a potentially unbounded
number) that may be needed to communicate with other networks. We therefore
assume that a common protocol is to be used between hosts or processes in
different networks and that the interface between networks should take as
small a role as possible in this protocol.

To allow networks under different ownership to interconnect, some accounting
will undoubtedly be needed for traffic that passes across the interface. In
its simplest terms, this involves an accounting of packets handled by each
net for which charges are passed from net to net until the buck finally
stops at the user or his representative. Furthermore, the interconnection
must preserve intact the internal operation of each individual network. This
is easily achieved if two networks interconnect as if each were a host to
the other network, but without utilizing or indeed incorporating any
elaborate host protocol transformations.


It is thus apparent that the <span class="mark">interface between networks must play a central
role in the development of any network interconnection strategy. We give a
special name to this interface that performs these functions and call it a
*Gateway*.</span>


## The Gateway Notion

In Fig. 2 we illustrate three individual networks labeled A, B, and C which
are joined by gateways M and N gateway interfaces network A with network B,
and gateway N interfaces network B to network C. We assume that an
individual network may have more than one gateway (e.g., network B) and that
there may be more than one gateway path to use in going between a pair of
networks. The responsibility for properly routing data resides in the
gateway.

<figure>
<label for="mn-fig-2" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-2" class="margin-toggle">
<span class="marginnote">
 Fig. 1. Three networks interconnected by two gateways
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig2.png'>
</figure>

In practice, a gateway between two networks may be composed of two halves,
each associated with its own network. It is possible to implement each half
of a gateway so it need only embed internetwork packets in local packet
format or extract them. We propose that the gateways handle internetwork
packets in a standard format, but we are not proposing any particular transmission procedure between gateway halves.

Let us now trace the flow of data through the inter connected networks. We
assume a packet of data from process X enters network A destined for process
Y in network C. The address of Y is initially specified by process X and the
address of gateway M is derived from the address of process Y. We make no
attempt to specify whether the choice of gateway is made by process X, its
host, or one of the packet switches in network A. The packet traverses
network A until it reaches gateway M. At the gateway, the packet is reformatted
to meet the requirements of network B, account is taken of this unit of flow
between A and B, and the gateway delivers the packet to network B. Again the
derivation of the next gateway address is accomplished based on the address of
the destination Y. In this case, gateway N is the next one. The packet
traverses network B until it finally reached gateway N where it is formatted
to meet the requirement of network C. Account is again taken of this unit
of flow between networks B and C. Upon entering network C the packet is
routed to the host in which process Y resides and there it is delivered to
its ultimate destination.

<span class="mark">Since the gateway must understand the address of the source and destination
hosts, this information must be available in a standard format in every
packet which arrives at the gateway. This information is contained in an
*internetwork header* prefixed to the packet by the source host.</span> The packet
format, including the internetwork header, is illustrated in Fig. 3. The
source and destination entries uniformly and uniquely identify the address
of every host in the composite network. Addressing is a subject of
considerable complexity which is discussed in greater detail in the next
section. The next two entries in the header provide a sequence number and a
byte count that may be used to properly sequence the packets upon delivery
to the destination and may also enable the gateways to detect fault
conditions affecting the packet. The flag field is used to convey specific
control information and is discussed in the section on retransmission and
duplicate detection later. The remainder of the packet consists of text for
delivery to the destination and a trailing check sum used for end-to-end
software verification. The gateway does *not* modify the text and merely
forwards the check sum along without computing or recomputing it.

<figure>
<label for="mn-fig-3" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-3" class="margin-toggle">
<span class="marginnote">
 Fig. 3. Internetwork packet format (fields not shown to scale).
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig3.png'>
</figure>


Each network may need to augment the packet format before it can pass
through the individual network. We have indicated a *local header* in the
figure which is prefixed to the beginning of the packet. This local header
is introduced merely to illustrate the concept of embedding an internetwork
packet in the format of the individual network through which the packet must
pass. It will obviously vary in its exact form from network to network and
may even be unnecessary in some cases. Although not explicitly indicated in
the figure, it is also possible that a local trailer may be appended to the
end of the packet.

<span class="mark">Unless all transmitted packets are legislatively restricted to be small
enough to be accepted by every individual network, the gateway may be forced
to split a packet into two or more smaller packets. This action is called
fragmentation and must be done in such a way that the destination is able to
piece together the fragmented packet.</span> It is clear that the internetwork
header format imposes a minimum packet size which all networks must carry
(obviously all networks will want to carry packets larger than this
minimum). We believe the long range growth and development of internetwork
communication would be seriously inhibited by specifying how much larger
than the minimum a packet size can be, for the following reasons.

1. If a maximum permitted packet size is specified then it becomes
   impossible to completely isolate the internal packet size parameters of
   one network from the internal packet size parameters of all other
   networks.
2.  It would be very difficult to increase the maximum permitted packet size
    in response to new technology (e.g., large memory systems, higher data
    rate communication facilities, etc.) since this would require the
    agreement and then implementation by all participating networks.
3.  Associative addressing and packet encryption may require the size of a
    particular packet to expand during transit for incorporation of new
    information.

Provision for fragmentation (regardless of where it is performed) permits
packet size variations to be handled on an individual network basis without
global administration and also permits hosts and processes to be insulated
from changes in the packet sizes permitted in any networks through which
their data must pass.

If fragmentation must be done, it appears best to do it upon entering the
next network at the gateway since only this gateway (and not the other
networks) must be aware of the internal packet size parameters which made
the fragmentation necessary.

If a gateway fragments an incoming packet into two or more packets, they
must eventually be passed along to the destination host as fragments or
reassembled for the host. <mark>It is conceivable that one might desire the
gateway to perform the reassembly to simplify the task of the destination
host (or process) and/or to take advantage of a larger packet size. We take
the position that gateways should not perform this function since gateway
reassembly can lead to serious buffering problems, potential deadlocks, the
necessity for all fragments of a packet to pass through the same gateway,
and increased delay in transmission.</mark> Furthermore, it is not sufficient for
the gateways to provide this function since the final gateway may also have
to fragment a packet for transmission. Thus the destination host must be
prepared to do this task.

Let us now turn briefly to the somewhat unusual accounting effect which
arises when a packet may be fragmented by one or more gateways. We assume,
for simplicity, that each network initially charges a fixed rate per packet
transmitted, regardless of distance, and if one network can handle a larger
packet size than another, it charges a proportionally larger price per
packet. We also assume that a subsequent increase in any network’s packet
size docs not result in additional cost per packet to its users, The charge
to a user thus remains basically constant through any net which must
fragment a packet. The unusual effect occurs when a packet is fragmented
into smaller packets which must individually pass through a subsequent
network with a larger packet size than the original unfragmented packet. We
expect that most networks will naturally select packet sizes close to one
another, but in any case, an increase in packet size in one net, even when
it causes fragmentation, will not increase the cost of transmission and may
actually decrease it. In the event that any other packet charging policies
(than the one we suggest) are adopted, differences in cost can be used as an
economic lever toward optimization of individual network performance.

## Process Level Communication

We suppose that processes wish to communicate in full duplex with their
correspondents using unbounded but finite length messages. A single
character might constitute the text of a message from a process to a
terminal or vice versa. An entire page of characters might constitute the
text of a message from a file to a process. A data stream (e.g., a
continuously generated bit string) can be represented as a sequence of
finite length messages.

<mark>Within a host we assume the existence of a transmission control program
(TCP) which handles the transmission and acceptance of messages on behalf of
the processes it serves.</mark> The TCP is in turn served by one or more packet
switches connected to the host in which the TCP resides. Processes that want
to communicate present messages to the TCP for transmission, and TCP’s
deliver incoming messages to the appropriate destination processes. <mark>We allow
the TCP to break up messages into segments</mark> because the destination may
restrict the amount of data that may arrive, because the local network may
limit the maximum transmission size, or because the TCP may need to share
its resources among many processes concurrently. Furthermore, we constrain
the length of a segment to an integral number of 8-bit bytes. This
uniformity is most helpful in simplifying the software needed with Host
machines of different natural word lengths. Provision at the process level
can be made for padding a message that is not an integral number of bytes
and for identifying which of the arriving bytes of text contain information
of interest to the receiving process.

<mark>Mutliplexing and demultiplexing of segments among processes are fundamental tasks of the TCP.</mark> On transmission, a TCP must multiplex together segments from different source processes and produce internetwork packets for delivery to one of its serving packet switches. On reception, a TCP will accept a sequence of packets from its serving packet switch(es). From this sequence of arriving packets (generally from different hosts), the TCP must be able to reconstruct and deliver messages to the proper destination processes.

We assume that every segment is augmented with additional information that
allows transmitting and receiving TCP’s to identify destination and source
processes, respectively. At this point, we must face a major issue. How
should the source TCP format segments destined for the same destination TCP?
We consider two cases.


<mark>Case 1): If we take the position that segment boundaries are immaterial and
that a byte stream can be formed of segments destined for the same TCP, then
we may gain improved transmission efficiency and resource sharing by
arbitrarily parceling the stream into packets</mark>, permitting many segments to
share a single internetwork packet header. However, this position results in
the need to reconstruct exactly, and in order, the stream of text bytes
produced by the source TCP. At the destination, this stream must first be
parsed into segments and these in turn must be used to reconstruct messages
for delivery to the appropriate processes.

<mark>There are fundamental problems associated with this strategy due to the
possible arrival of packets out of order at the destination. The most
critical problem appears to be the amount of interference that processes
sharing the same TCP-TCP byte stream may cause among themselves.</mark> This is
especially so at the receiving end. First, the TCP may be put to some
trouble to parse the stream back into segments and then distribute them to
buffers where messages are reassembled. If it is not readily apparent that
all of a segment has arrived (remember, it may come as several packets), the
receiving TCP may have to suspend parsing temporarily until more packets
have arrived. Second, if a packet is missing, it may not be clear whether
succeeding segments, even if they are identifiable, can be passed on to the
receiving process, unless the TCP has knowledge of some process level
sequencing scheme. Such knowledge would permit the TCP to decide whether a
succeeding segment could be delivered to its waiting process. Finding the
beginning of a segment when there are gaps in the byte stream may also be
hard.

Case 2): Alternatively, we might take the position that the destination TCP should be able to determine, upon its arrival and without additional information, for which process or processes a received packet is intended, and if so, whether it should be delivered then.

<span class="mark">If the TCP is to determine for which process an arriving packet is intended, every packet must contain a *process header* (distinct from the internetwork header) that completely identifies the destination process.</span> For simplicity, we assume that each packet contains text from a single process which is destined for a single process. Thus each packet need contain only one process header. To decide whether the arriving data is deliverable to the destination process, the TCP must be able to determine whether the data is in the proper sequence (we can make provision for the destination process to instruct its TCP to ignore sequencing, but this is considered a special case). With the assumption that each arriving packet contains a process header, the necessary sequencing and destination process identification is immediately available to the destination TCP.

Both Cases 1) and 2) provide for the demultiplexing and delivery of segments
to destination processes, but only Case 2) does so without the introduction
of potential interprocess interference, Furthermore, Case 1) introduces
extra machinery to handle flow control on a host-to-host basis, since there
must also be some provision for process level control, and this machinery is
little used since the probability is small that within a given Host, two
processes will be coincidentally scheduled to send messages to the same
destination Host. For this reason, <span class="mark">we select the method of Case 2) as a part
of the *internetwork transmission protocol*.</span>

## Address Formats

The selection of address formats is a problem between networks because the local network addresses of TCP’s may vary substantially in format and size. A uniform internetwork TCP address space, understood by each gateway and TCP, is essential to routing and delivery of internetwork packets.

Similar troubles are encountered when we deal with process addressing and, more generally, port addressing. <span class="mark">We introduce the notion of *ports* in order to permit a process to distinguish between multiple message streams.</span> The port is simply a designator of one such message stream associated with a process. The means for identifying a port are generally different in different operating systems, and therefore, to obtain uniform addressing, a standard port address format is also required. A port address designates a full duplex message stream.

## TCP Addressing

TCP addressing is intimately bound up in routing issues, since a host or gateway must choose a suitable destination host or gateway for an outgoing internetwork packet. Let us postulate the following address format for the TCP address (Fig. 4). The choice for network identification (8 bits) allows up to 256 distinct networks. This size seems sufficient for the foreseeable future. Similarly, the TCP identifier field permits up to 65 536 distinct TCP’s to be addressed, which seems more than sufficient for any given network.


<figure>
<label for="mn-fig-4" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-4" class="margin-toggle">
<span class="marginnote">
 Fig. 4. TCP address.
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig4.png'>
</figure>


<mark>As each packet passes through a gateway, the gateway observes the destination network ID to determine how to route the packet. If the destination network is connected to the gateway, the lower 16 bits of the TCP address are used to produce a local TCP address in the destination network. If the destination network is not connected to the gateway, the upper 8 bits are used to select a subsequent gateway.</mark> We make no effort to specify how each individual network shall associate the internetwork TCP identifier with its local TCP address. We also do not rule out the possibility that the local network understands the internetwork addressing scheme and thus alleviates the gateway of the routing responsibility.

## Port Addressing

A receiving TCP is faced with the task of demultiplexing the stream of internetwork packets it receives and reconstructing the original messages for each destination process. <mark>Each operating system has its own internal means of identifying processes and ports. We assume that 16 bits are sufficient to serve as internetwork port identifiers. A sending process need not know how the destination port identification will be used.</mark> The destination TCP will be able to parse this number appropriately to find the proper buffer into which it will place arriving packets. We permit a large port number field to support processes which want to distinguish between many different message streams concurrently. In reality, we do not care how the 16 bits are sliced up by the TCP’s involved.

Even though the transmitted port name field is large, it is still a compact external name for the internal representation of the port. The use of short names for port identifiers is often desirable to reduce transmission overhead and possibly reduce packet processing time at the destination TCP. Assigning short names to each port, however, requires an initial negotiation between source and destination to agree on a suitable short name assignment, the subsequent maintenance of conversion tables at both the source and the destination, and a final transaction to release the short name. For dynamic assignment of port names, this negotiation is generally necessary in any case.

## Segment and Packet Formats

As shown in Fig. 5, messages are broken by the TCP into segments whose format is shown in more detail in Fig. 6. The field lengths illustrated are merely suggestive. The first two fields (source port and destination port in the figure) have already been discussed in the preceding section on addressing. The uses of the third and fourth fields (window and acknowledgement in the figure) will be discussed later in the section on retransmission and duplicate detection.


<figure>
<label for="mn-fig-5" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-5" class="margin-toggle">
<span class="marginnote">
Fig. 5. Creation of segments and packets from messages.
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig5.png'>
</figure>


<figure>
<label for="mn-fig-6" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-6" class="margin-toggle">
<span class="marginnote">
 Fig. 6. Segment format (process header and text).
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig6.png'>
</figure>


We recall from Fig. 3 that an internetwork header contains both a sequence number and a byte count, as well as a flag field and a check sum. The uses of these fields are explained in the following section.

## Reassembly and Sequencing

The reconstruction of a message at the receiving TCP clearly requires<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">
In the case of encrypted packets, a preliminary stage of reassembly may be required prior to decryption.
</span>
 that each internetwork packet carry a sequence number which is unique to its particular destination port message stream. The sequence numbers must be monotonic increasing (or decreasing) since they are used to reorder and reassemble arriving packets into a message. If the space of sequence numbers were infinite, we could simply assign the next one to each new packet. Clearly, this space cannot be infinite, and we will consider what problems a finite sequence number space will cause when we discuss retransmission and duplicate detection in the next section. We propose the following scheme for performing the sequencing of packets and hence the reconstruction of messages by the destination TCP.

A pair of ports will exchange one or more messages over a period of time. We could view the sequence of messages produced by one port as if it were embedded in an infinitely long stream of bytes. Each byte of the message has a unique sequence number which we take to be its byte location relative to the beginning of the stream. <mark>When a segment is extracted from the message by the source TCP and formatted for internetwork transmission, the relative location of the first byte of segment text is used as the sequence number for the packet. The byte count field in the internetwork header accounts for all the text in the segment (but does not include the check-sum bytes or the bytes in either internetwork or process header).</mark> We emphasise that the sequence number associated with a given packet is unique only to the pair of ports that are communicating (see Fig. 7). Arriving packets are examined to determine for which port they are intended. The sequence numbers on each arriving packet are then used to determine the relative location of the packet text in the messages under reconstruction. We note that this allows the exact position of the data in the reconstructed message to be determined even when pieces are still missing.

<figure>
<label for="mn-fig-7" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-7" class="margin-toggle">
<span class="marginnote">
 Fig. 7. Assignment of sequence numbers. (process header and text).
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig7.png'>
</figure>


Every segment produced by a source TCP is packaged in a single internetwork packet and a check sum is computed over the text and process header associated with the segment.

The splitting of messages into segments by the TCP and the potential splitting of segments into smaller pieces by gateway creates the necessity for indicating to the destination TCP when the end of a segment (ES) has arrived and when the end of a message (EM) has arrived. The flag field of the internetwork header is used for this purpose (see Fig. 8).

<figure>
<label for="mn-fig-8" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-8" class="margin-toggle">
<span class="marginnote">
 Fig. 8. Internetwork header flag field.
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig8.png'>
</figure>


The ES flag is set by the source TCP each time it prepares a segment for transmission. If it should happen that the message is completely contained in the segment, then the EM flag would also be set. The EM flag is also set on the last segment of a message, if the message could not be contained in one segment. These two flags are used by the destination TCP, respectively, to discover the presence of a check sum for a given segment and to discover that a complete message has arrived.

The ES and EM flags in the internetwork header are known to the gateway and are of special importance when packets must be split apart from propagation through the next local network. We illustrate their use with an example in Fig. 9.

<figure>
<label for="mn-fig-9" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-9" class="margin-toggle">
<span class="marginnote">
 Fig. 9. Message splitting and packet splitting.
</span>
<img src='/assets/images/classic_papers/tcp_ip/fig9.png'>
</figure>


The original message $A$ in Fig. 9 is shown split into two segments $A_1$ and $A_2$ and formatted by the TCP into a pair of internetwork packets. Packets $A_1$ and $A_2$ have their ES bits set, and $A_2$ has its EM bit set as well. When packet $A_1$ passes through the gateway, it is split into two pieces: packet $A_{11}$ for which neither EM nor ES bits are set, and packet $A_{12}$ whose ES bit is set. Similarly, packet $A_2$ is split such that the first piece, packet $A_{21}$, has neither bit set, but packet $A_{22}$ has both bits set. The sequence number field (SEQ) and the byte count field (CT) of each packet is modified by the gateway to properly identify the text bytes of each packet. The gateway need only examine the internetwork header to do fragmentation.

The destination TCP, upon reassembling segment $A_1$, will detect the ES flag and will verify the check sum it knows is contained in packet $A_{12}$. Upon receipt of packet $A_{22}$, assuming all other packets have arrived, the destination TCP detects that it has reassembled a complete message and can now advise the destination process of its receipt.

## Retransmission and Duplicate Detection

No transmission can be 100 percent reliable. We propose a timeout and positive acknowledgement mechanism which will allow TCP’s to recover from packet losses from one host to another. A TCP transmits packets and waits for replies (acknowledgements) that are carried in the reverse packet stream. If no acknowledgement for a particular packet is received, the TCP will retransmit. It is our expectation that the host level retransmission mechanism, which is described in the following paragraphs, will not be called upon very often in practice. Evidence already exists2 that individual networks can be effectively constructed without this feature. However, the inclusion of a host retransmission capability makes it possible to recover from occasional network problems and allows a wide range of host protocol strategies to be incorporated. We envision it will occasionally be invoked to allow host accommodation to infrequent overdemands for limited buffer resources, and otherwise not used much.

Any retransmission policy requires some means by which the receiver can detect duplicate arrivals. Even if an infinite number of distinct packet sequence numbers were available, the receiver would still have the problem of knowing how long to remember previously received packets in order to detect duplicates. Matters are complicated by the fact that only a finite number of distinct sequence numbers are in fact available, and if they are reused, the receiver must be able to distinguish between new transmissions and retransmissions.

A *window* strategy, similar to that used by the French CYCLADES system (voie virtuelle transmission mode [8]) and the ARPANET very distant HOST connection [18]), is proposed here (see Fig. 10).

Suppose that the sequence number field in the internetwork header permits sequence numbers to range from 0 to n - 1. We assume that the sender will not transmit more than w bytes without receiving an acknowledgment. The w bytes serve as the window (see Fig. 11). Clearly, w must be less than n. The rules for sender and receiver are as follows.

