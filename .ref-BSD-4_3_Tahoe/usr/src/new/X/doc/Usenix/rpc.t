.SH
Stub Generators and the X Protocol
.PP
The X protocol is not a remote procedure call protocol as
defined in the literature [4,5],
as client calls are not given the same guarantee of completion and
error handling that an RPC protocol provides.
The X protocol transports fairly large amounts of data and
executes many more requests than typically seen in true RPC systems.
Given this generation of display hardware and processors,
X may handle greater than 1000 requests/second from client applications to
a fast display.
.PP
X clients only block when they need information from the server.
Performance would be unacceptable if X were a synchronous RPC protocol,
both because of round trip times and because of system call overhead.
This is the most significant difference between X and its predecessor
W, written by Paul Asente of Stanford University.
On the other hand,
a procedural interface to the window system is essential for easy use.
We spent much time crafting the procedure stubs for the several
library interfaces built during X development.
.PP
The original implementation of the client library would always
write each request at the time the request was made.
This implies a write system call per X request.
There was implicit buffering from the start in the connection to
the server due to the stream connection.
Over a year ago, we received new firmware for the Vs100, and
were no longer able to keep up with the display.
We changed the client library to buffer the requests in a manner
similar to the standard I/O library; this improved performance dramatically,
as the client library performs many fewer write system calls.
.PP
Many current RPC [6] argument martialing
mechanisms perform at least one procedure
call per procedure argument to martial that argument.
This is almost certainly too expensive to use for this application.
Even if martialing the argument took no time in the procedure,
the call overhead would account for ~10% of the CPU.
Stub generators need to be able to emit direct assignment code for
simple argument types.
Complex	argument types can probably afford a procedure call,
but these are not common in the current X design.
.PP
Proper stub generation tools would have saved several months over the
course of the project,
had they been available at the proper time.
Arguments could be made that the hand-crafted stubs in the X client library
are more efficient than machine generated stubs would have been.
On the other hand, to keep the protocol simple, X often
sends requests with unused data, for which it pays with higher communications
cost.
It would be instructive to reimplement X using such a stub generator and
see the relative performance between it and the current mechanism.
.PP
Machine dependencies in such transport mechanisms need further work.
The protocol design deserves careful study.
Issues such as byte swapping cannot be ignored.
With strictly blocking RPC, the overhead per request is already so
high that network byte order is probably not too expensive,
given the current implementation of RPC systems on 
.UX .
With the higher performance of the X protocol,
this issue becomes significant.
It is desirable that two machines of the same architecture
pay no penalty in performance in the transport protocols.
Our solution was to define two ports that the X server listens at,
one for VAX byte order connections, and one for 68000 byte order connections.
At a late stage of X development,
after X client code had already been ported to a Sun workstation
and would interoperate with a VAX display,
another different machine architecture showed that the protocol was
not as conservatively designed as we would like.
Care should be taken in protocol design that all data
be aligned naturally (words on word boundaries, longwords on
longword boundaries, and so on) to ensure portability of code
implementing them.
.PP
X would not be feasible if round trip process to process times over TCP
were too long.
On a MicroVAX\(dg II running Ultrix\(dd,
or on a VAX 11/780 running 4.2, these times
have been measured between 20 and 25 milliseconds using TCP.
.FS
\(dg VAX is a trademark of Digital Equipment Corporation.
.sp
\(dd Ultrix is a trademark of Digital Equipment Corporation.
.FE
As this time degrades, interactive "feel" becomes worse,
as we have chosen to put as much as possible in client code.
Birrell and Nelson report
much lower times using carefully crafted and
tuned RPC protocols on faster hardware; even extrapolating
for differences in hardware,
.UX
may be several times slower than it could be.
Given a much faster kernel message interface, one should be able to
improve on the current times substantially.
The X protocol requires reliable in order delivery of messages.
.PP
The argument against using such  specific message mechanisms are:
1) the buffering provided by the stream layer is used to good advantage
at the server and client ends of the transmissions.
2) Lless interoperability.
X has been run over both
TCP and DECNET, and would be simple to build a forwarder between
the domains if needed.
This reduces the number of system calls required to get the data
from the kernel at either end, particularly when loaded.
.PP
These times have been improved somewhat by optimizing
the local TCP connection, and could be further improved
by using
.UX
domain connections in the local case.
.PP
In general
.UX
needs a much cheaper message passing transport mechanism
than can currently
be built on top of existing 4.2BSD facilities.
Stub generators need serious work both for RPC systems
and other message systems
particularly in light of some of the issues discussed above.
We would make a plea that there be further serious study of
non-blocking protocols[7].
There should be some way to read multiple packets from the kernel
in a single system call for efficient implementation of
RPC and other protocols.
