.LP
.SH
X Window System Design
.PP
While this paper is not specifically about the X window system,
X will be used as an example for much of the discussion.
X is best described using a client/server model.
X consists of a collection of client programs which
communicate with the window system server.
They are implemented entirely in user code.
All communications with the window system occur over a stream
connection to the window system.
X is completely network transparent; i.e. a client program
can be running on any machine in a network, and the clients and
the server need not be executing on the same machine architecture.
The block diagram shown in Figure 1 describes the structure of the system.
.PP
X supports overlapping possibly transparent windows and
subwindows to arbitrary depth.
Client programs can create, destroy, move, resize, and restack windows.
X will inform clients on request of key presses,
key releases, button presses, button releases,
mouse window entry and exit, mouse motion,
a number of types of window exposure,
unmap (removal of a window from the screen),
and input focus change.
Cut and paste buffers are provided in the server as untyped bytes.
Graphic primitives provided include dashed and dotted
multi-width lines, and splines.
There is a full complement of raster operations available.
The implementation supports color,
though the current protocol limits the depth of a display to
16 bits/pixel.
.PP
The X window system consists of a collection of 
.UX
programs
providing various services on a bitmap display.
There is only a minimal device driver to field interrupts from
mouse, keyboard, and potentially a display engine.
The X server accepts connections from client applications programs.
Window, text and graphics commands are all multiplexed over (usually)
a single connection per client.
Multiple clients may connect to the server.
.PP
The X protocol is the only way to communicate to the window system.
The X server enforces clipping, does resource allocation,
multiplexes output from multiple clients, performs hit detection
for mouse and keyboard events, and performs graphics
primitives in windows.
The protocol is entirely based on a stream.
The current implementation uses TCP as its stream
transport layer; though it
has been run experimentally using DECNET stream connections.
A client program may run on any machine in a network.
On a local net, performance is the same or better when run remotely
as when run locally given two identical  unloaded processors.
.PP
The X server is a single threaded server program.
Requests from clients are processed in a round robin fashion
to provide apparently simultaneous output.
This has proven to be sufficient,
and vastly simplified the design and implementation.
Single threading provides all locking and synchronization without any
complex structure.
The X server must therefore be very careful never to block waiting
on a client,
and exploits the observation that each individual graphics operation
is very
fast on a human time scale (though it may be slow on a systems time scale).
The 4.2BSD facilities that make this easy to implement
include select(2), non-blocking I/O, and the network mechanism
(IPC to unrelated processes).
.PP
The current X server implementation does NOT maintain the contents of windows.
Refresh of a damaged window is the responsibility of the client.
The server will inform a client if the contents of a window has been
damaged.
This was motivated by a number of observations:
1) clients often have their own backing store, and this must be maintained
by most programs when resized anyway;
if the window system provides backing store, it is often duplicating
existing facilities.
2) keep the window system simple and FAST.
3) the amount of data that would have to be stored for bitmap backing
store on color displays is very large.
Naive 
.UX
applications are run under a terminal emulator which provides
the refresh function for them.
.PP
X delegates as much to a client as possible.
It provides low level ``hooks'' for window management.
No less than three window manager programs (a separate client program in
the X design from the window system) have been written to date,
and provide quite different user interfaces.
Menus are left to client code to implement, using the
flexible primitives X provides.
There have been four different styles of menus implemented to
date, including a quite sophisticated ``deck of cards'' menu package.
.KS
.sp 20
.ce 1
Figure 1: Block Diagram Structure of X
.KE
.PP
X runs as of this writing on four quite different types of hardware,
from very intelligent to very simple.
An example of a very intelligent (and reasonably well designed) piece of
hardware from the programmers point of view is the DEC Vs100,
though it suffers due to the nature of its interface to a VAX,
which adds overhead and latencies to each operation.
A QVSS display on a MicroVAX (VS1 and VS2)
is at the opposite end of the spectrum,
being a simple bitmap with no hardware assist.
Other ports are in progress.
