.SH
File Descriptors
.PP
Andrew, the window system developed at the ITC at CMU [1]
uses one connection (file descriptor)
per window.
While simple from a conceptual level,
also allowing naive applications to do output to a window,
it ties an expensive resource (file descriptor and connection) to
what should be a cheap resource (a window or sub-window).
It requires more kernel resources
in the form of socket buffers for each file descriptor.
In addition, the handshaking required for opening a connection is
expensive in terms of time and will become more so once connections
become authenticated.
The attraction of having a simple stream interface to a window
can be had by other means [2].
In addition, if a window is tied to a file descriptor, the
application loses the
implicit time sequencing provided by the event stream coming over a single
connection.
.PP
One X application uses more than 120 subwindows,
all multiplexed over a single connection.
One could postulate a single connection per client for input,
and a single connection
per window for output; with the limited number of file descriptors in 4.2BSD
and other current versions of 
.UX , 
this was eliminated as a possibility.
Sixteen client programs seems to be sufficient for most people,
(this is limited by 20 file descriptors on standard 
.UX ,
with four file descriptors needed
for X; one for the display, keyboard and mouse, two to listen for
incoming connections, and one for reading fonts).
Sixteen is not a tolerable limit on the total number of (sub)windows,
however.
.PP
4.3BSD lifts this limit to sixty four.
(It can be configured to any size.)
While this increase in the number of file descriptors is beneficial,
it is still too expensive a resource to use one per window.

