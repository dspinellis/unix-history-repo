.NH
Introduction
.XS ii
Table of Contents
.XE
.XS
Introduction
.XE
.PP
The X window system supports overlapping (sub)windows.
All the windows in an X server are arranged in a strict hierarchy.  At
the top of the hierarchy is the `root' window,
which covers the entire display screen.
The root window is partially or completely covered by child windows.
All windows have parents except for the root window.
There is usually at least one window per application program.
.IN "Definitions" "Child Window"
.IN "Definitions" "Parent Window"
Child windows  may in turn have
their own children.
In this way, an application program can create a tree of arbitrary depth.
.PP
A child window may be larger than its parent--that is, part or all of
the child window may extend beyond the boundaries of the parent.
However, all output to a window is clipped by the boundaries of its
parent window.
.PP
.IN "Definitions" "Stacking order"
If several children of a window have overlapping locations, then one of
the children is considered to be `on top of' or `raised over' the
others, obscuring them.
Output to areas covered by other windows will be suppressed by the window
system.
If a window  is obscured
by a second window, 
then the window will obscure only those ancestors of the second window which
are also ancestors of the window.
Normally, child windows obscure their parents
as well (but there are exceptions--see the discussions of
.IN "XOpenTransparency"
.IN "XClipMode"
\fIXOpenTransparency\fP and \fIXClipMode\fP for details).
.PP
.IN "Definitions" "Events"
.IN "Events"
Input from X takes the form of `events'.
Events may either be side effects of a command (restacking windows,
for example, generates exposure events),
or completely asynchronous (for example, the keyboard).
A client program asks to be informed of such events;
X never sends events a program did not ask for.
.PP
X does not take responsibility for the contents of windows; when (part or
all of) a window is hidden and then brought back onto the screen, its
contents may be lost lost and the client program is notified (by an exposure
event) that (part or all
of) the window needs to be repainted.
Programs should be prepared to regenerate their windows on demand.
.PP
.IN "Definitions" "Resource ID"
Most of the subroutines in Xlib just add requests to an output buffer;
these requests later execute asynchronously on the X server
(often referred to as ``display server'' below).
Subroutines that return values do not return (``block'')
until an explicit reply is
received or an error occurs.
This may be queried information or a ``resource id'' discussed below.
If such a call results in an error, the error will
generally not be reported (by a call to an optional error handler)
until some later, blocking call is made.
.PP
.IN "XSync"
If a
client does not want a request to execute asynchronously, he can follow
it with a call to \fIXSync\fP, which will block until all previously buffered
asynchronous events have been sent and acted on.
.PP
As an important side effect, 
.IN "XPending"
.IN "XNextEvent"
.IN "XWindowEvent"
.IN "XFlush"
.IN "XSync"
the output buffer is always flushed by a call to any subroutine
which returns a value or by calls to \fIXPending\fP,
\fIXNextEvent\fP, \fIXWindowEvent\fP,
\fIXFlush\fP or \fIXSync\fP.
.PP
.IN "Resource ID" "Window"
.IN "Resource ID" "Font"
.IN "Resource ID" "Pixmap"
.IN "Resource ID" "Bitmap"
.IN "Resource ID" "Cursor"
Many subroutines will return an integer resource id.
These can be of type ``Window'', ``Font'', ``Pixmap'', ``Bitmap'',
and ``Cursor'',
.IN "File" "<X/X.h>"
as defined in \fI<X/X.h>\fP.
Some subroutines return ``Status'', an integer error indication.
If the subroutine fails, it will return 0.
.PP
.IN "Definitions" "Status"
Since C does not provide multiple return values, many subroutines return
their results by writing into client-passed storage.  Any pointer that is
used to return a value is designated as ``/* RETURN */'' in the procedure
declarations below.
All other pointers passed to these subroutines are
used for reading only.
If such a procedure returns a status of 0,
then it has NOT updated the return parameters.
.IN "Error Handling"
By default, errors are handled by a standard library subroutine,
or you can provide your own.
Subroutines that return pointers to strings will return NULL pointers if
the string does not exist.
.PP
.PP
Input events (e.g. key pressed, mouse moved) arrive asynchronously from
the server and are queued until they are requested by a call to
\fIXNextEvent\fP or \fIXWindowEvent\fP.  In addition, some of the library
.IN "XChangeWindow"
.IN "XRaiseWindow"
subroutines (e.g. \fIXChangeWindow\fP and \fIXRaiseWindow\fP)
generate ``exposure''
events--requests to repaint sections of a window that do not have valid
contents.  These events also arrive asynchronously, but the client may
.IN "XSync"
wish to explicitly wait for them by calling \fIXSync\fP after calling a
subroutine which may generate exposure events.
