.NH
Input Event Handling
.XS
Input Event Handling
.XE
.PP
.IN "Input Control"
.IN "Output" "Control"
Input from the window system is asynchronous, but the window
system never gives you information you did not ask for.
.IN "Definitions" "Event"
This input comes in the form of `events'.
There are keyboard, mouse button, window crossing, mouse motion,
exposure, unmap and focus change events.
.IN "File" "<X/Xlib.h>"
There are structures defined for these events in \fI<X/Xlib.h>\fP.
These may arrive at any time in the input stream even between the
time you send a command and get a reply.
Accordingly, the library queues any events received while waiting for
a reply for later use.
.PP
Only the type and window fields are defined for all types of events.
.IN "Definitions" "XEvent"
There is a generic structure declared called \fIXEvent\fP defined in
.IN "File" "<X/Xlib.h>"
\fI<X/Xlib.h>\fP.
Once the type has been determined, future references to the event structure
should be made using the structures declared in the library.
.FD
.IN "Definitions" "XCompressEvents"
.IN "XCompressEvents"
.IN "Definitions" "XExpandEvents"
.IN "XExpandEvents"
XCompressEvents()
.sp
XExpandEvents()
.FN
.PP
.IN "Event Compression"
If the X server sends multiple \fIMouseMoved\fP events without any other
intervening events or replies, the X library by default suppresses all but
the last such event.
.PP
If you want to see all events, you can call \fIXExpandEvents\fP
and the library
will supply all events without compression.
.NH 2
Event Handling
.PP
Before the window system will send an event, you must
request it.
You can request events in particular circumstances,
for example,
mouse motion when a mouse button is held down.
In this example,
mouse motion events are generated;
X does not distinguish between different types of mouse events,
even though you can request them under different circumstances.
.IN "Event" "Types"
.FD
.IN "Definitions" "XSelectInput"
.IN "XSelectInput"
XSelectInput (w, mask)
	Window w;
	int mask;	/* event mask */
.FN	
\fIXSelectInput\fP defines which input events the window is interested in.  If
a window is not interested in an event, it usually will propagate up to
the closest ancestor that is interested.
.IN "Event" "Propagation"
The bits of the mask are
.IN "File" "<X/X.h>"
defined in \fI<X/X.h>\fP:
.IN "Event Types" "EnterWindow"
.IN "Event Types" "LeaveWindow"
.IN "Event Types" "MouseMoved"
.IN "Event Types" "ExposeWindow"
.IN "Event Types" "ExposeRegion"
.IN "Event Types" "ExposeCopy"
.IN "Event Types" "ExposeRegion"
.IN "Event Types" "MouseMoved"
.IN "Event Types" "FocusChange"
.IN "Event Types" "UnmapWindow"
.IN "Event Types" "Keyboard"
.IN "Event Types" "Button"
.IN "Key Event"
.IN "Button Event"
.IN "EnterWindow Event"
.IN "LeaveWindow Event"
.IN "MouseMoved Event"
.IN "ExposeWindow Event"
.IN "ExposeRegion Event"
.IN "ExposeCopy Event"
.IN "MouseMoved Event"
.IN "Mouse" "Event"
.IN "FocusChange Event"
.in "UnmapWindow Event"
.LP
.TS
center,box;
c c c
___
l c l.
Request	Hex Code	Circumstances
KeyPressed	0x0001	keyboard key pressed
KeyReleased	0x0002	keyboard key released
ButtonPressed	0x0004	mouse button pressed
ButtonReleased	0x0008	mouse button released
EnterWindow	0x0010	mouse entering window
LeaveWindow	0x0020	mouse leaving window
MouseMoved	0x0040	mouse moves within window
ExposeWindow	0x0080	full window changed and/or exposed
ExposeRegion	0x0100	region of window exposed
ExposeCopy	0x0200	region exposed by XCopyArea
RightDownMotion	0x0400	mouse moves with right button down
MiddleDownMotion	0x0800	mouse moves with middle button down
LeftDownMotion	0x1000	mouse moves with left button down
UnmapWindow	0x2000	window is unmapped
FocusChange	0x4000	keyboard focus changed
.TE
Selecting \fIExposeRegion\fP also selects \fIExposeWindow\fP.
.PP
.IN "XSelectInput"
A call on \fIXSelectInput\fP overrides any previous call on \fIXSelectInput\fP
for the
same window, whether from the same client or a different one.
It is not
possible for two clients to each select events simultaneously
from the same window.
Initially,
no events will be generated on a window.
.PP
If a window has both \fIButtonPressed\fP and \fIButtonReleased\fP selected,
then
a \fIButtonPressed\fP event in that window will automatically `grab' the
mouse until all buttons are released, with events sent to windows as
.IN "XGrabMouse"
described for \fIXGrabMouse\fP.
This ensures that a window will see the release event
corresponding to the pressed event,
even though the mouse may have exited the window in the meantime.
.PP
If \fIMouseMoved\fP is selected, events will be sent independent of the state
of the mouse buttons.  If instead, one or more of \fIRightDownMotion\fP,
\fIMiddleDownMotion\fP, \fILeftDownMotion\fP is selected,
\fIMouseMoved\fP events will be
generated only when one or more of the specified buttons is depressed.
(There are NO events of type \fIRightDownMotion\fP, \fIMiddleDownMotion\fP, or
\fILeftDownMotion\fP; 
these are ways to request \fIMouseMoved\fP events only when particular buttons
are held down).
.FD
.IN "Definitions" "XFlush"
.IN "XFlush"
XFlush ()
.FN
\fIXFlush\fP sends (`flushes') all output requests that have been buffered but
not yet sent.
Flushing is done automatically the next time input is
.IN "XPending"
.IN "XNextEvent"
.IN "XWindowEvent"
read (with \fIXPending\fP, \fIXNextEvent\fP, or \fIXWindowEvent\fP),
so most clients
should not need to use this subroutine.
.FD
.IN "Definitions" "XSync"
.IN "XSync"
XSync (discard)
	int discard;	/* 0 or 1 */
.FN
\fIXSync\fP flushes the output buffer, then waits until all events and errors
resulting from previous calls have been received and processed by
the X server.
Events are placed on
the input queue.
.IN "XError"
The client's \fIXError\fP subroutine is called once for EACH
error received. 
.IN "XFlush"
Even fewer clients need to use this subroutine than \fIXFlush\fP.
.PP
If discard is true, \fIXSync\fP then discards all events on the input queue
(including those events that were on the queue before \fIXSync\fP was called).
.FD
.IN "Definitions" "XPending"
.IN "XPending"
int XPending ()
.FN	
\fIXPending\fP flushes the output buffer, then returns the number of input
events that have been received from the server, but not yet removed from
.IN "XNextEvent"
.IN "XWindowEvent"
the queue. (Events are removed from the queue by calling \fIXNextEvent\fP or
\fIXWindowEvent\fP, described below.)
.PP
.IN "XPending"
.IN "Unix System Call" "select"
You should always call \fIXPending\fP before doing a select(2) on the
file descriptor contained in the display structure.
The input you are trying to wait for may have already arrived and be
sitting in Xlib's queue.
.IN "XFlush"
Another strategy might be to call \fIXFlush\fP after
finding out if there are any unprocessed events on the queue by
.IN "Macro" "QLength"
using the \fIQLength()\fP macro before calling \fIselect(2)\fP.
.FD
.IN "Definitions" "XNextEvent"
XNextEvent (rep)
	XEvent *rep;	/* RETURN */
.FN
\fIXNextEvent\fP flushes the output buffer, then removes an input event from
.IN "XEvent"
the head of the queue and copies it into an \fIXEvent\fP
supplied by the caller.
If the queue is empty, \fIXNextEvent\fP blocks until an event is received.
.FD
.IN "Definitions" "XPutBackEvent"
.IN "XPutBackEvent"
XPutBackEvent(event)
	XEvent *event;
.FN
\fIXPutBackEvent\fP pushes an event back onto the head of the current
display's input queue.
This can be useful if you have read an event and then decide that you'd
rather deal with it later.
.FD
.IN "Definitions" "XPeekEvent"
.IN "XPeekEvent"
XPeekEvent (rep)
	XEvent *rep;	/* RETURN */
.FN
\fIXPeekEvent\fP flushes the output buffer, then peeks at an input event from
the head of the queue and copies it into an 
\fIXEvent\fP supplied by the caller,
without removing it from the input queue.
If the queue is empty, \fIXPeekEvent\fP blocks until an event is received.
.IN "Macro" "QLength()"
You can use the \fIQLength()\fP macro to determine if there are any
events to peek at.
.PP
Note:  We may add more specialized peek functions later as the need arises.
.FD
.IN "Definitions" "XWindowEvent"
.IN "XWindowEvent"
XWindowEvent (w, mask, rep)
	Window w;
	int mask;	/* event mask */
	XEvent *rep;	/* RETURN */
.FN
This subroutine is used to look for specific events from specific windows.
\fIXWindowEvent\fP flushes the output buffer, then removes the next event in
the queue which matches both the passed window and the passed \fImask\fP.
The
.IN "XEvent"
event is copied into an \fIXEvent\fP supplied by the caller.
Events earlier in
the queue are not discarded.  If no such event has been queued,
\fIXWindowEvent\fP blocks until one is received.
.FD
.IN "Definitions" "XMaskEvent"
.IN "XMaskEvent"
XMaskEvent (mask, rep)
	int mask;	/* event mask */
	XEvent *rep;	/* RETURN */
.FN
This subroutine is used to look for specific events.
\fIXMaskEvent\fP flushes the output buffer, then removes the next event in
the queue which matches the passed mask.  The
event is copied into an \fIXEvent\fP supplied by the caller.  Events earlier in
the queue are not discarded.  If no such event has been queued,
\fIXMaskEvent\fP blocks until one is received.
.FD
.IN "Definitions" "XCheckWindowEvent"
.IN "XCheckWindowEvent"
int XCheckWindowEvent (w, mask, rep)
	Window w;
	int mask;	/* event mask */
	XEvent *rep;	/* RETURN */
.FN
This subroutine is used to look for specific events from specific windows.
XCheckWindowEvent flushes the output buffer, then removes the next event in
the queue which matches both the passed window and the passed mask.  The
.IN "XEvent"
event is copied into an XEvent supplied by the caller and XCheckWindowEvent
returns 1.  Events earlier in the queue are not discarded.  If no such
event has been queued, XCheckWindowEvent immediately returns 0.
.FD
.IN "Definitions" "XCheckMaskEvent"
.IN "XCheckMaskEvent"
int XCheckMaskEvent (mask, rep)
	int mask;	/* event mask */
	XEvent *rep;	/* RETURN */
.FN
This subroutine is used to look for specific events.
XCheckMaskEvent flushes the output buffer, then removes the next event in
the queue which matches the passed mask.  The
event is copied into an XEvent supplied by the caller and
XCheckMaskEvent returns 1.  Events earlier in
the queue are not discarded.  If no such event has been queued,
XCheckMaskEvent immediately returns 0.

