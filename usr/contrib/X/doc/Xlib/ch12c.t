.NH 2
Mouse, Button and Server Grabbing
.PP
Some programs need to  gain exclusive access to the mouse,
a single mouse button under some circumstances,
or the entire X server for the duration of some operation.
.FD
.IN "Definitions" "XGrabMouse"
.IN "XGrabMouse"
.IN "Mouse" "Grabbing"
Status XGrabMouse (w, cursor, mask)
	Window w;
	Cursor cursor;
	int mask;	/* event mask */
.FN
After \fIXGrabMouse\fP is called, all future mouse events will only go to
.IN "XSelectInput"
windows for which the client has previously called \fIXSelectInput\fP.  The
\fIButtonPressed\fP, \fIButtonReleased\fP, \fIEnterWindow\fP,
\fILeaveWindow\fP, \fIMouseMoved\fP,
\fILeftDownMotion\fP, \fIMiddleDownMotion\fP,
and \fIRightDownMotion\fP bits of the \fImask\fP
parameter temporarily override the corresponding bits in any mask
previously passed to \fIXSelectInput\fP; other bits in \fImask\fP are ignored.
.PP
.IN "XSelectInput"
If one of the above-mentioned events occurs, and the client has not
called \fIXSelectInput\fP on the window where the event would normally be
sent, then the event will be sent to the window \fIw\fP, provided that the
event is specified in 
\fImask\fP and is not \fIEnterWindow\fP or \fILeaveWindow\fP.
.PP
.IN "Grabbing Error"
An error will occur if a different client has already grabbed the mouse
and has not ungrabbed it.  It is not an error for the same client to
grab the mouse more than once without ungrabbing it in between.  A
mouse-grabbing client may want to do this in order to change the cursor
or event mask without ungrabbing the mouse.
.PP
Grabbing the mouse overrides any \fIXGrabButton\fP calls previously issued by
this or any other client, until the mouse is ungrabbed.
.PP
Note that this procedure returns a status and is therefore synchronous,
even though no other values are returned.
It returns 0 if the mouse could not be grabbed, non-zero if the
mouse was successfully grabbed.
.FD
.IN "Definitions" "XGrabMouse"
.IN "XGrabMouse"
XUngrabMouse ()
.FN
\fIXUngrabMouse\fP releases hold of the mouse if it was grabbed by 
\fIXGrabMouse\fP.
.FD
.IN "Definitions" "XGrabButton"
.IN "XGrabButton"
Status XGrabButton (w, cursor, buttonMask, eventMask)
	Window w;
	Cursor cursor;
	int buttonMask;
	int eventMask;
.FN
After \fIXGrabButton\fP has been called, the mouse will automatically be
grabbed whenever a particular mouse button is pressed while certain keys are
down.
.IN "Button Mask"
The combination is specified in \fIbuttonMask\fP;  this mask must
have exactly one of the \fILeftMask\fP, \fIMiddleMask\fP, and \fIRightMask\fP
bits set,
and may have some combination of the \fIControlMask\fP, \fIMetaMask\fP,
\fIShiftLockMask\fP and
\fIShiftMask\fP bits set as well.
.PP
If the specified button is pressed while exactly the specified keys are down,
this and all future mouse events are grabbed until all buttons are
.IN "XGrabMouse"
released, with events sent to windows as described under \fIXGrabMouse\fP
above.
.IN "Event Mask"
The \fIeventMask\fP determines what mouse events are reported while the
mouse is grabbed.
.PP
An error will occur if another client has already grabbed the same
button/key combination and has not ungrabbed it.
.PP
Note that this procedure returns a status and is therefore synchronous,
It returns 0 if the button could not be grabbed, non-zero if the
the button was grabbed successfully.
.FD
.IN "Definitions" "XUngrabButton"
.IN "XGrabButton"
XUngrabButton (mask)
	int mask;	/* button mask;  see XGrabButton */
.FN
\fIXUngrabButton\fP notifies the server that the client is no longer
interested in grabbing the mouse when the specified button/key
combination occurs.
See \fIXGrabButton\fP for the meaning of the button
mask bits.
This grab is overridden by a grab mouse request.
.FD
.IN "XGrabServer"
.IN "Definitions" "XGrabServer"
.IN "Grabbing" "Server"
XGrabServer()

XUngrabServer()
.FN
These requests can be used to control processing of output on other
connections by the window system server.
No processing of requests or close downs on all other connections
will occur while the server is grabbed.
.PP
.IN "Menus"
.IN "Window" "Managers"
This may be useful for menus or window manager programs who may want to
preserve bits on the screen while temporarily suspending processing
on other connections.
