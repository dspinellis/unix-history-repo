.NH 2
When An Event?
.PP
The following discussion is meant to explain precisely when
events are generated and precisely what information is contained in
an event.
The structures below are all contained in \fI<X/Xlib.h>\fP;
the declarations below are for illustration only, 
to explain the information in the structures.
.IN "File" "<X/Xlib.h>"
The \fIXlib.h\fP file is the final authority.
.PP
Events are often said to be generated on a window.
What this really means is that the window system
figures out which client to send the event to
by looking for the smallest enclosing window for which
a client program has selected input for that event type.
If no program has selected input for that event type somewhere
in the heirarchy, it will eventually be thrown away.
Some client programs need to override this behavior, and
may `grab' a button for some period of time under some circumstances.
.PP
.IN "Automatic Grabbing"
.IN "Grabbing" "Temporarily Button"
If both \fIButtonPressed\fP
and \fIButtonReleased\fP are selected by a client on a window,
then a \fIButtonPressed\fP event in that window will automatically `grab' the
mouse until all buttons are released.
.PP
If the mouse has been grabbed, either from a button press as above, or
.IN "XGrabMouse"
.IN "XGrabButton"
.IN "Grabbing" "Mouse"
.IN "Grabbing" "Button"
by a call to \fIXGrabMouse\fP or \fIXGrabButton\fP,
then \fIButtonPressed\fP, \fIButtonReleased\fP,
\fIMouseMoved\fP, \fIEnterWindow\fP,
and \fILeaveWindow\fP events will only go to windows
fo
.IN "XSelectInput"
which the grabbing client has called \fIXSelectInput\fP.
If the client
has not called \fIXSelectInput\fP on the window where the event would
normally be sent, then the window where the original \fIButtonPressed\fP
occurred will receive the event,
provided the event has been selected in that window
and the event is not \fIEnterWindow\fP or \fILeaveWindow\fP.
.PP
When an event is received,
programs first have to determine what kind of event was generated.
A generic event structure,
\fIXEvent\fP has been defined in the library which contains
information in common to all events.
A user will normally switch on the event type to handle different
events.
.nf
.DS
.TA .5i 2.5i
.ta .5i 2.5i
/* Definition of a generic event.  It must be cast to a specific event
 * type before one can read event-specific data */

typedef struct _XEvent {
    	unsigned long type;	/* of event (KeyPressed, ExposeWindow, etc.) */
	Window window;	/* which selected this event */
	long pad_l1, pad_l2;	/* event-specific data */
	Window subwindow;	/* child window (if any) event actually happened in */
	long pad_l4;	/* event-specific data */
} XEvent;
.DE
.fi
Only the type, window, and subwindow information is shared in common
among different event types.
.IN "Data Structures" "XEvent"
.PP
\fIKeyPressed\fP, \fIKeyReleased\fP, \fIButtonPressed\fP, 
\fIButtonReleased\fP, \fIEnterWindow\fP,
\fILeaveWindow\fP, and \fIMouseMoved\fP events contain the same information
and have the following structure:
.nf
.DS
.TA .5i 2.5i
.ta .5i 2.5i
struct _XKeyOrButtonEvent {
	unsigned long type;	/* of event (KeyPressed, ButtonReleased, etc.) */
	Window window;	/* which selected this event */
	unsigned short time;	/* in 10 millisecond ticks */
	short detail;	/* event-dependent data (key state, etc.) */
	short x;	/* mouse x coordinate within event window */
	short y;	/* mouse y coordinate within event window */
	Window subwindow;	/* child window (if any) mouse was in */
	Locator location;	/* absolute coordinates of mouse */
};
.IN "Data Structures" "XKeyEvent"
.IN "Data Structures" "XKeyOrButtonEvent"
.IN "Data Structures" "XKeyPressedEvent"
.IN "Data Structures" "XKeyReleasedEvent"
.IN "Data Structures" "XButtonPressedEvent"
.IN "Data Structures" "XButtonReleasedEvent"
.IN "Data Structures" "XButtonButtonEvent"
typedef struct _XKeyOrButtonEvent XKeyEvent;
typedef struct _XKeyOrButtonEvent XKeyOrButtonEvent;
typedef struct _XKeyOrButtonEvent XKeyPressedEvent;
typedef struct _XKeyOrButtonEvent XKeyReleasedEvent;

typedef struct _XKeyOrButtonEvent XButtonEvent;
typedef struct _XKeyOrButtonEvent XButtonPressedEvent;
typedef struct _XKeyOrButtonEvent XButtonReleasedEvent;
.DE
.DS
.TA .5i 2.5i
.ta .5i 2.5i
struct _XMouseOrCrossingEvent {
	unsigned long type;	/* EnterWindow, LeaveWindow, or MouseMoved */
	Window window;	/* which selected this event */
	short pad_s2; 	      
	short detail;	/* event-dependent data (key state, etc. ) */
	short x;	/* mouse x coordinate within event window */
	short y;	/* mouse y coordinate within event window */
	Window subwindow;	/* child window (if any) mouse was in */
	Locator location;	/* absolute coordinates of mouse */
};
.IN "Data Structures" "XMouseOrCrossingEvent"
.IN "Data Structures" "XEnterWindowEvent"
.IN "Data Structures" "XLeaveWindowEvent"
.IN "Data Structures" "XMouseMovedEvent"

typedef struct _XMouseOrCrossingEvent XMouseOrCrossingEvent;
typedef struct _XMouseOrCrossingEvent XEnterWindowEvent;
typedef struct _XMouseOrCrossingEvent XLeaveWindowEvent;
typedef struct _XMouseOrCrossingEvent XMouseMovedEvent;
.fi
.DE
.PP
The coordinates of the mouse relative to the event window are reported, even
if the mouse is not in the window (because of mouse grabbing or keyboard
focusing).  
If the mouse is also in a decendent of the event window, the sub window
is set to that decendent, otherwise the sub window is 0.  The locator defines
the mouse coordinates in absolute terms, and can be used as an argument
.IN "XInterpretLocator"
to \fIXInterpretLocator\fP to interpret
the coordinates with respect to new window
configurations.  
The representation of the Locator is <x,,y> in absolute screen
coordinates.
.PP
The time value is present only for \fIKeyPressed\fP, \fIKeyReleased\fP,
\fIButtonPressed\fP, and \fIButtonReleased\fP events.
Note that there are only 16 bits of time, which
wraps after approximately 11 minutes, so only time differences between
clustered events is interesting.
.PP
.IN "Event" "Detail"
For all events containing details,
the high bits of the detail encode the state of
various keys and buttons just before the event:
.IN "Key Mask"
.IN "Button Mask"
.KS
.TS
center;
lcl.
ControlMask	0x4000	Control key
MetaMask	0x2000	Meta (Symbol) key
ShiftMask	0x1000	Shift key
ShiftLockMask	0x0800	ShiftLock key
LeftMask	0x0400	Left button
MiddleMask	0x0200	Middle button
RightMask	0x0100	Right button
.TE
.KE
For \fIKeyPressed\fP and \fIKeyReleased\fP,
the low byte of the detail gives the keycode.
This is not an ASCII character.
.IN "File" "<X/Xkeyboard.h>"
.IN "Keyboard" "Classification"
.IN "Macros" "Keyboard"
There are useful macros defined in the file \fI<X/Xkeyboard.h>\fP
to classify keycodes.
The tests \fIIsShiftKey()\fP, \fIIsCursorKey()\fP, \fIIsKeypadKey()\fP,
\fIIsFunctionKey()\fP, \fIIsPFKey()\fP and \fIIsTypeWriterKey()\fP may be used
with a key code to classify the key as to grouping.
See the warning at the beginning of the section.
.IN "XLookupMapping"
Normally, however, a client will use the supplied \fIXLookupMapping\fP
subroutine to determine what string (if any) is associated with the key.
.LP
For \fIButtonPressed\fP and \fIButtonReleased\fP events
the low byte of the detail
is one of:
.IP
	RightButton	0
.br
	MiddleButton	1
.br
	LeftButton	2
.LP
For \fIEnterWindow\fP and \fILeaveWindow\fP events,
the low byte of the detail is either zero
or one of:
.IP
	IntoOrFromSubwindow	1
.br
	VirtualCrossing		2
.LP
\fIEnterWindow\fP and \fILeaveWindow\fP events are generated as follows:
.nf
    When the mouse moves from window A to window B, and B is an ancestor of A:
	A will get a \fILeaveWindow\fP with detail 0
	windows between A and B exclusive that have \fILeaveWindow\fP selected
		will get a \fILeaveWindow\fP with detail 2
	B will get an \fIEnterWindow\fP with detail 1

    When the mouse moves from window A to window B, and B is a descendant of A:
	A will get a \fILeaveWindow\fP with detail 1
	windows between A and B exclusive that have \fIEnterWindow\fP selected
		will get an \fIEnterWindow\fP with detail 2
	B will get an \fIEnterWindow\fP with detail 0

    When the mouse moves from window A to window B, with window C being their
    least common ancestor:
	A will get a \fILeaveWindow\fP with detail 0
	windows between A and C exclusive that have \fILeaveWindow\fP selected
		will get a \fILeaveWindow\fP with detail 2
	windows between C and B exclusive that have \fIEnterWindow\fP selected
		will get an \fIEnterWindow\fP with detail 2
	B will get an \fIEnterWindow\fP with detail 0

    At the start of a mouse grab, either automatically from a button press,
    or from an \fIXGrabMouse\fP or \fIXGrabButton\fP, with the mouse in window A:
	A will get a \fILeaveWindow\fP with detail 0 if the grabbing client has
		not issued an \fIXSelectInput\fP command on A.
	ancestors of A (not including the root) will get a \fILeaveWindow\fP with
		detail 2 if the grabbing client has not issued an \fIXSelectInput\fP
		on the window and the window has \fILeaveWindow\fP selected.

    At the end of a mouse grab, with the mouse in window A:
	ancestors of A (not including the root) will get an \fIEnterWindow\fP with
		detail 2 if the grabbing client has not issued an \fIXSelectInput\fP
		on the window and the window has \fIEnterWindow\fP selected.
	A will get an \fIEnterWindow\fP with detail 0 if the grabbing client has
		not issued an \fIXSelectInput\fP command on A.
.fi
.LP
Another way to look at this is:
.nf
   a detail of 0 means that the mouse entered this window from, or left
      this window towards, some place outside the window's hierarchy.
   a detail of 1 means that the mouse entered this window from, or left
      this window towards, one of its descendents.
   a detail of 2 means that the mouse moved from a descendent of the
      window to a place outside the window's hierarchy, or vice versa.
.fi
\fIEnterWindow\fP and \fILeaveWindow\fP events with detail 0 or 1 will propagate
to the smallest enclosing window that has actually selected the event.
.PP
There are a set of structure definitions
outlining the information and type of each event in \fI<X/Xlib.h>\fP.
Coordinates are relative to the inside of the window.
.PP
.IN "ExposeWindow Event"
.IN "ExposeRegion Event"
\fIExposeWindow\fP and \fIExposeRegion\fP
events are triggered as (parts of) windows become
exposed.
When an entire window becomes exposed (as when a window is mapped or
changes size), an \fIExposeWindow\fP event is sent.
The width and height of the
entire window is given, and the coordinates are (0, 0).
When only parts of a
window become exposed (as when an obscuring window is moved), 
\fIExposeRegion\fP
events are sent describing each newly exposed area.  However, if only
\fIExposeWindow\fP has been selected,
a single \fIExposeWindow\fP event will be sent instead.
.IN "XCopyArea"
.IN "XMoveArea"
If the region exposure is the result of an \fIXCopyArea\fP
or \fIXMoveArea\fP, then \fIExposeCopy\fP will
be set in the detail word.  
If the exposure is actually that of a child of the
window selecting the event, the sub window is set to that child and the
coordinates are actually for the sub window, otherwise the sub window is 0.
For a given window exposure or \fIXCopyArea\fP
or \fIXMoveArea\fP,
all resulting \fIExposeRegion\fP events
will be sent contiguously, with no other events interspersed.
.PP
\fIXExposeCopyEvent\fP
are only sent to terminate a string of \fIExposeRegion\fP
events
that might be
.IN "XCopyArea"
.IN "XMoveArea"
generated by \fIXCopyArea\fP or \fIXMoveArea\fP commands.
Note that there is no information in the event as to what area was
exposed, as this is just an acknowledgement that all side effects have
been generated.
.PP
The structures for exposure events are given below:
.sp
.DS
.TA .5i 2.5i
.ta .5i 2.5i
struct _XExposeEvent {
	unsigned long type;	/* ExposeWindow or ExposeRegion */
	Window window;	/* that selected this event */
	short pad_s2; 	      
	short detail;	/* 0 or ExposeCopy */
	short width;	/* width of exposed area */
	short height;	/* height of exposed area */
	Window subwindow;	/* child window (if any) actually exposed */
	short y;	/* top of exposed area (0 for ExposeWindow) */
	short x;	/* left edge of exposed area (0 for ExposeWindow) */
};
.IN "Data Structures" "XExposeEvent"
.IN "Data Structures" "XExposeWindowEvent"
.IN "Data Structures" "XExposeCopyEvent"
typedef struct _XExposeEvent XExposeEvent;
typedef struct _XExposeEvent XExposeWindowEvent;
typedef struct _XExposeEvent XExposeRegionEvent;
.DE
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _XExposeCopyEvent {
    	unsigned long type;	/* ExposeCopy */
	Window window;	/* that selected this event */
	long pad_l1;
	long pad_l2;	      
	Window subwindow;	/* child window (if any) actually exposed */
	long pad_l4;	      
} XExposeCopyEvent;
.DE
.fi
.sp
.PP
.IN "XCopyArea"
If the XCopyArea was done in a child of the window selecting the event,
the subwindow is set to that child, otherwise the subwindow is 0.
.PP
The X server always clears to the
background color an exposed area before sending an exposure
event to the client.
Unfortunately, the client may have sent output to
a window between the time it is cleared and the time he reads the
exposure event off Xlib's queue.  This can lead to difficulty in two
cases:
.LP
\(bu the exposure event was an \fIExposeWindow\fP with new window dimensions.
The client probably wants output to go to a different place after a size
change than before.
.LP
\(bu the client is using a display function which is not invertable;  that
is, applying the function more than once does not have the same effect
as applying it just once.  
.IN "Display Functions"
Seven of the 16 display functions are not
invertable:  \fIGXandReverse\fP, \fIGXxor\fP, \fIGXnor\fP, \fIGXequiv\fP,
\fIGXinvert\fP, \fIGXorReverse\fP,
and \fIGXnand\fP.
The most obvious example of an invertable function is \fIGXinvert\fP,
which is a no-op if applied twice.
.PP
In both of these cases, the client should explicitly clear the exposed
area (i.e. paint it with the background) upon receipt of an exposure
event, BEFORE doing any repainting.
Failure to do so will cause ugly
glitches on the screen.
.PP
Unmap events have the following structure:
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _XUnmapEvent {
	unsigned long type;	/* UnmapWindow */
	Window window;	/* that selected this event */
	long pad_l1;
	long pad_l2;	      
	Window subwindow;	/* child window (if any) actually unmapped */
	long pad_l4;	      
} XUnmapEvent;
.DE
.fi
.sp
.LP
These events will be sent to clients that ask to be informed when
they are unmapped from the screen.
For example, both the clock and the load monitor program
will stop updating themselves when turned into
an icon (i.e. their main window is unmapped by a window manager).
.PP
To allow certain styles of user interfaces to be built,
it is also possible to request to be informed if the
input focus is set or removed from a window.
The event has the following structure:
.IN "Data Structures" "XFocusChangeEvent"
.IN "Event Types" "FocusChange"
.IN "FocusChange Event"
.sp
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _XFocusChangeEvent {
	unsigned long type;	/* FocusChange */
	Window window;	/* that selected this event */
	short pad_s2;
	short detail;	/* EnterWindow or LeaveWindow */
	long pad_l2;
	Window subwindow;	/* child window (if any) of actual focus change*/
	long pad_l4;	      
} XFocusChangeEvent;
.DE
.sp
