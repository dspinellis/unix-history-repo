.NH
Creating and Destroying Windows
.XS
Window Related Operations
.XE
.PP
.IN "Definitions" "Opaque Window"
.IN "Definitions" "Transparent Window"
There are two kinds of windows, opaque and transparent.  
If an opaque
window is stacked `on top of' another window, it obscures that other
window for the purpose of both input and output.
Attempts to output to
the obscured area will do nothing, and no input events (e.g. mouse
motion) will be generated for the obscured area.
Most windows are opaque.
Opaque windows have borders of a programmable width and pattern
and a background pattern (or `tile').
.IN "Tile Pixmaps"
Tile and border patterns are described using Pixmaps.
In a program, you refer to the window using its resource id,
of type `Window'.
.PP
.IN "Resource ID's" "Freeing"
.IN "Freeing" "Resources"
The background and border pixmaps may be destroyed immediately after
creating the window if no further explicit references to them
are to be made.
.PP
.IN "Definitions" "Tile Mode"
An opaque window's background is tiled with a pattern (most commonly a constant
color).
This pattern can either be relative to the parent (the pattern will be
shifted appropriately to match the parent window) or absolute
(the pattern will be positioned in the window independently of the
parent window).
This is called the `tile mode' of a window,
and can be set by subroutine calls.
.PP
.IN "Definitions" "Clip Mode"
It is also possible to specify if graphics operations will be clipped by
child windows or will write through the children.
This is called the `clip mode' of a window.
.PP
.IN "Input" "Transparent Window"
A transparent window obscures other windows for the purpose of input
only.
.IN "Parent Window"
.IN "Output" "On Transparent Windows"
Doing output to a transparent window is equivalent to output to
the parent window, except that the transparent window's own clip mode is
.IN "XClipMode"
used instead of the parent's (see \fIXClipMode\fP below), and the output is
clipped to the boundaries of the transparent window.
If a transparent window is moved, it has no effect on the display.
Transparent windows do not have borders.
.PP
.IN "Window" "RootWindow"
.IN "RootWindow"
.IN "Macro" "RootWindow"
The macro \fIRootWindow\fP is a convenient shorthand for referring to the
root of the window hierarchy, and is usually used as the \fIparent\fPn
argument to most calls.
.PP
When windows are first created, they are not visible on the screen.
Any output to a window not visible (not `mapped') on the screen
will be discarded.
An application may wish to create a window long before it is
mapped to the screen.
When an opaque window is eventually mapped to the screen 
(using \fIXMapWindow\fP)
.IN "Window" "Mapping to screen"
.IN "XMapWindow"
it will generate an exposure event for the window.
.IN "Exposure Event"
.FD
.IN "Definitions" "XCreateWindow"
.IN "XCreateWindow"
Window XCreateWindow (parent, x, y, width, height, borderwidth, border, bgnd)
	Window parent;
	int x, y, width, height;
	int borderwidth;	/* border width */
	Pixmap border;
	Pixmap background;
.FN
\fIXCreateWindow\fP creates an unmapped,
opaque subwindow of the specified parent window,
which must also be opaque.
The created window will always be wholly
contained within its \fIparent\fP,
any part of the window extending outside its parent window will be clipped.
.PP
The \fIx\fP and \fIy\fP
coordinates represent the top left outside corner of the new window's
borders.
They are relative to the inside of the \fIparent\fP window's
borders.
The \fIwidth\fP and \fIheight\fP parameters are the new window's inside
dimensions--they do not include the new window's borders,
which are entirely outside of the window.
The new window will have a border \fIborderwidth\fP pixels wide.
.PP
.IN "Window" "Border"
.IN "Window" "Background"
.IN "Macro" "BlackPixmap"
.IN "Macro" "WhitePixmap"
.IN "BlackPixmap"
.IN "WhitePixmap"
A \fIborder\fP Pixmap need not be given if the border width is zero,
in which case the window will not have a border.
If no \fIbackground\fP Pixmap is given, the parent's background Pixmap is
used.  A monochrome application may find the macros \fIBlackPixmap\fP and
\fIWhitePixmap\fP useful; these refer to solid black and white pixmaps that
are automatically created by \fIXOpenDisplay\fP.
.PP
.IN "Window" "Tile Mode"
The \fItilemode\fP of the new window is absolute.
.IN "Window" "Clip Mode"
The \fIclipmode\fP of the new window is `clipped'.
.IN "Icon" "Creating Windows"
The new window does not have an associated icon window.
.IN "Window" "Name"
The \fIname\fP of the window is the null string.
.PP
The created window is not yet displayed (`mapped') on the user's display;
to display the
.IN "XMapWindow"
window, call \fIXMapWindow\fP.
.IN "Cursor" "Initial State"
The new window will not have a cursor defined; the cursor will
be that of the window's parent until/unless a cursor is registered.
The window will not be visible on the screen unless it and all of its
ancestors are `mapped', and it is not obscured by any of its ancestors.
.IN "XDefineCursor"
.PP
The subroutine returns the window id of the created window, or 0 if
the subroutine fails.
.FD
.IN "Definitions" "XCreateTransparency"
.IN "XCreateTransparency"
Window XCreateTransparency (parent, x, y, width, height)
	Window parent;
	int x, y, width, height;
.FN
\fIXCreateTransparency\fP creates an unmapped  transparent
window.
Transparent windows do not have borders.
The coordinates are relative to the parents coordinate system.
.IN "Transparent Window" "Clip Mode"
.IN "Transparent Window" "Initial Cursor"
The
window will not initially have a cursor registered, and will have a
clipmode of \fIClipModeClipped\fP
(i.e. output to the window is obscured by subwindows of
the parent).
.IN "Transparent Window" "Background"
.IN "Transparent Window" "Border"
.IN "Transparent Window" "Tile Mode"
A transparent window does not have a border or background pattern.
It initially has a tile mode of \fITileModeRelative\fP.
.PP
The subroutine returns the window id of the created window, or 0 if
the subroutine fails.
.FD
.IN "Definitions" "XDestroyWindow"
.IN "XDestroyWindow"
XDestroyWindow (w)
	Window w;
.FN
\fIXDestroyWindow\fP unmaps and destroys the window and all of its subwindows.
The windows should never again be referenced.
\fIXDestroyWindow\fP may be
called on either opaque or transparent windows.
.PP
.IN "Icon" "Mapping Behavior"
If the window has an icon window, that icon window is automatically unmapped
and sent an unmap window event.
If the window is a mapped icon window,
its corresponding `regular' window is automatically mapped.
This prevents windows being lost when a window manager exits unexpectedly.
.PP
Destroying a mapped opaque window will generate exposure events on other
opaque windows that were obscured by the window being destroyed.
.PP
.IN "XCloseDisplay"
\fIXCloseDisplay\fP automatically destroys all windows that have been created
.IN "Unix System Call" "fork"
on that server  (unless called after a \fIfork()\fP--see note under 
\fIXCloseDisplay\fP).
.FD
.IN "Definitions" "XDestroySubwindows"
.IN "XDestroySubwindows"
XDestroySubwindows (w)
	Window w;
.FN
.IN "XDestroySubwindows"
\fIXDestroySubwindows\fP destroys all subwindows of this window.  The
subwindows should never again be referenced.  
.PP
\fIXDestroySubwindows\fP generates exposure events on \fIw\fP, if any mapped
opaque subwindows were actually destroyed.
.PP
Note that this is MUCH more efficient than deleting many windows
one at a time, as much of the work need only be performed once for all
of the windows rather than for each window.
.PP
To allow efficient creation of many similar windows or subwindows
simultaneously
(a function often wanted when creating menus or complex forms),
.IN "XCreateWindow"
.IN "XCreateTransparency"
.IN "XCreateWindowBatch"
both \fIXCreateWindow\fP and \fIXCreateTransparency\fP have analogous forms
which take structures of the form:
.DS
.TA .5i 3i
.ta .5i 3i
typedef struct _OpaqueFrame {
	Window self;	/* window id of the window, filled in later */
	short x, y;	/* where to create the window */
	short width, height;	/* window size */
	short bdrwidth;	/* border width */
	Pixmap border;	/* border pixmap */
	Pixmap background;	
} OpaqueFrame;
.DE
.IN "Definitions" "OpaqueFrame"
.IN "Data Structures" "OpaqueFrame"
.IN "OpaqueFrame"
.DS
.TA .5i 3i
.ta .5i 3i
typedef struct _TransparentFrame {
	Window self;	/* window id of the window, filled in later */
	short x, y;	/* where to create the window */
	short width, height;	
} TransparentFrame;
.DE
.IN "Definitions" "BatchFrame"
.IN "Data Structures" "BatchFrame"
.IN "BatchFrame"
.DS
.TA .5i 3i
.ta .5i 3i
typedef struct _BatchFrame {
	short type;	/* One of (IsOpaque, IsTransparent). */
	Window parent;	/* Window if of the window's parent. */
	Window self;	/* Window id of the window, filled in later. */
	short x, y;	/* Where to create the window. */
	short width, height;	/* Window width and height. */
	short bdrwidth;	/* Window border width. */
	Pixmap border;	/* Window border pixmap */
	Pixmap background;	/* Window background pixmap. */
} BatchFrame;
.DE
.fi
.IN "Definitions" "TransparentFrame"
.IN "Data Structures" "TransparentFrame"
.IN "TransparentFrame"
.FD
.IN "Definitions" "XCreateWindows"
.IN "XCreateWindows"
int XCreateWindows (parent, defs, ndefs)
	Window parent;
	OpaqueFrame defs[];
	int ndefs;
.FN
This subroutine takes an array of window information definitions
and creates them in a single handshake with the window system.
The caller should have filled in the structure except for the window id.
The window id's of the created windows are returned in the structure
passed to the subroutine.
You must specify the number of windows to be created using the
\fIndefs\fP argument.
.IN "XCreateWindow"
The other side-effects are the same as for \fIXCreateWindow\fP.
The subroutine returns the number of windows actually created.
.FD
.IN "Definitions" "XCreateTransparencies"
.IN "XCreateTransparencies"
int XCreateTransparencies(parent, defs, ndefs)
	Window parent;
	TransparentFrame defs[];
	int ndefs;
.FN
This subroutine takes an array of window information definitions
and creates the transparencies in a single handshake with the window system.
The caller should have filled in the structure except for the window id.
The window id's of the created windows are returned in the structure
passed to the subroutine.
You must specify the number of windows to be created using the
ndefs argument.
.IN "XCreateWindow"
The side effects are the same as for \fIXCreateWindow\fP.
The subroutine returns the number of windows actually created.
.FD
.IN "Definitions" "XCreateWindowBatch"
.IN "XCreateWindowBatch"
int XCreateWindowBatch(defs, ndefs)
	Window parent;
	BatchFrame defs[];
	int ndefs;
.FN
This subroutine takes an array of window information definitions
and creates the windows of the specified types
in a single handshake with the window system.
The caller should have filled in the structure except for the window id.
The window id's of the created windows are returned in the structure
passed to the subroutine.
You must specify the number of windows to be created using the
ndefs argument.
.IN "XCreateWindow"
.IN "XCreateTransparency"
The side effects are the same as for \fIXCreateWindow\fP and \fIXCreateTransparency\fP.
The subroutine returns the number of windows actually created.
The parent windows must already exist.
.FD
.IN "Definitions" "XCreate"
.IN "XCreate"
Window XCreate(prompt, program, geometry, default, frame, minwidth, minheight)
	char *prompt;	/* prompt string */
	char *program;	/* program name for Xdefaults */
	char *geometry, *default;	/* geometry specs */
	OpaqueFrame *frame;	/* background and border pixmaps in */
	int minwidth, minheight;	/* minimum size for the window */
.FN
XCreate does all the work for automatic and manual placement of a window,
and is commonly used by most applications for creating graphics related 
windows.
.PP
The \fIprompt\fP argument is used in a prompt window (if needed) to inform the
user what application wants to be placed.
The \fIprogram\fP name must be passed in with the program argument (usually
it should be \fIargv[0]\fP) so that \fIXGetDefault\fP can find out how the
user likes to be prompted to create the window.
.PP
The \fIgeometry\fP and \fIdefault\fP arguments are used to place the position
and/or size of the window.
.PP
The \fIframe\fP passed in must include the background and border pixmaps
already specified, and the border width of the window.
.PP
\fIMinwidth\fP and \fIminheight\fP specify the minimum size of the created window
in pixels.
.PP
The window is not mapped after creation.
The function returns the window id of the window just created,
and all values in the passed in window \fIframe\fP (as defined above)
will be set.
.PP
The following paragraphs describes the current user interface;
it may change in the
future somewhat.
.IN "XGeometry"
.IN "XParseGeometry"
If a sufficiently complete geometry specification (see \fIXParseGeometry\fP 
and \fIXGeometry\fP)
is passed in, the window will be created automatically.
If no X or Y locations are set by the geometry spec, the user will be
prompted to interactively position the window.
A prompt window will be popped up in the upper left hand corner,
and the mouse grabbed.
.PP
.IN "XGrabMouse"
.IN "Grabbing" "Mouse"
.IN "XGetDefault"
The ``MakeWindow'' X defaults ``BodyFont'', ``ReverseVideo'',
``BorderWidth'', ``InternalBorder'', ``Freeze'',
``Foreground'', ``Background'', ``Border''
``Mouse'', ``MouseMask'' control the appearance
of a prompt window and the cursor to be used when rubber banding.
If ``freeze'' is ``on'', the server will be frozen while the window is being
created.
So for example, one of these might be specified as ``.MakeWindow.Freeze''
in your \fI~/.Xdefaults\fP file.
.IN "XGrabServer"
.IN "Grabbing" "Server"
A box the size of the minimum window will be rubber banded on the
screen.
.PP
If the left button is pressed, the outline of the default window at its
default size and location will be shown; when the button is released, the
window will be created.
If the right button is pressed, the outline of the default window at its
default size and the current location of the mouse will be shown;
when the button is released, the window's upper left corner will be created
at the current cursor location.
If the center button is pressed, it indicates one corner of the window should
be set at the current mouse location.
When the center button is released, the window will be created with the
other corner of the window at the current mouse location unless the
minimum size has not been met.
.FD
.IN "Definitions" "XCreateTerm"
.IN "XCreateTerm"
Window XCreateTerm(prompt, program, geometry, default, frame, minwidth, minheight,
	xadder, yadder, cwidth, cheight, f, fwidth, fheight)
	char *prompt;	/* prompt string */
	char *program;	/* program name for Xdefaults */
	char *geometry, *default;	/* geometry specs */
	OpaqueFrame *frame;	/* background and border pixmaps in */
	int minwidth, minheight;	/* minimum size for the window */
	int xadder, yadder;	/* additional space inside window needed */
	int *cwidth, *cheight;	/* RETURNs created size of window */
	FontInfo *f;	/* font for prompt window */
	int fwidth, fheight;	/* size of geometry units */	
.FN
\fIXCreateTerm\fP
does all the work for automatic and manual placement of a window
which should be sized in multiples of \fIfwidth\fP and \fIfheight\fP,
and is commonly used by most applications for creating text related 
windows.
.PP
The \fIprompt\fP argument is used in a prompt window (if needed) to inform the
user what application wants to be placed.
The program name must be passed in with the \fIprogram\fP argument (usually
it should be \fIargv[0]\fP) so that \fIXGetDefault\fP can find out how the
user likes to be prompted to create the window.
.PP
The geometry and default arguments are used to place the position
and/or size of the window.
.PP
The \fIfwidth\fP and \fIfheight\fP arguments specify the size of the units used in
the geometry spec,
and the increments the window should be sized in.
These are typically the size of a fixed width font, or other
graphic object in a window.
.PP
The \fIframe\fP passed in must include the background and border pixmaps
already specified, and the border width of the window.
.PP
\fIMinwidth\fP and \fIminheight\fP specify the minimum size of the created window in
multiples of \fIfwidth\fP and \fIfheight\fP.
\fIXadder\fP and \fIyadder\fP is additional interior padding needed in the window.
The function returns the window id of the window just created,
and all values in the passed in window frame will be set.
.PP
The window is not `mapped' after creation.
.PP
The following paragraphs describes the current user interface;
it may change in the
future somewhat.
.IN "XGeometry"
.IN "XParseGeometry"
If a sufficiently complete geometry specification (see \fIXParseGeometry\fP 
and \fIXGeometry\fP)
is passed in, the window will be created automatically.
If no X or Y locations are set by the geometry spec, the user will be
prompted to interactively position the window.
A prompt window will be `popped' in the upper left hand corner,
and the mouse grabbed.
In the prompt window will be appended the width and height of the window
in the units specified by \fIfwidth\fP and \fIfheight\fP.
.PP
.IN "XGrabMouse"
.IN "Grabbing" "Mouse"
.IN "XGetDefault"
The ``MakeWindow'' X defaults ``BodyFont'', ``ReverseVideo'',
``BorderWidth'', ``InternalBorder'', ``Freeze'',
``Foreground'', ``Background'', ``Border''
``Mouse'', ``MouseMask'' control the appearance
of a prompt window and the cursor to be used when rubber banding.
If ``freeze'' is ``on'', the server will be frozen while the window is being
created.
So for example, one of these might be specified as ``.MakeWindow.Freeze''
in your X defaults file.
.IN "XGrabServer"
.IN "Grabbing" "Server"
A box the size of the minimum window will be rubber banded on the
screen.
.PP
If the left button is pressed, the outline of the default window at the
mouse's current position of the default size
will be shown; when the button is released, the
window will be created.
If the right button is pressed, the outline of the default window at its
default size and the current location of the mouse will be shown;
when the button is released, the window's upper left corner will be created
at the current cursor location, and the height of the window will be
determined by the height of the screen.
If ``.MakeWindow.ClipToScreen'' is ``on'', the window will be
clipped to the screen until and unless the minimum size requirements
preclude it.
If the center button is pressed, it indicates one corner of the window should
be set at the current mouse location.
When the center button is released, the window will be created with the
other corner of the window at the current mouse location unless the
minimum size has not been met.
