.NH 2
Status and Mode Window Operations
.FD
.IN "Definitions" "XQueryWindow"
.IN "XQueryWindow"
Status XQueryWindow (w, info)
	Window w;
	WindowInfo *info;	/* RETURN */
.FN
\fIXQueryWindow\fP gets various facts about a window.
It fills in the client-passed
.IN "WindowInfo"
.IN "File" "<X/Xlib.h>"
\fIWindowInfo\fP, which is defined in \fI<X/Xlib.h>\fP as
.sp
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _WindowInfo {
        short width, height;	/* Width and height. */
        short x, y;	/* X and y coordinates. */
        short bdrwidth;	/* Border width. */
        short mapped;	/* IsUnmapped, IsMapped or IsInvisible.*/
        short type;	/* IsTransparent, IsOpaque or IsIcon. */
        Window assoc_wind;	/* Associated icon or opaque Window. */
} WindowInfo;
.DE
.IN "Data Structures" "WindowInfo"
.IN "Definitions" "WindowInfo"
.LP
The \fImapped\fP field is set to \fIIsUnmapped\fP if the window is unmapped,
\fIIsMapped\fP if the
window is mapped and displayed (i.e. all of its ancestors are also
mapped), and \fIIsInvisible\fP if it is mapped but some ancestor is not mapped.
.PP
If the window is a normal opaque window,
the \fItype\fP will be \fIIsOpaque\fP.
\fIAssoc_wind\fP will contain the window's Icon window.
.IN "Icon"
.IN "XSetIconWindow"
This is normally set using the \fIXSetIconWindow\fP request.
If the window is an icon window,
the type will be \fIIsIcon\fP and \fIassoc_wind\fP
will contain the Icon's corresponding
regular window.
.PP
The procedure returns 0 if it fails, non-zero if it succeeds.
The procedure may fail if the window has been destroyed.
.FD
.IN "Definitions" "XQueryTree"
.IN "XQueryTree"
Status XQueryTree (w, parent, nchildren, children)
	Window w;
	Window *parent;	/* RETURN */
	int *nchildren;	/* RETURN */
	Window **children;	/* RETURN */
.FN
.IN "Child Window"
.IN "Parent Window"
\fIXQueryTree\fP returns a list of children of the specified window, its parent,
and the number of children of this window.
It returns a pointer to a list of the children windows.
The list must
be deallocated when no longer needed.
The children are listed in current stacking order, from bottom-most (first)
to top-most (last).
.PP
The procedure returns 0 if it fails, non-zero if it succeeds.
.FD
.IN "Definitions" "XChangeBackground"
.IN "XChangeBackground"
.IN "Window" "Background"
XChangeBackground(w, tile)
	Window w;
	Pixmap tile;
.FN
Change the background \fItile\fP of a window.
If no background Pixmap is specified, 
the background pixmap of the window's parent is used.
On the root window, the default background will be restored.
.PP
.IN "Resource ID's" "Freeing"
.IN "Freeing" "Resources"
The background Pixmap can immediately be freed if no further explicit
references to it are to be made.
.PP
This can only be performed on an opaque window.
An error will result if you try to change the background of a transparent
window.
.PP
NOTE:  This does not change the current contents of the window, and you
may wish to clear and repaint the screen after this function since it
will not repaint the background you just set.
.FD
.IN "Definitions" "XChangeBorder"
.IN "XChangeBorder"
XChangeBorder(w, tile)
	Window w;
	Pixmap tile;
.FN
Changes and repaints the border \fItile\fP of a window.
.LP
.IN "Resource ID's" "Freeing"
.IN "Freeing" "Resources"
The border Pixmap can be freed immediately if no further explicit
references to it are to be made.
.PP
This can only be performed on an opaque window that has a border.
It will cause an error to perform this on a transparent window or
an opaque window that has no border.
.FD
.IN "Definitions" "XTileAbsolute"
.IN "Definitions" "XTileRelative"
.IN "XTileAbsolute"
.IN "XTileRelative"
XTileAbsolute(w)

XTileRelative(w)
	Window w;
.FN
.IN "Tile Mode"
Sets the tile mode of the window.
In ``absolute'' mode (the normal case for opaque windows),
tiles are laid out with the upper left corder of the window as an effective
origin.
In ``relative'' mode
(the default for transparent windows), tiles are laid out with the upper
left corner of the closest parent window with an absolute tile mode
as an effective origin.
The tile mode affects painting of the background for exposures
.IN "XClear"
and for \fIXClear\fP,
.IN "XTileFill"
.IN "XDrawFilled"
as well as the \fIXTileFill\fP and \fIXDrawFilled\fP requests.
.PP
NOTE:  This does not change the current contents of the window, and you
may wish to clear and repaint the screen after this function.
.FD
.IN "Definitions" "XClipDrawThrough"
.IN "Definitions" "XClipClipped"
.IN "XClipDrawThrough"
.IN "XClipClipped"
XClipDrawThrough (w)

XClipClipped (w)
	Window w;
.FN
.IN "Clip Mode"
These functions set 
the `clip mode' of a window, which determines what
happens when you draw on a part of the window which is
obscured by a child window.
.PP
If the clip mode is \fIClipped\fP, output into areas covered by
children is suppressed.
All windows start out this way when created by \fIXCreateWindow\fP.
.PP
If the clip mode is \fIDrawThrough\fP, output will be drawn on the screen as if
the child window wasn't there.
\fIDrawThrough\fP is useful for drawing window outlines when
moving or resizing windows.
Note that any such requests must be atomic and return
the screen to its original state in a single X request.
The root window's clip-mode is initially of \fIDrawThrough\fP.
.IN "XGrabServer"
.IN "Window" "Managers"
.IN "Grabbing" "Server"
.PP
Another approach when writing code (usually window manager type programs) that
does not result in flickering lines is possible.
You can grab control of the server using the \fIXGrabServer\fP call.
This means that for the duration of the `grab' you can display
static graphics on the screen so long as you put everything back before
releasing the server, since no other client can perform output while
the server is grabbed.
.FD
.IN "Definitions" "XStoreName"
.IN "XStoreName"
.IN "Window" "Name"
XStoreName (w, name)
	Window w;
	char *name;	/* null-terminated string */
.FN
\fIXStoreName\fP assigns a name to a window.
The name should be a
null-terminated string.  
This name will be returned by
any subsequent call to
.IN "XFetchName"
\fIXFetchName\fP.
Windows are typically named for the convenience of window managers.
This allows a window manager to display a text representation of a window
when its icon is being displayed.
.FD	
.IN "Definitions" "XFetchName"
.IN "XFetchName"
Status XFetchName (w, name)
	Window w;
	char **name;	/* RETURN */
.FN
\fIXFetchName\fP sets \fIname\fP to a pointer to the name of the window
which will be null-terminated.  
If no name was ever set, it
sets \fIname\fP to NULL. The client must free
the name string when finished with it.
.PP
\fIXFetchName\fP returns 0 if it fails, non-zero otherwise.  (Note: if
the window has never had a name set, this is not considered a
failure and \fIXFetchName\fP will return a non-zero status.)
.FD
.IN "Definitions" "XSetResizeHint"
.IN "XSetResizeHint"
XSetResizeHint (w, width0, height0, widthinc, heightinc)
	Window w;
	int width0,  height0, widthinc, heightinc;
.FN
.IN "Window" "Resize Hint"
\fIXSetResizeHint\fP is used to give a hint to the window system
that can be used by a window manager program  to
define the desired shape of a window.
The inside height of the
window should be the base height (\fIheight0\fP) plus some multiple of the
height increment (\fIheightinc\fP), and the inside width of the window should
be the base width (\fIwidth0\fP) plus some multiple of the width increment
(\fIwidthinc\fP).
These parameters are hints for the window manager, but there is
no guarantee that they will be honored.
.PP
By default, a window's resize hint is (0, 0, 1, 1).
.PP	
The base height and width must be non-negative, and the height and width
increments must be positive.
.PP
This is used by window managers to avoid resizing windows to sizes that
may not be convenient for the clients,
but CLIENTS must not presume that the window is the correct size.
.FD
.IN "Definitions" "XGetResizeHint"
.IN "XGetResizeHint"
.IN "Window" "Resize Hint"
XGetResizeHint (w, width0, height0, widthinc, heightinc)
	Window w;
	int *width0, *height0;	/* RETURN */
	int *widthinc, *heightinc;	/* RETURN */
.FN
\fIXGetResizeHint\fP asks for the window's size parameters and assigns them to
the client's variables. 
.FD
.IN "Definitions" "XSetIconWindow"
.IN "XSetIconWindow"
.IN "Window" "Icon Window"
.IN "Icons"
.IN "Definitions" "XClearIconWindow"
.IN "XClearIconWindow"
XSetIconWindow(w, iw)
	Window w;
	Window iw;	/* icon window */

XClearIconWindow(w)
.FN
\fIXSetIconWindow\fP sets and \fIXClearIconWindow\fP
clears the icon window for a window.
The icon window must be a sibling of the specified window,
both windows must be opaque, and neither can already be an icon window.
When created, windows do not have icon windows defined.
.PP
.IN "ExposeWindow Event"
The icon window facility is provided because many window manager programs
allow the user to `turn a window into an icon.'
A client should call \fIXSetIconWindow\fP if it wants to control the
contents of the window's icon.
If the client has not called \fIXSetIconWindow\fP,
the window manager should create its own icon window for the window.
If a window is destroyed and has a mapped icon window, that icon
window is umapped and receives and UnmapWindow event.
If a window is destoryed and is a mapped icon window, its corresponding
`regular' window is mapped.
.FD
Status XQueryMouse (w, x, y, subw)
	Window w;
	int *x, *y;	/* RETURN */
	Window *subw;	/* RETURN */
.FN
.IN "Definitions" "XQueryMouse"
.IN "XQueryMouse"
.IN "Mouse" "Position"
\fIXQueryMouse\fP determines the current mouse coordinates.
The coordinates
returned are relative to the top left inside corner of the window, even
if the mouse is outside the window (they can be negative).
If the mouse is also in a child
window, then \fIsubw\fP is set to that child, otherwise \fIsubw\fP is set to 0.
.FD
.IN "Definitions" "XQueryMouseButtons"
.IN "XQueryMouseButtons"
.IN "Mouse" "Position and Buttons"
Status XQueryMouseButtons (w, x, y, subw, state)
	Window w;
	int *x, *y;	/* RETURN */
	Window *subw;	/* RETURN */
	short *state;	/* RETURN */
.FN
.IN "XQueryMouse"
Returns the same information as \fIXQueryMouse\fP,
with the addition of the state of the mouse buttons (which buttons
on the mouse are depressed).
.FD
.IN "Definitions" "XUpdateMouse"
.IN "XUpdateMouse"
.IN "Mouse" "Get Position"
Status XUpdateMouse (w, x, y, subw)
	Window w;
	int *x, *y;	/* RETURN */
	Window *subw;	/* RETURN */
.FN
\fIXUpdateMouse\fP is like \fIXQueryMouse\fP, but also reads pending events and
eliminates any MouseMoved events at the head of the queue.
A good way to
track the mouse is to use a MouseMoved event as a "hint", by calling
this routine to get up to date coordinates.
.FD
.IN "Definitions" "XWarpMouse"
.IN "XWarpMouse"
.IN "Mouse" "Moving the Mouse Cursor"
XWarpMouse (w, x, y)
	Window w;
	int x, y;
.FN
\fIXWarpMouse\fP moves the mouse to the specified position in the specified
window.
The \fIx\fP and \fIy\fP coordinates are relative to the inside top left
corner of the window.
.FD
.IN "Definitions" "XCondWarpMouse"
.IN "XCondWarpMouse"
.IN "Mouse" "Moving the Mouse Cursor"
XCondWarpMouse (dw, dx, dy, sw, sx, sy, swidth, sheight)
	Window dw, sw;
	int dx, dy, sx, sy;
	int swidth, sheight;
.FN
Moves the mouse to the destination position relative to the origin
of the destination window (\fIdw\fP),
but only if the mouse is currently in a visible region of the
specified region of the source window.
.PP
If the source height is zero,
the current height (\fIsheight\fP)
of the source window minus the source y (sy) coordinate.
If the source width (\fIswidth\fP) is zero,
the current width of the source window minus the source left (\fIsx\fP)
coordinate is used.
.FD
.IN "Definitions" "XInterpretLocator"
.IN "XInterpretLocator"
.IN "Window" "Conversion to Window Coordinates"
Status XInterpretLocator (w, x, y, subw, loc)
	Window w;
	int *x, *y;	/* RETURN */
	Window *subw;	/* RETURN */
	Locator loc;
.FN
\fIXInterpretLocator\fP converts absolute coordinates to coordinates which are
relative to the top left inner corner of a window.
These
window-relative coordinates are assigned to the variables \fIx\fP and \fIy\fP.
Normally, the Locator will be obtained from an
input event rather than constructed from a separate \fIx\fP and \fIy\fP coordinate.
It is seldom useful to deal with absolute coordinates, but if you must
convert absolute coordinates to a locator,
a locator is constructed from a coordinate pair by (x<<16) | y.
If the absolute coordinates also fall within a child window, the routine
sets \fIsubw\fP to that child window, otherwise it sets \fIsubw\fP to 0.
