.NH 2
Manipulating Windows
.PP
.IN "XMapWindow"
A window is considered `mapped' if a \fIXMapWindow\fP
call has been made on it.
It may not be visible on the screen for either of two reasons:
1) it may be obscured by another opaque sibling window or
2) one of its ancestors is not mapped.
.IN "Exposure Events"
Exposure events will be generated for the window when part or all of
it become visible on the screen; a client will only receive the
exposure events if it has asked for them using \fIXSelectInput\fP.
.IN "XSelectInput"
.FD
.IN "Definitions" "XMapWindow"
.IN "XMapWindow"
XMapWindow (w)
	Window w;
.FN
\fIXMapWindow\fP maps the window and raises the window and all of its
subwindows which have had map requests to the top of the stack of windows.
A subwindow will appear on the screen so long as all of its ancestors
are mapped.
The previous contents of all
opaque windows are lost; mapping of transparent windows does not affect
the screen.
.PP
Mapping a window that has an unmapped ancestor does not display the
window, but marks it as eligible for display when the ancestor becomes
mapped.
.PP
Mapping an already mapped window has no effect
(it does NOT raise it).
.PP
.IN "XMapWindow"
.IN "ExposeWindow Event"
If the window is opaque,
\fIXMapWindow\fP generates \fIExposeWindow\fP events on
each opaque window that it causes to become displayed.  If the client
first maps the window, then paints the window, then begins
processing input events, the window will be painted twice.   To avoid this,
the client should either
.IN "XSelectInput"
  a) first Map, then call \fIXSelectInput\fP for exposure events,
then repaint the
window(s) explicitly, or
  b) first call \fIXSelectInput\fP for exposure events, 
then map, then process input
events normally.  The event list will include \fIExposeWindow\fP for each
window that has appeared on the screen; the client's normal response to
an \fIExposeWindow\fP should be to repaint the window.
Method b) is preferred as it usually leads to simpler programs.
.FD
.IN "Definitions" "XMapSubwindows"
.IN "XMapSubwindows"
XMapSubwindows (w)
	Window w;
.FN
.PP
\fIXMapSubwindows\fP maps all subwindows of the specified window in
an unpredictable order.
.PP
.IN "ExposeWindow Event"
It also generates an \fIExposeWindow\fP
event on each newly displayed opaque window.
.PP
Note that this is MUCH more efficient than mapping many windows
one at a time, as much of the work need only be performed once for all
of the windows rather than for each window.
.FD
.IN "Definitions" "XUnmapWindow"
.IN "XUnmapWindow"
XUnmapWindow (w)
	Window w;
.FN
\fIXUnmapWindow\fP unmaps the window.
Any child window will no longer be visible until another map call is
made on the parent.
(Another words, the subwindows are still mapped, but not visible
until the parent is mapped.)
.IN "UnmapWindow Event"
It generates an \fIUnmapWindow\fP event for \fIw\fP, regardless
of whether it is opaque or transparent.
Child windows will NOT receive \fIUnmapWindow\fP events.
.PP
Unmapping a transparent window does not affect the
screen or generate any exposure events.
Unmapping an opaque window will
generate exposure events on opaque windows that were formerly obscured
by it and its children.
.FD
.IN "Definitions" "XUnmapTransparent"
.IN "XUnmapTransparent"
XUnmapTransparent (w)
	Window w;
.FN
Unmaps the window but does not affect the
screen, even if the window is opaque, and does not generate any
exposure (or unmap) events.
.IN "Menus"
This is intended for use mainly by pop-up
menus in conjunction with
.IN "XPixmapSave"
\fIXPixmapSave\fP to suppress exposure events; the client should normally
restore the saved pixmap to the area formerly covered by the unmapped
window.
.FD
.IN "Definitions" "XUnmapSubwindows"
.IN "XUnmapSubwindows"
XUnmapSubwindows(w)
	Window w;
.FN
Unmaps all subwindows of the specified window.
.IN "UnmapWindow Event"
It also generates an UnmapWindow event on each subwindow and generates
Exposure events on formerly obscured opaque windows.
.PP
Note that this is MUCH more efficient than unmapping many windows
one at a time, as much of the work need only be performed once for all
of the windows rather than for each window.
.FD 
.IN "Definitions" "XMoveWindow"
.IN "XMoveWindow"
XMoveWindow (w, x, y)
	Window w;
	int x, y;
.FN	
\fIXMoveWindow\fP moves and raises the window, without changing its size.
It does not change the mapping state of the window.
The
\fIx\fP and \fIy\fP are the new location of the top
left pixel of the window's border (or the window itself, if it has
no border).
Moving an mapped opaque window may or may not lose its contents depending on
1) if its tile mode is
relative or 2) if the window is obscured by non-children.
Moving a transparent window does not affect the screen.
.IN "Exposure Events"
If the
contents are lost, exposure events will be generated for the window and
any mapped opaque subwindows.
Moving a transparent window does not
affect the screen.
.PP
.IN "Exposure Events"
Moving a mapped opaque window will generate exposure events on any
formerly obscured opaque windows.
.FD
.IN "Definitions" "XChangeWindow"
.IN "XChangeWindow"
XChangeWindow (w, width, height)
	Window w;
	int width, height;
.FN	
\fIXChangeWindow\fP changes the size of the window without
changing its upper left coordinate.  
The new \fIwidth\fP and \fIheight\fP are inside
dimensions; they do not include the window's borders.
.PP
\fIXChangeWindow\fP always raises the window.
.IN "ExposeWindow Event"
Changing the size of a mapped opaque window loses its contents and generates
an \fIExposeWindow\fP event.
Changing the size of a transparent window does
not affect the screen.
.PP
If a mapped opaque window is made smaller, exposure events will be generated on
opaque windows that it formerly obscured.
.PP
The origin of the window is not changed.
.FD
.IN "Definitions" "XConfigureWindow"
.IN "XConfigureWindow"
XConfigureWindow (w, x, y, width, height)
	Window w;
	int x, y, width, height;
.FN
\fIXConfigureWindow\fP changes the size and location of the window.
Configuring a mapped opaque window loses its contents and generates an
.IN "ExposeWindow Event"
ExposeWindow event;  configuring a transparent window does not affect the
screen.
\fIXConfigureWindow\fP always raises the window.
.PP
Configuring a window may generate exposure events on opaque windows that the
window formerly obscured, depending on the new size and location parameters.
.PP
.IN "XChangeWindow"
To change the size only, call \fIXChangeWindow\fP.  To change the location
.IN "XMoveWindow"
only, call \fIXMoveWindow\fP.
.FD
.IN "Definitions" "XRaiseWindow"
.IN "XRaiseWindow"
XRaiseWindow (w)
	Window w;
.FN
\fIXRaiseWindow\fP `raises' this window so that no sibling window obscures it.
If the windows are regarded as overlapping sheets of paper stacked on a desk,
then raising a window is analogous to moving the sheet to the top of
the stack, while leaving its x and y location on the desk constant.
.PP
.IN "Exposure Events"
Raising a mapped opaque window may generate exposure events for the
window and any mapped opaque subwindows that were formerly obscured.  
.PP
Raising a transparent window does not affect the screen.
Transparent windows never obscure other windows for the purposes
of output, but do obscure for the purposes of cursor and input
control.
.FD
.IN "Definitions" "XLowerWindow"
.IN "XLowerWindow"
XLowerWindow (w)
	Window w;
.FN
\fIXLowerWindow\fP `lowers' this window so that it does 
not obscure any sibling
windows. 
If the windows are regarded as overlapping sheets of paper
stacked on a desk, then lowering a window is analogous to moving the
sheet to the bottom of the stack, while leaving its x and y location on
the desk constant.
.PP
.IN "Exposure Events"
Lowering a mapped opaque window will generate exposure events on any
opaque windows it formerly obscured.
.PP
Lowering a transparent window does not affect the screen.
.FD
.IN "Definitions" "XCircWindowUp"
.IN "XCircWindowUp"
XCircWindowUp (w)
	Window w;
.FN
\fIXCircWindowUp\fP raises the lowest mapped child of this window that is partially
or completely
obscured by another child.
Completely unobscured children are not affected.
.IN "Exposure Events"
This will
generate exposure events on that child (and its opaque descendents) if any part
of it was formerly obscured. 
Repeated executions lead to round robin raising.
.FD
.IN "Definitions" "XCircWindowDown"
XCircWindowDown (w)
	Window w;
.FN
Lower the highest mapped child of the window that partially or completely
obscures another child.
Completely unobscured children are not affected.
Generates exposure events on any window formerly obscured.
Repeated executions lead to round robin lowering.
