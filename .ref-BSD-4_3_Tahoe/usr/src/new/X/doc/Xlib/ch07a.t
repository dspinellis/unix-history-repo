.NH
Cursor Definition
.XS
Cursors
.XE
.PP
These functions allow you to load and change cursors associated with
windows.
Each window can have a different cursor defined for it.
Whenever the mouse cursor is in a visible window, it will be set to the
cursor defined for that window.
If no cursor was defined for that window, the cursor
will be the that defined for the parent window.
.PP
From X's perspective, a cursor consists of a cursor shape, mask, colors
for the shape and mask, `hot spot', and logical function.
The cursor bitmap determines the shape of the cursor.
The mask bitmap determines the bits which will be modified by the cursor.
The colors determine the colors of the shape and mask.
The hot spot defines the point on the cursor which will be reported
when a mouse event occurs.
The logical function determines the boolean function applied to the cursor.
There may be (and probably are) limitations imposed by the hardware on
cursors as to size, whether a mask is implemented, and logical function.
.IN "XQueryCursorShape"
\fIXQueryCursorShape\fP can be used to find out what sizes are possible.
In the longer run, it is intended most ``standard'' cursors will
be stored as a special font.
.FD
.IN "Definitions" "XStoreCursor"
.IN "XStoreCursor"
.IN "Cursor Definition"
Cursor XStoreCursor(cursor, mask, xoff, yoff, foreground, background, func)
	Bitmap cursor, mask;
	int xoff, yoff;
	int foreground, background;
	int func;
.FN
.IN "Definitions" "Cursor"
This function stores a cursor in the window system.
The cursor is defined to be a two plane cursor
and therefore takes two bitmaps, the \fIcursor\fP, and the \fImask\fP.
The color of the mask and cursor are defined by the pixel values
\fIforeground\fP and \fIbackground\fP.
The logical function used is specified by the \fIfunc\fP argument.
If the \fImask\fP is zero, all pixels of the cursor are displayed.
The mask bitmap, if present, must be the same size as the
cursor bitmap.
The offsets define the point that actually corresponds to the mouse position;
this must be a point in the cursor bitmap.
The function is almost always GXcopy.
.PP
.IN "Resource ID's" "Freeing"
.IN "Freeing" "Resources"
The bitmaps can be freed immediately if no further explicit
references to them are to be made.
.LP
.IN "Cursor" "Limitations"
The components of the cursor may be transformed arbitrarily to meet hardware
limitations.
.FD
.IN "Limitations" "Cursor"
.IN "Definitions" "XQueryCursorShape"
.IN "XQueryCursorShape"
XQueryCursorShape (width, height, rwidth, rheight)
	int width, height;
	int *rwidth, *rheight;	/* RETURN */
.FN
Some displays allows larger cursors than other displays.
This call provides a way to find out what size cursors are actually
possible on the display.
This function returns the `closest' shape actually supported by the
display hardware a cursor.
For a cursor shape,
.IN "XStoreCursor"
it returns a Bitmap shape acceptable for \fIXStoreCursor\fP.
Applications should be prepared to use smaller cursors on displays which
cannot support large ones.
.FD
.IN "Definitions" "XFreeCursor"
.IN "XFreeCursor"
XFreeCursor(cursor)
	Cursor cursor;
.FN
The specified cursor is destroyed.
It should not be referred to again or an error will be generated.
.FD
.IN "Definitions" "XCreateCursor"
.IN "XCreateCursor"
Cursor XCreateCursor(width, height, cursor, mask, xoff, yoff, foreground, background, func)
	int width, height;
	short *cursor, *mask;
	int xoff, yoff;
	int foreground, background;
	int func;
.FN
Creates a cursor out of its component parts from data in the 
calling program.
The \fIcursor\fP bits and \fImask\fP bits should be in bitmap format.
This function is used if all components of a cursor are in the client
program, and saves round trip times in defining the cursor.
.FD
.IN "Definitions" "XDefineCursor"
.IN "XDefineCursor"
.IN "Window" "Define Cursor"
XDefineCursor (w, cursor)
	Window w;
	Cursor cursor;
.FN
If a cursor is specified, it will be used when the mouse is in the window.
.FD
.IN "Definitions" "XUndefineCursor"
.IN "XUndefineCursor"
.IN "Window" "Undefine Cursor"
XUndefineCursor (w)
	Window w;
.FN
\fIXUndefineCursor\fP undoes the effect of a previous \fIXDefineCursor\fP for
this window.
When the mouse is in the window, the parent's cursor will
now be used.
.PP
On the root window, with no cursor specified, the default cursor is restored.
