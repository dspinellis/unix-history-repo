.NH 2
Moving Bits and Pixels to and From the Screen
.PP
A few  programs may want to manipulate pixels that they later display on
the screen.
This section describes calls which will move pixels from the program to
the window system, or from the window system to the program.
Care must be taken to adhere to the data representation described at the
beginning of this document to keep programs portable between machine
architectures.
If you always manipulate Bitmaps and XY-format Pixmaps
in terms of 16 bit quantities, you will be quite safe.
.FD
.IN "Definitions" "XPixmapBitsPutXY"
.IN "XPixmapBitsPutXY"
.IN "Definitions" "XPixmapBitsPutZ"
.IN "XPixmapBitsPutZ"
.IN "Raster Op to Screen"
XPixmapBitsPutXY (w, x, y, width, height, data, mask, func, planes)
	short *data;

XPixmapBitsPutZ (w, x, y, width, height, data, mask, func, planes)
	Window w;
	int x, y, width, height;
	caddr_t data;
	Bitmap mask;	/* which bits to modify */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXPixmapBitsPut\fP copies client-supplied bits into a window according to the
specified display function in the format specified by the name of the 
procedure.
.PP
.IN "XY-Format Pixmap"
.IN "Z-Format Pixmap"
See the section on data representation for the
difference between XY and Z format pixmaps.
.PP
The area modified will be controlled by the \fImask\fP argument, if it is
nonzero.
Only the bits in the \fImask\fP will be modified on the screen.
.IN "Icon" "Display"
This is often useful for Icon generation.
.FD
.IN "Definitions" "XBitmapBitsPut"
.IN "XBitmapBitsPut"
XBitmapBitsPut (w, x, y, width, height, data, foreground, background, mask, func, planes)
	Window w;
	int x, y, width, height, foreground, background;
	short *data;
	Bitmap mask;	/* which bits to modify */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
Performs a function in a region of the window using a pixmap defined by a 
bitmap and a pair of source pixels defining the \fIforeground\fP
and \fIbackground\fP
pixel values.
The plane mask defines which destination bit planes are affected.
The display function is computed on each bit plane.
.PP
See the section on data representation for the format of Bitmap data.
.PP
The area modified will be controlled by the \fImask\fP argument.
Only the bits in the \fImask\fP will be modified on the screen.
This is often useful for Icon generation.
.IN "Icon" "Display"
.FD
.IN "Definitions" "XPixmapSave"
.IN "XPixmapSave"
.IN "Saving Bits from Screen"
Pixmap XPixmapSave (w, x, y, width, height)
	Window w;
	int x, y, width, height;
.FN
\fIXPixmapSave\fP creates a Pixmap from the given portion of the window.
The
pixmap will contain a direct image of that portion of the screen,
including any visible portions of subwindows or overlapping windows, so
this routine should be used with caution.
Its main use will probably be
.IN "Menus"
.IN "XUnmapTransparent"
in conjunction with \fIXUnmapTransparent\fP, in implementing pop-up menus or
other `temporary' windows which save the bits under them and then
restore those bits when destroyed.
.PP	
The window must be mapped, and it must be true that if there were no
overlapping windows or subwindows, the specified portion of the window
would be fully visible on the screen.
.PP
The function returns the Pixmap id for the saved pixmap, or 0
if it failed.
.FD
.IN "Definitions" "XPixmapGetXY"
.IN "XPixmapGetXY"
.IN "Definitions" "XPixmapGetZ"
.IN "XPixmapGetZ"
.IN "Retrieving Bits from the Screen"
XPixmapGetXY (w, x, y, width, height, data)

XPixmapGetZ (w, x, y, width, height, data)
	Window w;
	int x, y, width, height;
	short *data;	/* RETURN */
.FN
\fIXPixmapGet\fP returns the pixmap in the specified format into the specified
area of memory.
.PP
See the section on data representation to determine
how large the area of memory must be reserved for the returned data.
This function is intended for screen dump purposes.
.PP
The window must be mapped and it must be the case that if there were
no subwindows or overlapping windows, the specified portion of the window
would be fully visible on the screen.
