.NH 2
Screen Raster Operations
.LP
.FD
.IN "Definitions" "XClear"
.IN "XClear"
.IN "Window" "Clearing the Window"
XClear (w)
	Window w;
.FN
\fIXClear\fP clears the window and repaints it with the background.  
If the
window is transparent, it is cleared and repainted with its parent's
background. 
\fIXClear\fP never generates exposure events.
.FD
.IN "Definitions" "XPixSet"
.IN "Definitions" "XPixFill"
.IN "XPixSet"
.IN "XPixFill"
XPixSet (w, x, y, width, height, pixel)

XPixFill (w, x, y, width, height, pixel, clipmask, func, planes)
	Window w;
	int x, y, width, height;
	int pixel;	/* pixel value */
	Bitmap clipmask;	/* which bits to modify */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXPixFill\fP performs a display function in a region of the window.  
The \fIpixel\fP value is used as the source.
.IN "Icons"
If a \fIclipmask\fP bitmap is specified,
it defines the shape of the source and which pixels of the
destination will be affected.
This can be useful for defining non-rectangular icons.
.PP
\fIXPixSet\fP sets the area in all planes to the specified pixel with no
clipping mask. (\fIfunc\fP=\fIGXcopy\fP,
\fIAllPlanes\fP, and no \fIclipmask\fP).
.FD
.IN "Definitions" "XPixmapPut"
.IN "XPixmapPut"
.IN "Pixmap"
XPixmapPut (w, srcX, srcY, dstX, dstY, width, height, pixmap, func, planes)
	Window w;
	int srcX, srcY;
	int dstX, dstY;
	int width, height;
	Pixmap pixmap;
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXPixmapPut\fP
performs a display function on a specified area of the pixmap and
a specified area of the screen.
.FD
.IN "Limitiations" "Tile Shape"
.IN "Definitions" "XQueryTileShape"
.IN "XQueryTileShape"
XQueryTileShape (width, height, rwidth, rheight)
	int width, height;
	int rwidth, rheight;	/* RETURN */
.FN
This function returns the `closest' shape actually supported by the display
hardware for tiling.
Not all hardware will allow arbitrary shape pixmaps for tile patterns.
.FD
.IN "Definitions" "XTileSet"
.IN "Definitions" "XTileFill"
.IN "XTileSet"
.IN "XTileFill"
XTileSet (w, x, y, width, height, tile)

XTileFill (w, x, y, width, height, tile, cmask, func, planes)
	Window w;
	int x, y, width, height;
	Pixmap tile;
	Bitmap clipmask;
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXTileFill\fP performs a display function in a region of the window using a
repeating pattern defined by the \fItile\fP pixmap.
and a specified area of the screen.
The tiling origin is controlled by the window's \fItilemode\fP.
If a mask Bitmap is specified in the \fIclipmask\fP argument,
it defines which pixels of the
destination will be affected, and must be the same height and width as the
destination.
.IN "Tile Area"
.PP
\fIXTileSet\fP defaults to modifying all planes of the display with no
clipping mask.
(\fIfunc\fP = \fIGXcopy\fP, \fIAllPlanes\fP, \fIclipmask\fP = 0).
.FD
.IN "Definitions" "XMoveArea"
.IN "XMoveArea"
.IN "Definitions" "XCopyArea"
.IN "XCopyArea"
.IN "Window Raster Op"
.IN "Raster Op"
XMoveArea (w, srcX, srcY, dstX, dstY, width, height)

XCopyArea (w, srcX, srcY, dstX, dstY, width, height, func, planes)
	Window w;
	int srcX, srcY, dstX, dstY, width, height;
	int planes;	/* plane mask */
	int func;	/* display function */
.FN
\fIXCopyArea\fP copies one region of the window to another (possibly
overlapping) region of the same window, using the supplied display function
\fIfunc\fP.
.PP
If parts of the source region are obscured, the corresponding parts of
the destination are painted with the background tile.
.IN "ExposeCopy Event"
If a client
has called \fIXSelectInput\fP on this window with the \fIExposeCopy\fP bit set,
then
.IN "ExposeRegion Event"
\fIExposeRegion\fP events will be generated on any such parts of the
destination, and then an \fIExposeCopy event\fP will be generated.  All of
these events are guaranteed to be together in the stream, with no
intervening events.
This sequence makes it possible to `scroll' the contents of a window,
getting exposure events from where ever the window was obscured to
refresh those areas of the screen.
.PP
\fIXMoveArea\fP moves the pixels in the specified source area to the
specified destination.
(\fIfunc\fP = \fIGXcopy\fP, \fIplanes\fP = \fIAllPlanes\fP)
