.NH 2
Storing and Freeing Bitmaps and Pixmaps
.PP
Bitmaps and Pixmaps are off screen resources which are used for a number of
operations.
These include defining cursors,
temporarily saving some part of the screen for later,
as tiling patterns, and as the source for certain raster operations.
.FD
.IN "Definitions" "XStorePixmapXY"
.IN "Definitions" "XStorePixmapZ"
.IN "XStorePixmapXY"
.IN "XStorePixmapZ"
Pixmap XStorePixmapXY (width, height, data)
	short *data;

Pixmap XStorePixmapZ (width, height, data)
	caddr_t data;
	int height, width, format;
.FN
This creates a Pixmap of the specified size and returns a Pixmap id.
The data must be in the format specified by the subroutine name; see
the section on data representation.
This data is stored in the window system for later use.
.PP
This function returns 0 if the pixmap could not be created.
The client should call \fIXFreePixmap\fP when finished with the pixmap.
.FD
.IN "Definitions" "XStoreBitmap"
.IN "XStoreBitmap"
Bitmap XStoreBitmap (width, height, data)
	short *data;
	int width, height;
.FN	
\fIXStoreBitmap\fP creates a bitmap for later use.
.IN "XFreeBitmap"
The client should call \fIXFreeBitmap\fP
when finished with it.
.PP
See the section on data representation.
.PP
This function returns 0 if the operation fails, or the Bitmap
if it succeeds.
.FD
.IN "Definitions" "XMakePixmap"
.IN "XMakePixmap"
Pixmap XMakePixmap (bitmap, foreground, background)
	Bitmap bitmap;
	int foreground, background;
.FN
\fIXMakePixmap\fP
returns a Pixmap constructed from a bitmap and two pixel values.
Wherever there is a one in the bitmap, the pixmap will have a pixel
value of the \fIforeground\fP, and wherever there is a zero in the bitmap,
the pixmap will have a pixel value of the \fIbackground\fP.
If zero is specified for the \fIbitmap\fP argument, it returns a Pixmap of
indefinite size suitable for use as a constant tiling pixmap.
.FD
.IN "Definitions" "XMakeTile"
.IN "XMakeTile"
Pixmap XMakeTile (pixel)
	int pixel;	/* pixel value */
.FN
This function returns a Pixmap suitable for use as a tiling argument.
.FD
.IN "Definitions" "XFreePixmap"
.IN "XFreePixmap"
XFreePixmap (pixmap)
	Pixmap pixmap;
.FN	
\fIXFreePixmap\fP frees all the storage associated with this Pixmap.
The Pixmap should never be referenced again.
.PP
.FD
.IN "Definitions" "XFreeBitmap"
.IN "XFreeBitmap"
XFreeBitmap (bitmap)
	Bitmap bitmap;
.FN
\fIXFreeBitmap\fP frees all the storage associated with this Bitmap.
The Bitmap should never be referenced again.
.FD
.IN "Definitions" "XCharBitmap"
.IN "XCharBitmap"
Bitmap XCharBitmap (font, char)
	Font font;
	int char;
.FN
This request creates a bitmap from the specified character of the
specified font.
.PP
.IN "Resource ID's" "Freeing"
.IN "Freeing" "Resources"
The font can be freed immediately if no further explicit
references to it are to be made.
