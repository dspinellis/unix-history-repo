.EQ
delim $$
.EN
.NH
Data Representation
.XS
Data Representation
.XE
.PP
.IN "Definitions" "Bitmap"
.IN "Definitions" "Pixmap"
A Bitmap is a single plane (bit) rectangle.
A Pixmap is a rectangle of pixels, and is therefore as ``deep'' as the
display.
For example, if your color display has four bits/pixel,
a Pixmap is a rectangle of four bit pixels.
In this implementation of the underlying protocol,
the depth of the display is limited to 16 bit planes.
.IN "Limitations" "Bits per Pixel"
The subroutine interface, however, is designed to support display hardware of
up to 32 bits per pixel.
The ``pixel value'' can range from 0 to $2 sup n$,
where \fIn\fP is the number of
bits per pixel.
.IN "Definitions" "Pixel values"
.IN "Definitions" "Cursors"
Cursors are used as mouse pointers and typically track the mouse; a
cursor is an arbitrary two-color shape with an arbitrary ``hot spot''.
.PP
.IN "Definitions" "Locator"
A Locator is
an absolute point on the screen, represented as <x,y> with the X-coordinate
in the most significant 16 bits and the Y-coordinate in the least significant
16 bits.
.EQ
delim off
.EN
.PP
.IN "Definitions" "Bitmap"
A Bitmap is represented by ((width + 15) / 16) * height * 2 bytes of data.
Each row is stored starting on a word (16 bit) boundary, and is padded to
the right to a 16 bit boundary.
It is stored top line to bottom line in memory.
It has a size of (width x height) pixels.
.IN "Macro" "BitmapSize()"
The macro \fIBitmapSize(width,height)\fP can be used to compute the size in
bytes of the Bitmap.
.IN "Definitions" "Bit Order"
The
bits are in scanline order, with each scanline padded if necessary to a
multiple of 16 bits.
Within a scanline, the bits are represented left to
right, stored in 16-bit words.
The least significant bit of a word is the leftmost visible pixel on the 
display.
Analysis showed that the bit reversal cpu time incurred on some
machine architectures was small relative to the
data transmission time,
so a fixed client bitmap representation was selected for portability's sake.
.PP
.IN "Definitions" "Pixmap"
.IN "Definitions" "XY-format"
.IN "Definitions" "Z-format"
A Pixmap can be represented in either `XY format' or `Z format'.
In XY format,
each plane is represented as a Bitmap, and the planes appear from most to least
significant bit order.
The total number of bytes is thus
((width + 15) / 16) * height * 2 * depth.
.IN "Macro" "XYPixmapSize()"
The macro \fIXYPixmapSize(width, height, planes)\fP can be used to
compute the size in bytes of a Pixmap in XY format.
In Z format, the pixels are in
scanline order, left to right within a scanline.
For hardware with 2 to 8
planes, each pixel value is represented by a single byte;
the total number of bytes
is thus width * height.
For hardware with 9 to 16 planes, each pixel value is
represented by a 16-bit word; the total number of bytes is thus
2 * width * height.
.IN "Macro" "BZPixmapSize()"
.IN "Macro" "WZPixmapSize()"
The macros \fIBZPixmapSize(width, height)\fP and 
\fIWZPixmapSize(width, height)\fP
can be used to compute the size in bytes of a pixmap in Z format for
2 to 8 bit planes and 9 to 16 bitplanes respectively.
.IN "Restrictions" "Z-format"
Z-format cannot be used on monochrome display hardware.
.PP
Background tiling uses Pixmaps to specify the
pattern.
Since you may want such patterns either relative to
.IN "Definitions" "Tile Mode"
the window or absolute, there is a concept of `tile mode' associated
with a window, allowing the pattern to be absolutely positioned to the window,
or relative to the parent window (often the root window).
This allows alignment of patterns either to the window you are working
in, or to the parent window.
Both are useful; icon windows often want relative alignment, while
normal windows usually want absolute alignment.
.PP
.IN "Macro" "BlackPixel"
.IN "Macro" "WhitePixel"
The pixel values \fIBlackPixel\fP and \fIWhitePixel\fP (0 and 1) are
.IN "File" "<X/X.h>
always defined on every display server, as defined in \fI<X/X.h>\fP.
These constants are intended for use in monochrome applications.
On color hardware, the colors of black and white may
be redefined by the user.
