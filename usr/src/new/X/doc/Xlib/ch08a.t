.NH
Color Map Manipulation
.XS
Color Map Manipulation
.XE
.PP
The following commands manipulate the representation of color on the
screen.
For each possible value a pixel may take on a display
(for example, if a display is 4 bits deep, pixel values 0 through 15 are
defined), there is a color cell in the color map.
The color map is the collection of the color cells.
A color cell consists of a triple of red, green and blue.
As each pixel is read out of display memory, its value
is taken and looked up in the color map.
The values of the cell determine what color is displayed on the screen.
On a multiplane  display with a black and white monitor (greyscale, but
not color), these values may be combined or not combined to determine the
brightness on the screen.
.PP
Note that the introduction of color changes the view a programmer should
take when dealing with a bitmap display.
For example, when printing text, you
write in a `color' (pixel value) rather than setting or clearing bits.
Hardware will impose limits (number of significant
bits, for example) on these values.
Typically, one allocates particular pixel values or sets of
values.
If read only, the pixel values may be shared among multiple applications.
If read/write, they are exclusively owned by the program,
and the color cell associated with the pixel value may be changed at will.
.PP
The functions in this section operate on a Color structure:
.DS
.TA .5i 2.5i
.ta .5i 2.5i
typedef struct _Color {
	int pixel;	/* pixel value */
	short red, green, blue;
} Color;
.DE
.IN "Data Structures" "Color"
.PP
The red, green and blue values are scaled between 0 and 65535; that is
`on full' in a color is a value of 65535 independent of the number
of bit planes of the display.
Half brightness in a color would be a value of 32767, and off of 0.
This representation gives uniform results for color values across
displays with different number of bit planes.
.IN "Color"
.LP
There are two ways of allocating color cells; explicitly as read only
.IN "XGetHardwareColor"
entries by pixel value (\fIXGetHardwareColor\fP) or read/write,
where you can allocate N colors and planes simultaneously.
.IN "Read/Write Color Map Cells"
Note that read write cells you allocate do not have defined colors until
.IN "XStoreColors"
set with \fIXStoreColors\fP.
.FD
.IN "Definitions" "XGetHardwareColor"
.IN "XGetHardwareColor"
.IN "Color Allocation"
Status XGetHardwareColor (def)
	Color *def;	/* RETURN */
.FN
When passed a color definition structure \fIdef\fP,
with red, green and blue values set,
fills in the pixel value with the closest color provided by the hardware.
.IN "Pixel Values"
The corresponding color map cell is read-only.
It returns 0 if there were some problem (typically lack of resources),
or non-zero if it succeeded.
.PP
.IN "Color Map"
.IN "Color Allocation"
.IN "Allocation" "Color Map"
.IN "Read-only Color Map Cells"
Read-only color map cells are shared among clients.
When the last client deallocates a shared cell, it is deallocated.
.EQ
delim $$
.EN
.FD
.IN "Definitions" "XGetColorCells"
.IN "XGetColorCells"
.IN "Color Allocation"
Status XGetColorCells (contig, ncolors, nplanes, planes, pixels)
	int contig;		/* 1 if the planes must be contiguous, 0 otherwise */
	int ncolors;
	int nplanes;
	int *planes;	/* RETURN */
	int pixels[ncolors];	/* RETURN pixel values */
.FN
Allocates $N * 2 sup P$ color map cells, where N (\fIncolors\fP)
is the number of colors
and P (\fInplanes\fP) is the number of planes specified.
It returns  a plane mask, which will be contiguous if requested.
Additional pixel values are obtained by or'ing in
one or more bits from the plane mask.
The initial colors for all of these cells is undefined.
.EQ
delim off
.EN
.PP
If zero colors are requested,
then the request allocates all cells with a pixel value
having at least one non-zero bit in the plane mask.
At most one such request will succeed.
This will typically be your favorite window manager.
Allocations are automatically deallocated when clients exit.
.FD
.IN "Definitions" "XGetColor"
.IN "XGetColor"
.IN "Color" "Naming"
Status XGetColor (colorname, hard_def, exact_def)
	char *colorname;
	Color *hard_def;	/* RETURN */
	Color *exact_def;	/* RETURN */
.FN
Applications often need to know what the correct value of "red" may be
on display by name to provide a good user interface.
Given a text string (\fIcolorname\fP, for example ``red''),
returns the Color structure in the supplied structure.
It uses a database on the server to resolve the color by name
from the file \fI/usr/lib/rgb\fP.
.IN "Color" "Data Base"
.IN "File" "/usr/lib/rgb.txt"
A text representation of this file can be found in \fI/usr/lib/rgb.txt\fP.
It returns 0 if there were some problem, or non-zero if
it succeeded.
.PP
Both the `exact' data base definition, and 
the `closest' color supported by the hardware are returned.
.FD
.IN "Definitions" "XFreeColors"
.IN "XFreeColors"
XFreeColors (pixels, npixels, planes)
	int pixels[];	/* pixel values */
	int npixels;
.FN
This request frees color map cells.  
The cells represented by pixels whose values
are in the array are freed.
If any planes are specified, they are freed.
.FD
.IN "Color" "Setting Cells"
.IN "Definitions" "XStoreColors"
.IN "XStoreColors"
XStoreColors (ncolors, defs)
	int ncolors;	/* number of color definitions */
	Color *defs;
.FN
Changes the colors of \fIncolors\fP pixels to the closest available hardware colors.
Note that these must be read/write cells.
.FD
.IN "Definitions" "XStoreColor"
.IN "XStoreColor"
XStoreColor (def)
	Color *def;	/* RETURN */
.FN
Sets the color of the specified pixel value to the closest available color.
Note that it must be a read/write cell.
.FD
.IN "Definitions" "XQueryColor"
.IN "XQueryColor"
Status XQueryColor (def)
	Color *def;	/* RETURN */
.FN
For the specified pixel value in the definition \fIdef\fP,
returns the color values for
a pixel value.
It returns zero if there were some problem, or non-zero if
it succeeded.
.FD
.IN "Color" "Getting Values"
.IN "Definitions" "XQueryColors"
.IN "XQueryColors"
XQueryColors (defs, ncolors)
	Color defs[];
	int ncolors;
.FN
For the specified pixel value in each definition,
this routine returns the color values for each pixel value.
.FD
.IN "Color" "Parsing Command Lines"
.IN "Definitions" "XParseColor"
.IN "XParseColor"
Status XParseColor (spec, def)
	char *spec;
	Color *def;	/* RETURN */
.FN
This subroutine is provided to make a standard user interface to
color simple.
Takes a string specification of a color, typically from a command line
or \fIXGetDefault\fP option, and returns the corresponding red, green, and blue
values,
suitable for a subsequent call to \fIXGetHardwareColor\fP or
\fIXStoreColor\fP.
The color can be specified either as a color name (as in \fIXGetColor\fP), or
as an initial sharp sign character following by a numeric specification,
in one of the following formats:
.nf
.TA .5i 3i
.ta .5i 3i
	#RGB	(4 bits each)
	#RRGGBB	(8 bits each)
	#RRRGGGBBB	(12 bits each)
	#RRRRGGGGBBBB	(16 bits each)
.fi
where R, G, and B represent single hexadecimal digits (upper or lower case).
When fewer than 16 bits each are specified, they represent the most significant
bits of the value.  For example, #3a7 is the same as #3000a0007000.
.PP
This routine will fail if the initial character is a sharp sign but the string
otherwise fails to fit of the above formats, or if the initial character is
not a sharp sign and the named color does not exist in the server's database.
