.NH
Graphic Output to a Window
.XS
Graphic Operations
.XE
.NH 2
Display Functions
.PP
.IN "Definitions" "Display Functions"
.IN "Definitions" "Source"
.IN "Definitions" "Destination"
You use display functions when you update  a section of the screen (the
`destination') with bits from somewhere else (the
`source').  
Many procedures below take one of these display functions as an argument.
The function defines how the new destination bits are to be
computed from the source bits and the old destination bits.
\fIGXcopy\fP is typically the most useful as it will work on a color display,
but special applications may use other functions,
particularly in concert with particular planes of a color display.
There are
.IN "File" "<X/X.h>
16 such functions, defined in \fI<X/X.h>\fP:
.KS
.L
.TS
center;
l c c
l c l.
Function Name	Hex Code	Operation
_
GXclear	0x0	0
GXand	0x1	src AND dst
GXandReverse	0x2	src AND NOT dst
GXcopy	0x3	src
GXandInverted	0x4	(NOT src) AND dst
GXnoop	0x5	dst
GXxor	0x6	src XOR dst
GXor	0x7	src OR dst
GXnor	0x8	(NOT src) AND NOT dst
GXequiv	0x9	(NOT src) XOR dst
GXinvert	0xa	NOT dst
GXorReverse	0xb	src OR NOT dst
GXcopyInverted	0xc	NOT src
GXorInverted	0xd	(NOT src) OR dst
GXnand	0xe	(NOT src) OR NOT dst
GXset	0xf	1
.TE
.PP
.KE
.IN "Pixel Values"
Many of the color functions below take either pixel values or
\fIplanes\fP as an argument.
The \fIplanes\fP is an integer which specifies which planes of the
display are to be modified, one bit per plane.
.IN "Definitions" "Plane Masks"
A monochrome display has only one plane and
will be the least significant bit of the word.
As planes are added to the display hardware, they will occupy more
significant bits in the plane mask.
.PP
.IN "Macro" "AllPlanes"
A macro constant \fIAllPlanes\fP can be used to refer to all planes of a display
simultaneously (``~0'').
.PP
.IN "Definitions" "Brush"
A `brush' is a rectangular area of certain allowable sizes
which will be painted in the line drawing subroutines at each point
of the line or curve.
The upper left corner of the brush follows
the line or curve.
The brush is defined by \fIheight\fP and \fIwidth\fP parameters
to many of the line- and curve-drawing routines.
.PP
If the width or height of the brush is greater than one pixel,
the display hardware
may paint some pixels more than once.
It is therefore inadvisable to use
such brushes with display functions such as \fIGXxor\fP and \fIGXinvert\fP,
which do
not have the same effect if applied more than once.
