.NH 2
Line Drawing and Fill Operations
.FD
.IN "Definitions" "XLine"
.IN "XLine"
.IN "Line" "Drawing"
XLine (w, x1, y1, x2, y2, width, height, pixel, func, planes)
	Window w;
	int width, height;	/* brush */
	int x1, y1, x2, y2;	/* beginning and end points */
	int pixel;	/* pixel value */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXLine\fP draws a line from pixel (\fIx1\fP,\fIy1\fP) to pixel
(\fIx2\fP,\fIy2\fP) inclusive
in the specified window, using the specified display function \fIfunc\fP.
.PP
The line will be drawn in the color specified by the \fIpixel\fP value.
.PP
\fIWidth\fP and \fIheight\fP
specify the size of a brush to be drawn along the line.
See the description of brushes at the beginning of this section.
.PP
The plane mask \fIplanes\fP
specifies which planes of the display will be modified.
.FD
.IN "Definitions" "XDraw"
.IN "XDraw"
.IN "Line" "Drawing"
XDraw (w, vlist, vcount, width, height, pixel, func, planes)
	Window w;
	int vcount;	/* number of vertices */
	Vertex *vlist;
	int height, width;	/* brush */
	int pixel;	/* pixel value */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
\fIXDraw\fP draws an arbitrary polygon or curve using the specified function.
The figure drawn is defined by the list of Vertexes \fIvlist\fP
that the caller supplies.
The points will be connected by lines as specified in the flags
in the vertex structure.
.PP
\fIWidth\fP and \fIheight\fP
specify the size of a brush to be drawn along the line.
See the description of brushes at the beginning of this section.
.PP
The plane mask \fIplanes\fP
specifies which planes of the display will be modified.
.PP
.IN "File" "<X/X.h>"
Each Vertex is a structure (defined in \fI<X/X.h>\fP):
.IN "Data Structures" "Vertex"
.IN "Vertex"
.nf
.sp
   typedef struct _Vertex {
	short x, y;
	unsigned short flags;
   } Vertex;
.fi
.LP
\fIX\fP and \fIy\fP are the coordinates of the vertex,
relative to either the upper
.IN "Vertex" "Flags"
left inside corner of the window (if the \fIVertexRelative\fP
bit in \fIflags\fP
 is 0) or
the previous vertex (if \fIVertexRelative\fP is 1).
.PP
.IN "File" "<X/X.h>"
.IN "Vertex" "Flags"
The flags, as defined in \fI<X/X.h>\fP, are as follows:
.LP
.TS
center;
l c l.
VertexRelative	0x0001	else absolute
VertexDontDraw	0x0002	else draw
VertexCurved	0x0004	else straight
VertexStartClosed	0x0008	else not
VertexEndClosed	0x0010	else not
VertexDrawLastPoint	0x0020	else don't
.TE
.IN "VertexRelative"
\fIVertexRelative\fP is described above.
If bit 0 is not set, the coordinates are absolute (relative to the window).
The first vertex must be an absolute vertex.
.PP
.IN "VertexDontDraw"
If the \fIVertexDontDraw\fP bit is 1, no line or curve is drawn from the previous
vertex to this one.  This is analogous to picking up the pen and moving
to another place before drawing another line.
.PP
.IN "VertexCurved"
If the \fIVertexCurved\fP bit is 1, a spline algorithm is used to draw a smooth
curve from the previous vertex, through this one, to the next vertex.
Otherwise, a straight line is drawn from the previous vertex.
It makes
sense to set \fIVertexCurved\fP to 1 only if a previous and next vertex are
both defined (either explicitly in the array, or through the definition
of a closed curve--see below.)
.PP
It is permissible for \fIVertexDontDraw\fP
bits and \fIVertexCurved\fP bits to both be 1; 
this is useful if you want to define the `previous point' for the smooth
curve, but don't want an actual curve drawing to start until this
point.
.PP
.IN "VertexStartClosed"
If \fIVertexStartClosed\fP bit is 1, then this point marks the beginning of a
closed curve.  
This vertex MUST be followed later in the array by
another vertex whose absolute coordinates are identical, and which has
.IN "VertexEndClosed"
\fIVertexEndClosed\fP bit of 1.
The points in between form a cycle for the purpose
of determining predecessor and successor vertices for the spline
algorithm.
It makes sense to set \fIVertexStartClosed\fP bit or \fIVertexEndClosed\fP bit
to 1 only if \fIVertexCurved\fP is also 1.
.PP
Normally, the end point of a curve or line is not drawn, since it is
probably the beginning point of the next curve or line.
.IN "Display Functions"
This is important if
a display function such as \fIGXinvert\fP or \fIGXxor\fP
is used, since drawing a
point twice with such a function produces a different result than
drawing it just once.
If \fIVertexDrawLastPoint\fP is 1, the end point is drawn.  
.PP
.PP
The line will be drawn in the color specified by the pixel value.
.FD
.IN "Definitions" "XAppendVertex"
.IN "XAppendVertex"
int XAppendVertex(vertices,nvert)
	Vertex vertices[];
	int nvert;
.FN
This function appends \fIvertices\fP to the output buffer.
It is the responsibility of higher level subroutines to determine
if the previous draw command was of the same type.
This subroutine is NOT intended for normal users of this library,
but as a hook for certain libraries built on this library.
This function returns a 0 if there is no Draw command currently in
the output buffer, -1 if the vertices would not fit before the end of
the buffer, or \fInvert\fP if it was successful at appending the vertices.
.FD
.IN "Definitions" "XClearVertexFlag"
.IN "XClearVertexFlag"
XClearVertexFlag()
.FN
This routine clears the state of the flag marking if it is safe to
append vertices.
Again, this is principally for use of more sophisticated libraries.
.sp 2
.PP
When drawing lines, one may want to draw `dashed' or `dotted' lines.
This means you want to be able to some how specify the length of the
dashes, and the distance between them.
These functions therefore take  a `pattern' specifier.
.PP
.IN "Macro" "DashedLine"
.IN "Macro" "DottedLine"
.IN "Macro" "SolidLine"
\fIXlib.h\fP has some predefined pattern values (actually macro invocations)
of \fIDashedLine\fP, \fIDottedLine\fP,
\fISolidLine\fP.
.IN "Definitions" "Patterns"
.IN "Patterns"
Since these predefined patterns are macro invocations and evaluate
to an integer,
you can use the predefined patterns to initialize static data.
.FD
.IN "Definitions" "XMakePattern"
.IN "XMakePattern"
.IN "Patterns"
Pattern XMakePattern(pattern, length, multiplier)
	int pattern, length, multiplier;
.FN
In this macro, \fIpattern\fP is a bit string of the specified \fIlength\fP (at
most 16 bits).
The \fImultiplier\fP specifies how many times each bit in the
string should be repeated before moving to the next bit.
It cannot be more than 4096.
The bits are used least significant bit first, and repeated as many times
as needed.
.FD
.IN "Definitions" "XDrawPatterned"
.IN "Definitions" "XDrawDashed"
.IN "XDrawPatterned"
.IN "XDrawDashed"
.IN "Patterns"
XDrawPatterned (w, vlist, vcount, width, height, pixel, altpix, pattern, func, planes)
	int altpix;

XDrawDashed (w, vlist, vcount, width, height, pixel, pattern, func, planes)
	Window w;
	int pixel;	/* pixel value */
	int height, width;	/* brush size */
	int vcount;	/* number of vertices */
	Vertex *vlist;
	Pattern pattern;	/* pattern specification */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
.IN "XDraw"
These functions are similar to \fIXDraw\fP,
except they draw a dashed rather than a
solid line.
The pattern is a value encoding the pattern to be used.
.PP
For a dashed line, the destination is only updated when the pattern bit
is one.
.PP
For a patterned line,
the alternate source pixel value is used when the pattern bit is zero.
.FD
.IN "Definitions" "XDrawTiled"
.IN "Definitions" "XDrawFilled"
.IN "XDrawTiled"
.IN "XDrawFilled"
XDrawTiled (w, vlist, vcount, tile, func, planes)
	Pixmap tile;

XDrawFilled (w, vlist, vcount, pixel, func, planes)
	Window w;
	Vertex *vlist;
	int vcount;	/* number of vertices */
	int pixel;	/* pixel value */
	int func;	/* display function */
	int planes;	/* plane mask */
.FN
.IN "Polygons"
\fIXDrawFilled\fP draws arbitrary polygons or curves and fills them with 
the specified \fIpixel\fP value.
.IN "Filled Polygons"
XDrawTiled draws arbitrary polygons or curves and fills them with 
the specified \fItile\fP Pixmap.
.IN "XDraw"
The \fIvlist\fP, \fIvcount\fP, \fIfunc\fP and \fIplanes\fP
arguments are identical to \fIXDraw\fP.
Note that there may be implementation restrictions on the
nature of the \fItile\fP Pixmap.
(See \fIXQueryTileShape\fP.)
The vertex list should consist only of one or more closed regions.
A point is defined to be inside a region if an infinite ray with the point
as an origin crosses the path of the region an odd number of times.
.IN "XQueryTileShape"
.FD
.IN "Definitions" "XQueryBrushShape"
.IN "XQueryBrushShape"
.IN "Limitations" "Brush Shape"
XQueryBrushShape (width, height, cwidth, height)
	int width, height;
	int *cwidth, *cheight;	/* RETURN of `closest' legal size */
.FN
This function returns the `closest' shape actually supported by the
display hardware for brushes, since not all hardware is capable of supporting
arbitrary size brushes.
Painting lines using brushes of widths not supported by the hardware
has unpredictable results.
