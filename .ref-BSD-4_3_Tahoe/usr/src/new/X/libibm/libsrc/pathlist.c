#ifndef lint
static char *rcsid_pathlist_c = "$Header: pathlist.c,v 10.1 86/11/19 10:43:18 jg Exp $";
#endif	lint
/* pathlist.c - Coverter for vertex list
 *
 *	PathListConverter	Convert a list of vertices 
 *				into absolute striaght lines
 *	Spline			Generates a series of line segments
 *				that make up a smooth curve
 *	Matrix			Utility rtn used by Spline to interpolate
 *				points along the curve
 *
 *	Author:
 *	    Scott Bates
 *	    Brown University
 *	    IRIS, Box 1946
 *	    Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "pathlist.h"

/*
 * Convert vertex list
 */

PathListConverter(verts, vertcount, xbase, ybase, newverts, newvertcount, type)
    register Vertex *verts;
    int vertcount;
    short xbase, ybase;
    Vertex **newverts;
    int *newvertcount;
    int type;
{
    register Vertex *ThisVertex = verts;
    register Vertex *LastVertex;
    register Vertex *NewVerts;
    register Segment *CurrentSegment;
    register i, j;
    int VertexCount;
    int VertexIndex = 0;
    int SegmentIndex = 0;
    int CurvedSegments = 0;
    int MaxSegments = INITIAL_SEGMENTS;
    int TotalVertexCount = vertcount;

#ifdef TRACE_X
    fprintf (stderr, "In PathListConverter\n");
    fflush (stderr);
#endif TRACE_X

    /*
     * Perform initial allocation of segment table
     */

    CurrentSegment = SegmentTable =
	(Segment *)malloc(MaxSegments * sizeof(Segment));
    if(SegmentTable == NULL) {
	return(NULL);
    }

    /*
     * Prime first segment table entry
     */

    ThisVertex->x += xbase;
    ThisVertex->y += ybase;

    CurrentSegment->Index = 0;
    CurrentSegment->Count = 1;

    switch(ThisVertex->flags & VERTEX_TYPE_MASK) {
    case(LINE):			/* First segment is a line */

	CurrentSegment->Type = LINE_SEGMENT;
	break;

    case(START_CLOSED_CURVE):	/* First segment is a closed curve */

	CurrentSegment->Type = CLOSED_CURVE_SEGMENT;
	CurvedSegments++;
	break;

    default:			/* First segment is a line */

	/*
	 * turn off bogus flags and make this first segment a line
	 */

	ThisVertex->flags &= ~(START_CLOSED_CURVE | END_CLOSED_CURVE);
	CurrentSegment->Type = LINE_SEGMENT;
    }

    /*
     * Convert remaining vertices to absolute coordinates and
     * divide them up into there appropriate segemnts.
     */

    do {
	if(++VertexIndex < vertcount) {
	    /*
	     * Move on to next vertex and save current vertex
	     */

	    LastVertex = ThisVertex++;
	} else {
	    /*
	     * Conversion has completed. If the last segment was a
	     * curved segment verify it before exiting loop.
	     */

	    if(CurrentSegment->Type == CLOSED_CURVE_SEGMENT &&
	      ((ThisVertex->flags & VERTEX_TYPE_MASK) != END_CLOSED_CURVE ||
	      CurrentSegment->Count < 3)) {
		    return(NULL);
	    } else if(CurrentSegment->Type == OPEN_CURVE_SEGMENT &&
	      CurrentSegment->Count < 3) {
		    return(NULL);
	    }
	    break;
	}

	/*
	 * Make Vertex an absolute coordinate
	 */

	if(ThisVertex->flags & VertexRelative) {
	    ThisVertex->x += LastVertex->x;
	    ThisVertex->y += LastVertex->y;
	} else	{
	    ThisVertex->x += xbase;
	    ThisVertex->y += ybase;
	}

	/*
	 * If this is the last vertex turn off any bogus flags
	 * before processing it
	 */

	if((VertexIndex + 1) == vertcount &&
	  CurrentSegment->Type != CLOSED_CURVE_SEGMENT) {
	     ThisVertex->flags &= ~(END_CLOSED_CURVE);
	}

	/*
	 * add this vertex to the current segement or start a new one.
	 */

	switch(ThisVertex->flags & VERTEX_TYPE_MASK) {

	case(LINE):			/* This vertex is a LINE */
	    switch(LastVertex->flags & VERTEX_TYPE_MASK) {

	    case(LINE):			/* Last vertex was a LINE */
		/*
		 * Add this vertex to the current segment
		 */

		CurrentSegment->Count++;
		break;

	    case(CURVE):		/* Last vertex was a CURVE */
		/*
		 * If the current segment type is a closed curve
		 * convert it to an open curve segment.
		 */

		if(CurrentSegment->Type == CLOSED_CURVE_SEGMENT) {
		    CurrentSegment->Type = OPEN_CURVE_SEGMENT;
		    if(CurrentSegment->Index > 0) {
			CurrentSegment->Index--;
			CurrentSegment->Count++;
		    }
		}
		if(++CurrentSegment->Count < 3) {
		    return(NULL);
		}

	    case(END_CLOSED_CURVE):	/* Last vertex was a END_CLOSED_CURVE */
		/*
		 * Start a line segment
		 */

		StartNewSegment(LINE_SEGMENT, VertexIndex, 1);
		break;

	    case(START_CLOSED_CURVE):	/* Last vertex was start closed curve */
		/*
		 * Convert the current segment to a line segment
		 */

		CurrentSegment->Type = LINE_SEGMENT;
		CurrentSegment->Count++;
		CurvedSegments--;
	    }
	    break;

	case(CURVE):			/* This vertex was a curve */

	    switch(LastVertex->flags & VERTEX_TYPE_MASK) {

	    case(LINE):			/* Last vertex was a line or */
	    case(END_CLOSED_CURVE):	/* end closed curve	     */
		/*
		 * Start an open curve segment
		 */

		StartNewSegment(OPEN_CURVE_SEGMENT, VertexIndex - 1, 2);
		CurvedSegments++;
		break;

	    case(CURVE):		/* Last vertex was a curve or start */
	    case(START_CLOSED_CURVE):	/* closed curve			    */
		/*
		 * Add this vertex to current segment
		 */

		CurrentSegment->Count++;

	    }
	    break;

	case(START_CLOSED_CURVE):	/* This vertex is start closed curve */

	    switch(LastVertex->flags & VERTEX_TYPE_MASK) {

	    case(CURVE):		/* Last vertex was a curve */
		/* If the current segment type is a closed curve
		 * convert it to a open curve segment. Then start 
		 * a closed curve segment using this vertex.
		 */

		if(CurrentSegment->Type == CLOSED_CURVE_SEGMENT) {
		    CurrentSegment->Type = OPEN_CURVE_SEGMENT;
		    if(CurrentSegment->Index > 0) {
			CurrentSegment->Index--;
			CurrentSegment->Count++;
		    }
		}
		if(++CurrentSegment->Count < 3) {
		    return(NULL);
		}

	    case(LINE):			/* Last vertex was a line or */
	    case(END_CLOSED_CURVE):	/* end closed curve	     */
		/*
		 * Start closed curve segemnt
		 */

		StartNewSegment(CLOSED_CURVE_SEGMENT, VertexIndex, 1);
		CurvedSegments++;
		break;

	    case(START_CLOSED_CURVE):	/* Last vertex was start closed curve */
		/*
		 * Indicate to caller that there was a
		 * path list error.
		 */

		return(NULL);
	    }
	    break;

	case(END_CLOSED_CURVE):		/* This vertex is end closed curve */
	    /*
	     * Add this vertex to the current segment
	     */

	    ++CurrentSegment->Count;

	    /*
	     * Last vertex was a curve
	     */

	    if((LastVertex->flags & VERTEX_TYPE_MASK) == CURVE) {
		/*
		 * Vaild vertex count of curved segment
		 */

		if(CurrentSegment->Count < 3) {
		      return(NULL);
		} 

		/*
		 * If the current segment is a closed segment
		 * validate that the last vertex of the segment
		 * equals the first.
		 */

		if(CurrentSegment->Type == CLOSED_CURVE_SEGMENT) {
		    if(verts[CurrentSegment->Index].x != verts[VertexIndex].x ||
		      verts[CurrentSegment->Index].y != verts[VertexIndex].y) {
			return(NULL);
		    }
		} else {
		    /*
		     * Start a line segment using this vertex
		     */

		    StartNewSegment(LINE_SEGMENT, VertexIndex, 1);
		}
	    }
	    break;

	default:
	    /*
	     * Indicate to caller that there was a
	     * path list error.
	     */

	    return(NULL);
	}
    } while(1);

    /*
     * If there are curved segments in this path list
     * then perform the required setup and call the spline 
     * rtn . 
     */

    SplineUsed = 0;
    if(CurvedSegments) {
	Vertex *Vertex_A;
	Vertex *Vertex_B;
	Vertex *Vertex_C;
	Vertex *Vertex_D;
	int Count;

	/*
	* Initial allocating of the spline vertex buffer
	*/

	SplineVertexIndex = 0;
	MaxSplineVerts = INITIAL_SPLINE_VERTS;
	SplineVertexBuffer = (Vertex *)malloc(MaxSplineVerts * sizeof(Vertex));
	if(SplineVertexBuffer == NULL) {
	    return(NULL);
	}
	SplineUsed++;

	/*
	* Loop thru all the path list segments looking 
	* for all open and closed curve segments.
	*/

	for(i = 0; i <= SegmentIndex; i++) {

	switch((CurrentSegment = &SegmentTable[i])->Type) {

	case(LINE_SEGMENT):
	    /*
	    * Ignore line segments
	    */
    
	    continue;
    
	case(OPEN_CURVE_SEGMENT):
	    /*
	     * Generate a series of line segments
	     * that represent the open curve defined
	     * by this segment.
	     */

	    Count = 0;
	    Vertex_A = &verts[CurrentSegment->Index +
		    CurrentSegment->Count - 1];
	    Vertex_B = &verts[CurrentSegment->Index];
	    Vertex_C = Vertex_B + 1;
	    Vertex_D = Vertex_C + 1;
	    CurrentSegment->Index = SplineVertexIndex;
	    j = CurrentSegment->Count - 2;

	    while(1) {
		Count += Spline(Vertex_A, Vertex_B, Vertex_C, Vertex_D);
		if(--j == 0) {
		break;
		}
		Vertex_A = Vertex_B;
		Vertex_B = Vertex_C;
		Vertex_C = Vertex_D++;
	    }
	    break;

	case(CLOSED_CURVE_SEGMENT):
	    /*
	     * Generate a series of line segments
	     * that represent the closed curve defined
	     * by this segment.
	     */

	    Count = 0;
	    LastVertex = &verts[CurrentSegment->Index + 1];
	    Vertex_A = &verts[CurrentSegment->Index +
		    CurrentSegment->Count - 2];
	    Vertex_B = &verts[CurrentSegment->Index];
	    CurrentSegment->Index = SplineVertexIndex;
	    Vertex_C = Vertex_B + 1;
	    Vertex_D = Vertex_C;
	    j = CurrentSegment->Count - 2;

	    while(j--) {
		Vertex_D++;
		Count += Spline(Vertex_A, Vertex_B, Vertex_C, Vertex_D);
		Vertex_A = Vertex_B;
		Vertex_B = Vertex_C;
		Vertex_C = Vertex_D;
	    }
	    Vertex_D = LastVertex;
	    Count += Spline(Vertex_A, Vertex_B, Vertex_C, Vertex_D);

	    }

	    /*
	     * Adjust the current segment count and
	     * increase the total vertex to reflect 
	     * the new points generated by the spline rtn.
	     */

	    CurrentSegment->Count = Count;
	    TotalVertexCount += Count;

	    /*
	     * If there are no more curved segments exit early
	     */

	    if(--CurvedSegments == 0) {
		break;
	    }
	}
    }

    /*
     * Allocate space for new vertex list
     */

    NewVerts = *newverts = (Vertex *)malloc((TotalVertexCount << 1) *
		sizeof(Vertex));
    if(NewVerts == NULL) {
	return(NULL);
    }

    /*
     * Loop thru coordinate list
     */

    for(VertexCount = 0, i = 0; i <= SegmentIndex; i++) {
	/* 
	 * If this segment is a line segment get verts from original
	 * vertex list else use the spline vertex list.
	 */

	if((CurrentSegment = &SegmentTable[i])->Type == LINE_SEGMENT) {
	    ThisVertex = &verts[CurrentSegment->Index];
	} else {
	    ThisVertex = &SplineVertexBuffer[CurrentSegment->Index];
	}

	/*
	 * Get segment vertex count
	 */

	j = CurrentSegment->Count;

	/*
	 * Convert path list to fill format
	 */

	if(type == FILL_PATH_LIST) {
	    do { 
		/*
		 * Something to draw ?
		 */

		if(ThisVertex->flags & VertexDontDraw) {
		    /*
		     * Indicate start of closed polygon
		     */

		    ThisVertex->flags |= START_OF_CLOSED_POLY;

		    /*
		     * Increment vertex pointers
		     */

		    LastVertex = ThisVertex++;

		    /*
		     * Continue processing of current segment
		     */

		    continue;
		}

		/*
		 * If this vertex is not a dup save the 
		 * line segment represented by the last 
		 * vertex and this one in NewVerts.
		 * If it is a dup ignore this vertex.
		 */

		if(ThisVertex->x != LastVertex->x ||
		   ThisVertex->y != LastVertex->y) {

		    /*
		     * Save start and end points of
		     * visible line
		     */

		    *NewVerts++ = *LastVertex;
		    *NewVerts++ = *ThisVertex;
	
		    /*
		     * Increment vertex count
		     */

		    VertexCount += 2;

		    /*
		     * Increment vertex pointers
		     */

		    LastVertex = ThisVertex++;
		} else {
		    /*
		     * Ignore this vertex
		     */

		    ThisVertex++;
		}
	    }while(--j);
	} else {
	    do {
		/*
		 * Something to draw ?
		 */

		if(ThisVertex->flags & VertexDontDraw) {
		    /*
		     * Increment vertex pointers
		     */

		    LastVertex = ThisVertex++;

		    /*
		     * Continue processing current segment
		     */

		    continue;
		}

		if(ThisVertex->x != LastVertex->x ||
		   ThisVertex->y != LastVertex->y) {
		    /*
		     * Save start and end points of
		     * visible line
		     */

		    *NewVerts++ = *LastVertex;
		    *NewVerts = *ThisVertex;

		    /*
		     * Shorten line by one point if
		     * "VertexDrawLastPoint" flag is off
		     */

		    if(!(ThisVertex->flags & VertexDrawLastPoint)) {
			int DeltaX, DeltaY;
			int SignX = 0, SignY = 0;
	
			if((DeltaX = ThisVertex->x - LastVertex->x) < 0) {
			    SignX = -1;
			    DeltaX = -DeltaX;
			}

			if((DeltaY = ThisVertex->y - LastVertex->y) < 0) {
			    SignY = -1;
			    DeltaY = -DeltaY;
			}
	
			if (DeltaX > DeltaY) {
			    SignX < 0 ? NewVerts->x++ : NewVerts->x--;
			    if ((DeltaX >> 1) <= DeltaY) {
				SignY < 0 ? NewVerts->y++ : NewVerts->y--;
			    }
			} else if (DeltaX < DeltaY ) {
			    SignY < 0 ? NewVerts->y++ : NewVerts->y--;
			    if ((DeltaY >> 1) <= DeltaX) {
				SignX < 0 ? NewVerts->x++ : NewVerts->x--;
			    }
			} else {
			    if (DeltaX > 0) {
				SignX < 0 ? NewVerts->x++ : NewVerts->x--;
				SignY < 0 ? NewVerts->y++ : NewVerts->y--;
			    } else {
				/*
				 * Line now has a length of zero
				 * so we skip this one. Back up the
				 * buffer pointer and move on to the
				 * next vertex
				 */

				NewVerts--;

				/*
				 * Increment vertex pointers
				 */
	    
				LastVertex = ThisVertex++;
				continue;
			    }
			}
		    }
	
		    /*
		     * Advance buffer pointer
		     */
	
		    NewVerts++;
	
		    /*
		     * Increment vertex count
		     */

		    VertexCount += 2;

		    /*
		     * Increment vertex pointers
		     */

		    LastVertex = ThisVertex++;
		} else {
		    /*
		     * Ignore this vertex
		     */

		    ThisVertex++;
		}
	    } while(--j);
	}
    }

    /*
     * Save final vertex count and free any resources used
     * during path list conversion.
     */

    *newvertcount = VertexCount;
    free((caddr_t)SegmentTable);
    if(SplineUsed) {
	free((caddr_t)SplineVertexBuffer);
    }
    return(1);
}

/*
 * Generate a series of points that will form
 * a curve between Vertex_B and Vertex_C.
 */

static
Spline(Vertex_A, Vertex_B, Vertex_C, Vertex_D)
    register Vertex *Vertex_A;
    register Vertex *Vertex_B;
    register Vertex *Vertex_C;
    register Vertex *Vertex_D;
{
    register Vertex *Verts = &SplineVertexBuffer[SplineVertexIndex];
    register i;
    int nls = 1;
    int Delta_X, Delta_Y;
    long Matrix();

#ifdef TRACE_X
    fprintf (stderr, "In Spline\n");
    fflush (stderr);
#endif TRACE_X

    GrowSplineVertexBuffer(Verts, 2);
    *Verts++ = *Vertex_B;
    SplineVertexIndex++;
    if((Vertex_C->flags & VertexDontDraw) == 0) {
	/* 
	 * Compute how many points to generate
	 * based on the largest delta change in either 
	 * the X or Y direction. This number represents
	 * the maximum number of points to be generated
	 * and may be reduced by an increasing amount
	 * as the change (delta) gets larger. This allows
	 * us to generate fewer points and therefore 
	 * improve performace and still generate
	 * quality smooth curve.
	 */

	if((Delta_X = Vertex_C->x - Vertex_B->x) < 0) {
	    Delta_X = -Delta_X;
	}

	if((Delta_Y = Vertex_C->y - Vertex_B->y) < 0) {
	    Delta_Y = -Delta_Y;
	}

	if(Delta_X > Delta_Y) {
	    if(Delta_X > 64) {
		nls = Delta_X >> 3;
	    } else if(Delta_X > 32) {
		nls = Delta_X >> 2;
	    } else if(Delta_X > 16) {
		nls = Delta_X >> 1;
	    } else {
		nls = Delta_X;
	    }
	} else	{
	    if(Delta_Y > 64) {
		nls = Delta_Y >> 3;
	    } else if(Delta_Y > 32) {
		nls = Delta_Y >> 2;
	    } else if(Delta_Y > 16) {
		nls = Delta_Y >> 1;
	    } else {
		nls = Delta_Y;
	    }
	}

	/*
	 * Generate the actual points
	 */

	if(nls) {
	    GrowSplineVertexBuffer(Verts, nls);
	    for(i = 1; i < nls; i++, Verts++) {
		Verts->x = Matrix((long)Vertex_A->x, (long)Vertex_B->x,
		    (long)Vertex_C->x, (long)Vertex_D->x, nls, i);
		Verts->y = Matrix((long)Vertex_A->y, (long)Vertex_B->y,
		    (long)Vertex_C->y, (long)Vertex_D->y, nls, i);
		Verts->flags = Vertex_C->flags & VertexDrawLastPoint;
	    }
	    SplineVertexIndex += nls;
	} else {
	    nls = 1;
	    SplineVertexIndex++;
	}
    } else {
	SplineVertexIndex++;
    }
    *Verts = *Vertex_C;

    /*
     * Return the number of points generated 
     */

    return(nls + 1);
}

/* 
 * This rtn performs the matrix math require to interpolate a
 * point on the curve represented by a, b, c, and d. The generated 
 * point will be between points b and c.
 */

static long
Matrix(a, b, c, d, nls, i)
    register long a, b, c, d;
    register nls;
    int i;
{
    register long p = SHIFT_LEFT_16(-a + b - c + d);

#ifdef TRACE_X
        fprintf (stderr, "In Matrix\n");
        fflush (stderr);
#endif TRACE_X

    p = PERCENT_16(p, i, nls) + SHIFT_LEFT_16((a << 1) - (b << 1) + c - d);
    p = PERCENT_16(p, i, nls) + SHIFT_LEFT_16(-a + c);
    return(ROUND_16(PERCENT_16(p, i, nls) + SHIFT_LEFT_16(b)));
}
