/* $Header: pathlist.h,v 10.1 86/11/19 10:46:00 jg Exp $ */
/* pathlist.h - Constants, macros, typedefs and globals
 *              used by PathListConverter()
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
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

/*
 * Path list Segment structure
 */

typedef struct _Segment {
	int Index;
	int Type;
	int Count;
} Segment;

/*
 * Segment types
 */

#define LINE_SEGMENT		0
#define OPEN_CURVE_SEGMENT	1
#define CLOSED_CURVE_SEGMENT	2

/*
 * Default values for segemnt allocation
 */

#define INITIAL_SEGMENTS	1024
#define ADDITIONAL_SEGMENTS     512

/*
 * This macro starts a new segment and when required 
 * expands the size of the segment table
 */

#define StartNewSegment(type, index, count) {				   \
    if(++SegmentIndex == MaxSegments) {				   	   \
	Segment *TempSeg;						   \
									   \
	MaxSegments += 1 + ADDITIONAL_SEGMENTS;	   			   \
	TempSeg = (Segment *)realloc((caddr_t)SegmentTable,	 	   \
	             (unsigned)(MaxSegments * sizeof(Segment)));	   \
	if(TempSeg == NULL) {   					   \
	   return(NULL);						   \
	}								   \
        CurrentSegment = &((SegmentTable = TempSeg)[SegmentIndex]); 	   \
    } else {								   \
	CurrentSegment++;						   \
    }									   \
    CurrentSegment->Index = (index);				 	   \
    CurrentSegment->Type = (type);				 	   \
    CurrentSegment->Count = (count);				 	   \
}

/*
 * Defaults for spline vertex buffer allocation
 */

#define INITIAL_SPLINE_VERTS	4096
#define ADDITIONAL_SPLINE_VERTS	1024

/*
 * This macro increases the size of the spline vertex buffer
 * if it is to small to handle "VertexCount" entries.
 */

#define GrowSplineVertexBuffer(CurrentVertex, VertexCount) {		   \
	if((SplineVertexIndex + (VertexCount)) > MaxSplineVerts) {	   \
	    Vertex *TempVerts;						   \
	    								   \
	    MaxSplineVerts += ((VertexCount) - (MaxSplineVerts -	   \
			SplineVertexIndex)) + ADDITIONAL_SPLINE_VERTS;	   \
				    		      			   \
	    TempVerts = (Vertex *)realloc((caddr_t)SplineVertexBuffer,	   \
		    (unsigned)(MaxSplineVerts * sizeof(Vertex)));	   \
            if(TempVerts == NULL) {					   \
	        return(NULL);						   \
            }								   \
	    (CurrentVertex) =						   \
		&((SplineVertexBuffer = TempVerts)[SplineVertexIndex]);    \
	}								   \
}

/*
 * Vertex type flags
 */

#define VERTEX_TYPE_MASK        0x001C
#define LINE                    0x0000
#define CURVE                   0x0004
#define START_CLOSED_CURVE      0x000C
#define END_CLOSED_CURVE        0x0014
#define START_OF_CLOSED_POLY    0x8000

/*
 * glocal variables
 */

static Vertex *SplineVertexBuffer;
static int SplineVertexIndex;
static int MaxSplineVerts;
static int SplineUsed;
static Segment *SegmentTable;
