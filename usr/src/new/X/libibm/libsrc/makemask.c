#ifndef lint
static char *rcsid_makemask_c = "$Header: makemask.c,v 10.1 86/11/19 10:42:30 jg Exp $";
#endif	lint
/* makemask.c -  builds a mask from vertex list
 *
 *	MakeMask     builds a mask from vertex list
 *	AddEdge	     adds edges to edge table for fill operation
 *	FillMask     fills mask using scan line algorithm
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

#include "private.h"
#include "makemask.h"
#include "pathlist.h"

/*
 * Build mask from vertex list
 */

BITMAP *
MakeMask(Verts, VertCount)
	Vertex *Verts;
	int VertCount;
{
    	register Vertex *LastPoint;
    	register Vertex *ThisPoint;
    	register Vertex *NextPoint;
	register Vertex *Poly;
	register struct Polygon *PolyData;
	register End;
	int Width = 0, Height = 0;
	int FirstScanLine = 10240;
	int Index, Size;
	BITMAP *BitMap;
	struct Polygon *FirstPoly;

#ifdef TRACE_X
        fprintf (stderr, "In MakeMask\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Allocate space for polygon pointers
	 */

    	FirstPoly = (struct Polygon *) calloc(VertCount << 1,
		   sizeof(struct Polygon));
        if (FirstPoly == NULL) {
            return (NULL);
        }

	/*
	 * Determine width and height of off-screen bitmap
	 * and the first scan line to be filled.
	 */

	PolyData = FirstPoly - 1;
	for(Index = 0; Index < VertCount; Index += 2) {
		if(Width < Verts[Index].x)
			Width = Verts[Index].x;
		if(Height < Verts[Index].y)
			Height = Verts[Index].y;
		if(FirstScanLine > Verts[Index].y)
			FirstScanLine = Verts[Index].y;
		if(Verts[Index].flags & START_OF_CLOSED_POLY) {
			(++PolyData)->PolyPoints = &Verts[Index];
			PolyData->PolyCount = 2;
		} else {
			PolyData->PolyCount += 2;
		}
	}
	(++PolyData)->PolyPoints = (Vertex *)NULL;

        /*
         * Allocate space for bitmap structure
         */

        BitMap = (BITMAP *) Xalloc (sizeof (BITMAP));

        /*
         * Fill in bitmap structure
	 *
	 * NOTE: The bitmaps width has been rounded up 
	 *       to the nearest 32 bit bound. This was done
	 *       so that the mask could be fill 32 bits at
	 *       a time instead of the usual 16.
         */

        BitMap->width = Width = ((Width + 32) >> 5) << 5;
        BitMap->height = ++Height;

        /*
         * Allocated space to hold bitimage data
         */
	
        if((BitMap->data = calloc(1, BitmapSize(Width, Height))) == NULL) {
            free ((caddr_t) BitMap);
            return (NULL);
        }

	/*
	 * Allocate space for edges and reset EdgeCount
	 */

    	Edges = (struct edge *) calloc(VertCount << 1, sizeof(struct edge));
        if (Edges == NULL) {
            free ((caddr_t) BitMap->data);
            free ((caddr_t) BitMap);
            return (NULL);
        }
        if ((EdgeTable = (struct edge **)calloc(Height + 1, 4)) == NULL) {
	    free((caddr_t) Edges);
            free ((caddr_t) BitMap->data);
            free ((caddr_t) BitMap);
            return (NULL);
        }
	EdgeCount = 0;

	/*
	 * Add the edges of all polygons to
	 * the edge table
	 */

	Poly = (PolyData = FirstPoly)->PolyPoints;
	do {
		/*
		 * Test for valid polygon
		 */

		if(PolyData->PolyCount > 5) {
			/*
			 * AddEdge() initialization before adding
			 * each polygon
			 */

			ShortenStartOfEdge = 0;
			Direction = Poly[PolyData->PolyCount - 2].y > Poly[0].y;
			End = PolyData->PolyCount - 4;
			LastPoint = Poly;
			ThisPoint = NextPoint =  Poly + 2;

			/*
	 	 	 * Add edges of this polygon to edge table
	 	 	 */

			while(End) {
				NextPoint += 2;
				AddEdge(LastPoint, ThisPoint, NextPoint);
				LastPoint = ThisPoint;
				ThisPoint = NextPoint;
				End -= 2;
			}
			NextPoint = &Poly[0];
			AddEdge(LastPoint, ThisPoint, NextPoint);
			LastPoint = ThisPoint;
			ThisPoint = NextPoint;
			NextPoint += 2;
			AddEdge(LastPoint, ThisPoint, NextPoint);
		}

		/*
		 * Increment pointer to next polygon to add
		 */

		Poly = (++PolyData)->PolyPoints;

	} while(Poly);

	if(EdgeCount) {
		/*
	 	 * Fill the mask
	 	 */

		FillMask(BitMap, FirstScanLine);

		/*
	 	 * Frame mask to remove any abnomalities
	 	 */

		for (Index = 0; Index < VertCount; Index += 2) {
			SinglePixelLine(BitMap, Verts[Index].x, Verts[Index].y,
					Verts[Index + 1].x, Verts[Index + 1].y,
					(CLIP *) 0, GXor, DrawSolidLine, 1, 0,
					0, 0, 0);
		}

		/* 
	 	 * Return mask bitmap
	 	 */

		return(BitMap);
	}

	/*
	 * Indicate nothing to fill
	 */

	return(NULL);
}

/*
 * Add edge to edge table
 */

static
AddEdge(LastPoint, ThisPoint, NextPoint)
    register Vertex *LastPoint;
    register Vertex *ThisPoint;
    register Vertex *NextPoint;
{
    register struct edge *NewEdge = &Edges[EdgeCount];

#ifdef TRACE_X
    fprintf (stderr, "In AddEdge\n");
    fflush (stderr);
#endif TRACE_X

    /*
     * Fill in new edge data
     */

    if (ThisPoint->y != LastPoint->y) {
	register Min_Y;

        if (ThisPoint->y < LastPoint->y) {
            Min_Y = ThisPoint->y;
            NewEdge->Min_X = SHIFT_LEFT_16(ThisPoint->x);
            NewEdge->Max_Y = LastPoint->y;
            NewEdge->Delta_X = SHIFT_LEFT_16(LastPoint->x - ThisPoint->x) /
				(LastPoint->y - ThisPoint->y);
        } else {
            Min_Y = LastPoint->y;
            NewEdge->Min_X = SHIFT_LEFT_16(LastPoint->x);
            NewEdge->Max_Y = ThisPoint->y;
            NewEdge->Delta_X = SHIFT_LEFT_16(ThisPoint->x - LastPoint->x) /
				(ThisPoint->y - LastPoint->y);	
        }

	/*
	 * Shorten edge if not maxima or minima (shortens end of edge)
	 */

        if((ThisPoint->y > LastPoint->y) ?
	   (NextPoint->y > ThisPoint->y) : (NextPoint->y < ThisPoint->y)) {
            	if (LastPoint->y > ThisPoint->y) {
                	Min_Y++;
                	NewEdge->Min_X += NewEdge->Delta_X;
            	} else	{
            		NewEdge->Max_Y--;
		}
        }

	/*
	 * Check to see if this edge needs to be shortened
	 * at its start.
	 */

	if(ShortenStartOfEdge) {
            	if (ThisPoint->y > LastPoint->y) {
                	Min_Y++;
                	NewEdge->Min_X += NewEdge->Delta_X;
            	} else	{
            		NewEdge->Max_Y--;
		}
		ShortenStartOfEdge = 0;
	}

	/*
	 * Save direction of this edge
	 */

	if (NextPoint->y == ThisPoint->y) {
		Direction = LastPoint->y > ThisPoint->y;
	}

	/*
	 * Insert edge into edge table
	 */

        if (NewEdge->Max_Y >= Min_Y) {
    	    register struct edge *EdgeList = (struct edge *)&EdgeTable[Min_Y];

	    while(EdgeList->NextEdge) {
		if(EdgeList->NextEdge->Min_X >= NewEdge->Min_X)
		    break;
		EdgeList = EdgeList->NextEdge;
	    }
	    NewEdge->NextEdge = EdgeList->NextEdge;
	    EdgeList->NextEdge = NewEdge;
	    EdgeCount++;
        }
    } else  { 
	/*
	 * This is a horizontal edge. Therefore, if there is
	 * a change of direction between the preceding edge and
	 * and the next edge the next edge must be shortened at its 
	 * start.
	 */

	if(NextPoint->y != ThisPoint->y &&
	   (Direction != (NextPoint->y > ThisPoint->y))) {
		ShortenStartOfEdge++;
	}
    }
}

/*
 * Fill polygon(s) to create mask
 */

static
FillMask(BitMap, FirstScanLine)
    BITMAP *BitMap;
    int FirstScanLine;
{
    register long *Bits;
    register Start, Stop;
    register FirstWord, LastWord;
    register struct edge *AET;
    struct edge *ActiveEdgeTable;
    struct edge *TempEdge;
    int NumberOfWords, CurrentScanLine, SortAgain;

#ifdef TRACE_X
    fprintf (stderr, "In FillMask\n");
    fflush (stderr);
#endif TRACE_X

    NumberOfWords = (BitMap->width + 31) >> 5;
    Bits = (long *)BitMap->data + FirstScanLine * NumberOfWords;
    ActiveEdgeTable = EdgeTable[FirstScanLine];
    CurrentScanLine = FirstScanLine;

    while(EdgeCount) {
	    /*
	     * Fill all ranges for current scan line
	     */

    	    AET = (struct edge *)&ActiveEdgeTable;

	    while(AET->NextEdge && AET->NextEdge->NextEdge) {
		Start = ROUND_16(AET->NextEdge->Min_X);
		Stop = ROUND_16(AET->NextEdge->NextEdge->Min_X);

		if ((FirstWord = Start >> 5) < (LastWord = Stop >> 5)) {
			Bits[FirstWord] |= RightMasks[Start & 0x1F];
			while (++FirstWord < LastWord)
				Bits[FirstWord] = 0xFFFFFFFF;
			Bits[LastWord] |= LeftMasks[Stop & 0x1F];
		} else {
			Bits[FirstWord] |= (RightMasks[Start & 0x1F] &
				LeftMasks[Stop & 0x1F]);
		}

		AET = AET->NextEdge->NextEdge;
	    }
	    Bits += NumberOfWords;

	    /*
	     * Remove finished edges from active edge table 
	     * and add any new edges for next scan line.
	     */

    	    AET = (struct edge *)&ActiveEdgeTable;
    	    while(AET->NextEdge) {
		    if(AET->NextEdge->Max_Y == CurrentScanLine) {
			    AET->NextEdge = AET->NextEdge->NextEdge;
			    EdgeCount--;
		    } else  {
			    AET->NextEdge->Min_X += AET->NextEdge->Delta_X;
			    AET = AET->NextEdge;
		    }
    	    }
    	    AET->NextEdge = EdgeTable[++CurrentScanLine];

	    /*
	     * Sort active edge table
	     */

	    do {
		SortAgain = 0;
    	    	AET = (struct edge *)&ActiveEdgeTable;
    	    	while(AET->NextEdge && AET->NextEdge->NextEdge) {
		    if(AET->NextEdge->Min_X > AET->NextEdge->NextEdge->Min_X) {
			TempEdge = AET->NextEdge;
		    	AET->NextEdge = AET->NextEdge->NextEdge;
			TempEdge->NextEdge = AET->NextEdge->NextEdge;
			AET->NextEdge->NextEdge = TempEdge;
			SortAgain = 1;
		    }
		    AET = AET->NextEdge;
    	    	}
	    }while(SortAgain);
    }
    free((caddr_t) EdgeTable);
    free((caddr_t) Edges);
}
