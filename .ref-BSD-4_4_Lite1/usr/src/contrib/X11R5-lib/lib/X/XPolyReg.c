/* $XConsortium: XPolyReg.c,v 11.21 92/09/15 14:22:38 rws Exp $ */
/************************************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

************************************************************************/

#define LARGE_COORDINATE 1000000
#define SMALL_COORDINATE -LARGE_COORDINATE

#include "Xlibint.h"
#include "Xutil.h"
#include "region.h"
#include "poly.h"

/*
 *     InsertEdgeInET
 *
 *     Insert the given edge into the edge table.
 *     First we must find the correct bucket in the
 *     Edge table, then find the right slot in the
 *     bucket.  Finally, we can insert it.
 *
 */
static void
InsertEdgeInET(ET, ETE, scanline, SLLBlock, iSLLBlock)
    EdgeTable *ET;
    EdgeTableEntry *ETE;
    int scanline;
    ScanLineListBlock **SLLBlock;
    int *iSLLBlock;
{
    register EdgeTableEntry *start, *prev;
    register ScanLineList *pSLL, *pPrevSLL;
    ScanLineListBlock *tmpSLLBlock;

    /*
     * find the right bucket to put the edge into
     */
    pPrevSLL = &ET->scanlines;
    pSLL = pPrevSLL->next;
    while (pSLL && (pSLL->scanline < scanline)) 
    {
        pPrevSLL = pSLL;
        pSLL = pSLL->next;
    }

    /*
     * reassign pSLL (pointer to ScanLineList) if necessary
     */
    if ((!pSLL) || (pSLL->scanline > scanline)) 
    {
        if (*iSLLBlock > SLLSPERBLOCK-1) 
        {
            tmpSLLBlock = 
		  (ScanLineListBlock *)Xmalloc(sizeof(ScanLineListBlock));
            (*SLLBlock)->next = tmpSLLBlock;
            tmpSLLBlock->next = (ScanLineListBlock *)NULL;
            *SLLBlock = tmpSLLBlock;
            *iSLLBlock = 0;
        }
        pSLL = &((*SLLBlock)->SLLs[(*iSLLBlock)++]);

        pSLL->next = pPrevSLL->next;
        pSLL->edgelist = (EdgeTableEntry *)NULL;
        pPrevSLL->next = pSLL;
    }
    pSLL->scanline = scanline;

    /*
     * now insert the edge in the right bucket
     */
    prev = (EdgeTableEntry *)NULL;
    start = pSLL->edgelist;
    while (start && (start->bres.minor_axis < ETE->bres.minor_axis)) 
    {
        prev = start;
        start = start->next;
    }
    ETE->next = start;

    if (prev)
        prev->next = ETE;
    else
        pSLL->edgelist = ETE;
}

/*
 *     CreateEdgeTable
 *
 *     This routine creates the edge table for
 *     scan converting polygons. 
 *     The Edge Table (ET) looks like:
 *
 *    EdgeTable
 *     --------
 *    |  ymax  |        ScanLineLists
 *    |scanline|-->------------>-------------->...
 *     --------   |scanline|   |scanline|
 *                |edgelist|   |edgelist|
 *                ---------    ---------
 *                    |             |
 *                    |             |
 *                    V             V
 *              list of ETEs   list of ETEs
 *
 *     where ETE is an EdgeTableEntry data structure,
 *     and there is one ScanLineList per scanline at
 *     which an edge is initially entered.
 *
 */

static void
CreateETandAET(count, pts, ET, AET, pETEs, pSLLBlock)
    register int count;
    register XPoint *pts;
    EdgeTable *ET;
    EdgeTableEntry *AET;
    register EdgeTableEntry *pETEs;
    ScanLineListBlock   *pSLLBlock;
{
    register XPoint *top, *bottom;
    register XPoint *PrevPt, *CurrPt;
    int iSLLBlock = 0;
    int dy;

    if (count < 2)  return;

    /*
     *  initialize the Active Edge Table
     */
    AET->next = (EdgeTableEntry *)NULL;
    AET->back = (EdgeTableEntry *)NULL;
    AET->nextWETE = (EdgeTableEntry *)NULL;
    AET->bres.minor_axis = SMALL_COORDINATE;

    /*
     *  initialize the Edge Table.
     */
    ET->scanlines.next = (ScanLineList *)NULL;
    ET->ymax = SMALL_COORDINATE;
    ET->ymin = LARGE_COORDINATE;
    pSLLBlock->next = (ScanLineListBlock *)NULL;

    PrevPt = &pts[count-1];

    /*
     *  for each vertex in the array of points.
     *  In this loop we are dealing with two vertices at
     *  a time -- these make up one edge of the polygon.
     */
    while (count--) 
    {
        CurrPt = pts++;

        /*
         *  find out which point is above and which is below.
         */
        if (PrevPt->y > CurrPt->y) 
        {
            bottom = PrevPt, top = CurrPt;
            pETEs->ClockWise = 0;
        }
        else 
        {
            bottom = CurrPt, top = PrevPt;
            pETEs->ClockWise = 1;
        }

        /*
         * don't add horizontal edges to the Edge table.
         */
        if (bottom->y != top->y) 
        {
            pETEs->ymax = bottom->y-1;  /* -1 so we don't get last scanline */

            /*
             *  initialize integer edge algorithm
             */
            dy = bottom->y - top->y;
            BRESINITPGONSTRUCT(dy, top->x, bottom->x, pETEs->bres);

            InsertEdgeInET(ET, pETEs, top->y, &pSLLBlock, &iSLLBlock);

	    if (PrevPt->y > ET->ymax)
		ET->ymax = PrevPt->y;
	    if (PrevPt->y < ET->ymin)
		ET->ymin = PrevPt->y;
            pETEs++;
        }

        PrevPt = CurrPt;
    }
}

/*
 *     loadAET
 *
 *     This routine moves EdgeTableEntries from the
 *     EdgeTable into the Active Edge Table,
 *     leaving them sorted by smaller x coordinate.
 *
 */

static void
loadAET(AET, ETEs)
    register EdgeTableEntry *AET, *ETEs;
{
    register EdgeTableEntry *pPrevAET;
    register EdgeTableEntry *tmp;

    pPrevAET = AET;
    AET = AET->next;
    while (ETEs) 
    {
        while (AET && (AET->bres.minor_axis < ETEs->bres.minor_axis)) 
        {
            pPrevAET = AET;
            AET = AET->next;
        }
        tmp = ETEs->next;
        ETEs->next = AET;
        if (AET)
            AET->back = ETEs;
        ETEs->back = pPrevAET;
        pPrevAET->next = ETEs;
        pPrevAET = ETEs;

        ETEs = tmp;
    }
}

/*
 *     computeWAET
 *
 *     This routine links the AET by the
 *     nextWETE (winding EdgeTableEntry) link for
 *     use by the winding number rule.  The final 
 *     Active Edge Table (AET) might look something
 *     like:
 *
 *     AET
 *     ----------  ---------   ---------
 *     |ymax    |  |ymax    |  |ymax    | 
 *     | ...    |  |...     |  |...     |
 *     |next    |->|next    |->|next    |->...
 *     |nextWETE|  |nextWETE|  |nextWETE|
 *     ---------   ---------   ^--------
 *         |                   |       |
 *         V------------------->       V---> ...
 *
 */
static void
computeWAET(AET)
    register EdgeTableEntry *AET;
{
    register EdgeTableEntry *pWETE;
    register int inside = 1;
    register int isInside = 0;

    AET->nextWETE = (EdgeTableEntry *)NULL;
    pWETE = AET;
    AET = AET->next;
    while (AET) 
    {
        if (AET->ClockWise)
            isInside++;
        else
            isInside--;

        if ((!inside && !isInside) ||
            ( inside &&  isInside)) 
        {
            pWETE->nextWETE = AET;
            pWETE = AET;
            inside = !inside;
        }
        AET = AET->next;
    }
    pWETE->nextWETE = (EdgeTableEntry *)NULL;
}

/*
 *     InsertionSort
 *
 *     Just a simple insertion sort using
 *     pointers and back pointers to sort the Active
 *     Edge Table.
 *
 */

static int
InsertionSort(AET)
    register EdgeTableEntry *AET;
{
    register EdgeTableEntry *pETEchase;
    register EdgeTableEntry *pETEinsert;
    register EdgeTableEntry *pETEchaseBackTMP;
    register int changed = 0;

    AET = AET->next;
    while (AET) 
    {
        pETEinsert = AET;
        pETEchase = AET;
        while (pETEchase->back->bres.minor_axis > AET->bres.minor_axis)
            pETEchase = pETEchase->back;

        AET = AET->next;
        if (pETEchase != pETEinsert) 
        {
            pETEchaseBackTMP = pETEchase->back;
            pETEinsert->back->next = AET;
            if (AET)
                AET->back = pETEinsert->back;
            pETEinsert->next = pETEchase;
            pETEchase->back->next = pETEinsert;
            pETEchase->back = pETEinsert;
            pETEinsert->back = pETEchaseBackTMP;
            changed = 1;
        }
    }
    return(changed);
}

/*
 *     Clean up our act.
 */
static void
FreeStorage(pSLLBlock)
    register ScanLineListBlock   *pSLLBlock;
{
    register ScanLineListBlock   *tmpSLLBlock;

    while (pSLLBlock) 
    {
        tmpSLLBlock = pSLLBlock->next;
        Xfree((char *)pSLLBlock);
        pSLLBlock = tmpSLLBlock;
    }
}

/*
 *     Create an array of rectangles from a list of points.
 *     If indeed these things (POINTS, RECTS) are the same,
 *     then this proc is still needed, because it allocates
 *     storage for the array, which was allocated on the
 *     stack by the calling procedure.
 *
 */
static int PtsToRegion(numFullPtBlocks, iCurPtBlock, FirstPtBlock, reg)
    register int  numFullPtBlocks, iCurPtBlock;
    POINTBLOCK *FirstPtBlock;
    REGION *reg;
{
    register BOX  *rects;
    register XPoint *pts;
    register POINTBLOCK *CurPtBlock;
    register int i;
    register BOX *extents;
    register int numRects;
 
    extents = &reg->extents;
 
    numRects = ((numFullPtBlocks * NUMPTSTOBUFFER) + iCurPtBlock) >> 1;
 
    if (!(reg->rects = (BOX *)Xrealloc((char *)reg->rects, 
	    (unsigned) (sizeof(BOX) * numRects))))       return(0);
 
    reg->size = numRects;
    CurPtBlock = FirstPtBlock;
    rects = reg->rects - 1;
    numRects = 0;
    extents->x1 = MAXSHORT,  extents->x2 = MINSHORT;
 
    for ( ; numFullPtBlocks >= 0; numFullPtBlocks--) {
	/* the loop uses 2 points per iteration */
	i = NUMPTSTOBUFFER >> 1;
	if (!numFullPtBlocks)
	    i = iCurPtBlock >> 1;
	for (pts = CurPtBlock->pts; i--; pts += 2) {
	    if (pts->x == pts[1].x)
		continue;
	    if (numRects && pts->x == rects->x1 && pts->y == rects->y2 &&
		pts[1].x == rects->x2 &&
		(numRects == 1 || rects[-1].y1 != rects->y1) &&
		(!i || pts[2].y > pts[1].y)) {
		rects->y2 = pts[1].y + 1;
		continue;
	    }
	    numRects++;
	    rects++;
	    rects->x1 = pts->x;  rects->y1 = pts->y;
	    rects->x2 = pts[1].x;  rects->y2 = pts[1].y + 1;
	    if (rects->x1 < extents->x1)
		extents->x1 = rects->x1;
	    if (rects->x2 > extents->x2)
		extents->x2 = rects->x2;
        }
	CurPtBlock = CurPtBlock->next;
    }

    if (numRects) {
	extents->y1 = reg->rects->y1;
	extents->y2 = rects->y2;
    } else {
	extents->x1 = 0;
	extents->y1 = 0;
	extents->x2 = 0;
	extents->y2 = 0;
    }
    reg->numRects = numRects;
 
    return(TRUE);
}

/*
 *     polytoregion
 *
 *     Scan converts a polygon by returning a run-length
 *     encoding of the resultant bitmap -- the run-length
 *     encoding is in the form of an array of rectangles.
 */
Region 
XPolygonRegion(Pts, Count, rule)
    int       Count;                 /* number of pts           */
    XPoint     *Pts;		     /* the pts                 */
    int	rule;			     /* winding rule */
{
    Region region;
    register EdgeTableEntry *pAET;   /* Active Edge Table       */
    register int y;                  /* current scanline        */
    register int iPts = 0;           /* number of pts in buffer */
    register EdgeTableEntry *pWETE;  /* Winding Edge Table Entry*/
    register ScanLineList *pSLL;     /* current scanLineList    */
    register XPoint *pts;             /* output buffer           */
    EdgeTableEntry *pPrevAET;        /* ptr to previous AET     */
    EdgeTable ET;                    /* header node for ET      */
    EdgeTableEntry AET;              /* header node for AET     */
    EdgeTableEntry *pETEs;           /* EdgeTableEntries pool   */
    ScanLineListBlock SLLBlock;      /* header for scanlinelist */
    int fixWAET = FALSE;
    POINTBLOCK FirstPtBlock, *curPtBlock; /* PtBlock buffers    */
    POINTBLOCK *tmpPtBlock;
    int numFullPtBlocks = 0;
 
    if (! (region = XCreateRegion())) return (Region) NULL;

    /* special case a rectangle */
    pts = Pts;
    if (((Count == 4) ||
	 ((Count == 5) && (pts[4].x == pts[0].x) && (pts[4].y == pts[0].y))) &&
	(((pts[0].y == pts[1].y) &&
	  (pts[1].x == pts[2].x) &&
	  (pts[2].y == pts[3].y) &&
	  (pts[3].x == pts[0].x)) ||
	 ((pts[0].x == pts[1].x) &&
	  (pts[1].y == pts[2].y) &&
	  (pts[2].x == pts[3].x) &&
	  (pts[3].y == pts[0].y)))) {
	region->extents.x1 = min(pts[0].x, pts[2].x);
	region->extents.y1 = min(pts[0].y, pts[2].y);
	region->extents.x2 = max(pts[0].x, pts[2].x);
	region->extents.y2 = max(pts[0].y, pts[2].y);
	if ((region->extents.x1 != region->extents.x2) &&
	    (region->extents.y1 != region->extents.y2)) {
	    region->numRects = 1;
	    *(region->rects) = region->extents;
	}
	return(region);
    }

    if (! (pETEs = (EdgeTableEntry *)
	   Xmalloc((unsigned) (sizeof(EdgeTableEntry) * Count))))
	return (Region) NULL;

    pts = FirstPtBlock.pts;
    CreateETandAET(Count, Pts, &ET, &AET, pETEs, &SLLBlock);
    pSLL = ET.scanlines.next;
    curPtBlock = &FirstPtBlock;
 
    if (rule == EvenOddRule) {
        /*
         *  for each scanline
         */
        for (y = ET.ymin; y < ET.ymax; y++) {
            /*
             *  Add a new edge to the active edge table when we
             *  get to the next edge.
             */
            if (pSLL != NULL && y == pSLL->scanline) {
                loadAET(&AET, pSLL->edgelist);
                pSLL = pSLL->next;
            }
            pPrevAET = &AET;
            pAET = AET.next;
 
            /*
             *  for each active edge
             */
            while (pAET) {
                pts->x = pAET->bres.minor_axis,  pts->y = y;
                pts++, iPts++;
 
                /*
                 *  send out the buffer
                 */
                if (iPts == NUMPTSTOBUFFER) {
                    tmpPtBlock = (POINTBLOCK *)Xmalloc(sizeof(POINTBLOCK));
                    curPtBlock->next = tmpPtBlock;
                    curPtBlock = tmpPtBlock;
                    pts = curPtBlock->pts;
                    numFullPtBlocks++;
                    iPts = 0;
                }
                EVALUATEEDGEEVENODD(pAET, pPrevAET, y);
            }
            (void) InsertionSort(&AET);
        }
    }
    else {
        /*
         *  for each scanline
         */
        for (y = ET.ymin; y < ET.ymax; y++) {
            /*
             *  Add a new edge to the active edge table when we
             *  get to the next edge.
             */
            if (pSLL != NULL && y == pSLL->scanline) {
                loadAET(&AET, pSLL->edgelist);
                computeWAET(&AET);
                pSLL = pSLL->next;
            }
            pPrevAET = &AET;
            pAET = AET.next;
            pWETE = pAET;
 
            /*
             *  for each active edge
             */
            while (pAET) {
                /*
                 *  add to the buffer only those edges that
                 *  are in the Winding active edge table.
                 */
                if (pWETE == pAET) {
                    pts->x = pAET->bres.minor_axis,  pts->y = y;
                    pts++, iPts++;
 
                    /*
                     *  send out the buffer
                     */
                    if (iPts == NUMPTSTOBUFFER) {
                        tmpPtBlock = (POINTBLOCK *)Xmalloc(sizeof(POINTBLOCK));
                        curPtBlock->next = tmpPtBlock;
                        curPtBlock = tmpPtBlock;
                        pts = curPtBlock->pts;
                        numFullPtBlocks++;    iPts = 0;
                    }
                    pWETE = pWETE->nextWETE;
                }
                EVALUATEEDGEWINDING(pAET, pPrevAET, y, fixWAET);
            }
 
            /*
             *  recompute the winding active edge table if
             *  we just resorted or have exited an edge.
             */
            if (InsertionSort(&AET) || fixWAET) {
                computeWAET(&AET);
                fixWAET = FALSE;
            }
        }
    }
    FreeStorage(SLLBlock.next);	
    (void) PtsToRegion(numFullPtBlocks, iPts, &FirstPtBlock, region);
    for (curPtBlock = FirstPtBlock.next; --numFullPtBlocks >= 0;) {
	tmpPtBlock = curPtBlock->next;
	Xfree((char *)curPtBlock);
	curPtBlock = tmpPtBlock;
    }
    Xfree((char *)pETEs);
    return(region);
}
