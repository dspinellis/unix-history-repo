/***********************************************************
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

******************************************************************/
/* $XConsortium: mfbline.c,v 5.12 91/08/13 18:49:01 keith Exp $ */
#include "X.h"

#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "regionstr.h"
#include "scrnintstr.h"
#include "mistruct.h"

#include "mfb.h"
#include "maskbits.h"

/* single-pixel lines on a color frame buffer

   NON-SLOPED LINES
   horizontal lines are always drawn left to right; we have to
move the endpoints right by one after they're swapped.
   horizontal lines will be confined to a single band of a
region.  the code finds that band (giving up if the lower
bound of the band is above the line we're drawing); then it
finds the first box in that band that contains part of the
line.  we clip the line to subsequent boxes in that band.
   vertical lines are always drawn top to bottom (y-increasing.)
this requires adding one to the y-coordinate of each endpoint
after swapping.

   SLOPED LINES
   when clipping a sloped line, we bring the second point inside
the clipping box, rather than one beyond it, and then add 1 to
the length of the line before drawing it.  this lets us use
the same box for finding the outcodes for both endpoints.  since
the equation for clipping the second endpoint to an edge gives us
1 beyond the edge, we then have to move the point towards the
first point by one step on the major axis.
   eventually, there will be a diagram here to explain what's going
on.  the method uses Cohen-Sutherland outcodes to determine
outsideness, and a method similar to Pike's layers for doing the
actual clipping.

*/

#define OUTCODES(result, x, y, pbox) \
    if (x < pbox->x1) \
	result |= OUT_LEFT; \
    else if (x >= pbox->x2) \
	result |= OUT_RIGHT; \
    if (y < pbox->y1) \
	result |= OUT_ABOVE; \
    else if (y >= pbox->y2) \
	result |= OUT_BELOW;

#define round(dividend, divisor) \
( (((dividend)<<1) + (divisor)) / ((divisor)<<1) )
#define ceiling(m,n)  (((m)-1)/(n) + 1)

/*
#define SignTimes(sign, n) ((sign) * ((int)(n)))
*/

#define SignTimes(sign, n) \
    ( ((sign)<0) ? -(n) : (n) )

#define SWAPINT(i, j) \
{  register int _t = i; \
   i = j; \
   j = _t; \
}

#define SWAPPT(i, j) \
{  DDXPointRec _t; \
   _t = i; \
   i = j; \
   j = _t; \
}
   

void
#ifdef POLYSEGMENT
mfbSegmentSS (pDrawable, pGC, nseg, pSeg)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		nseg;
    register xSegment	*pSeg;
#else
mfbLineSS (pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		mode;		/* Origin or Previous */
    int		npt;		/* number of points */
    DDXPointPtr pptInit;
#endif
{
    int nboxInit;
    register int nbox;
    BoxPtr pboxInit;
    register BoxPtr pbox;
#ifndef POLYSEGMENT
    register DDXPointPtr ppt;	/* pointer to list of translated points */
#endif

    unsigned int oc1;		/* outcode of point 1 */
    unsigned int oc2;		/* outcode of point 2 */

    int *addrl;		/* address of destination pixmap */
    int nlwidth;		/* width in longwords of destination pixmap */
    int xorg, yorg;		/* origin of window */

    int adx;		/* abs values of dx and dy */
    int ady;
    int signdx;		/* sign of dx and dy */
    int signdy;
    int e, e1, e2;		/* bresenham error and increments */
    int len;			/* length of segment */
    int axis;			/* major axis */

				/* a bunch of temporaries */
    register int y1, y2;
    register int x1, x2;
    RegionPtr cclip;
    int		    alu;

    if (!(pGC->planemask & 1))
	return;

    cclip = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip;
    alu = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->rop;
    pboxInit = REGION_RECTS(cclip);
    nboxInit = REGION_NUM_RECTS(cclip);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrl = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrl = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    xorg = pDrawable->x;
    yorg = pDrawable->y;
#ifdef POLYSEGMENT
    while (nseg--)
#else
    ppt = pptInit;
    x2 = ppt->x + xorg;
    y2 = ppt->y + yorg;
    while(--npt)
#endif
    {
	nbox = nboxInit;
	pbox = pboxInit;

#ifdef POLYSEGMENT
	x1 = pSeg->x1 + xorg;
	y1 = pSeg->y1 + yorg;
	x2 = pSeg->x2 + xorg;
	y2 = pSeg->y2 + yorg;
	pSeg++;
#else
	x1 = x2;
	y1 = y2;
	++ppt;
	if (mode == CoordModePrevious)
	{
	    xorg = x1;
	    yorg = y1;
	}
	x2 = ppt->x + xorg;
	y2 = ppt->y + yorg;
#endif

	if (x1 == x2)
	{
	    /* make the line go top to bottom of screen, keeping
	       endpoint semantics
	    */
	    if (y1 > y2)
	    {
		register int tmp;

		tmp = y2;
		y2 = y1 + 1;
		y1 = tmp + 1;
#ifdef POLYSEGMENT
		if (pGC->capStyle != CapNotLast)
		    y1--;
#endif
	    }
#ifdef POLYSEGMENT
	    else if (pGC->capStyle != CapNotLast)
		y2++;
#endif
	    /* get to first band that might contain part of line */
	    while ((nbox) && (pbox->y2 <= y1))
	    {
		pbox++;
		nbox--;
	    }

	    if (nbox)
	    {
		/* stop when lower edge of box is beyond end of line */
		while((nbox) && (y2 >= pbox->y1))
		{
		    if ((x1 >= pbox->x1) && (x1 < pbox->x2))
		    {
			int y1t, y2t;
			/* this box has part of the line in it */
			y1t = max(y1, pbox->y1);
			y2t = min(y2, pbox->y2);
			if (y1t != y2t)
			{
			    mfbVertS (alu,
				      addrl, nlwidth, 
				      x1, y1t, y2t-y1t);
			}
		    }
		    nbox--;
		    pbox++;
		}
	    }
#ifndef POLYSEGMENT
	    y2 = ppt->y + yorg;
#endif
	}
	else if (y1 == y2)
	{
	    /* force line from left to right, keeping
	       endpoint semantics
	    */
	    if (x1 > x2)
	    {
		register int tmp;

		tmp = x2;
		x2 = x1 + 1;
		x1 = tmp + 1;
#ifdef POLYSEGMENT
		if (pGC->capStyle != CapNotLast)
		    x1--;
#endif
	    }
#ifdef POLYSEGMENT
	    else if (pGC->capStyle != CapNotLast)
		x2++;
#endif

	    /* find the correct band */
	    while( (nbox) && (pbox->y2 <= y1))
	    {
		pbox++;
		nbox--;
	    }

	    /* try to draw the line, if we haven't gone beyond it */
	    if ((nbox) && (pbox->y1 <= y1))
	    {
		int tmp;

		/* when we leave this band, we're done */
		tmp = pbox->y1;
		while((nbox) && (pbox->y1 == tmp))
		{
		    int	x1t, x2t;

		    if (pbox->x2 <= x1)
		    {
			/* skip boxes until one might contain start point */
			nbox--;
			pbox++;
			continue;
		    }

		    /* stop if left of box is beyond right of line */
		    if (pbox->x1 >= x2)
		    {
			nbox = 0;
			break;
		    }

		    x1t = max(x1, pbox->x1);
		    x2t = min(x2, pbox->x2);
		    if (x1t != x2t)
		    {
			mfbHorzS (alu,
				  addrl, nlwidth, 
				  x1t, y1, x2t-x1t);
		    }
		    nbox--;
		    pbox++;
		}
	    }
#ifndef POLYSEGMENT
	    x2 = ppt->x + xorg;
#endif
	}
	else	/* sloped line */
	{
	    adx = x2 - x1;
	    ady = y2 - y1;
	    signdx = sign(adx);
	    signdy = sign(ady);
	    adx = abs(adx);
	    ady = abs(ady);

	    if (adx > ady)
	    {
		axis = X_AXIS;
		e1 = ady << 1;
		e2 = e1 - (adx << 1);
		e = e1 - adx;

	    }
	    else
	    {
		axis = Y_AXIS;
		e1 = adx << 1;
		e2 = e1 - (ady << 1);
		e = e1 - ady;
	    }

	    /* we have bresenham parameters and two points.
	       all we have to do now is clip and draw.
	    */

	    while(nbox--)
	    {
		oc1 = 0;
		oc2 = 0;
		OUTCODES(oc1, x1, y1, pbox);
		OUTCODES(oc2, x2, y2, pbox);
		if ((oc1 | oc2) == 0)
		{
		    if (axis == X_AXIS)
			len = adx;
		    else
			len = ady;
#ifdef POLYSEGMENT
		    if (pGC->capStyle != CapNotLast)
			len++;
#endif
		    mfbBresS (alu,
			  addrl, nlwidth,
			  signdx, signdy, axis, x1, y1,
			  e, e1, e2, len);
		    break;
		}
		else if (oc1 & oc2)
		{
		    pbox++;
		}
		else
		{
	    	    /*
	     	     * let the mfb helper routine do our work;
	     	     * better than duplicating code...
	     	     */
	    	    BoxRec box;
    	    	    DDXPointRec pt1Copy;	/* clipped start point */
    	    	    DDXPointRec pt2Copy;	/* clipped end point */
    	    	    int err;			/* modified bresenham error term */
    	    	    int clip1, clip2;		/* clippedness of the endpoints */
    	    	
    	    	    int clipdx, clipdy;		/* difference between clipped and
				       	       	   unclipped start point */
		    DDXPointRec	pt1;
    	    	
    	
	    	    pt1.x = pt1Copy.x = x1;
		    pt1.y = pt1Copy.y = y1;
	    	    pt2Copy.x = x2;
		    pt2Copy.y = y2;
	    	    box.x1 = pbox->x1;
	    	    box.y1 = pbox->y1;
	    	    box.x2 = pbox->x2-1;
	    	    box.y2 = pbox->y2-1;
	    	    clip1 = 0;
	    	    clip2 = 0;
    	
		    if (mfbClipLine (pbox, box,
				     &pt1, &pt1Copy, &pt2Copy, 
				     adx, ady, signdx, signdy, axis,
				     &clip1, &clip2) == 1)
		    {
		    	if (axis == X_AXIS)
			    len = abs(pt2Copy.x - pt1Copy.x);
		    	else
			    len = abs(pt2Copy.y - pt1Copy.y);
    
#ifdef POLYSEGMENT
		    	if (clip2 != 0 || pGC->capStyle != CapNotLast)
			    len++;
#else
		    	len += (clip2 != 0);
#endif
		    	if (len)
		    	{
			    /* unwind bresenham error term to first point */
			    if (clip1)
			    {
			    	clipdx = abs(pt1Copy.x - x1);
			    	clipdy = abs(pt1Copy.y - y1);
			    	if (axis == X_AXIS)
				    err = e+((clipdy*e2) + ((clipdx-clipdy)*e1));
			    	else
				    err = e+((clipdx*e2) + ((clipdy-clipdx)*e1));
			    }
			    else
			    	err = e;
			    mfbBresS   
				     (alu,
				      addrl, nlwidth,
				      signdx, signdy, axis, pt1Copy.x, pt1Copy.y,
				      err, e1, e2, len);
		    	}
		    }
		    pbox++;
		}
	    } /* while (nbox--) */
	} /* sloped line */
    } /* while (nline--) */

#ifndef POLYSEGMENT

    /* paint the last point if the end style isn't CapNotLast.
       (Assume that a projecting, butt, or round cap that is one
        pixel wide is the same as the single pixel of the endpoint.)
    */

    if ((pGC->capStyle != CapNotLast) &&
	((ppt->x + xorg != pptInit->x + pDrawable->x) ||
	 (ppt->y + yorg != pptInit->y + pDrawable->y) ||
	 (ppt == pptInit + 1)))
    {
	unsigned int _mask;
	int _incr;

	if (alu == RROP_BLACK)
		_mask = rmask[x2 & 0x1f];
	else
		_mask = mask[x2 & 0x1f];
	_incr = (y2 * nlwidth) + (x2 >> 5);

	nbox = nboxInit;
	pbox = pboxInit;
	while (nbox--)
	{
	    if ((x2 >= pbox->x1) &&
		(y2 >= pbox->y1) &&
		(x2 <  pbox->x2) &&
		(y2 <  pbox->y2))
	    {
		addrl += _incr;
		switch(alu)
		{
		    case RROP_BLACK:
		        *addrl &= _mask;
			break;
		    case RROP_WHITE:
		        *addrl |= _mask;
			break;
		    case RROP_INVERT:
		        *addrl ^= _mask;
			break;
		}
		break;
	    }
	    else
		pbox++;
	}
    }
#endif
}

/*
 * Draw dashed 1-pixel lines.
 */

void
#ifdef POLYSEGMENT
mfbSegmentSD (pDrawable, pGC, nseg, pSeg)
    DrawablePtr	pDrawable;
    register GCPtr	pGC;
    int		nseg;
    register xSegment	*pSeg;
#else
mfbLineSD( pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr pDrawable;
    register GCPtr pGC;
    int mode;		/* Origin or Previous */
    int npt;		/* number of points */
    DDXPointPtr pptInit;
#endif
{
    int nboxInit;
    register int nbox;
    BoxPtr pboxInit;
    register BoxPtr pbox;
#ifndef POLYSEGMENT
    register DDXPointPtr ppt;	/* pointer to list of translated points */
#endif

    register unsigned int oc1;	/* outcode of point 1 */
    register unsigned int oc2;	/* outcode of point 2 */

    int *addrl;		/* address of destination pixmap */
    int nlwidth;		/* width in longwords of destination pixmap */
    int xorg, yorg;		/* origin of window */

    int adx;		/* abs values of dx and dy */
    int ady;
    int signdx;		/* sign of dx and dy */
    int signdy;
    int e, e1, e2;		/* bresenham error and increments */
    int len;			/* length of segment */
    int axis;			/* major axis */
    int x1, x2, y1, y2;
    RegionPtr cclip;
    int		    fgrop, bgrop;
    unsigned char   *pDash;
    int		    dashOffset;
    int		    numInDashList;
    int		    dashIndex;
    int		    isDoubleDash;
    int		    dashIndexTmp, dashOffsetTmp;
    int		    unclippedlen;

    if (!(pGC->planemask & 1))
	return;

    cclip = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip;
    fgrop = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->rop;
    pboxInit = REGION_RECTS(cclip);
    nboxInit = REGION_NUM_RECTS(cclip);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrl = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrl = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    /* compute initial dash values */
     
    pDash = (unsigned char *) pGC->dash;
    numInDashList = pGC->numInDashList;
    isDoubleDash = (pGC->lineStyle == LineDoubleDash);
    dashIndex = 0;
    dashOffset = 0;
    miStepDash ((int)pGC->dashOffset, &dashIndex, pDash,
		numInDashList, &dashOffset);

    if (isDoubleDash)
	bgrop = mfbReduceRop(pGC->alu, pGC->bgPixel);

    xorg = pDrawable->x;
    yorg = pDrawable->y;
#ifdef POLYSEGMENT
    while (nseg--)
#else
    ppt = pptInit;
    x2 = ppt->x + xorg;
    y2 = ppt->y + yorg;
    while(--npt)
#endif
    {
	nbox = nboxInit;
	pbox = pboxInit;

#ifdef POLYSEGMENT
	x1 = pSeg->x1 + xorg;
	y1 = pSeg->y1 + yorg;
	x2 = pSeg->x2 + xorg;
	y2 = pSeg->y2 + yorg;
	pSeg++;
#else
	x1 = x2;
	y1 = y2;
	++ppt;
	if (mode == CoordModePrevious)
	{
	    xorg = x1;
	    yorg = y1;
	}
	x2 = ppt->x + xorg;
	y2 = ppt->y + yorg;
#endif

	adx = x2 - x1;
	ady = y2 - y1;
	signdx = sign(adx);
	signdy = sign(ady);
	adx = abs(adx);
	ady = abs(ady);

	if (adx > ady)
	{
	    axis = X_AXIS;
	    e1 = ady << 1;
	    e2 = e1 - (adx << 1);
	    e = e1 - adx;
	    unclippedlen = adx;
	}
	else
	{
	    axis = Y_AXIS;
	    e1 = adx << 1;
	    e2 = e1 - (ady << 1);
	    e = e1 - ady;
	    unclippedlen = ady;
	}

	/* we have bresenham parameters and two points.
	   all we have to do now is clip and draw.
	*/

	while(nbox--)
	{
	    oc1 = 0;
	    oc2 = 0;
	    OUTCODES(oc1, x1, y1, pbox);
	    OUTCODES(oc2, x2, y2, pbox);
	    if ((oc1 | oc2) == 0)
	    {
#ifdef POLYSEGMENT
		if (pGC->capStyle != CapNotLast)
		    unclippedlen++;
		dashIndexTmp = dashIndex;
		dashOffsetTmp = dashOffset;
		mfbBresD (fgrop, bgrop,
		      &dashIndexTmp, pDash, numInDashList,
		      &dashOffsetTmp, isDoubleDash,
		      addrl, nlwidth,
		      signdx, signdy, axis, x1, y1,
		      e, e1, e2, unclippedlen);
		break;
#else
		mfbBresD (fgrop, bgrop,
		      &dashIndex, pDash, numInDashList,
		      &dashOffset, isDoubleDash,
		      addrl, nlwidth,
		      signdx, signdy, axis, x1, y1,
		      e, e1, e2, unclippedlen);
		goto dontStep;
#endif
	    }
	    else if (oc1 & oc2)
	    {
		pbox++;
	    }
	    else /* have to clip */
	    {
		/*
		 * let the mfb helper routine do our work;
		 * better than duplicating code...
		 */
		BoxRec box;
		DDXPointRec pt1Copy;	/* clipped start point */
		DDXPointRec pt2Copy;	/* clipped end point */
		int err;			/* modified bresenham error term */
		int clip1, clip2;		/* clippedness of the endpoints */
	    
		int clipdx, clipdy;		/* difference between clipped and
					       unclipped start point */
		DDXPointRec	pt1;
    
		pt1.x = pt1Copy.x = x1;
		pt1.y = pt1Copy.y = y1;
		pt2Copy.x = x2;
		pt2Copy.y = y2;
		box.x1 = pbox->x1;
		box.y1 = pbox->y1;
		box.x2 = pbox->x2-1;
		box.y2 = pbox->y2-1;
		clip1 = 0;
		clip2 = 0;
    
		if (mfbClipLine (pbox, box,
				       &pt1, &pt1Copy, &pt2Copy, 
				       adx, ady, signdx, signdy, axis,
				       &clip1, &clip2) == 1)
		{
    
		    dashIndexTmp = dashIndex;
		    dashOffsetTmp = dashOffset;
		    if (clip1)
		    {
		    	int dlen;
    
		    	if (axis == X_AXIS)
			    dlen = abs(pt1Copy.x - x1);
		    	else
			    dlen = abs(pt1Copy.y - y1);
		    	miStepDash (dlen, &dashIndexTmp, pDash,
				    numInDashList, &dashOffsetTmp);
		    }
		    if (axis == X_AXIS)
		    	len = abs(pt2Copy.x - pt1Copy.x);
		    else
		    	len = abs(pt2Copy.y - pt1Copy.y);
    
#ifdef POLYSEGMENT
		    if (clip2 != 0 || pGC->capStyle != CapNotLast)
		    	len++;
#else
		    len += (clip2 != 0);
#endif
		    if (len)
		    {
		    	/* unwind bresenham error term to first point */
		    	if (clip1)
		    	{
			    clipdx = abs(pt1Copy.x - x1);
			    clipdy = abs(pt1Copy.y - y1);
			    if (axis == X_AXIS)
			    	err = e+((clipdy*e2) + ((clipdx-clipdy)*e1));
			    else
			    	err = e+((clipdx*e2) + ((clipdy-clipdx)*e1));
		    	}
		    	else
			    err = e;
		    	mfbBresD (fgrop, bgrop,
			      	  &dashIndexTmp, pDash, numInDashList,
			      	  &dashOffsetTmp, isDoubleDash,
			      	  addrl, nlwidth,
			      	  signdx, signdy, axis, pt1Copy.x, pt1Copy.y,
			      	  err, e1, e2, len);
		    }
		}
		pbox++;
	    }
	} /* while (nbox--) */
#ifndef POLYSEGMENT
	/*
	 * walk the dash list around to the next line
	 */
	miStepDash (unclippedlen, &dashIndex, pDash,
		    numInDashList, &dashOffset);
dontStep:	;
#endif
    } /* while (nline--) */

#ifndef POLYSEGMENT
    /* paint the last point if the end style isn't CapNotLast.
       (Assume that a projecting, butt, or round cap that is one
        pixel wide is the same as the single pixel of the endpoint.)
    */

    if ((pGC->capStyle != CapNotLast) &&
        ((dashIndex & 1) == 0 || isDoubleDash) &&
	((ppt->x + xorg != pptInit->x + pDrawable->x) ||
	 (ppt->y + yorg != pptInit->y + pDrawable->y) ||
	 (ppt == pptInit + 1)))
    {
	nbox = nboxInit;
	pbox = pboxInit;
	while (nbox--)
	{
	    if ((x2 >= pbox->x1) &&
		(y2 >= pbox->y1) &&
		(x2 <  pbox->x2) &&
		(y2 <  pbox->y2))
	    {
		unsigned long _mask;
		int rop;

		rop = fgrop;
		if (dashIndex & 1)
		    rop = bgrop;
		if (rop == RROP_BLACK)
		    _mask = rmask[x2 & 0x1f];
		else
		    _mask = mask[x2 & 0x1f];
		addrl += (y2 * nlwidth) + (x2 >> 5);
		if (rop == RROP_BLACK)
		    *addrl &= _mask;
		else if (rop == RROP_WHITE)
		    *addrl |= _mask;
		else
		    *addrl ^= _mask;
		break;
	    }
	    else
		pbox++;
	}
    }
#endif
}

#ifndef POLYSEGMENT
/*
    the clipping code could be cleaned up some; most of its
mess derives from originally being inline in the line code,
then pulled out to make clipping dashes easier.
*/

int
mfbClipLine(pbox, box,
	    ppt1Orig, ppt1, ppt2, 
	    adx, ady, signdx, signdy, axis,
	    pclip1, pclip2)
BoxPtr pbox;			/* box to clip to */
BoxRec box;			/* box to do calculations with */
DDXPointPtr ppt1Orig, ppt1, ppt2;
int adx, ady;
int signdx, signdy;
register int axis;
int *pclip1, *pclip2;
{
    DDXPointRec pt1Orig, pt1, pt2;
    register int swapped = 0;
    int clipDone = 0;
    register unsigned long utmp;
    register int oc1, oc2;
    int clip1, clip2;

    pt1Orig = *ppt1Orig;
    pt1 = *ppt1;
    pt2 = *ppt2;
    clip1 = 0;
    clip2 = 0;

    do
    {
        oc1 = 0;
        oc2 = 0;
        OUTCODES(oc1, pt1.x, pt1.y, pbox);
        OUTCODES(oc2, pt2.x, pt2.y, pbox);

        if (oc1 & oc2)
	    clipDone = -1;
        else if ((oc1 | oc2) == 0)
        {
	    clipDone = 1;
	    if (swapped)
	    {
	        SWAPPT(pt1, pt2);
	        SWAPINT(oc1, oc2);
	        SWAPINT(clip1, clip2);
	    }
        }
        else /* have to clip */
        {
	    /* only clip one point at a time */
	    if (!oc1)
	    {
	        SWAPPT(pt1, pt2);
	        SWAPINT(oc1, oc2);
	        SWAPINT(clip1, clip2);
	        swapped = !swapped;
	    }
    
	    clip1 |= oc1;
	    if (oc1 & OUT_LEFT)
	    {
	      pt1.x = box.x1;
	      utmp = abs(box.x1 - pt1Orig.x);
	      utmp *= ady;
	      if(axis==X_AXIS)
	      {
	        pt1.y = pt1Orig.y + SignTimes(signdy, round(utmp, adx));
	      }
	      else
	      {
		utmp <<= 1;
		if (swapped)
		    utmp += ady;
		else
		    utmp -= ady;
		pt1.y = pt1Orig.y + SignTimes(signdy, ceiling(utmp, 2*adx));
		if (swapped)
		    pt1.y -= signdy;
	      }
	    }
	    else if (oc1 & OUT_ABOVE)
	    {
	      pt1.y = box.y1;
	      utmp = abs(box.y1 - pt1Orig.y);
	      utmp *= adx;
	      if (axis == Y_AXIS)
	      {
	        pt1.x = pt1Orig.x + SignTimes(signdx, round(utmp, ady));
	      }
	      else
	      {
		utmp <<= 1;
		if (swapped)
		    utmp += adx;
		else
		    utmp -= adx;
		pt1.x = pt1Orig.x + SignTimes(signdx, ceiling(utmp, 2*ady));
		if (swapped)
		    pt1.x -= signdx;
	      }
	    }
	    else if (oc1 & OUT_RIGHT)
	    {
	      pt1.x = box.x2;
	      utmp = abs(pt1Orig.x - box.x2);
	      utmp *= ady;
	      if (axis == X_AXIS)
	      {
	        pt1.y = pt1Orig.y + SignTimes(signdy, round(utmp, adx));
	      }
	      else
	      {
		utmp <<= 1;
		if (swapped)
		    utmp += ady;
		else
		    utmp -= ady;
		pt1.y = pt1Orig.y + SignTimes(signdy, ceiling(utmp, 2*adx));
		if (swapped)
		    pt1.y -= signdy;
	      }
	    }
	    else if (oc1 & OUT_BELOW)
	    {
	      pt1.y = box.y2;
	      utmp = abs(pt1Orig.y - box.y2);
	      utmp *= adx;
	      if (axis == Y_AXIS)
	      {
	        pt1.x = pt1Orig.x + SignTimes(signdx, round(utmp, ady));
	      }
	      else
	      {
		utmp <<= 1;
		if (swapped)
		    utmp += adx;
		else
		    utmp -= adx;
		pt1.x = pt1Orig.x + SignTimes(signdx, ceiling(utmp, 2*ady));
		if (swapped)
		    pt1.x -= signdx;
	      }
	    }
        } /* else have to clip */
    } while(!clipDone);
    *ppt1 = pt1;
    *ppt2 = pt2;
    *pclip1 = clip1;
    *pclip2 = clip2;

    return clipDone;
}
#endif
