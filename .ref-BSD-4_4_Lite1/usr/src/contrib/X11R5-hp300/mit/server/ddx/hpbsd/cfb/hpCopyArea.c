/*
Copyright (c) 1986, 1987, 1988 by Hewlett-Packard Company
HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.
*/
/* $XConsortium: hpCopyArea.c,v 1.4 88/09/30 14:16:22 jim Exp $ */

/* 
 * hpCopyArea.c : a copy area for HP displays
 * Author: C Durland  (aided and abetted by Mr. J. Daniels)
 */

#include "X.h"

#include "gcstruct.h"
#include "pixmapstr.h"
#include "windowstr.h"
#include "scrnintstr.h"
#include "regionstr.h"
#include "mi.h"
#include "servermd.h"

#include "cfb.h"

/* 
 MTOS : main memory to screen
 MTOM : main mem to main mem, screen to main mem
 STOS : screen to screen
			    DESTINATION
			       | screen | main memory
			window | pixmap | pixmap
	SOURCE	      .--------|--------|-------
   window	      |  STOS  |  STOS  | MTOM
   screen pixmap      |  STOS  |  STOS  | MTOM
   main memory pixmap |  MTOS  |  MTOS  | MTOM

			Dest clip list		Source clip list
   window		composite clip		inferiors or window clip list
   screen pixmap	maybe a client clip	no
   main memory pixmap	maybe a client clip	no
*/

#define COPYROW(to,from,n) memcpy(to,from,n)

#define PIXER(Drawable)  ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)
#define SCRMER(Drawable) getPrivScreenPtr(Drawable->pScreen)
#define DEVKIND(Drawable) ((PixmapPtr)Drawable)->devKind

#define SWEAT_PLANES_MASK(Drawable,planesmask)		\
  ((SCRMER(Drawable)->planesMask & planesmask) !=	\
    SCRMER(Drawable)->planesMask)

	/* Clip rectangle (x,y)-(x+w,y+h) against (a,b)-(c,d)
	 * Returns the cliped (x,y) and w,h
	 */
void 
clipper(x,y,w,h, a,b,c,d, cx,cy, cw,ch)
     register int x,y,w,h, a,b,c,d;
     int *cx,*cy, *cw,*ch;
{
    *cw = min(x+w,c) - (*cx = max(x,a));
    *ch = min(y+h,d) - (*cy = max(y,b));
}

	/* Check to see if can copy area between two rectangles:
	 *   If the dest is above the source: can copy
	 *   else if they overlap:  Can't copy
	 * Have to check horizontal because of clip lists.  boxes is the sum
	 *  of the number of clip rectangles for the source and dest.
	 * Returns: FALSE if can copy area (ie any overlap don't matter)
	 *    true if got a overlap problem
	 */
static int
lapdog(srcx,srcy, w,h, dstx,dsty, boxes)
     int srcx,srcy, w,h, dstx,dsty, boxes;
{
    return
	( (srcy <= dsty) || ((boxes > 2) && (srcx < dstx)) ) &&
	    ( ((min(srcx,dstx)+w -max(srcx,dstx)) >0) &&
	     ((min(srcy,dsty)+h -max(srcy,dsty)) >0) );
}

	/* Copy rectangles from screen to screen using the block mover
	 * Overlapping moves:
	 *   Assumes that block mover can handle overlapping copies.
	 *   If there is a clip list, care must be taken so that copying
	 *     a block does not trash a block to be copied.
	 * (?tx, ?ty) are translation constants to convert pixmaps
	 *   to screen coordinates.  0 if not a pixmap.
	 */
void
ScreenToScreen(drawable,gc, sx,sy,width,height, dx,dy,
	sbox,sboxes, dbox,dboxes, stx,sty, dtx,dty)
     DrawablePtr drawable;
     GCPtr gc;
     int sx,sy,width,height, dx,dy, sboxes, dboxes, stx,sty, dtx,dty;
     BoxPtr sbox, dbox;	/* the clip lists */
{
    register BoxPtr btr;
    int j, x1,y1, w1,h1, x2,y2, w2,h2,   a,b,c,d, tx = sx-dx, ty = sy-dy;
    ScreenPtr Screen = drawable->pScreen;

    if (gc->alu==GXnoop)
	return;			/* no op => don't do nothin */

    for (; sboxes--; sbox++)	/* for each source box */
    {
	/* intersect source and source clip rectangle */
	clipper(sx,sy,width,height, sbox->x1,sbox->y1, sbox->x2,sbox->y2,
		&x1,&y1, &w1,&h1);
	if (w1<=0 || h1<=0)
	    continue;
	/* translate box to dst coordinates */
	a = x1 -tx; b = y1 -ty; c = a +w1; d = b +h1;
	for (j = dboxes, btr = dbox; j--; btr++) /* copy to the dest box */
	{
	    /* intersect dst and dst clip rectangles */
	    clipper(dx,dy,width,height, btr->x1,btr->y1, btr->x2,btr->y2,
		    &x2,&y2, &w2,&h2);
	    if (w2<=0 || h2<=0)
		continue;
	    /* intersect clipped src and clipped dst rectangles */
	    clipper(x2,y2,w2,h2, a,b,c,d, &x1,&y1, &w1,&h1);
	    if (w1<=0 || h1<=0)
		continue;
	    (*getPrivScreenPtr(Screen)->MoveBits)
		(Screen, gc->planemask, gc->alu,
		 x1+tx+stx, y1+ty+sty, x1+dtx, y1+dty, w1, h1);
	}
    }
}

	/* copy a rectangle of bytes: no processing */
static void
CopyRec0(dst,w,h,dst_stride, alu)
     register char *dst;
     int w,h,dst_stride, alu;
{
    register int z;

    switch (alu)
    {
      case GXclear: z = 0; break;		/* 0 */
      case GXset: z = ~0; break;		/* 1 */
    }
    while (h--)			/* copy h rows */
    {
	memset(dst,z,w);
	dst += dst_stride;	/* move to next row */
    }
}

	/* copy a rectangle of bytes: only process src */
	/* two cases: GXcopy & GXcopyInverted */
static void
CopyRec1(src,dst,w,h,src_stride,dst_stride, alu)
     register char *src, *dst;
     int w,h,src_stride,dst_stride, alu;
{
    register int j;

    if (alu==GXcopy)
	while (h--)		/* copy h rows */
	{
	    COPYROW(dst,src,w);
	    src += src_stride; dst += dst_stride;	/* move to next row */
	}
    else
	while (h--)		/* copy h rows */
	{
	    j = w;
	    while (j--) dst[j] = ~src[j];
	    src += src_stride; dst += dst_stride;	/* move to next row */
	}
}

	/* copy a rectangle of bytes: process src & dst */
static void
CopyRec2(src,dst,w,h,src_stride,dst_stride, alu,planesmask)
     register char *src, *dst;
     int w,h,src_stride,dst_stride, alu;
     register long int planesmask;
{
    register char a, b;
    register int j;

    /*!!! slime bag note: if planesmask > 8 bits this routine no workie */

    while (h--)			/* copy h rows */
    {
	j = w;
	while (j--)		/* copy a row */
	{
	    a = src[j]; b = dst[j];
	    switch (alu)
	    {
	      case GXclear: a = 0; break;		/* 0 */
	      case GXset: a = ~0; break;		/* 1 */

	      case GXcopyInverted: b = ~a; break;	/* ~src */
/*	      case GXcopy: a = a; break;		/* src */

	      case GXand: a = a & b; break; 		/* src AND dst */
	      case GXandReverse: a = a & (~b); break;   /* src AND NOT dst */
	      case GXandInverted: a = (~a) & b; break;  /* NOT src AND dst */
	      case GXxor: a = a ^ b; break;		/* src XOR dst */
	      case GXor: a = a | b; break;		/* src OR dst */
	      case GXnor: a = (~a) & (~b); break;     /* NOT src AND NOT dst */
	      case GXequiv: a = (~a) ^ b; break;	/* NOT src XOR dst */
	      case GXinvert: a = ~b; break;		/* NOT dst */
	      case GXorReverse: a = a | (~b); break;    /* src OR NOT dst */
	      case GXorInverted: a = (~a) | b; break;   /* NOT src OR dst */
	      case GXnand: a = (~a) | (~b); break;    /* NOT src OR NOT dst */
	    }
	    dst[j] = (a & planesmask) | (b & ~planesmask);
	}
	src += src_stride; dst += dst_stride;	/* move to next row */
    }
}

	/* move bytes from main memory to main memory
	 *   or move bytes from screen to main memory
	 * concerns: Replacement rule, overlapping copies,
	 * Only works for depths of 1 and multiples of 8.
	 */
void
MemToMem(SrcDrawable,DstDrawable,gc, sx,sy,width,height, dx,dy,
	 sbox,sboxes, dbox,dboxes)
     DrawablePtr SrcDrawable,DstDrawable;
     GCPtr gc;
     int sx,sy,width,height, dx,dy, sboxes, dboxes;
     BoxPtr sbox, dbox;	/* the clip lists */
{
    unsigned char *src, *dst, *presrc;
    register int src_stride, dst_stride;
    int
	j, x1,y1, w1,h1, x2,y2, w2,h2,   a,b,c,d, tx = sx-dx, ty = sy-dy,
	alu, fake_alu,
	DepthInBytes = SrcDrawable->depth/8;
    unsigned long int planesmask;
    register BoxPtr btr;

    if ((alu=gc->alu)==GXnoop) return;	/* no op => don't do nothin */

    fake_alu = alu; planesmask = gc->planemask;

    if (DepthInBytes<1)	/* moving bits */
    {
	DepthInBytes = 1;	/* fake around */
	if (planesmask==0) return;	/* no bits will be changed */
	planesmask = ~0;		/* all bits can be changed */
    }

	/* check to see if gotta sweat the planes mask */
    if (SWEAT_PLANES_MASK(SrcDrawable,planesmask)) fake_alu = 666;

    dst_stride = PIXER(DstDrawable)->stride;
    if (SrcDrawable->type == DRAWABLE_WINDOW)	/* from screen */
    {
	src_stride = SCRMER(SrcDrawable)->stride;
	presrc = SCRMER(SrcDrawable)->bits;
    }
    else			/* from main memory or offscreen pixmap */
    {
	src_stride = PIXER(SrcDrawable)->stride;
	presrc = PIXER(SrcDrawable)->bits;
    }

    for (; sboxes--; sbox++)	/* for each source box */
    {
	/* intersect src and src clip rectangle */
	clipper(sx,sy,width,height, sbox->x1,sbox->y1,sbox->x2,sbox->y2,
		&x1,&y1, &w1,&h1);
	if (w1<=0 || h1<=0) continue;
	/* translate box to dst coordinates */
	a = x1 -tx; b = y1 -ty; c = a +w1; d = b +h1;
	for (j = dboxes, btr = dbox; j--; btr++)
	{
	    /* intersect dst and dst clip rectangles */
	    clipper(dx,dy,width,height, btr->x1,btr->y1, btr->x2,btr->y2,
		    &x2,&y2, &w2,&h2);
	    if (w2<=0 || h2<=0) continue;
	    /* intersect clipped src and clipped dst rectangles */
	    clipper(x2,y2,w2,h2, a,b,c,d, &x1,&y1, &w1,&h1);
	    if (w1<=0 || h1<=0) continue;
	    w1 *= DepthInBytes;
	    /* convert rectangle to addresses */
	    src = presrc + (x1+tx)*DepthInBytes +(y1+ty)*src_stride;
	    dst = PIXER(DstDrawable)->bits +x1*DepthInBytes +y1*dst_stride;

	    switch (fake_alu)
	    {
	      case GXclear:
	      case GXset:
		CopyRec0(dst,w1,h1,dst_stride, alu); break;
	      case GXcopy:
	      case GXcopyInverted:
		CopyRec1(src,dst,w1,h1,src_stride,dst_stride, alu);
		break;
	      default:
		CopyRec2(src,dst,w1,h1,src_stride,dst_stride, alu,planesmask);
	    }
	}
    }
}


	/* Move bytes from main memory to screen memory.
	 * (dtx,dty) are translation constants to convert pixmaps
	 *   to screen coordinates.  0 if not a pixmap.
	 */
void
MemToScreen(SrcDrawable,DstDrawable,gc, sx,sy,width,height, dx,dy,
	    dboxes, dbox, dtx,dty)
     DrawablePtr SrcDrawable,DstDrawable;
     GCPtr gc;
     int sx,sy,width,height, dx,dy, dboxes, dtx,dty;
     BoxPtr dbox;	/* the dst clip list */
{
    register unsigned char *src, *dst;
    register int src_stride, dst_stride;
    int
	w,h, x1,y1,
	DepthInBytes = SrcDrawable->depth/8;

	/* setup hardware */
    SET_REGISTERS_FOR_WRITING(gc->pScreen,gc->planemask,gc->alu);
    src_stride = PIXER(SrcDrawable)->stride;
    dst_stride = SCRMER(DstDrawable)->stride;
    for (; dboxes--; dbox++)
    {
	/* intersect dst and clip rectangles */
	clipper(dx,dy,width,height, dbox->x1,dbox->y1,dbox->x2,dbox->y2,
		&x1,&y1, &w,&h);
	w *= DepthInBytes;
	if (w<=0 || h<=0) continue;
	/* convert rectangle to addresses */
	src = PIXER(SrcDrawable)->bits
	    +(sx +(x1-dx))*DepthInBytes +(sy +(y1-dy))*src_stride;
	dst = SCRMER(DstDrawable)->bits
	    +(x1+dtx)*DepthInBytes +(y1+dty)*dst_stride;
	/* copy rectangle */
	while (h--)
	{
	    COPYROW(dst,src,w);
	    src += src_stride; dst += dst_stride;	/* move to next row */
	}
    }
}

	/* reverse banding */
static void
spud(list,n)
     BoxRec *list;
     int n;
{
    register BoxRec box;
    register int i, j, k;

    for (j = 0, i = n, k = n/2; j<k; )
    {
	box = list[j]; list[j++] = list[--i]; list[i] = box;
    }
}

	/* reverse horizontal banding */
static void
spudd(list,n)
     register BoxRec *list;
     int n;
{
    register int i,j;

    for (j=0; j<n; j = i)
    {
	for (i = j+1; i<n && list[i].y1==list[j].y1; i++)
	    ;
	spud(&list[j],i-j);
    }
}

extern RegionPtr NotClippedByChildren();
static RegionPtr hpfbCopyArea();

RegionPtr 
hpcCopyArea(pSrcDrawable, pDstDrawable,
	    pGC, xIn, yIn, widthSrc, heightSrc, xOut, yOut)
     DrawablePtr pSrcDrawable, pDstDrawable;
     GCPtr pGC;
     int xIn, yIn, widthSrc, heightSrc, xOut, yOut;
{
    BoxRec *scl, *dcl, sbox;
    int
	srcx,srcy, dstx,dsty, width,height, dboxes,sboxes,
	stx,sty, dtx,dty,
	expose = 0, lowlife = 0;
    RegionPtr sHitList = NULL;	/* the source hit list */
    RegionPtr prgnExposed = NULL;
    extern RegionPtr mfbCopyArea ();

/* ignore UNDRAWABLE_WINDOWs in the hope DIX takes care of them */

    if ((pDstDrawable->type == DRAWABLE_WINDOW) && 
	(!((WindowPtr)pDstDrawable)->realized))
	return (RegionPtr)NULL;

    dstx = xOut; dsty = yOut; width = widthSrc; height = heightSrc;
	/* clip the left and top edges of the source */
    if (xIn<0)
    { 
	expose = 1; srcx = pSrcDrawable->x; width += xIn;
    }
    else
	srcx = xIn + pSrcDrawable->x;
    if (yIn<0)
    {
	expose = 1; srcy = pSrcDrawable->y; height += yIn;
    }
    else
	srcy = yIn + pSrcDrawable->y;

	/* lookup or create the source clip lists */
    stx = sty = 0;
    if (pSrcDrawable->type == DRAWABLE_PIXMAP)
    {
	/* clip right and bottom edges of source */
	if (width > pSrcDrawable->width)
	{
	    expose = 1; width = pSrcDrawable->width;
	}
	if (height > pSrcDrawable->height)
	{
	    expose = 1; height = pSrcDrawable->height;
	}
	/* if screen pixmap & going to use the block mover, translate */
	if (DEVKIND(pSrcDrawable) == PIXMAP_FRAME_BUFFER)
	{
	    stx = PIXER(pSrcDrawable)->pChunk->x;
	    sty = PIXER(pSrcDrawable)->pChunk->y;
	}
	sbox.x2 = (sbox.x1 = srcx) + width;
	sbox.y2 = (sbox.y1 = srcy) + height;
	scl = &sbox; sboxes = 1;
    }
    else	/* source is a window */
    {
	expose = 1;	/* hard to figure out for a window so expose always */
	/* translate window to screen coordinates */
	if (pGC->subWindowMode == IncludeInferiors)
	{
	    /* included window can write over parent => overlap problem
	     *  (if included window is source and parent is dest)
	     */
	    if (pDstDrawable->type == DRAWABLE_WINDOW)
		lowlife = 1;
	    if (pSrcDrawable == pDstDrawable && pGC->clientClipType == CT_NONE)
	    {
		scl = REGION_RECTS(((cfbPrivGC *)
				    pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
		sboxes = REGION_NUM_RECTS(((cfbPrivGC *)
					   pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
	    }
	    else	/* gotta create a new clip list */
	    {
		sHitList = NotClippedByChildren((WindowPtr)pSrcDrawable);
		scl = REGION_RECTS(sHitList);
		sboxes = REGION_NUM_RECTS(sHitList);
	    }
	}
	else
	{
	    scl    = REGION_RECTS(&((WindowPtr)pSrcDrawable)->clipList);
	    sboxes = REGION_NUM_RECTS(&((WindowPtr)pSrcDrawable)->clipList);
	}
    }

	/* lookup the dest clip list and any translation */
    dcl    = REGION_RECTS(((cfbPrivGC *)
			   pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
    dboxes = REGION_NUM_RECTS(((cfbPrivGC *)
			       pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
    dtx = dty = 0;
    if (pDstDrawable->type == DRAWABLE_PIXMAP)
    {
	if (DEVKIND(pDstDrawable) == PIXMAP_FRAME_BUFFER)
	{
	    dtx = PIXER(pDstDrawable)->pChunk->x;
	    dty = PIXER(pDstDrawable)->pChunk->y;
	}
	/* else dest is in main mem & composite clip is OK */
    }
    else	/* dest is a window */
    {
	if (pGC->miTranslate) /* translate window to screen coordinates */
	{
	    dstx += pDstDrawable->x;
	    dsty += pDstDrawable->y;
	}
    }
  
	/* figure out who to call to actually do the copy area */
    if (pDstDrawable->type == DRAWABLE_PIXMAP &&
	DEVKIND(pDstDrawable) == PIXMAP_HOST_MEMORY)		/* MTOM */
    {
	if (pSrcDrawable->depth==1)	/* can only handle some bitmaps */
	{
	    if ((pSrcDrawable!=pDstDrawable ||
		 !lapdog(srcx,srcy, width,height, dstx,dsty, sboxes+dboxes)) &&
		srcx==0 && srcy==0 && dstx==0 && dsty==0 && (width % 8)==0)
	    {
		width /= 8; goto mtom;
	    }
		/* let mfb handle most of the bit maps */
	    return mfbCopyArea(pSrcDrawable, pDstDrawable,
			       pGC, xIn, yIn, widthSrc, heightSrc, xOut, yOut);
	}
	/* can't handle overlapping moves unless overlap don't matter */
	if (pSrcDrawable==pDstDrawable &&
	    lapdog(srcx,srcy,width,height,dstx,dsty,sboxes+dboxes) &&
	    (SWEAT_PLANES_MASK(pSrcDrawable,pGC->planemask) ||
	     (pGC->alu!=GXclear && pGC->alu!=GXset && pGC->alu!=GXnoop)) )
	{
	    return hpfbCopyArea(pSrcDrawable, pDstDrawable,
				pGC, xIn, yIn, widthSrc, heightSrc, xOut, yOut);
	}
      mtom:
	MemToMem(pSrcDrawable,pDstDrawable,pGC,
		 srcx,srcy,width,height, dstx,dsty, scl,sboxes, dcl,dboxes);
    }
    else
	if (pSrcDrawable->type == DRAWABLE_PIXMAP &&
	    DEVKIND(pSrcDrawable) == PIXMAP_HOST_MEMORY)	/* MTOS */
	    MemToScreen(pSrcDrawable,pDstDrawable,pGC,
			srcx,srcy,width,height, dstx,dsty,
			dboxes,dcl, dtx,dty);
	else							/* STOS */
	    if ( (pSrcDrawable==pDstDrawable || lowlife) &&
		(sboxes +dboxes >2) &&
		lapdog(srcx,srcy, width,height, dstx,dsty, sboxes+dboxes) &&
		(srcy<dsty || srcx<dstx) )			/* overlap */
	    {
		BoxRec *sl, *dl;
		int j;

		/* allocate new clip lists */
		sl = (BoxRec *)xalloc((unsigned long)
				      ((sboxes+dboxes)*sizeof(BoxRec)));
		dl = &sl[sboxes];
		for (j=0; j<sboxes; j++) sl[j] = scl[j];
		for (j=0; j<dboxes; j++) dl[j] = dcl[j];
		/* reverse vertical & horizontal banding */
		if (srcy<dsty || (srcy<dsty && srcx<dstx))
		{
		    spud(sl,sboxes); spud(dl,dboxes);
		}
		else		/* reverse horizontal banding */
		{
		    spudd(sl,sboxes); spudd(dl,dboxes);
		}
		ScreenToScreen(pSrcDrawable, pGC, srcx,srcy, width,height,
			       dstx,dsty, sl,sboxes, dl,dboxes,
			       stx,sty, dtx,dty);
		xfree((char *)sl);
	    }
	    else
		ScreenToScreen(pSrcDrawable, pGC, srcx,srcy, width,height,
			       dstx,dsty, scl,sboxes, dcl,dboxes,
			       stx,sty, dtx,dty);

    /* let miHandleExposures() handle all the exposure stuff 'cause
     *   it knows lots more than I do.  It also sends noExpose
     *   events if needbe.
     * Note: Other CopyAreas use (cfbPrivGC *)(pGC->devPriv))->fExpose
     *   instead of pGC->graphicsExposures.  This is because
     *   mfbPutImage is brain damaged and since HP don't use it, I
     *   can use graphicsExposures.
     */
    if (pGC->graphicsExposures)
	prgnExposed = miHandleExposures(pSrcDrawable, pDstDrawable, pGC, xIn, yIn,
		widthSrc, heightSrc, xOut, yOut,0);
    if (sHitList)
	(*pGC->pScreen->RegionDestroy)(sHitList);
    return prgnExposed;
}

/* HPFBCOPYAREA -- "public" entry for the CopyArea request 
 * For requests operating within a single topcat frame buffer.
 * For each rectangle in the source region
 *   move rectangle using topcat pixel mover hardware
 */
static RegionPtr
hpfbCopyArea(pSrcDrawable, pDstDrawable,
	     pGC, xIn, yIn, widthSrc, heightSrc, xOut, yOut)
    register DrawablePtr 	pSrcDrawable;
    register DrawablePtr 	pDstDrawable;
    GCPtr 			pGC;
    int 			xIn, yIn;
    int 			widthSrc, heightSrc;
    int 			xOut, yOut;
{
    DDXPointPtr		ppt, pptFirst;
    unsigned int	*pwidthFirst, *pwidth, *pbits;
    BoxRec 		srcBox, *prect;
    			/* may be a new region, or just a copy */
    RegionPtr 		prgnSrcClip, prgnDstClip;
    			/* non-0 if we've created a src clip */
    int 		realSrcClip = 0,
                        useOrdering = 0;
    int			srcx, srcy, dstx, dsty, i, j, y, width, height,
    			xMin, xMax, yMin, yMax;
    unsigned int        *ordering;
    RegionPtr		prgnExposed;
    int			numRects;
    BoxPtr		boxes;

    /* clip the left and top edges of the source */
    if (xIn < 0)
    {
        widthSrc += xIn;
        srcx = pSrcDrawable->x;
    }
    else
	srcx = xIn + pSrcDrawable->x;
    if (yIn < 0)
    {
        heightSrc += yIn;
        srcy = pSrcDrawable->y;
    }
    else
	srcy = yIn + pSrcDrawable->y;

    /* If the destination isn't realized, this is easy */
    if ((pDstDrawable->type == DRAWABLE_WINDOW) && 
	(!((WindowPtr)pDstDrawable)->realized))
	return (RegionPtr)NULL;

    /* clip the source */
    if (pSrcDrawable->type == DRAWABLE_PIXMAP)
    {
	BoxRec box;

	box.x1 = pSrcDrawable->x;
	box.y1 = pSrcDrawable->y;
	box.x2 = pSrcDrawable->x + (int) pSrcDrawable->width;
	box.y2 = pSrcDrawable->y + (int) pSrcDrawable->height;

	prgnSrcClip = (*pGC->pScreen->RegionCreate)(&box, 1);
	realSrcClip = 1;
    }
    else
	prgnSrcClip = &((WindowPtr)pSrcDrawable)->clipList;

    srcBox.x1 = srcx;
    srcBox.y1 = srcy;
    srcBox.x2 = srcx + widthSrc;
    srcBox.y2 = srcy + heightSrc;

    dstx = xOut;
    dsty = yOut;
    if (pGC->miTranslate)
    {
	dstx += pDstDrawable->x;
	dsty += pDstDrawable->y;
    }

    prgnDstClip = ((cfbPrivGC *) (pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip;

    numRects = REGION_NUM_RECTS(prgnSrcClip);
    boxes = REGION_RECTS(prgnSrcClip);
    ordering = (unsigned int *)
        ALLOCATE_LOCAL(numRects * sizeof(unsigned int));
    if (!ordering)
	return (RegionPtr)NULL;

    /* If not the same drawable then order of move doesn't matter.
       Following assumes that boxes are sorted from top
       to bottom and left to right.
    */
    if (pSrcDrawable != pDstDrawable)
	for (i=0; i < numRects; i++)
	    ordering[i] = i;
    else
    {   /* within same drawable, must sequence moves carefully! */
	useOrdering = 1; /* must pay attention to this ordering later! */
	if (dsty <= srcBox.y1)
	{			/* Scroll up or stationary vertical.
				   Vertical order OK */
	    if (dstx <= srcBox.x1) /* Scroll left or stationary horizontal.
				      Horizontal order OK as well */
		for (i=0; i < numRects; i++)
		    ordering[i] = i;
	    else
	    {   /* scroll right. must reverse horizontal banding of rects. */
		for (i=0, j=1, xMax=0; i < numRects; j=i+1, xMax=i)
		{
		    /* find extent of current horizontal band */
		    y=boxes[i].y1; /* band has this y coordinate */
		    while ((j < numRects) && (boxes[j].y1 == y))
			j++;
		    /* reverse the horizontal band in the output ordering */
		    for (j-- ; j >= xMax; j--, i++)
			ordering[i] = j;
		}
	    }
	}
	else
	{     /* Scroll down. Must reverse vertical banding. */
	    if (dstx < srcBox.x1)
	    {			/* Scroll left. Horizontal order OK. */
		for (i=numRects-1, j=i-1, yMin=i, yMax=0;
		     i >= 0;
		     j=i-1, yMin=i)
		{
		    /* find extent of current horizontal band */
		    y=boxes[i].y1; /* band has this y coordinate */
		    while ((j >= 0) && (boxes[j].y1 == y))
			j--;
		    /* reverse the horizontal band in the output ordering */
		    for (j++ ; j <= yMin; j++, i--, yMax++)
			ordering[yMax] = j;
		}
	    }
	    else /* Scroll right or horizontal stationary.
		    Reverse horizontal order as well (if stationary, horizontal
		    order can be swapped without penalty and this is faster
		    to compute). */
		for (i=0, j=numRects-1; i < numRects; i++, j--)
		    ordering[i] = j;
	}
    }

    if ((pSrcDrawable->pScreen == pDstDrawable->pScreen) &&
	
	(((pSrcDrawable->type == DRAWABLE_PIXMAP) &&
	  (((PixmapPtr)pSrcDrawable)->devKind == PIXMAP_FRAME_BUFFER)) ||
	 (pSrcDrawable->type == DRAWABLE_WINDOW)) &&
	
	(((pDstDrawable->type == DRAWABLE_PIXMAP) &&
	  (((PixmapPtr)pDstDrawable)->devKind == PIXMAP_FRAME_BUFFER)) ||
	 (pDstDrawable->type == DRAWABLE_WINDOW))
	)
    {
	
	/* Copy area within portions of a single screens frame buffer.
	 * For each visible portion of source, move into visible
	 * portions of destination utilizing area mover.
	 */
	BoxRec  dstBox,  *prect2;
	int     sxMin, sxMax, syMin, syMax,  /* source for actual move */
	dxMin, dxMax, dyMin, dyMax;  /* dest for actual move */
	
	register hpPrivScreenPtr pPrivScreen;
	void (*bitMover)(), (*maskConfig)();

	pPrivScreen = getPrivScreenPtr(pGC->pScreen);
	bitMover = pPrivScreen->MoveBits;
	maskConfig = pPrivScreen->MaskConfig;

	if (pSrcDrawable->type == DRAWABLE_PIXMAP)
	{			/* make screen relative */
	    register hpChunk *pixChunk =
		((hpPrivPixmapPtr) (((PixmapPtr)pSrcDrawable)->devPrivate.ptr))->pChunk;
	    boxes[0].x1 += pixChunk->x;
	    boxes[0].y1 += pixChunk->y;
	    boxes[0].x2 += pixChunk->x;
	    boxes[0].y2 += pixChunk->y;
	    
	    srcBox.x1 += pixChunk->x;
	    srcBox.y1 += pixChunk->y;
	    srcBox.x2 += pixChunk->x;
	    srcBox.y2 += pixChunk->y;
	    
	    srcx += pixChunk->x;
	    srcy += pixChunk->y;
	}
	
	if (pDstDrawable->type == DRAWABLE_PIXMAP)
	{			/* make screen relative */
	    register hpChunk *pixChunk = (hpChunk *)
		((hpPrivPixmapPtr) (((PixmapPtr)pDstDrawable)->devPrivate.ptr))->pChunk;
	    
	    REGION_RECTS(prgnDstClip)[0].x1 += pixChunk->x;
	    REGION_RECTS(prgnDstClip)[0].y1 += pixChunk->y;
	    REGION_RECTS(prgnDstClip)[0].x2 += pixChunk->x;
	    REGION_RECTS(prgnDstClip)[0].y2 += pixChunk->y;
	    
	    dstx += pixChunk->x;
	    dsty += pixChunk->y;
	}
	
	for (i = 0; i < numRects; i++)
	{
	    prect = &boxes[ordering[i]];
	    /* find portion of move contained in this visible portion of window */
	    xMin = max(prect->x1, srcBox.x1);
	    xMax = min(prect->x2, srcBox.x2);
	    yMin = max(prect->y1, srcBox.y1);
	    yMax = min(prect->y2, srcBox.y2);
	    /* exit loop unless there is something visible */
	    if (xMax <= xMin || yMax <= yMin)
		continue;
		
	    /* destination box for visible portion of source */
	    dstBox.x1 = xMin - (srcx - dstx);
	    dstBox.y1 = yMin - (srcy - dsty);
	    dstBox.x2 = dstBox.x1 + xMax - xMin;
	    dstBox.y2 = dstBox.y1 + yMax - yMin;
	    
	    /* find visible portions of destination */
	    prect2 = REGION_RECTS(prgnDstClip);
	    for (j = 0; j < REGION_NUM_RECTS(prgnDstClip); j++)
	    {
		if (useOrdering)
		    prect2 = &REGION_RECTS(prgnDstClip)[ordering[j]];
		else
		    prect2 = &REGION_RECTS(prgnDstClip)[j];
		dxMin = max(prect2->x1, dstBox.x1);
		dxMax = min(prect2->x2, dstBox.x2);
		dyMin = max(prect2->y1, dstBox.y1);
		dyMax = min(prect2->y2, dstBox.y2);
		/* any portion of destination visible in this area? */
		if (dxMax <= dxMin || dyMax <= dyMin)
		    continue;
		    
		/* will further clip source if destination was also clipped */
		sxMin = xMin + max((prect2->x1 - dstBox.x1), 0);
		syMin = yMin + max((prect2->y1 - dstBox.y1), 0);
		sxMax = xMax + min((prect2->x2 - dstBox.x2), 0);
		syMax = yMax + min((prect2->y2 - dstBox.y2), 0);
		    
		(*bitMover)(pGC->pScreen, pGC->planemask, pGC->alu,
			    sxMin, syMin, dxMin, dyMin,
			    (sxMax - sxMin), (syMax - syMin));
	    }
	}
    }
    else
    {				/* no place for hardware assist */
	pptFirst = ppt = (DDXPointPtr)
	    ALLOCATE_LOCAL(heightSrc * sizeof(DDXPointRec));
	pwidthFirst = pwidth = (unsigned int *)
	    ALLOCATE_LOCAL(heightSrc * sizeof(unsigned int));
	if (!pptFirst || !pwidthFirst)
	{
	    DEALLOCATE_LOCAL(ordering);
	    if (pptFirst)
		DEALLOCATE_LOCAL(pptFirst);
	    if (pwidthFirst)
		DEALLOCATE_LOCAL(pwidthFirst);
	    return (RegionPtr)NULL;
	}
	for (i = 0; i < numRects; i++)
	{
	    prect = &boxes[ordering[i]];
	    xMin = max(prect->x1, srcBox.x1);
	    xMax = min(prect->x2, srcBox.x2);
	    yMin = max(prect->y1, srcBox.y1);
	    yMax = min(prect->y2, srcBox.y2);
	    /* is there anything visible here? */
	    if (xMax <= xMin || yMax <= yMin)
		continue;
	  
	    ppt = pptFirst;
	    pwidth = pwidthFirst;
	    y = yMin;
	    height = yMax - yMin;
	    width = xMax - xMin;
	  
	    for (j = 0; j < height; j++)
	    {
		ppt->x = xMin;
		ppt++->y = y++;
		*pwidth++ = width;
	    }
	    pbits = (unsigned int *)xalloc(height * PixmapBytePad(width,
								  pSrcDrawable->depth));
	    if (pbits)
	    {
		(*pSrcDrawable->pScreen->GetSpans)(pSrcDrawable, width, pptFirst,
						   pwidthFirst, height, pbits);
		ppt = pptFirst;
		pwidth = pwidthFirst;
		xMin -= (srcx - dstx);
		y = yMin - (srcy - dsty);
		for (j = 0; j < height; j++)
		{
		    ppt->x = xMin;
		    ppt++->y = y++;
		    *pwidth++ = width;
		}
	  
		(*pGC->ops->SetSpans)(pDstDrawable, pGC, pbits, pptFirst,
				      pwidthFirst, height, TRUE);
		xfree(pbits);
	    }
	}
	DEALLOCATE_LOCAL(pptFirst);
	DEALLOCATE_LOCAL(pwidthFirst);
    }
    DEALLOCATE_LOCAL(ordering);
    prgnExposed = miHandleExposures(pSrcDrawable, pDstDrawable, pGC, xIn, yIn,
				    widthSrc, heightSrc, xOut, yOut, (unsigned long)0);
    if (realSrcClip)
	(*pGC->pScreen->RegionDestroy)(prgnSrcClip);
    return prgnExposed;
}
