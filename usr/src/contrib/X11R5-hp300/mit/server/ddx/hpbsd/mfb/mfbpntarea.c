/* Combined Purdue/PurduePlus patches, level 2.0, 1/17/89 */
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
/* $XConsortium: mfbpntarea.c,v 5.2 89/11/24 18:06:43 rws Exp $ */
#include "X.h"

#include "windowstr.h"
#include "regionstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "mfb.h"
#include "maskbits.h"

/* 
   the solid fillers are called for rectangles and window backgrounds.
   the boxes are already translated.
   maybe this should always take a pixmap instead of a drawable?

   NOTE:
   iy = ++iy < tileHeight ? iy : 0
is equivalent to iy%= tileheight, and saves a division.
*/

/*
	MFBSOLIDFILLAREA	OPEQ	EQWHOLEOWRD
	mfbSolidWhiteArea	|=	= ~0
	mfbSolidBlackArea	&=~	= 0
	mfbSolidInvertArea	^=	^= ~0

EQWHOLEWORD is used to write whole longwords.  it could use OPEQ,
but *p++ |= ~0 on at least two compilers generates much
worse code than *p++ = ~0.  similarly for *p++ &= ~~0
and *p++ = 0.

*/

/*ARGSUSED*/
void
MFBSOLIDFILLAREA(pDraw, nbox, pbox, alu, nop)
    DrawablePtr pDraw;
    int nbox;
    BoxPtr pbox;
    int alu;
    PixmapPtr nop;
{
    int nlwidth;	/* width in longwords of the drawable */
    int w;		/* width of current box */
    register int h;	/* height of current box */
    register unsigned int *p;	/* pointer to bits we're writing */
    register int nlw;	/* loop version of nlwMiddle */
    register int startmask;
    register int endmask;/* masks for reggedy bits at either end of line */
    register int nlwExtra;	
		        /* to get from right of box to left of next span */
    int nlwMiddle;	/* number of longwords between sides of boxes */
    unsigned int *pbits;	/* pointer to start of drawable */

    if (pDraw->type == DRAWABLE_WINDOW)
    {
	pbits = (unsigned int *) getPrivScreenPtr(pDraw->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDraw->pScreen)->stride >> 2;
    }
    else
    {
	pbits = (unsigned int *) getPrivPixmapPtr(pDraw)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDraw)->stride >> 2;
    }


    while (nbox--)
    {
	w = pbox->x2 - pbox->x1;
	h = pbox->y2 - pbox->y1;
	p = pbits + (pbox->y1 * nlwidth) + (pbox->x1 >> 5);

	if ( ((pbox->x1 & 0x1f) + w) < 32)
	{
	    maskpartialbits(pbox->x1, w, startmask);
	    nlwExtra = nlwidth;
	    Duff(h, *p OPEQ startmask; p += nlwExtra);
	}
	else
	{
	    maskbits(pbox->x1, w, startmask, endmask, nlwMiddle);
	    nlwExtra = nlwidth - nlwMiddle;

	    if (startmask && endmask)
	    {
		nlwExtra -= 1;
		while (h--)
		{
		    nlw = nlwMiddle;
		    *p OPEQ startmask;
		    p++;
		    Duff(nlw, *p++ EQWHOLEWORD);
		    *p OPEQ endmask;
		    p += nlwExtra;
		}
	    }
	    else if (startmask && !endmask)
	    {
		nlwExtra -= 1;
		while (h--)
		{
		    nlw = nlwMiddle;
		    *p OPEQ startmask;
		    p++;
		    Duff(nlw, *p++ EQWHOLEWORD);
		    p += nlwExtra;
		}
	    }
	    else if (!startmask && endmask)
	    {
		while (h--)
		{
		    nlw = nlwMiddle;
		    Duff(nlw, *p++ EQWHOLEWORD);
		    *p OPEQ endmask;
		    p += nlwExtra;
		}
	    }
	    else /* no ragged bits at either end */
	    {
		while (h--)
		{
		    nlw = nlwMiddle;
		    Duff(nlw, *p++ EQWHOLEWORD);
		    p += nlwExtra;
		}
	    }
	}
        pbox++;
    }
}



/* stipple a list of boxes

you can use the reduced rasterop for stipples.  if rrop is
black, AND the destination with (not stipple pattern).  if rrop is
white OR the destination with the stipple pattern.  if rrop is invert,
XOR the destination with the stipple pattern.

	MFBSTIPPLEFILLAREA	OPEQ
	mfbStippleWhiteArea	|=
	mfbStippleBlackArea	&=~
	mfbStippleInveryArea	^=
*/

/*ARGSUSED*/
void
MFBSTIPPLEFILLAREA(pDraw, nbox, pbox, alu, pstipple)
    DrawablePtr pDraw;
    int nbox;
    BoxPtr pbox;
    int alu;
    PixmapPtr pstipple;
{
    register unsigned int *psrc;
			/* pointer to bits in tile, if needed */
    int tileHeight;	/* height of the tile */
    register unsigned int srcpix;	

    int nlwidth;	/* width in longwords of the drawable */
    int w;		/* width of current box */
    register int nlw;	/* loop version of nlwMiddle */
    register unsigned int *p;	/* pointer to bits we're writing */
    register int h;	/* height of current box */
    int startmask;
    int endmask;	/* masks for reggedy bits at either end of line */
    int nlwMiddle;	/* number of longwords between sides of boxes */
    int nlwExtra;	/* to get from right of box to left of next span */
    
    register int iy;	/* index of current scanline in tile */


    unsigned int *pbits;	/* pointer to start of drawable */

    if (pDraw->type == DRAWABLE_WINDOW)
    {
	pbits = (unsigned int *) getPrivScreenPtr(pDraw->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDraw->pScreen)->stride >> 2;
    }
    else
    {
	pbits = (unsigned int *) getPrivPixmapPtr(pDraw)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDraw)->stride >> 2;
    }

    tileHeight = pstipple->drawable.height;
    psrc = (unsigned int *)(((hpPrivPixmapPtr)(pstipple->devPrivate.ptr))->bits);

    while (nbox--)
    {
	w = pbox->x2 - pbox->x1;
	h = pbox->y2 - pbox->y1;
	iy = pbox->y1 % tileHeight;
	p = pbits + (pbox->y1 * nlwidth) + (pbox->x1 >> 5);

	if ( ((pbox->x1 & 0x1f) + w) < 32)
	{
	    maskpartialbits(pbox->x1, w, startmask);
	    nlwExtra = nlwidth;
	    while (h--)
	    {
		srcpix = psrc[iy];
		iy = ++iy < tileHeight ? iy : 0;
		*p OPEQ (srcpix & startmask);
		p += nlwExtra;
	    }
	}
	else
	{
	    maskbits(pbox->x1, w, startmask, endmask, nlwMiddle);
	    nlwExtra = nlwidth - nlwMiddle;

	    if (startmask && endmask)
	    {
		nlwExtra -= 1;
		while (h--)
		{
		    srcpix = psrc[iy];
		    iy = ++iy < tileHeight ? iy : 0;
		    nlw = nlwMiddle;
		    *p OPEQ (srcpix & startmask);
		    p++;
		    Duff (nlw, *p++ OPEQ srcpix);
		    *p OPEQ (srcpix & endmask);
		    p += nlwExtra;
		}
	    }
	    else if (startmask && !endmask)
	    {
		nlwExtra -= 1;
		while (h--)
		{
		    srcpix = psrc[iy];
		    iy = ++iy < tileHeight ? iy : 0;
		    nlw = nlwMiddle;
		    *p OPEQ (srcpix & startmask);
		    p++;
		    Duff(nlw, *p++ OPEQ srcpix);
		    p += nlwExtra;
		}
	    }
	    else if (!startmask && endmask)
	    {
		while (h--)
		{
		    srcpix = psrc[iy];
		    iy = ++iy < tileHeight ? iy : 0;
		    nlw = nlwMiddle;
		    Duff(nlw, *p++ OPEQ srcpix);
		    *p OPEQ (srcpix & endmask);
		    p += nlwExtra;
		}
	    }
	    else /* no ragged bits at either end */
	    {
		while (h--)
		{
		    srcpix = psrc[iy];
		    iy = ++iy < tileHeight ? iy : 0;
		    nlw = nlwMiddle;
		    Duff(nlw, *p++ OPEQ srcpix);
		    p += nlwExtra;
		}
	    }
	}
        pbox++;
    }
}


