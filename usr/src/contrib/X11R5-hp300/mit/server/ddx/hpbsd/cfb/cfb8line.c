/*
 * $XConsortium: cfb8line.c,v 1.19 91/07/09 16:07:32 rws Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include "X.h"

#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "regionstr.h"
#include "scrnintstr.h"
#include "mistruct.h"

#include "cfb.h"
#include "cfbmskbits.h"
#include "cfbrrop.h"

#if defined(__GNUC__) && defined(mc68020)
#define STUPID volatile
#define REARRANGE
#else
#define STUPID
#endif

#ifdef __GNUC__
/* lame compiler doesn't even look at 'register' attributes */
#define I_H do{
#define I_T }while(0);
#define IMPORTANT_START I_H I_H I_H I_H I_H I_H I_H I_H I_H I_H
#define IMPORTANT_END	I_T I_T I_T I_T I_T I_T I_T I_T I_T I_T
#else
#define IMPORTANT_START
#define IMPORTANT_END
#endif

#define OUTCODES(result, x, y, box) \
    if (x < box->x1) \
	result |= OUT_LEFT; \
    if (x >= box->x2) \
	result |= OUT_RIGHT; \
    if (y < box->y1) \
	result |= OUT_ABOVE; \
    if (y >= box->y2) \
	result |= OUT_BELOW;

#define isClipped(c,ul,lr)  ((((c) - (ul)) | ((lr) - (c))) & ClipMask)

#ifdef POLYSEGMENT

# ifdef sun
#  define WIDTH_FAST  1152
# endif

# ifdef ultrix
#  define WIDTH_FAST  1024
# endif

# ifdef Mips
#  define WIDTH_FAST 4096
# endif
# ifdef WIDTH_FAST
#  if WIDTH_FAST == 1024
#   define FAST_MUL(y)	((y) << 10)
#  endif

#  if WIDTH_FAST == 1152
#   define FAST_MUL(y)	(((y) << 10) + ((y) << 7))
#  endif

#  if WIDTH_FAST == 1280
#   define FAST_MUL(y)	(((y) << 10) + ((y) << 8))
#  endif

#  if WIDTH_FAST == 2048
#   define FAST_MUL(y)	((y) << 11)
#  endif

#  if WIDTH_FAST == 4096
#   define FAST_MUL(y)	((y) << 12)
#  endif
# endif

# if defined(WIDTH_SHIFT)
#  ifdef FAST_MUL
#   define FUNC_NAME(e)	    RROP_NAME(RROP_NAME_CAT(e,Shift))
#   if RROP == GXcopy
#    define INCLUDE_OTHERS
#    define SERIOUS_UNROLLING
#   endif
#   define INCLUDE_DRAW
#   define NWIDTH(nwidth)   WIDTH_FAST
#   define WIDTH_MUL(y,w)   FAST_MUL(y)
#  endif
# else
#  define FUNC_NAME(e)	    RROP_NAME(e)
#  define WIDTH_MUL(y,w)    ((y) * (w))
#  define NWIDTH(nwidth)    (nwidth)
#  define INCLUDE_DRAW
#  if !defined (FAST_MUL) && RROP == GXcopy
#   define INCLUDE_OTHERS
#   define SERIOUS_UNROLLING
#  endif
# endif
#else

# define INCLUDE_DRAW
# define WIDTH_MUL(y,w)	((y) * (w))
# define NWIDTH(nwidth)	nwidth
# ifdef PREVIOUS
#  define FUNC_NAME(e)	RROP_NAME(RROP_NAME_CAT(e,Previous))
# else
#  define FUNC_NAME(e)	RROP_NAME(e)
#  if RROP == GXcopy
#   define INCLUDE_OTHERS
#   ifdef PLENTIFUL_REGISTERS
#    define SAVE_X2Y2
#   endif
#   define ORIGIN
#   define SERIOUS_UNROLLING
#  else
#   define EITHER_MODE
#  endif
# endif
#endif

#ifdef INCLUDE_DRAW

int
#ifdef POLYSEGMENT
FUNC_NAME(cfb8SegmentSS1Rect) (pDrawable, pGC, nseg, pSegInit)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		nseg;
    xSegment	*pSegInit;
#else
FUNC_NAME(cfb8LineSS1Rect) (pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int	mode;		/* Origin or Previous */
    int	npt;		/* number of points */
    DDXPointPtr pptInit;
#endif
{
    register int    e;
    register int    y1_or_e1;
    register unsigned char   *addrb;
    register int    stepmajor;
    register int    stepminor;
#ifndef REARRANGE
    register int    e3;
#endif
#ifdef mc68000
    register short  x1_or_len;
#else
    register int    x1_or_len;
#endif
    RROP_DECLARE

#ifdef SAVE_X2Y2
# define c2 y2
#else
    register int    c2;
#endif

    register int    upperleft, lowerright;
#ifdef POLYSEGMENT
    register int    capStyle;
#endif
#ifdef SAVE_X2Y2
    register int    x2, y2;
# define X1  x1_or_len
# define Y1  y1_or_e1
# define X2  x2
# define Y2  y2
#else
# ifdef POLYSEGMENT
#  define X1  x1_or_len
#  define Y1  y1_or_e1
# else
#  define X1  intToX(y1_or_e1)
#  define Y1  intToY(y1_or_e1)
# endif
# define X2  intToX(c2)
# define Y2  intToY(c2)
#endif
    unsigned long    ClipMask = 0x80008000;
    unsigned char   *addr;
    int		    nwidth;
    cfbPrivGCPtr    devPriv;
    BoxPtr	    extents;
    int		    *ppt;

    devPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr); 
    cfbGetByteWidthAndPointer (pDrawable, nwidth, addr);
    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#ifndef REARRANGE
    RROP_FETCH_GCPRIV(devPriv);
#endif
    extents = &devPriv->pCompositeClip->extents;
    c2 = *((int *) &pDrawable->x);
    c2 -= (c2 & 0x8000) << 1;
    upperleft = *((int *) &extents->x1) - c2;
    lowerright = *((int *) &extents->x2) - c2 - 0x00010001;
    addr = addr + WIDTH_MUL(pDrawable->y,nwidth) + pDrawable->x;
#ifdef POLYSEGMENT
    capStyle = pGC->capStyle - CapNotLast;
    ppt = (int *) pSegInit;
    while (nseg--)
#else

#ifdef EITHER_MODE
    mode -= CoordModePrevious;
#endif
    ppt = (int *) pptInit;
    c2 = *ppt++;
    if (isClipped (c2, upperleft, lowerright))
    {
#ifndef ORIGIN
#ifdef EITHER_MODE
	if (!mode)
#endif
	{
	    e = *ppt;
	    *ppt = e + c2 - ((e & 0x8000) << 1);
	}
#endif	
	return 1;
    }
#ifdef SAVE_X2Y2
    intToCoord(c2,x2,y2);
#endif
    addrb = addr + WIDTH_MUL(Y2, nwidth) + X2;
    while (--npt)
#endif
    {
#ifdef POLYSEGMENT
	y1_or_e1 = ppt[0];
	c2 = ppt[1];
	ppt += 2;
	if (isClipped(y1_or_e1,upperleft,lowerright)|isClipped(c2,upperleft,lowerright))
	    break;
	intToCoord(y1_or_e1,x1_or_len,y1_or_e1);
	/* compute now to avoid needing x1, y1 later */
	addrb = addr + WIDTH_MUL(y1_or_e1, nwidth) + x1_or_len;
#else
#ifndef SAVE_X2Y2
	y1_or_e1 = c2;
#else
	y1_or_e1 = y2;
	x1_or_len = x2;
#endif
#ifndef ORIGIN
	e = c2;
	c2 = *ppt++;
#ifdef EITHER_MODE
	if (!mode)
#endif
	    c2 = c2 + e - ((c2 & 0x8000) << 1);
#else
	c2 = *ppt++;
#endif
	if (isClipped (c2, upperleft, lowerright))
	{
#ifndef ORIGIN
#ifdef EITHER_MODE
	    if (!mode)
#endif
	    {
		ppt[-2] = e;
		ppt[-1] = c2;
	    }
#endif
	    break;
	}
#ifdef SAVE_X2Y2
	intToCoord(c2,x2,y2);
#endif
#endif
	stepmajor = 1;
	if ((x1_or_len = X2 - X1) < 0)
	{
	    x1_or_len = -x1_or_len;
	    stepmajor = -1;
	}
	stepminor = NWIDTH(nwidth);
	if ((y1_or_e1 = Y2 - Y1) < 0)
	{
	    y1_or_e1 = -y1_or_e1;
	    stepminor = -stepminor;
	}
#ifdef POLYSEGMENT
	/*
	 * although the horizontal code works for polyline, it
	 * slows down 10 pixel lines by 15%.  Thus, this
	 * code is optimized for horizontal segments and
	 * random orientation lines, which seems like a reasonable
	 * assumption
	 */
	if (y1_or_e1 != 0)
	{
#endif
	if (x1_or_len < y1_or_e1)
	{
#ifdef REARRANGE
	    register int	e3;
#endif

	    e3 = x1_or_len;
	    x1_or_len = y1_or_e1;
	    y1_or_e1 = e3;

	    e3 = stepminor;
	    stepminor = stepmajor;
	    stepmajor = e3;
	}

	e = -x1_or_len;
#ifdef POLYSEGMENT
	if (!capStyle)
	    x1_or_len--;
#endif

	{
#ifdef REARRANGE
	register int e3;
	RROP_DECLARE
	RROP_FETCH_GCPRIV(devPriv);
#endif

	y1_or_e1 = y1_or_e1 << 1;
	e3 = e << 1;

#define body {\
	    RROP_SOLID(addrb); \
	    addrb += stepmajor; \
	    e += y1_or_e1; \
	    if (e >= 0) \
	    { \
		addrb += stepminor; \
		e += e3; \
	     } \
	}

#ifdef LARGE_INSTRUCTION_CACHE

# ifdef SERIOUS_UNROLLING
#  define UNROLL	16
# else
#  define UNROLL	4
# endif
# define CASE(n)	case -n: body

	while ((x1_or_len -= UNROLL) >= 0)
	{
	    body body body body
# if UNROLL >= 8
	    body body body body
# endif
# if UNROLL >= 12
	    body body body body
# endif
# if UNROLL >= 16
	    body body body body
# endif
	}
	switch (x1_or_len)
	{
	CASE(1) CASE(2) CASE(3)
# if UNROLL >= 8
	CASE(4) CASE(5) CASE(6) CASE(7)
# endif
# if UNROLL >= 12
	CASE(8) CASE(9) CASE(10) CASE(11)
# endif
# if UNROLL >= 16
	CASE(12) CASE(13) CASE(14) CASE(15)
# endif
	}
#else

	IMPORTANT_START
	IMPORTANT_START

	if (x1_or_len & 1)
	    body
	x1_or_len >>= 1;
	while (x1_or_len--) {
	    body body
	}

	IMPORTANT_END
	IMPORTANT_END
#endif

#ifdef POLYSEGMENT
	RROP_SOLID(addrb);
#endif
	}
#undef body
#ifdef POLYSEGMENT
	}
	else
	{
# ifndef POLYSEGMENT
	    unsigned char    *t;
#endif

# ifdef REARRANGE
	    register int    e3;
	    RROP_DECLARE
	    RROP_FETCH_GCPRIV(devPriv);
# endif
	    if (stepmajor < 0)
	    {
		addrb -= x1_or_len;
# ifndef POLYSEGMENT
		t = addrb;
# else
		if (capStyle)
		    x1_or_len++;
		else
# endif
		    addrb++;
	    }
	    else
	    {
# ifndef POLYSEGMENT
		t = addrb + x1_or_len;
# else
		if (capStyle)
		    x1_or_len++;
# endif
	    }
	    y1_or_e1 = ((int) addrb) & 3;
	    addrb = addrb - y1_or_e1;
	    if (y1_or_e1 + x1_or_len <= PPW)
	    {
		if (x1_or_len)
		{
		    maskpartialbits(y1_or_e1, x1_or_len, e)
		    RROP_SOLID_MASK((int *) addrb, e);
		}
	    }
	    else
	    {
	    	maskbits(y1_or_e1, x1_or_len, e, e3, x1_or_len)
	    	if (e)
	    	{
		    RROP_SOLID_MASK((int *) addrb, e);
		    addrb += 4;
	    	}
		RROP_SPAN(addrb, x1_or_len)
	    	if (e3)
		    RROP_SOLID_MASK((int *) addrb, e3);
	    }
# ifndef POLYSEGMENT
	    addrb = t;
# endif
	}
#endif
    }
#ifdef POLYSEGMENT
    if (nseg >= 0)
	return (xSegment *) ppt - pSegInit;
#else
    if (npt)
	return ((DDXPointPtr) ppt - pptInit) - 1;
#endif

#ifndef POLYSEGMENT
# ifndef ORIGIN
#  define C2  c2
# else
#  define C2  ppt[-1]
# endif
    if (pGC->capStyle != CapNotLast && C2 != *((int *) pptInit))
    {
# ifdef REARRANGE
	RROP_DECLARE

	RROP_FETCH_GCPRIV(devPriv);
# endif
	RROP_SOLID (addrb);
    }
#endif
    return -1;
}

#endif /* INCLUDE_DRAW */


#ifdef INCLUDE_OTHERS

extern void cfb8ClippedLineCopy(), cfb8ClippedLineXor(), cfb8ClippedLineGeneral(); 

#ifdef POLYSEGMENT

extern int cfb8SegmentSS1RectCopy(), cfb8SegmentSS1RectXor(), cfb8SegmentSS1RectGeneral(); 
#ifdef FAST_MUL
extern int cfb8SegmentSS1RectShiftCopy();
#endif

void
cfb8SegmentSS1Rect (pDrawable, pGC, nseg, pSegInit)
    DrawablePtr	    pDrawable;
    GCPtr	    pGC;
    int		    nseg;
    xSegment	    *pSegInit;
{
    int	    (*func)();
    void    (*clip)();
    int	    drawn;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr); 
    switch (devPriv->rop)
    {
    case GXcopy:
	func = cfb8SegmentSS1RectCopy;
	clip = cfb8ClippedLineCopy;
#ifdef FAST_MUL
	if (cfbGetByteWidth (pDrawable) == WIDTH_FAST)
	    func = cfb8SegmentSS1RectShiftCopy;
#endif
	break;
    case GXxor:
	func = cfb8SegmentSS1RectXor;
	clip = cfb8ClippedLineXor;
	break;
    default:
	func = cfb8SegmentSS1RectGeneral;
	clip = cfb8ClippedLineGeneral;
	break;
    }
    while (nseg)
    {
	drawn = (*func) (pDrawable, pGC, nseg, pSegInit);
	if (drawn == -1)
	    break;
	(*clip) (pDrawable, pGC,
			 pSegInit[drawn-1].x1, pSegInit[drawn-1].y1,
			 pSegInit[drawn-1].x2, pSegInit[drawn-1].y2,
			 &devPriv->pCompositeClip->extents,
			 pGC->capStyle == CapNotLast);
	pSegInit += drawn;
	nseg -= drawn;
    }
}

#else /* POLYSEGMENT */

extern int cfb8LineSS1RectCopy(), cfb8LineSS1RectXor(), cfb8LineSS1RectGeneral(); 
extern int cfb8LineSS1RectPreviousCopy();

void
cfb8LineSS1Rect (pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		mode;
    int		npt;
    DDXPointPtr	pptInit;
{
    int	    (*func)();
    void    (*clip)();
    int	    drawn;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr); 
    switch (devPriv->rop)
    {
    case GXcopy:
	func = cfb8LineSS1RectCopy;
	clip = cfb8ClippedLineCopy;
	if (mode == CoordModePrevious)
	    func = cfb8LineSS1RectPreviousCopy;
	break;
    case GXxor:
	func = cfb8LineSS1RectXor;
	clip = cfb8ClippedLineXor;
	break;
    default:
	func = cfb8LineSS1RectGeneral;
	clip = cfb8ClippedLineGeneral;
	break;
    }
    while (npt > 1)
    {
	drawn = (*func) (pDrawable, pGC, mode, npt, pptInit);
	if (drawn == -1)
	    break;
	(*clip) (pDrawable, pGC,
			 pptInit[drawn-1].x, pptInit[drawn-1].y,
			 pptInit[drawn].x, pptInit[drawn].y,
			 &devPriv->pCompositeClip->extents,
			 drawn != npt - 1 || pGC->capStyle == CapNotLast);
	pptInit += drawn;
	npt -= drawn;
    }
}

#define round(dividend, divisor) \
( (((dividend)<<1) + (divisor)) / ((divisor)<<1) )
#define ceiling(m,n)  (((m)-1)/(n) + 1)
#define SignTimes(sign,n)   (((sign) < 0) ? -(n) : (n))

cfbClipPoint (oc, xp, yp, dx, dy, boxp)
    int	oc;
    int	*xp, *yp;
    BoxPtr  boxp;
{
    int	x, y;
    int	adx, ady, signdx, signdy;
    int	utmp;
    
    signdx = 1;
    if (dx < 0)
    {
    	signdx = -1;
    	dx = -dx;
    }
    signdy = 1;
    if (dy  < 0)
    {
    	signdy = -1;
    	dy = -dy;
    }
    if (oc & (OUT_LEFT | OUT_RIGHT))
    {
    	if (oc & OUT_LEFT)
    	{
	    x = boxp->x1;
	    utmp = x - *xp;
    	}
    	else
    	{
	    x = boxp->x2 - 1;
	    utmp = *xp - x;
    	}
    	utmp *= dy;
	if (dy > dx)
	{
	    utmp = (utmp << 1) - dy + 1;
	    y = *yp + SignTimes(signdy, ceiling(utmp, (dx << 1)));
	}
	else
	{
    	    y = *yp + SignTimes(signdy, round(utmp, dx));
	}
	oc = 0;
	OUTCODES (oc, x, y, boxp);
    }
    if (oc & (OUT_ABOVE | OUT_BELOW))
    {
    	if (oc & OUT_ABOVE)
    	{
    	    y = boxp->y1;
    	    utmp = y - *yp;
    	}
    	else
    	{
    	    y = boxp->y2 - 1;
    	    utmp = *yp - y;
    	}
	utmp *= dx;
	if (dx > dy)
	{
	    utmp = (utmp << 1) - dx + 1;
	    x = *xp + SignTimes(signdx, ceiling(utmp, (dy << 1)));
	}
	else
	{
	    x = *xp + SignTimes(signdx, round(utmp, dy));
	}
	oc = 0;
	OUTCODES (oc, x, y, boxp);
    }
    *xp = x;
    *yp = y;
    return oc;
}

#endif /* else POLYSEGMENT */
#endif /* INCLUDE_OTHERS */

#if !defined(POLYSEGMENT) && !defined (PREVIOUS)

void
RROP_NAME (cfb8ClippedLine) (pDrawable, pGC, x1, y1, x2, y2, boxp, shorten)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		x1, y1, x2, y2;
    BoxPtr	boxp;
    Bool	shorten;
{
    int		    oc1, oc2;
    int		    signdx, signdy, axis, e, e1, e3, len;
    int		    adx, ady;

    unsigned char   *addr;
    int		    nwidth;
    int		    stepx, stepy;
    int		    xorg, yorg;


    cfbGetByteWidthAndPointer(pDrawable, nwidth, addr);

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    xorg = pDrawable->x;
    yorg = pDrawable->y;
    x1 += xorg;
    y1 += yorg;
    x2 += xorg;
    y2 += yorg;
    oc1 = 0;
    oc2 = 0;
    OUTCODES (oc1, x1, y1, boxp);
    OUTCODES (oc2, x2, y2, boxp);

    if (oc1 & oc2)
	return;

    signdx = 1;
    stepx = 1;
    if ((adx = x2 - x1) < 0)
    {
	adx = -adx;
	signdx = -1;
	stepx = -1;
    }
    signdy = 1;
    stepy = nwidth;
    if ((ady = y2 - y1) < 0)
    {
	ady = -ady;
	signdy = -1;
	stepy = -nwidth;
    }
    axis = X_AXIS;
    if (adx <= ady)
    {
	int	t;

	t = adx;
	adx = ady;
	ady = t;

	t = stepx;
	stepx = stepy;
	stepy = t;
	
	axis = Y_AXIS;
    }
    e1 = ady << 1;
    e3 = - (adx << 1);
    e = - adx;
    len = adx;
    if (oc2)
    {
	int xt = x2, yt = y2;
	int	dx = x2 - x1, dy = y2 - y1;
	int change;

	oc2 = cfbClipPoint (oc2, &xt, &yt, -dx, -dy, boxp);
	if (axis == Y_AXIS)
	    change = y2 - yt;
	else
	    change = x2 - xt;
	if (change < 0)
	    change = -change;
	len -= change;
    } else if (shorten)
	len--;
    if (oc1)
    {
	int	xt = x1, yt = y1;
	int	dx = x2 - x1, dy = y2 - y1;
	int	changex, changey;

	oc1 = cfbClipPoint (oc1, &xt, &yt, dx, dy, boxp);
	changex = x1 - xt;
	if (changex < 0)
	    changex = -changex;
	changey = y1 - yt;
	if (changey < 0)
	    changey = -changey;
	if (axis == X_AXIS)
	{
	    len -= changex;
	    e = e + changey * e3 + changex * e1;
	}
	else
	{
	    len -= changey;
	    e = e + changex * e3 + changey * e1;
	}
	x1 = xt;
	y1 = yt;
    }
    if (oc1 | oc2 || len < 0)
	return;

    {
    register unsigned char	*addrb;
    RROP_DECLARE

    RROP_FETCH_GC(pGC);

    addrb = addr + (y1 * nwidth) + x1;

#ifndef REARRANGE
    if (!ady)
    {
#define body	{ RROP_SOLID(addrb); addrb += stepx; }
	while (len >= 4)
	{
	    body body body body
	    len -= 4;
	}
	switch (len)
	{
	case  3: body case 2: body case 1: body
	}
#undef body
    }
    else
#endif
    {
#define body {\
	    RROP_SOLID(addrb); \
	    addrb += stepx; \
	    e += e1; \
	    if (e >= 0) \
	    { \
		addrb += stepy; \
		e += e3; \
	     } \
	}

#ifdef LARGE_INSTRUCTION_CACHE
	while ((len -= 4) >= 0)
	{
	    body body body body
	}
	switch (len)
	{
	case  -1: body case -2: body case -3: body
	}
#else
	IMPORTANT_START

	while ((len -= 2) >= 0)
	{
	    body body
	}
	if (len & 1)
	    body;

	IMPORTANT_END

#endif
    }
    RROP_SOLID(addrb);
#undef body

    }
}

#endif /* !POLYSEGMENT && !PREVIOUS */
