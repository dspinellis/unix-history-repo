/*
 */
#include "X.h"
#define  NEED_EVENTS
#include "Xproto.h"
#include "scrnintstr.h"
#include "cursorstr.h"
#include "pixmapstr.h"
#include "inputstr.h"
#include "regionstr.h"
#include <sys/types.h>

#include "hppriv.h"
#include "hyperion.h"

#include "../mfb/mfb.h"
#include "../mfb/maskbits.h"

#ifdef FASTGETBITS
#define getunalignedword(psrc, x, dst) { \
        register int _tmp; \
        FASTGETBITS(psrc, x, 32, _tmp); \
        dst = _tmp; \
}
#else
#define getunalignedword(psrc, x, dst) \
{ \
    dst = (SCRLEFT((unsigned) *(psrc), (x))) | \
          (SCRRIGHT((unsigned) *((psrc)+1), 32-(x))); \
}
#endif  /* FASTGETBITS */

#include "../mfb/fastblt.h"


void
hyperMoveBits(pScreen, planeMask, alu,
	      sourceX, sourceY, destX, destY, w, h)
     ScreenPtr	pScreen;
     u_char	planeMask;
     int	alu, sourceX, sourceY, destX, destY, w, h;
{
    unsigned int *pBase;	
				/* start of src and dst bitmaps */
    int width;	/* add to get to same position in next line */

    unsigned int *psrcLine, *pdstLine;	
				/* pointers to line with current src and dst */
    register unsigned int *psrc;/* pointer to current src longword */
    register unsigned int *pdst;/* pointer to current dst longword */

				/* following used for looping through a line */
    unsigned int startmask, endmask;	/* masks for writing ends of dst */
    int nlMiddle;		/* whole longwords in dst */
    register int nl;		/* temp copy of nlMiddle */
    register unsigned int tmpSrc;
				/* place to store full source word */
    register int xoffSrc;	/* offset (>= 0, < 32) from which to
			           fetch whole longwords fetched 
				   in src */
    int nstart;			/* number of ragged bits at start of dst */
    int nend;			/* number of ragged bits at end of dst */
    int srcStartOver;		/* pulling nstart bits from src
				   overflows into the next word? */

    if (h == 0 || w == 0)
	return;
    
    pBase = (unsigned int *) getPrivScreenPtr(pScreen)->bits;
    width = getPrivScreenPtr(pScreen)->memWidth >> 5;

    if (sourceY < destY) /* start at last scanline of rectangle */
    {
	psrcLine = pBase + ((sourceY+h-1) * width);
	pdstLine = pBase + ((destY+h-1) * width);
	width = -width;
    }
    else /* start at first scanline */
    {
	psrcLine = pBase + (sourceY * width);
	pdstLine = pBase + (destY * width);
    }

    /* special case copy */
    if (alu == GXcopy)
    {
	register unsigned int bits;
	register unsigned int bits1;
	int xoffSrc, xoffDst;
	int	leftShift, rightShift;

	if ((destX & 0x1f) + w <= 32)
	{
	    pdst = pdstLine + (destX >> 5);
	    psrc = psrcLine + (sourceX >> 5);
	    xoffSrc = sourceX & 0x1f;
	    xoffDst = destX & 0x1f;
	    while (h--)
	    {
		getandputbits(psrc, xoffSrc, xoffDst, w, pdst);
		psrc += width;
		pdst += width;
	    }
	}
	else
	{
	    maskbits(destX, w, startmask, endmask, nlMiddle);
	    if (sourceX >= destX)
	    {
		xoffSrc = sourceX & 0x1f;
		xoffDst = destX & 0x1f;
		pdstLine += (destX >> 5);
		psrcLine += (sourceX >> 5);
		if (xoffSrc == xoffDst)
		{
		    while (h--)
		    {
			psrc = psrcLine;
			pdst = pdstLine;
			pdstLine += width;
			psrcLine += width;
			if (startmask)
			{
			    *pdst = (*pdst & ~startmask) | (*psrc++ & startmask);
			    pdst++;
			}
			nl = nlMiddle;

			DuffL(nl, label1, *pdst++ = *psrc++;)
			if (endmask)
			    *pdst = (*pdst & ~endmask) | (*psrc++ & endmask);
		    }
		}
		else
		{
		    if (xoffSrc > xoffDst)
		    {
			leftShift = (xoffSrc - xoffDst);
			rightShift = 32 - leftShift;
		    }
		    else
		    {
			rightShift = (xoffDst - xoffSrc);
			leftShift = 32 - rightShift;
		    }
		    while (h--)
		    {
			psrc = psrcLine;
			pdst = pdstLine;
			pdstLine += width;
			psrcLine += width;
			bits = 0;
			if (xoffSrc > xoffDst)
			    bits = *psrc++;
			if (startmask)
			{
			    bits1 = SCRLEFT(bits,leftShift);
			    bits = *psrc++;
			    bits1 |= SCRRIGHT(bits,rightShift);
			    *pdst = (*pdst & ~startmask) |
				(bits1 & startmask);
			    pdst++;
			}
			nl = nlMiddle;

			DuffL (nl,label2,
			       bits1 = BitLeft(bits, leftShift);
			       bits = *psrc++;
			       *pdst++ = bits1 | BitRight(bits, rightShift);
			       )
			if (endmask)
			{
			    bits1 = SCRLEFT(bits, leftShift);
			    if (SCRLEFT(endmask, rightShift))
			    {
				bits = *psrc++;
				bits1 |= SCRRIGHT(bits, rightShift);
			    }
			    *pdst = (*pdst & ~endmask) |
				(bits1 & endmask);
			}
		    }
		}
	    }
	    else	/* walk source right to left */
	    {
		xoffSrc = (sourceX + w - 1) & 0x1f;
		xoffDst = (destX+w - 1) & 0x1f;
		pdstLine += ((destX+w-1) >> 5) + 1;
		psrcLine += ((sourceX+w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst)
		{
		    while (h--)
		    {
			psrc = psrcLine;
			pdst = pdstLine;
			pdstLine += width;
			psrcLine += width;
			if (endmask)
			{
			    pdst--;
			    *pdst = (*pdst & ~endmask) | (*--psrc & endmask);
			}
			nl = nlMiddle;

			DuffL(nl,label3, *--pdst = *--psrc;)
			if (startmask)
			{
			    --pdst;
			    *pdst = (*pdst & ~startmask) | (*--psrc & startmask);
			}
		    }
		}
		else
		{
		    if (xoffDst > xoffSrc)
		    {
			rightShift = (xoffDst - xoffSrc);
			leftShift = 32 - rightShift;
		    }
		    else
		    {
			leftShift = (xoffSrc - xoffDst);
			rightShift = 32 - leftShift;
		    }
		    while (h--)
		    {
			psrc = psrcLine;
			pdst = pdstLine;
			pdstLine += width;
			psrcLine += width;
			bits = 0;
			if (xoffDst > xoffSrc)
			    bits = *--psrc;
			if (endmask)
			{
			    bits1 = SCRRIGHT(bits, rightShift);
			    bits = *--psrc;
			    bits1 |= SCRLEFT(bits, leftShift);
			    pdst--;
			    *pdst = (*pdst & ~endmask) |
				(bits1 & endmask);
			}
			nl = nlMiddle;

			DuffL (nl, label4,
			       bits1 = BitRight(bits, rightShift);
			       bits = *--psrc;
			       *--pdst = bits1 | BitLeft(bits, leftShift);
			       )

			if (startmask)
			{
			    bits1 = SCRRIGHT(bits, rightShift);
			    if (SCRRIGHT (startmask, leftShift))
			    {
				bits = *--psrc;
				bits1 |= SCRLEFT(bits, leftShift);
			    }
			    --pdst;
			    *pdst = (*pdst & ~startmask) |
				(bits1 & startmask);
			}
		    }
		}
	    }
	}
    }
    else /* do some rop */
    {
	/* x direction doesn't matter for < 1 longword */
	if (w <= 32)
	{
	    int srcBit, dstBit;	/* bit offset of src and dst */

	    pdstLine += (destX >> 5);
	    psrcLine += (sourceX >> 5);
	    psrc = psrcLine;
	    pdst = pdstLine;

	    srcBit = sourceX & 0x1f;
	    dstBit = destX & 0x1f;

	    while (h--)
	    {
		getandputrop(psrc, srcBit, dstBit, w, pdst, alu)
		pdst += width;
		psrc += width;
	    }
	}
	else
	{
	    maskbits(destX, w, startmask, endmask, nlMiddle)
	    if (startmask)
		nstart = 32 - (destX & 0x1f);
	    else
		nstart = 0;
	    if (endmask)
		nend = destX+w & 0x1f;
	    else
		nend = 0;

	    xoffSrc = ((sourceX & 0x1f) + nstart) & 0x1f;
	    srcStartOver = ((sourceX & 0x1f) + nstart) > 31;

	    if (sourceX >= destX) /* move left to right */
	    {
		pdstLine += (destX >> 5);
		psrcLine += (sourceX >> 5);

		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;

		    if (startmask)
		    {
			getandputrop(psrc, (sourceX & 0x1f), 
				     (destX & 0x1f), nstart, pdst, alu)
			pdst++;
			if (srcStartOver)
			    psrc++;
		    }

		    /* special case for aligned operations */
		    if (xoffSrc == 0)
		    {
			nl = nlMiddle;
			while (nl--)
			{
			    DoRop (*pdst, alu, *psrc++, *pdst);
			    pdst++;
			}
		    }
		    else
		    {
			nl = nlMiddle + 1;
			while (--nl)
			{
			    getunalignedword (psrc, xoffSrc, tmpSrc)
			    DoRop (*pdst, alu, tmpSrc, *pdst);
			    pdst++;
			    psrc++;
			}
		    }

		    if (endmask)
		    {
			getandputrop0(psrc, xoffSrc, nend, pdst, alu);
		    }

		    pdstLine += width;
		    psrcLine += width;
		}
	    }
	    else /* move right to left */
	    {
		pdstLine += (destX+w >> 5);
		psrcLine += (sourceX+w >> 5);
		/* if fetch of last partial bits from source crosses
		   a longword boundary, start at the previous longword
		   */
		if (xoffSrc + nend >= 32)
		    --psrcLine;

		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;

		    if (endmask)
		    {
			getandputrop0(psrc, xoffSrc, nend, pdst, alu);
		    }

		    nl = nlMiddle + 1;
		    while (--nl)
		    {
			--psrc;
			--pdst;
			getunalignedword(psrc, xoffSrc, tmpSrc)
			DoRop(*pdst, alu, tmpSrc, *pdst);
		    }

		    if (startmask)
		    {
			if (srcStartOver)
			    --psrc;
			--pdst;
			getandputrop(psrc, (sourceX & 0x1f), 
				     (destX & 0x1f), nstart, pdst, alu)
		    }

		    pdstLine += width;
		    psrcLine += width;
		}
	    } /* move right to left */
	}
    }
}
