/* $XConsortium: cfbtegblt.c,v 5.4 91/05/04 11:52:53 keith Exp $ */
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
#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"cfb.h"
#include	"fontstruct.h"
#include	"dixfontstr.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfbmskbits.h"

extern void miImageGlyphBlt();

/*
    this works for fonts with glyphs <= 32 bits wide, on an
    arbitrarily deep display.  Use cfbTEGlyphBlt8 for 8 bit displays.

    This should be called only with a terminal-emulator font;
this means that the FIXED_METRICS flag is set, and that
glyphbounds == charbounds.

    in theory, this goes faster; even if it doesn't, it reduces the
flicker caused by writing a string over itself with image text (since
the background gets repainted per character instead of per string.)
this seems to be important for some converted X10 applications.

    Image text looks at the bits in the glyph and the fg and bg in the
GC.  it paints a rectangle, as defined in the protocol dcoument,
and the paints the characters.

*/

void
cfbTEGlyphBlt(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GC 		*pGC;
    int 	x, y;
    unsigned int nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    unsigned char *pglyphBase;	/* start of array of glyphs */
{
    FontPtr	pfont = pGC->font;
    int widthDst;
    unsigned long *pdstBase;	/* pointer to longword with top row 
				   of current glyph */

    int w;			/* width of glyph and char */
    int h;			/* height of glyph and char */
    register int xpos=x;	/* current x%32  */
    int ypos=y;			/* current y%32 */
    register unsigned char *pglyph;
    int widthGlyph;

    register unsigned long *pdst;/* pointer to current longword in dst */
    int hTmp;			/* counter for height */
    BoxRec bbox;		/* for clipping */

    register int wtmp,xtemp,width;
    unsigned long bgfill,fgfill,*ptemp,tmpDst1,tmpDst2,*pdtmp;
    int tmpx;

    xpos += pDrawable->x;
    ypos += pDrawable->y;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    wtmp = FONTMAXBOUNDS(pfont,characterWidth);
    h = FONTASCENT(pfont) + FONTDESCENT(pfont);
    widthGlyph = GLYPHWIDTHBYTESPADDED(*ppci);

    xpos += FONTMAXBOUNDS(pfont,leftSideBearing);
    ypos -= FONTASCENT(pfont);

    bbox.x1 = xpos;
    bbox.x2 = xpos + (wtmp * nglyph);
    bbox.y1 = ypos;
    bbox.y2 = ypos + h;

    fgfill = PFILL(pGC->fgPixel);
    bgfill = PFILL(pGC->bgPixel);

    switch ((*pGC->pScreen->RectIn)(
                ((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip, &bbox))
    {
      case rgnOUT:
	break;
      case rgnPART:
	/* this is the WRONG thing to do, but it works.
	   calling the non-terminal text is easy, but slow, given
	   what we know about the font.

	   the right thing to do is something like:
	    for each clip rectangle
		compute at which row the glyph starts to be in it,
		   and at which row the glyph ceases to be in it
		compute which is the first glyph inside the left
		    edge, and the last one inside the right edge
		draw a fractional first glyph, using only
		    the rows we know are in
		draw all the whole glyphs, using the appropriate rows
		draw any pieces of the last glyph, using the right rows

	   this way, the code would take advantage of knowing that
	   all glyphs are the same height and don't overlap.

	   one day...
	*/
	miImageGlyphBlt(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);
	break;
      case rgnIN:

        pdtmp = pdstBase + (widthDst * ypos);
        while(nglyph--)
        {

	    pglyph = FONTGLYPHBITS(pglyphBase, *ppci++);
            pdst = pdtmp;
	    hTmp = h;

	    while (hTmp--)
	    {
		x = xpos;
		width = wtmp;
 	        xtemp = 0;

		while (width > 0)
		{
		    tmpx = x & PIM;
		    w = min(width, PPW - tmpx);
		    w = min(w, (32 - xtemp));

		    ptemp = (unsigned long *)(pglyph + (xtemp >> 5));
		    getstipplepixels(ptemp,xtemp,w,0,&bgfill,&tmpDst1);
		    getstipplepixels(ptemp,xtemp,w,1,&fgfill,&tmpDst2);

		    {
			unsigned long tmpDst = tmpDst1 | tmpDst2;
			unsigned long *pdsttmp = pdst + (x >> PWSH);
			putbits(tmpDst,tmpx,w,pdsttmp,pGC->planemask);
		    }
		    x += w;
		    xtemp += w;
		    width -= w;
		}
		pglyph += widthGlyph;
                pdst += widthDst;
	    }
	    xpos += wtmp;
        }     
	break;
    }
}
