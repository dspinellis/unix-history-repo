/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
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

#include <stdio.h>
#include <sys/types.h>

#include "X.h"
#include "Xproto.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "regionstr.h"

#include "../cfb/cfb.h"
#include "topcat.h"
#include "gcstruct.h"
#include "windowstr.h"
#include "mi.h"

extern u_char XHP_NewRule[16][6];

#define OUTCODES(result, x, y, pbox) \
    if (x < pbox->x1) \
        result |= OUT_LEFT; \
    if (y < pbox->y1) \
        result |= OUT_ABOVE; \
    if (x >= pbox->x2) \
        result |= OUT_RIGHT; \
    if (y >= pbox->y2) \
        result |= OUT_BELOW;


/********************************************************************
 * Routine: topcatZeroLine - draws 0-width lines quickly
 *   Most of this code was borrowed (really - I'll give it back!) from
 *   mfbline.c
 */

void
topcatZeroLine(dst, pgc, mode, nptInit, pptInit)
     DrawablePtr dst;
     GCPtr pgc;
     int mode;
     int nptInit;		/* number of points in polyline */
     DDXPointRec *pptInit;	/* points in the polyline */
{
    int nboxInit;
    register int nbox;
    BoxPtr pboxInit;
    register BoxPtr pbox;
    int nptTmp;
    int xorg, yorg;
    DDXPointPtr ppt;
    int npt;

    DDXPointRec pt1, pt2, pt1Orig, pt2Orig;
    int dx, dy;
    int adx, ady;
    int signdx, signdy;
    register int len;

    int du, dv;
    register int e, e1, e2;
    int axis;		/* major axis of line */
    int yinc;

    int err;
    unsigned int oc1, oc2;
    int clip1, clip2, clipdx, clipdy;
    int clipDone;
    int tmp;

    register int x,y;	/* current point on the line */
    int i;		/* traditional name for loop counter */
    CARD8 *pDstBase, *pPixel;
    unsigned int dstWidth;
    unsigned int fore = pgc->fgPixel;


    pboxInit = ((cfbPrivGC *)(pgc->devPriv))->pCompositeClip->rects;
    nboxInit = ((cfbPrivGC *)(pgc->devPriv))->pCompositeClip->numRects;

    if (dst->type == DRAWABLE_WINDOW) {
	xorg = ((WindowPtr)dst)->absCorner.x;
	yorg = ((WindowPtr)dst)->absCorner.y;
	pDstBase = (CARD8 *)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->bits;
	dstWidth = (unsigned int)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->stride;

    }
    else {
	cfbPrivPixmapPtr pPriv = (cfbPrivPixmapPtr)((PixmapPtr)dst)->devPrivate;
	if(((PixmapPtr)dst)->devKind == PIXMAP_FRAME_BUFFER) {
	  xorg = ((hpChunk *)pPriv->pChunk)->x;
	  yorg = ((hpChunk *)pPriv->pChunk)->y;
	  pDstBase = (CARD8 *)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->bits;
	  dstWidth = (unsigned int)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->stride;
	}
	else {
	  /* XXX THIS DOESN"T WORK, AND WILL NEVER WORK */
	  xorg = 0;
	  yorg = 0;
          pDstBase = (CARD8 *) pPriv->bits;
          dstWidth = (unsigned int) pPriv->stride;
	}

    }

    ppt = pptInit;
    npt = nptInit;
    if (mode == CoordModeOrigin) {
	if(xorg || yorg) {
	    while(npt--) {    
	        ppt->x += xorg;
	        ppt++->y += yorg;
	    }
	}
    }
    else {
	ppt->x += xorg;
	ppt->y += yorg;
	npt--;
	while(npt--) {
	    ppt++;
	    ppt->x += (ppt-1)->x;
	    ppt->y += (ppt-1)->y;
	}
    }

    ppt = pptInit;
    npt = nptInit;
    while (--npt) {

	DDXPointPtr pspan;
	DDXPointPtr pspanInit;
 	int *pwidth;
	int *pwidthInit;
	int width;

	nbox = nboxInit;
	pbox = pboxInit;

	pt1 = *ppt++;
	pt2 = *ppt;

	dx = pt2.x - pt1.x;
	dy = pt2.y - pt1.y;
	adx = abs(dx);
	ady = abs(dy);

	/* 
	 * ~jra Hack to go to directly to the block mover
	 */
	if (dx == 0){
		int starty;
		if(dy < 0) {
		  starty = pt2.y + 1;
		}
		else {
		  starty = pt1.y;
		}
		tcPaintBlockClipped(dst, pgc,
			pt1.x,		/* x coor */
			starty, /* y coor (topmost) */
			1,		/* width */
			ady 		/* height */
			);
		continue;
	}
	if (dy == 0) {
		int startx;
		if(dx < 0) {
		  startx = pt2.x + 1;
		}
		else {
		  startx = pt1.x;
		}
		tcPaintBlockClipped(dst, pgc,
			startx, /* x coor (leftmost) */
			pt1.y,		/* y coor */
			adx, 		/* width */
			1		/* height */
			);
		continue;
	}
	topcatMaskConfig(dst->pScreen, pgc->planemask, pgc->alu);
	signdx = sign(dx);
	signdy = sign(dy);

	if (adx > ady)
	{
	    axis = X_AXIS;
	    e1 = ady * 2;
	    e2 = e1 - 2 * adx;
	    e = e1 - adx;
	}
	else
	{
	    axis = Y_AXIS;
            e1 = adx*2;
            e2 = e1 - 2*ady;
            e = e1 - ady;
	}
	yinc = signdy * dstWidth;

	pt1Orig = pt1;
	pt2Orig = pt2;

	while(nbox--) {
	  BoxRec box;

	  pt1 = pt1Orig;
	  pt2 = pt2Orig;
	  clipDone = 0;
          box.x1 = pbox->x1;
          box.y1 = pbox->y1;
          box.x2 = pbox->x2-1;
          box.y2 = pbox->y2-1;
          clip1 = 0;
          clip2 = 0;

          oc1 = 0;
          oc2 = 0;
          OUTCODES(oc1, pt1.x, pt1.y, pbox);
          OUTCODES(oc2, pt2.x, pt2.y, pbox);

          if (oc1 & oc2)
              clipDone = -1;
          else if ((oc1 | oc2) == 0)
              clipDone = 1;
          else /* have to clip */
              clipDone = mfbClipLine(pbox, box, &pt1Orig, &pt1, &pt2,
                                     adx, ady, signdx, signdy, axis,
                                     &clip1, &clip2);
	  if(clipDone == -1) {
	    /*
	     * the line doesn't go through this box
	     */
	    pbox++;
	    continue;
	  }
	  if(axis == X_AXIS) 
	    len = abs(pt2.x - pt1.x);
	  else 
	    len = abs(pt2.y - pt1.y);
	  len += (clip2 != 0); /* if end clipped, draw the last point */

	  if(len) {
	    if(clip1) {
              clipdx = abs(pt1.x - pt1Orig.x);
              clipdy = abs(pt1.y - pt1Orig.y);
              if (axis == X_AXIS)
                  err = e+((clipdy*e2) + ((clipdx-clipdy)*e1));
              else
                  err = e+((clipdx*e2) + ((clipdy-clipdx)*e1));
            }
	    else err = e;

	    pPixel = pDstBase + (pt1.y * dstWidth) + pt1.x;
	    if(axis == X_AXIS) {
              if (signdx > 0) {
                while(len--) {
                  *pPixel = fore;
                  if (err < 0)
                    err += e1;
                  else {
                    pPixel += yinc;
                    err += e2;
                  }
                  pPixel++;
                }
	      }
	      else {
                while(len--) {
                  *pPixel = fore;
                  if (err <= 0)
                    err += e1;
                  else {
                    pPixel += yinc;
                    err += e2;
                  }
                  pPixel--;
                }
              }
            } /* if X_AXIS */
            else {
              if (signdx > 0) {
                while(len--) {
                  *pPixel = fore;
                  if (err < 0)
                    err += e1;
                  else {
		    pPixel++;
                    err += e2;
                  }
                  pPixel += yinc;
                }
              }
	      else {
                while(len--) {
                  *pPixel = fore;
                  if (err <= 0)
                    err += e1;
                  else {
                    pPixel--;
                    err += e2;
                  }
                  pPixel += yinc;
                }
              }
            } /* else Y_AXIS */
          }
          /* if segment is unclipped, skip remaining rectangles */
          if (!(clip1 || clip2))
              break;
          else
              pbox++;
        }
    } /* while(--npt) */

    if ((pgc->capStyle != CapNotLast) &&
	((ppt->x != pptInit->x) ||
	 (ppt->y != pptInit->y) ||
	 (ppt == pptInit + 1))) {
      pt1 = *ppt;

      nbox = nboxInit;
      pbox = pboxInit;
      while (nbox--) {
        if ((pt1.x >= pbox->x1) &&
            (pt1.y >= pbox->y1) &&
            (pt1.x <  pbox->x2) &&
            (pt1.y <  pbox->y2)) {

          pPixel = pDstBase + (pt1.y * dstWidth) + pt1.x;
	  topcatMaskConfig(dst->pScreen, pgc->planemask, pgc->alu);
	  *pPixel = fore;
        }
	pbox++;
      }
    }
}

/*********************************************************************
 * Far from the most space efficient implementation, this is a 
 * modified duplicate of topcatZeroLine.  It has bits and pieces of
 * miDashLine incorporated in it.
 */

void
topcatZeroDash(dst, pgc, mode, nptInit, pptInit)
     DrawablePtr dst;
     GCPtr pgc;
     int mode;
     int nptInit;		/* number of points in polyline */
     DDXPointRec *pptInit;	/* points in the polyline */
{
    int nboxInit;
    register int nbox;
    BoxPtr pboxInit;
    register BoxPtr pbox;
    int nptTmp;
    int xorg, yorg;
    DDXPointPtr ppt;
    int npt;

    DDXPointRec pt1, pt2, pt1Orig, pt2Orig;
    int dx, dy;
    int adx, ady;
    int signdx, signdy;
    register int len;

    int du, dv;
    register int e, e1, e2;
    int axis;		/* major axis of line */
    int yinc;

    int err;
    unsigned int oc1, oc2;
    int clip1, clip2, clipdx, clipdy;
    int clipDone;
    int tmp;

    int lenCur;		/* number of points used from this dash */
    int lenMax;		/* number of points in this dash */
    int iDash = 0;	/* index of current dash */
    int which;		/* EVEN_DASH or ODD_DASH */
    int linestyle = pgc->lineStyle;
    unsigned char *pDash = pgc->dash;
    int nDash = pgc->numInDashList;

    register int x,y;	/* current point on the line */
    int i;		/* traditional name for loop counter */
    CARD8 *pDstBase, *pPixel;
    unsigned int dstWidth;
    unsigned int fore = pgc->fgPixel;
    unsigned int back = pgc->bgPixel;


    pboxInit = ((cfbPrivGC *)(pgc->devPriv))->pCompositeClip->rects;
    nboxInit = ((cfbPrivGC *)(pgc->devPriv))->pCompositeClip->numRects;

    if (dst->type == DRAWABLE_WINDOW) {
	xorg = ((WindowPtr)dst)->absCorner.x;
	yorg = ((WindowPtr)dst)->absCorner.y;
	pDstBase = (CARD8 *)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->bits;
	dstWidth = (unsigned int)((cfbPrivScreenPtr)
				(dst->pScreen->devPrivate))->stride;

    }
    else {
	xorg = 0;
	yorg = 0;
        pDstBase = (CARD8 *)
            (((cfbPrivPixmapPtr)(((PixmapPtr)dst)->devPrivate))->bits);
        dstWidth = (unsigned int)
            (((cfbPrivPixmapPtr)(((PixmapPtr)dst)->devPrivate))->stride);

    }

    ppt = pptInit;
    npt = nptInit;
    if (mode == CoordModeOrigin) {
	if(xorg || yorg) {
	    while(npt--) {    
	        ppt->x += xorg;
	        ppt++->y += yorg;
	    }
	}
    }
    else {
	ppt->x += xorg;
	ppt->y += yorg;
	npt--;
	while(npt--) {
	    ppt++;
	    ppt->x += (ppt-1)->x;
	    ppt->y += (ppt-1)->y;
	}
    }

    ppt = pptInit;
    npt = nptInit;

    lenCur = pgc->dashOffset;
    which = EVEN_DASH;
    while(lenCur > pDash[iDash])
    {
        lenCur -= pDash[iDash];
        iDash++;
        if (iDash >= nDash)
            iDash = 0;
        which = ~which;
    }
    lenMax = pDash[iDash];

    topcatMaskConfig(dst->pScreen, pgc->planemask, pgc->alu);

    while (--npt) {

	DDXPointPtr pspan;
	DDXPointPtr pspanInit;
 	int *pwidth;
	int *pwidthInit;
	int width;
	int totalLength;

	nbox = nboxInit;
	pbox = pboxInit;

	pt1 = *ppt++;
	pt2 = *ppt;

	dx = pt2.x - pt1.x;
	dy = pt2.y - pt1.y;
	adx = abs(dx);
	ady = abs(dy);
	signdx = sign(dx);
	signdy = sign(dy);

	if (adx > ady)
	{
	    axis = X_AXIS;
	    e1 = ady * 2;
	    e2 = e1 - 2 * adx;
	    e = e1 - adx;
	    totalLength = adx;
	}
	else
	{
	    axis = Y_AXIS;
            e1 = adx*2;
            e2 = e1 - 2*ady;
            e = e1 - ady;
	    totalLength = ady;
	}
	yinc = signdy * dstWidth;

	pt1Orig = pt1;
	pt2Orig = pt2;

	while(nbox--) {
	  BoxRec box;
	  int tmpLenCur = lenCur;
	  int tmpLenMax = lenMax;
	  int tmpiDash = iDash;
	  int tmpWhich = which;
	  int clippedDashes;

	  pt1 = pt1Orig;
	  pt2 = pt2Orig;
	  clipDone = 0;
          box.x1 = pbox->x1;
          box.y1 = pbox->y1;
          box.x2 = pbox->x2-1;
          box.y2 = pbox->y2-1;
          clip1 = 0;
          clip2 = 0;

          oc1 = 0;
          oc2 = 0;
          OUTCODES(oc1, pt1.x, pt1.y, pbox);
          OUTCODES(oc2, pt2.x, pt2.y, pbox);

          if (oc1 & oc2)
              clipDone = -1;
          else if ((oc1 | oc2) == 0)
              clipDone = 1;
          else /* have to clip */
              clipDone = mfbClipLine(pbox, box, &pt1Orig, &pt1, &pt2,
                                     adx, ady, signdx, signdy, axis,
                                     &clip1, &clip2);
	  if(clipDone == -1) {
	    /*
	     * the line doesn't go through this box
	     */
	    pbox++;
	    continue;
	  }
	  if(axis == X_AXIS) 
	    len = abs(pt2.x - pt1.x);
	  else 
	    len = abs(pt2.y - pt1.y);
	  len += (clip2 != 0); /* if end clipped, draw the last point */

	  if(len) {
	    if(clip1) {
              clipdx = abs(pt1.x - pt1Orig.x);
              clipdy = abs(pt1.y - pt1Orig.y);
              if (axis == X_AXIS) {
		  clippedDashes = clipdx;
                  err = e+((clipdy*e2) + ((clipdx-clipdy)*e1));
	      }
              else {
		  clippedDashes = clipdy;
                  err = e+((clipdx*e2) + ((clipdy-clipdx)*e1));
	      }
	      while(clippedDashes >= (tmpLenMax - tmpLenCur)) {
		clippedDashes -= tmpLenMax - tmpLenCur;
		if(++tmpiDash >= nDash) tmpiDash = 0;
		tmpLenMax = pDash[tmpiDash];
		tmpLenCur = 0;
		tmpWhich = ~tmpWhich;
	      }
	      tmpLenCur = clippedDashes;
            }
	    else err = e;

	    pPixel = pDstBase + (pt1.y * dstWidth) + pt1.x;
	    if(axis == X_AXIS) {
              if (signdx > 0) {
                while(len--) {
		  if(tmpWhich == EVEN_DASH) *pPixel = fore;
		  else if(linestyle == LineDoubleDash) *pPixel = back;
                  if (err < 0)
                    err += e1;
                  else {
                    pPixel += yinc;
                    err += e2;
                  }
                  pPixel++;
		  if(++tmpLenCur >= tmpLenMax) {
		    if(++tmpiDash >= nDash) tmpiDash = 0;
		    tmpLenMax = pDash[tmpiDash];
		    tmpLenCur = 0;
		    tmpWhich = ~tmpWhich;
		  }
                }
	      }
	      else {
                while(len--) {
                  if(tmpWhich == EVEN_DASH) *pPixel = fore;
		  else if(linestyle == LineDoubleDash) *pPixel = back;
                  if (err <= 0)
                    err += e1;
                  else {
                    pPixel += yinc;
                    err += e2;
                  }
                  pPixel--;
		  if(++tmpLenCur >= tmpLenMax) {
		    if(++tmpiDash >= nDash) tmpiDash = 0;
		    tmpLenMax = pDash[tmpiDash];
		    tmpLenCur = 0;
		    tmpWhich = ~tmpWhich;
		  }
                }
              }
            } /* if X_AXIS */
            else {
              if (signdx > 0) {
                while(len--) {
                  if(tmpWhich == EVEN_DASH) *pPixel = fore;
		  else if(linestyle == LineDoubleDash) *pPixel = back;
                  if (err < 0)
                    err += e1;
                  else {
		    pPixel++;
                    err += e2;
                  }
                  pPixel += yinc;
		  if(++tmpLenCur >= tmpLenMax) {
		    if(++tmpiDash >= nDash) tmpiDash = 0;
		    tmpLenMax = pDash[tmpiDash];
		    tmpLenCur = 0;
		    tmpWhich = ~tmpWhich;
		  }
                }
              }
	      else {
                while(len--) {
                  if(tmpWhich == EVEN_DASH) *pPixel = fore;
		  else if(linestyle == LineDoubleDash) *pPixel = back;
                  if (err <= 0)
                    err += e1;
                  else {
                    pPixel--;
                    err += e2;
                  }
                  pPixel += yinc;
		  if(++tmpLenCur >= tmpLenMax) {
		    if(++tmpiDash >= nDash) tmpiDash = 0;
		    tmpLenMax = pDash[tmpiDash];
		    tmpLenCur = 0;
		    tmpWhich = ~tmpWhich;
		  }
                }
              }
            } /* else Y_AXIS */
          }
          /* if segment is unclipped, skip remaining rectangles */
          if (!(clip1 || clip2))
              break;
          else
              pbox++;
        }
	while(totalLength >= lenCur) {
	  totalLength -= lenCur;
	  if(++iDash >= nDash) iDash = 0;
	  lenCur = pDash[iDash];
	  which = ~which;
	}
	lenCur -= totalLength;
    } /* while(--npt) */

    if ((pgc->capStyle != CapNotLast) &&
	((ppt->x != pptInit->x) ||
	 (ppt->y != pptInit->y))) {
      pt1 = *ppt;

      nbox = nboxInit;
      pbox = pboxInit;
      while (nbox--) {
        if ((pt1.x >= pbox->x1) &&
            (pt1.y >= pbox->y1) &&
            (pt1.x <  pbox->x2) &&
            (pt1.y <  pbox->y2)) {

          pPixel = pDstBase + (pt1.y * dstWidth) + pt1.x;
          waitbusy(getPlanesMask(dst->pScreen), getTcHardware(dst->pScreen));
	  if (which == EVEN_DASH)
	      *pPixel = fore;
	  else if (linestyle == LineDoubleDash)
	      *pPixel = back;
        }
      }
    }
}
