#include "X.h"
#define  NEED_EVENTS
#include "Xproto.h"
#include "scrnintstr.h"
#include "cursorstr.h"
#include "pixmapstr.h"
#include "inputstr.h"
#include "regionstr.h"

#include "../cfb/cfb.h"
#include "mi.h"
#include "topcat.h"
#include "gcstruct.h"

extern u_char XHP_NewRule[16][6];

/*
 * tcSolidFS -- A fast fill routine for filling spans
 * with a solid color on a Topcat
 * This is the pGC->FillSpans function when FillStyle == Solid and
 * drawable is a window
 */
void
tcSolidFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    DDXPointPtr ppt;		/* pointer to list of start points */
    int *pwidth;		/* pointer to list of n widths */
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    ScreenPtr pScreen = pDrawable->pScreen;
    u_char pMask = getPlanesMask(pScreen);
    int zmask = pMask & pGC->planemask; 
    TOPCAT *gp_hardware = getTcHardware(pScreen);
    int rop = pGC->alu;
    int srcpix = pGC->fgPixel;

    if (!(pGC->planemask))
	return;

    n = nInit * 
	miFindMaxBand(((cfbPrivGC *)
		       (pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip);
    pwidth = (int *)ALLOCATE_LOCAL(n * sizeof(int));
    ppt = (DDXPointRec *)ALLOCATE_LOCAL(n * sizeof(DDXPointRec));
    if (!ppt || !pwidth)
    {
	DEALLOCATE_LOCAL(ppt); DEALLOCATE_LOCAL(pwidth);
	return;
    }
    pwidthFree = pwidth;
    pptFree = ppt;
    n = miClipSpans(((cfbPrivGC *)
		     (pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip,
		     pptInit, pwidthInit, nInit,
		     ppt, pwidth, fSorted);

    while (n--)
	if (*pwidth)
	{
	    /* Write a span from (ppt->x,ppt->y) of width *pwidth */
 
	    /* See if we can easily expand our span to a rectangle */
	    int w_height=1;
	    while (n &&
		   pwidth[w_height]==pwidth[w_height-1] &&
		   ppt[w_height].x==ppt[w_height-1].x &&
		   ppt[w_height].y==ppt[w_height-1].y+1)
	    {
		n--;
		w_height++;
	    }
 
	    waitbusy(pMask, gp_hardware);
    
	    gp_hardware -> write_enable = zmask & srcpix;
            gp_hardware -> window_move_replacement_rule = XHP_NewRule[rop][3];
            gp_hardware -> write_enable = zmask & ~srcpix;
            gp_hardware -> window_move_replacement_rule = XHP_NewRule[rop][0];
            gp_hardware -> write_enable = zmask;
            gp_hardware -> pixel_write_replacement_rule = GXcopy;

	    /* XXX catseye */
	    gp_hardware -> frame_buf_write_enable = zmask;

	    gp_hardware -> source_x = ppt->x;
	    gp_hardware -> source_y = ppt->y;
	    gp_hardware -> dest_x = ppt->x;
	    gp_hardware -> dest_y = ppt->y;
	    gp_hardware -> window_width = *pwidth;
	    gp_hardware -> window_height = w_height;

	    gp_hardware -> start_move = zmask;
 
	    pwidth+=w_height;
	    ppt+=w_height;
	}
    DEALLOCATE_LOCAL(pptFree); DEALLOCATE_LOCAL(pwidthFree);
}
