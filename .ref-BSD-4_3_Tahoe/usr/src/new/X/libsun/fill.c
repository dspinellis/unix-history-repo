#ifndef lint
static char *rcsid_fill_c = "$Header: fill.c,v 10.4 86/11/30 16:06:12 jg Rel $";
#endif	lint
#ifdef	sun
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef	lint
static char sccsid[] = "@(#)fill.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* fill.c	Perform a simple raster operation a section of the screen
 *
 *	PixFill	Do a function on the screen
 *
 */

#include "Xsun.h"
extern struct pixrect *PixRect;

/*ARGSUSED*/
PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
	int srcpix, dstx, dsty, width, height, clipcount, zmask;
	BITMAP *xymask;
	int func;
	CLIP *clips;
{
    int         op = SUN_FROM_X_OP(func) | PIX_COLOR(srcpix) | PIX_DONTCLIP;
    int allmask = -1;

    SetZmask(PixRect, &zmask);
    if (xymask == NULL) {
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;

		CheckCursor(tleft, ttop, twidth, theight);
		pr_rop(PixRect, tleft, ttop, twidth, theight, op, NULL, 0, 0);
	    }
	} while (--clipcount > 0);
    }
    else {
	struct pixrect stencil;
	struct mpr_data foo_data;
	extern struct pixrectops mem_ops;

	foo_data.md_linebytes = mpr_linebytes(xymask->width, 1);
	foo_data.md_image = (short *) xymask->data;
	foo_data.md_offset.x = 0;
	foo_data.md_offset.y = 0;
	foo_data.md_primary = 0;
	foo_data.md_flags = 0;
	stencil.pr_ops = &mem_ops;
	stencil.pr_size.x = xymask->width;
	stencil.pr_size.y = xymask->height;
	stencil.pr_depth = 1;
	stencil.pr_data = (caddr_t) &foo_data;
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;

		CheckCursor(tleft, ttop, twidth, theight);
		pr_stencil(PixRect, tleft, ttop, twidth, theight, op, &stencil,
			 tleft - dstx, ttop - dsty, NULL, 0, 0);
	    }
	} while (--clipcount > 0);
    }
    RestoreCursor();
    SetZmask(PixRect, &allmask);
}
/*
 * the following is bogus on a color sun, but as I know nothing about Suns,
 * might as well make b/w work....  -JG
 */
extern int errno;
#include <errno.h>

int StippleFill (srcpix, xoff, yoff, stipmask, dstx, dsty, width, height,
	clips, clipcount, func, zmask)
	int srcpix;		/* source pixel */
	int xoff, yoff;		/* stipple origin */
	BITMAP *stipmask;	/* stipple mask */
	int dstx, dsty;		/* destination */
	int width, height;
	CLIP *clips;		/* clipping rectangles */
	int clipcount;
	int func;		/* GX display function */
	int zmask;		/* plane mask */
{
    	static char funcmap[16][2] = {
		{GXandInverted,	GXandInverted},	/* GXclear */
		{GXandInverted, GXnoop},	/* GXand */
		{GXandInverted,	GXxor},		/* GXandReverse */
		{GXandInverted, GXor},		/* GXcopy */
		{GXnoop,	GXandInverted},	/* GXandInverted */
		{GXnoop,	GXnoop},	/* GXnoop */
		{GXnoop,	GXxor},		/* GXxor */
		{GXnoop,	GXor},		/* GXor */
		{GXxor,		GXandInverted},	/* GXnor */
		{GXxor,		GXnoop},	/* GXequiv */
		{GXxor,		GXxor},		/* GXinvert */
		{GXxor,		GXor},		/* GXorReverse */
		{GXor,		GXandInverted},	/* GXcopyInverted */
		{GXor,		GXnoop},	/* GXorInverted */
		{GXor,		GXxor},		/* GXnand */
		{GXor,		GXor}		/* GXset */
	};
	int newfunc = funcmap [func][srcpix & 1];
	PIXMAP *tile, *MakePixmap();

	if ((stipmask->width != 16) || (stipmask->height != 16)) {
		errno = EINVAL;
		return (0);
	}
	tile = MakePixmap (stipmask, 1, 0);
	TileFill (tile, xoff, yoff, 0, dstx, dsty, width, height,
		clips, clipcount, newfunc, zmask);
	FreePixmap (tile);
	return (1);
}
#endif	sun
