#ifndef lint
static char *rcsid_copy_c = "$Header: copy.c,v 10.3 86/11/29 13:47:25 jg Rel $";
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
static char sccsid[] = "@(#)copy.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* copy.c	Copy one section of the framebuffer to another
 *
 *	CopyArea	Copies a section of the framebuffer
 *
 */

#include "Xsun.h"

extern struct pixrect *PixRect;

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	int func;
	CLIP *clips;
{
    int op = SUN_FROM_X_OP(func);
    int allmask = -1;

    SetZmask(PixRect, &zmask);
    do {
	int         cleft, ctop, cwidth, cheight;

	GetNextClip(clips, cleft, ctop, cwidth, cheight);
	if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
	    int         tleft = (cleft > dstx ? cleft : dstx);
	    int         ttop = (ctop > dsty ? ctop : dsty);
	    int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
	    int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
	    int         sx = (tleft - dstx) + srcx;
	    int         sy = (ttop - dsty) + srcy;

	    CheckCursor(tleft, ttop, twidth, theight);
	    CheckCursor(sx, sy, twidth, theight);
	    pr_rop(PixRect, tleft, ttop, twidth, theight, op | PIX_DONTCLIP, PixRect, sx, sy);
	}
    } while (--clipcount > 0);
    RestoreCursor();
    SetZmask(PixRect, &allmask);
}
#endif	sun
