#ifndef lint
static char *rcsid_copy_c = "$Header: copy.c,v 10.1 86/11/29 13:50:52 jg Rel $";
#endif	lint
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

/* copy.c	Copy one section of the framebuffer to another
 *
 *	CopyArea	Copies a section of the framebuffer
 *
 */

#include "Xapollo.h"

status_$t status;

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	int func;
	CLIP *clips;
{
    int i;

    gpr_$window_t window, cwindow;
    gpr_$position_t dest;

    CheckCursor( srcx, srcy, width, height);
    CheckCursor( dstx, dsty, width, height);
  
    set_zmask( zmask );
    set_op( func );
    window.x_coord = srcx;
    window.y_coord = srcy;
    window.x_size = width;
    window.y_size = height;
    dest.x_coord = dstx;
    dest.y_coord = dsty;
    do {

        GetNextClip(clips, cwindow);
        gpr_$set_clip_window( cwindow, status);

        gpr_$pixel_blt(Screen.bm, window, dest, status); 
        } while (--clipcount > 0);
    RestoreCursor();
}
