#ifndef lint
static char *rcsid_fill_c = "$Header: fill.c,v 10.1 86/11/29 13:51:52 jg Rel $";
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

/* fill.c	Perform a simple raster operation a section of the screen
 *
 *	PixFill	Do a function on the screen
 *
 */

#include "Xapollo.h"
extern int old_op;

/*ARGSUSED*/
PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
     int srcpix, dstx, dsty, width, height, clipcount, zmask;
     register BITMAP *xymask;
     register int func;
     CLIP *clips;
{
    int i;

    gpr_$window_t window, cwindow;
    boolean active;
    status_$t status;
    gpr_$position_t dest;

/* Need to sort out the func/srcpix interactions here; the
   following special case is for gemacs' curosr
 */
    if ((func == GXxor) && (srcpix == 1))
        func = 10;
    if (xymask == NULL) {
        set_zmask(zmask);
	set_op( func );
    window.x_coord = dstx;
    window.y_coord = dsty;
    window.x_size = width;
    window.y_size = height;
    do {
      GetNextClip(clips, cwindow);
      CheckCursor(dstx, dsty, width, height);
      gpr_$set_clip_window( cwindow, status);
      
      if (func == 3) {
	gpr_$set_fill_value((gpr_$pixel_value_t)srcpix, status);
	gpr_$rectangle(window, status);
      }
      else /* just blit screen to self */
	{
	  dest.x_coord = dstx;
	  dest.y_coord = dsty;
	  gpr_$bit_blt(Screen.bm, window, (short)0, dest, (short)0, status);
	}
    } while (--clipcount > 0);
  } else {  
    fprintf(stderr,"PixFill: op=%d, srcpix=%d, xymask=%d\n",
                func, srcpix, xymask);
    }
    RestoreCursor();
}
