#ifndef lint
static char *rcsid_bitblt_cur_c = "$Header: bitblt_cur.c,v 10.1 86/11/19 10:51:09 jg Exp $";
#endif	lint
/*
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 *
 * Written by Daniel Stone, Brown University/IRIS  (des@iris)
 *
 * NOTE: A "cursor" is the thing that moves on the screen when it
 *	 is attached to the mouse.  It is sometimes refered to as a
 *	 "locator" also.
 *
 * If the screen to be dealt with uses a software cursor then the bits
 * the cursor overlayed must be replaced.  This is needed because
 * the cursor (or locator) is put on the the ACTUAL bitmap.
 */

#include "bitblt_int.h"

static short rem_loc = 0;

/*
 * This macro checks the XAddr shared memory area to see if the cursor
 * is in the way.
 */
#define S_ORIGIN_X (XAddr->mouse.x & (~(BPW-1)))
#define S_ORIGIN_Y (XAddr->mouse.y)
#define S_CORNER_X (S_ORIGIN_X + (2*BPW))
#define S_CORNER_Y (S_ORIGIN_Y + BPW)
#define LOC_IN_BOUNDS(oX,oY,cX,cY) ((oX < S_CORNER_X) && (S_ORIGIN_X < cX) &&\
				    (oY < S_CORNER_Y) && (S_ORIGIN_Y < cY))

/*
 * Save_cursor set up the shared memory, checks to see if the cursor
 * is in the rectangle bounds handed to save_cursor.  If it is then
 * an ioctl is done to the kernel to remove the cursor.  If the ioctl
 * fails then -1 is returned otherwise 0 is returned.
 */
save_cursor(oX,oY,cX,cY)
register short oX,oY,cX,cY;
{
	register XIoAddr *xptr;

	xptr = XAddr;
	xptr->hmbox.left = oX;
	xptr->hmbox.top = oY;
	xptr->hmbox.right = cX;
	xptr->hmbox.bottom = cY;
	xptr->hmbox.flags = 1;
	if (LOC_IN_BOUNDS(oX,oY,cX,cY)) {
		rem_loc = 1;
		if (ioctl(xdev,QIOCHIDECUR,0) == -1)
			return(-1);
	}
	return(0);
}

/*
 * Restore_cursor checks the shared memory area and if set it checks
 * to see if the cursor was removed by save_cursor or if it currently
 * is in the critical rectangle defined by hmbox.  If it is then
 * the approprate ioctl is called.  If the ioctl fails then -1 is return
 * otherwise 0 is returned.
 */
restore_cursor()
{
	register XIoAddr *xptr;

	xptr = XAddr;
	if (xptr->hmbox.flags)	{
		if (rem_loc || LOC_IN_BOUNDS(xptr->hmbox.left,
					     xptr->hmbox.top,
				 	     xptr->hmbox.right,
					     xptr->hmbox.bottom)) {
			xptr->hmbox.flags = 0;
			if (ioctl(xdev,QIOCSHOWCUR,0) == -1)
				return(-1);
		}
		else {
			xptr->hmbox.flags = 0;
		}
	}
	rem_loc = 0;
	return(0);
}
