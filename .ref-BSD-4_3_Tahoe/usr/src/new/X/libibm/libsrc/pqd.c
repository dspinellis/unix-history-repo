#ifndef lint
static char *rcsid_pqd_c = "$Header: pqd.c,v 10.1 86/11/19 10:43:30 jg Exp $";
#endif	lint
/*
 * pqd.c		device initialization
 *
 * 	pqd_init	pqd initialization routine
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"
#include "pqd.h"

/*
 * This routine resets and then tests for the PQD screen.
 */

pqd_init()
{
	register unsigned short *screen;
	register i,j;

#ifdef TRACE_X
        fprintf (stderr, "In pqd_init\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Initialize all registers then clear (or set) each plane not to be
	 * used.
	 */

	*(u_short *)PQD_WM1 = 0xFFFF;
	*(u_short *)PQD_WM0 = 0xFFFF;
	*(u_short *)PQD_VCR = 0x1F00;
	*(u_short *)PQD_WCR = 0x8000;
 
	SET_PLANE(RW_PLANE2 | RW_PLANE3);

	/*
	 * Test for the screen.
	 */

	*(u_short *)SCREEN_BASE = 0x0F0F;
	if (*(u_short *)SCREEN_BASE != 0x0F0F) {
                DeviceError("pqd not found !!");
		exit(2);
	} else {
		/*
		 * Clear every plane.
		 */

		for (i = 0; i < 4; i++) {
			SELECT_PLANE(i);
			screen = (u_short *)SCREEN_BASE;
			j = (REAL_SCREEN_WIDTH >> 4) * REAL_SCREEN_HEIGHT;
			for (; j > 0; j--)
				*screen++ = 0;
		}
		SET_PLANE(RW_PLANE2 | RW_PLANE3);
	}
}
