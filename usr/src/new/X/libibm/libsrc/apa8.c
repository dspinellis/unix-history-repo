#ifndef lint
static char *rcsid_apa8_c = "$Header: apa8.c,v 10.1 86/11/19 10:39:37 jg Exp $";
#endif	lint
/*
 * apa8.c		device initialization
 *
 * 	apa8_init	apa8 initialization routine
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
#include "apa8.h"

apa8_init()
{
        u_char tmp;

#ifdef TRACE_X
        fprintf (stderr, "In apa8_init\n");
        fflush (stderr);
#endif TRACE_X

        /*
         * Initialize (reset) the APA-8 screen
         */

	/*
	 * Set up the write and data mask.
	 */

	SET_WRITEMASK(NOMASK);
	SET_DATAMASK(HIDDEN_DM);

	/*
	 * Set up the APA8 control register to use its system read/write
	 * calls in the X direction.
	 */

	SET_FC(DCR_VEN | DCR_SEN | DCR_SWR | DCR_X | HIDDEN_FUNC);

        /*
         * Test for the APA8 screen.
         */

	tmp = *(u_char *)SCREEN_BASE;
        *(u_char *)SCREEN_BASE = 0x0F;
        if (*(u_char *)SCREEN_BASE != 0x0F) {
                DeviceError("apa8 not found !!");
		exit(2);
        }
	*(u_char *)SCREEN_BASE = tmp;
}
