#ifndef lint
static char *rcsid_apa16_c = "$Header: apa16.c,v 10.1 86/11/19 10:39:18 jg Exp $";
#endif	lint
/* apa16.c - APA16 controller initialization
 *
 * 	apa16_init	APA16 initialization routine
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
#include "apa16.h"

/*
 * Initialize APA16
 */

apa16_init()
{

#ifdef TRACE_X
        fprintf (stderr, "In apa16_init\n");
        fflush (stderr);
#endif TRACE_X

        /*
         * Initialize (reset) the APA16 screen
         */

        RESET_APA16();
        *(short *)MR = MR_DEFAULT;
        *(short *)CSR = WHITE_ON_BLACK;
        ENABLE_VD_OUT();
 
        /*
         * Test for the APA16 screen.
         */

        *(char *)SCREEN_BASE = 0x0F;
        if (*(char *)SCREEN_BASE != 0x0F) {
		/*
		 * Log error and exit
		 */

                DeviceError("apa16 not found !!");
		exit(2);
        }
}
