#ifndef lint
static char *rcsid_apa8c_c = "$Header: apa8c.c,v 10.1 86/11/19 10:39:58 jg Exp $";
#endif	lint
/* apa8c.c - APA8C controller initialization
 *
 * 	apa8c_init		APA8C initialization routine
 *	apa8c_set_colors	set defualt colors
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
#include "apa8c.h"

/*
 * Initialize APA8C
 */

apa8c_init()
{
	u_short tmp;

#ifdef TRACE_X
        fprintf (stderr, "In apa8c_init\n");
        fflush (stderr);
#endif TRACE_X

        /*
         * Initialize (reset) the APA8C screen
         */

	/*
	 * Set up the write and data mask.
	 */

	SET_APA8C_WRITEMASK(NOMASK);
	SET_APA8C_DATAMASK(HIDDEN_DM);

	/*
	 * Set up the APA8C control register to use its system read/write
	 * calls in the X direction.
	 */

	SET_APA8C_FC(DCR_VEN | DCR_SEN | DCR_SWR | DCR_X | HIDDEN_FUNC);

	/*
	 * Set up the color plane select register and foreground/background
	 * registers.
	 */

	*(u_short *)CPS_REG = DEFAULT_CPS;
	*(u_short *)FGBG_REG = DEFAULT_FGBG;

	/*
	 * Set default colors in Video Look-up Table.
	 */

	apa8c_set_colors();

	/*
	 * Test for the APA8C screen.
	 */

	tmp = *(u_short *)SCREEN_BASE;
	*(u_short *)SCREEN_BASE = 0x0F0F;
	if (*(u_short *)SCREEN_BASE != 0x0F0F) {
		/*
		 * Log error and exit
		 */

		DeviceError("apa8 color not found !!");
		exit(2);
	}
	*(u_short*)SCREEN_BASE = tmp;
}

/*
 * Sets default foreground and background colors in 
 * video look-up table. FG_COLOR and BG_COLOR are default 
 * colors. They are defined in apa8c.h
 */

static
apa8c_set_colors()
{
	register i;

#ifdef TRACE_X
        fprintf (stderr, "In apa8c_set_colors\n");
        fflush (stderr);
#endif TRACE_X

	for (i = 0; i < VLT_SIZE; i++) {
		if (i == 15)
			*(u_short *)VLT_REG = (u_short) (i | FG_COLOR);
		else
			*(u_short *)VLT_REG = (u_short) (i | BG_COLOR);
	}
}
