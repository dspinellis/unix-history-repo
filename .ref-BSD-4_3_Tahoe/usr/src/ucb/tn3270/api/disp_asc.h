/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)disp_asc.h	3.2 (Berkeley) 3/28/88
 */

/*
 * Define the translate tables used to go between 3270 display code
 * and ascii
 */

extern unsigned char
	disp_asc[256],		/* Goes between display code and ascii */
	asc_disp[256];		/* Goes between ascii and display code */
