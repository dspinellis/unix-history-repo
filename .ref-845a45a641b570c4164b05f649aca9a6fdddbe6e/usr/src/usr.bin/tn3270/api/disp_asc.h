/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)disp_asc.h	4.2 (Berkeley) %G%
 */

/*
 * Define the translate tables used to go between 3270 display code
 * and ascii
 */

extern unsigned char
	disp_asc[256],		/* Goes between display code and ascii */
	asc_disp[256];		/* Goes between ascii and display code */
