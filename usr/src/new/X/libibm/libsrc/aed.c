#ifndef lint
static char *rcsid_aed_c = "$Header: aed.c,v 10.1 86/11/19 10:38:53 jg Exp $";
#endif	lint
/* aed.c - AED display initialization routine
 *
 *	aed_init	Display initialization routine
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
#include "aed.h"

/*
 * Initialize AED
 */

aed_init()
{
	int *color = (int *)AED_COLOR;

#ifdef TRACE_X
        fprintf (stderr, "In aed_init\n");
        fflush (stderr);
#endif TRACE_X

	/*
	 * Set forground/background colors to match X
	 */

	*color = WHITE_ON_BLACK;

	/*
	 * Clear off-screen bitmap
	 */

	bzero((caddr_t)SCREEN_BASE, SCREEN_BM_SIZE);
}
