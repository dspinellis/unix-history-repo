/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	5.2 (Berkeley) 4/30/85";
#endif not lint


#include "aed.h"

/*---------------------------------------------------------
 *	Closepl does whatever is necessary to reset the characteristics
 *	of the AED512 after the program is finished.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The graphics display modes are reset.
 *---------------------------------------------------------
 */
closepl()
{
    fputs("Q00204\6", stdout);
    (void) fflush(stdout);
    (void) stty(fileno(stdout), &sgttyb);
}
