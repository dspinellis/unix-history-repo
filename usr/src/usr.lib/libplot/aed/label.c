/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.2 (Berkeley) 4/30/85";
#endif not lint


#include "aed.h"

/*---------------------------------------------------------
 *	This routine places a label starting at the current
 *	position.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string indicated by s starting at (curx, cury).
 *	The current position is updated accordingly.
 *---------------------------------------------------------
 */
label(s)
char *s;
{
    setcolor("02");
    putc('Q', stdout);
    outxy20(curx + (4096/scale), cury + (4096/scale));
    putc('\6', stdout);
    fputs(s, stdout);
    putc('\33', stdout);
    (void) fflush(stdout);
    curx += ((6*4096*strlen(s)) + 4000)/scale;
}
