/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Displays plot files on a AED512 graphics terminal.
 */

#include "aed.h"

char dbuf[BUFSIZ];	/* Used to buffer display characters */
struct sgttyb sgttyb;	/* Used to save terminal control bits */
int curx, cury;		/* Current screen position */
int xbot, ybot;		/* Coordinates of screen lower-left corner */
int scale;		/* The number of pixels per 2**12 units
			 * of world coordinates.
			 */

/*
 * The following is the color map, containing reg, green, and blue
 * values for color locations 0 and 1.
 */

static int colors[] = {200, 200, 200, 0, 0, 125, 125, 0, 0, 125, 0, 0};

/*---------------------------------------------------------
 *	Openpl initializes the graphics display and clears its screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The display is re-initialized and the file is remembered for
 *	use in all subsequent calls to this module.  The display's
 *	color map is reset.  The display is put into raw mode, but
 *	the previous mode bits are saved.
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
openpl()
{
    int flags, *p, i;
    char dum[4];

    /* First, grab up the display modes, then reset them to put it
     * into cooked mode.  Also, lock the terminal.
     */

    (void) gtty(fileno(stdout), &sgttyb);
    flags = sgttyb.sg_flags;
    sgttyb.sg_flags = (sgttyb.sg_flags & ~(RAW | CBREAK)) | EVENP | ODDP;
    (void) stty(fileno(stdout), &sgttyb);
    sgttyb.sg_flags = flags;

    /* Save the file pointer around for later use, then output an
     * initialization string to the display.  The initialization
     * string resets the terminal, sets formats, clears the display,
     * initializes the read and write masks, and sets the color map.
     */

    setbuf(stdout, dbuf);
    fputs("\33\33G1HHHN[00LFFCFFMFFFFFFFF", stdout);
    fputs("K0004", stdout);
    p = colors;
    for (i=0; i<12; i++)
    {
	chex(*p++, dum, 2);
	fputs(dum, stdout);
    }
    fputs("^15060AL", stdout);
    scale = 1<<12;
    curx = cury = xbot = ybot = 0;
    (void) fflush(stdout);
}
