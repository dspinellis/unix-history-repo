/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)prman.c	5.1 (Berkeley) %G%";
#endif /* not lint */

# include	"hangman.h"

/*
 * prman:
 *	Print out the man appropriately for the give number
 *	of incorrect guesses.
 */
prman()
{
	register int	i;

	for (i = 0; i < Errors; i++)
		mvaddch(Err_pos[i].y, Err_pos[i].x, Err_pos[i].ch);
	while (i < MAXERRS) {
		mvaddch(Err_pos[i].y, Err_pos[i].x, ' ');
		i++;
	}
}
