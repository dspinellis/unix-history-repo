/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)prdata.c	8.1 (Berkeley) %G%";
#endif /* not lint */

# include	"hangman.h"

/*
 * prdata:
 *	Print out the current guesses
 */
prdata()
{
	register bool	*bp;

	move(GUESSY, GUESSX + sizeof "Guessed: ");
	bp = Guessed;
	while (bp < &Guessed[26])
		if (*bp++)
			addch((bp - Guessed) + 'a' - 1);
	clrtoeol();
	mvprintw(NUMBERY, NUMBERX + sizeof "Word #:          ", "%d", Wordnum);
	mvprintw(AVGY, AVGX + sizeof       "Current Average: ", "%.3f",
				(Average * (Wordnum - 1) + Errors) / Wordnum);
	mvprintw(AVGY + 1, AVGX + sizeof   "Overall Average: ", "%.3f", Average);
}
