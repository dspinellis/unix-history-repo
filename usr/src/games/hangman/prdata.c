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
static char sccsid[] = "@(#)prdata.c	5.1 (Berkeley) %G%";
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
