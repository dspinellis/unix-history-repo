/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)playgame.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"hangman.h"

/*
 * playgame:
 *	play a game
 */
playgame()
{
	register bool	*bp;

	getword();
	Errors = 0;
	bp = Guessed;
	while (bp < &Guessed[26])
		*bp++ = FALSE;
	while (Errors < MAXERRS && index(Known, '-') != NULL) {
		prword();
		prdata();
		prman();
		getguess();
	}
	endgame();
}
