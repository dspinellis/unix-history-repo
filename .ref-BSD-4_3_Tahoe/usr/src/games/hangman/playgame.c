/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)playgame.c	5.2 (Berkeley) 6/18/88";
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
