/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)endgame.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"hangman.h"

/*
 * endgame:
 *	Do what's necessary at the end of the game
 */
endgame()
{
	register char	ch;

	prman();
	if (Errors >= MAXERRS)
		Errors = MAXERRS + 2;
	prword();
	prdata();
	move(MESGY, MESGX);
	if (Errors > MAXERRS)
		printw("Sorry, the word was \"%s\"\n", Word);
	else
		printw("You got it!\n");

	for (;;) {
		mvaddstr(MESGY + 1, MESGX, "Another word? ");
		leaveok(stdscr, FALSE);
		refresh();
		if ((ch = readch()) == 'n')
			die();
		else if (ch == 'y')
			break;
		mvaddstr(MESGY + 2, MESGX, "Please type 'y' or 'n'");
	}

	leaveok(stdscr, TRUE);
	move(MESGY, MESGX);
	addstr("\n\n\n");
}
