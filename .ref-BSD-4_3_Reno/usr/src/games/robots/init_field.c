/*
 * Copyright (c) 1980 Regents of the University of California.
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
static char sccsid[] = "@(#)init_field.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"robots.h"

/*
 * init_field:
 *	Lay down the initial pattern whih is constant across all levels,
 *	and initialize all the global variables.
 */
init_field()
{
	register int	i;
	register WINDOW	*wp;
	register int	j;
	static bool	first = TRUE;
	static char	*desc[] = {
				"Directions:",
				"",
				"y k u",
				" \\|/",
				"h- -l",
				" /|\\",
				"b j n",
				"",
				"Commands:",
				"",
				"w:  wait for end",
				"t:  teleport",
				"q:  quit",
				"^L: redraw screen",
				"",
				"Legend:",
				"",
				"+:  robot",
				"*:  junk heap",
				"@:  you",
				"",
				"Score: 0",
				NULL
	};

	Dead = FALSE;
	Waiting = FALSE;
	flushok(stdscr, TRUE);
	Score = 0;

	erase();
	move(0, 0);
	addch('+');
	for (i = 1; i < Y_FIELDSIZE; i++) {
		move(i, 0);
		addch('|');
	}
	move(Y_FIELDSIZE, 0);
	addch('+');
	for (i = 1; i < X_FIELDSIZE; i++)
		addch('-');
	addch('+');
	if (first)
		refresh();
	move(0, 1);
	for (i = 1; i < X_FIELDSIZE; i++)
		addch('-');
	addch('+');
	for (i = 1; i < Y_FIELDSIZE; i++) {
		move(i, X_FIELDSIZE);
		addch('|');
	}
	if (first)
		refresh();
	for (i = 0; desc[i] != NULL; i++) {
		move(i, X_FIELDSIZE + 2);
		addstr(desc[i]);
	}
	if (first)
		refresh();
	first = FALSE;
#ifdef	FANCY
	if (Pattern_roll)
		Next_move = &Move_list[-1];
#endif
}
