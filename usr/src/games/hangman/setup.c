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
static char sccsid[] = "@(#)setup.c	5.1 (Berkeley) %G%";
#endif /* not lint */

# include	"hangman.h"

/*
 * setup:
 *	Set up the strings on the screen.
 */
setup()
{
	register char		**sp;
	static struct stat	sbuf;

	noecho();
	crmode();

	mvaddstr(PROMPTY, PROMPTX, "Guess:");
	mvaddstr(GUESSY, GUESSX, "Guessed:");
	mvaddstr(NUMBERY, NUMBERX, "Word #:");
	mvaddstr(AVGY, AVGX, "Current Average:");
	mvaddstr(AVGY + 1, AVGX, "Overall Average:");
	mvaddstr(KNOWNY, KNOWNX, "Word: ");

	for (sp = Noose_pict; *sp != NULL; sp++) {
		move(sp - Noose_pict, 0);
		addstr(*sp);
	}

	srand(time(NULL) + getpid());
	if ((Dict = fopen(DICT, "r")) == NULL) {
		perror(DICT);
		endwin();
		exit(1);
	}
	fstat(fileno(Dict), &sbuf);
	Dict_size = sbuf.st_size;
}
