/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Barry Brachman.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)help.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <stdio.h>

#include "bog.h"
#include "extern.h"

int
help()
{
	extern int nlines;
	int eof, i;
	FILE *fp;
	WINDOW *win;
	char buf[BUFSIZ];

	if ((fp = fopen(HELPFILE, "r")) == NULL)
		return(-1);
	win = newwin(0, 0, 0, 0);
	clearok(win, 1);

	eof = 0;
	if (ungetc(getc(fp), fp) == EOF) {
		wprintw(win, "There doesn't seem to be any help.");
		eof = 1;			/* Nothing there... */
	}

	while (!eof) {
		for (i = 0; i < nlines - 3; i++) {
			if (fgets(buf, sizeof(buf), fp) == (char *) NULL) {
				eof = 1;
				break;
			}
			if (buf[0] == '.' && buf[1] == '\n')
				break;
			wprintw(win, "%s", buf);
		}
		if (eof || ungetc(getc(fp), fp) == EOF) {
			eof = 1;
			break;
		}
		wmove(win, nlines - 1, 0);
		wprintw(win,
		    "Type <space> to continue, anything else to quit...");
		wrefresh(win);
		if ((inputch() & 0177) != ' ')
			break;
		wclear(win);
	}

	fclose(fp);
	if (eof) {
		wmove(win, nlines - 1, 0);
		wprintw(win, "Hit any key to continue...");
		wrefresh(win);
		inputch();
	}
	delwin(win);
	clearok(stdscr, 1);
	refresh();
	return(0);
}
