/* vi: set tabstop=4 : */

#include <curses.h>
#include <stdio.h>

#include "bog.h"

help()
{
	int eof, i;
	FILE *fp;
	WINDOW *win;
	char buf[BUFSIZ];
	extern int ncols, nlines;

	if ((fp = fopen(HELPFILE, "r")) == (FILE *) NULL)
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
		wprintw(win, "Type <space> to continue, anything else to quit...");
		wrefresh(win);
		if ((inputch() & 0177) != ' ')
			break;
		wclear(win);
	}

	fclose(fp);
	if (eof) {
		extern char *version;

		wprintw(win, "%s", version);
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

