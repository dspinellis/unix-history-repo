/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)bdisp.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include "gomoku.h"
#include <stdio.h>
#include <curses.h>

#define	SCRNH		24		/* assume 24 lines for the moment */
#define	SCRNW		80		/* assume 80 chars for the moment */

static	int	lastline;
static	char	pcolor[] = "*O.?";

/*
 * Initialize screen display.
 */
cursinit()
{

	initscr();
	noecho();
	cbreak();
	leaveok(stdscr, TRUE);
}

/*
 * Restore screen display.
 */
cursfini()
{

	leaveok(stdscr, FALSE);
	move(23, 0);
	clrtoeol();
	refresh();
	endwin();
}

/*
 * Initialize board display.
 */
bdisp_init()
{
	register int i, j;

	/* top border */
	for (i = 1; i < BSZ1; i++) {
		move(0, 2 * i + 1);
		addch(letters[i]);
	}
	/* left and right edges */
	for (j = BSZ1; --j > 0; ) {
		move(20 - j, 0);
		printw("%2d ", j);
		move(20 - j, 2 * BSZ1 + 1);
		printw("%d ", j);
	}
	/* bottom border */
	for (i = 1; i < BSZ1; i++) {
		move(20, 2 * i + 1);
		addch(letters[i]);
	}
	bdwho(0);
	move(0, 47);
	addstr("#  black  white");
	lastline = 0;
	bdisp();
}

/*
 * Update who is playing whom.
 */
bdwho(update)
	int update;
{
	int i;
	extern char *plyr[];

	move(21, 0);
	clrtoeol();
	i = 6 - strlen(plyr[BLACK]) / 2;
	move(21, i > 0 ? i : 0);
	printw("BLACK/%s", plyr[BLACK]);
	i = 30 - strlen(plyr[WHITE]) / 2;
	move(21, i);
	printw("WHITE/%s", plyr[WHITE]);
	move(21, 19);
	addstr(" vs. ");
	if (update)
		refresh();
}

/*
 * Update the board display after a move.
 */
bdisp()
{
	register int i, j, c;
	register struct spotstr *sp;

	for (j = BSZ1; --j > 0; ) {
		for (i = 1; i < BSZ1; i++) {
			move(BSZ1 - j, 2 * i + 1);
			sp = &board[i + j * BSZ1];
			if (debug > 1 && sp->s_occ == EMPTY) {
				if (sp->s_flg & IFLAGALL)
					c = '+';
				else if (sp->s_flg & CFLAGALL)
					c = '-';
				else
					c = '.';
			} else
				c = pcolor[sp->s_occ];
			addch(c);
		}
	}
	refresh();
}

#ifdef DEBUG
/*
 * Dump board display to a file.
 */
bdump(fp)
	FILE *fp;
{
	register int i, j, c;
	register struct spotstr *sp;

	/* top border */
	fprintf(fp, "   A B C D E F G H J K L M N O P Q R S T\n");

	for (j = BSZ1; --j > 0; ) {
		/* left edge */
		fprintf(fp, "%2d ", j);
		for (i = 1; i < BSZ1; i++) {
			sp = &board[i + j * BSZ1];
			if (debug > 1 && sp->s_occ == EMPTY) {
				if (sp->s_flg & IFLAGALL)
					c = '+';
				else if (sp->s_flg & CFLAGALL)
					c = '-';
				else
					c = '.';
			} else
				c = pcolor[sp->s_occ];
			putc(c, fp);
			putc(' ', fp);
		}
		/* right edge */
		fprintf(fp, "%d\n", j);
	}

	/* bottom border */
	fprintf(fp, "   A B C D E F G H J K L M N O P Q R S T\n");
}
#endif /* DEBUG */

/*
 * Display a transcript entry
 */
dislog(str)
	char *str;
{

	if (++lastline >= SCRNH - 1) {
		/* move 'em up */
		lastline = 1;
	}
	if (strlen(str) >= SCRNW - 46)
		str[SCRNW - 46 - 1] = '\0';
	move(lastline, 46);
	addstr(str);
	clrtoeol();
	move(lastline + 1, 46);
	clrtoeol();
}

/*
 * Display a question.
 */
ask(str)
	char *str;
{
	int len = strlen(str);

	move(23, 0);
	addstr(str);
	clrtoeol();
	move(23, len);
	refresh();
}

getline(buf, size)
	char *buf;
	int size;
{
	register char *cp, *end;
	register int c;
	extern int interactive;

	cp = buf;
	end = buf + size - 1;	/* save room for the '\0' */
	while (cp < end && (c = getchar()) != EOF && c != '\n' && c != '\r') {
		*cp++ = c;
		if (interactive) {
			switch (c) {
			case 0x0c: /* ^L */
				wrefresh(curscr);
				cp--;
				continue;
			case 0x15: /* ^U */
			case 0x18: /* ^X */
				while (cp > buf) {
					cp--;
					addch('\b');
				}
				clrtoeol();
				break;
			case '\b':
			case 0x7f: /* DEL */
				if (cp == buf + 1) {
					cp--;
					continue;
				}
				cp -= 2;
				addch('\b');
				c = ' ';
				/* FALLTHROUGH */
			default:
				addch(c);
			}
			refresh();
		}
	}
	*cp = '\0';
	return(c != EOF);
}
