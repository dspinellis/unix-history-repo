/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)keyboard.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Keyboard input support.
 */

#include "systat.h"
#include <ctype.h>

keyboard()
{
        char ch, line[80];
	int oldmask;

        for (;;) {
                col = 0;
                move(CMDLINE, 0);
                do {
                        refresh();
                        ch = getch() & 0177;
                        if (ch == 0177 && ferror(stdin)) {
                                clearerr(stdin);
                                continue;
                        }
                        if (ch >= 'A' && ch <= 'Z')
                                ch += 'a' - 'A';
                        if (col == 0) {
#define	mask(s)	(1 << ((s) - 1))
                                if (ch == CTRL('l')) {
					oldmask = sigblock(mask(SIGALRM));
					wrefresh(curscr);
					sigsetmask(oldmask);
                                        continue;
                                }
				if (ch == CTRL('g')) {
					oldmask = sigblock(mask(SIGALRM));
					status();
					sigsetmask(oldmask);
					continue;
				}
                                if (ch != ':')
                                        continue;
                                move(CMDLINE, 0);
                                clrtoeol();
                        }
                        if (ch == _tty.sg_erase && col > 0) {
                                if (col == 1 && line[0] == ':')
                                        continue;
                                col--;
                                goto doerase;
                        }
                        if (ch == CTRL('w') && col > 0) {
                                while (--col >= 0 && isspace(line[col]))
                                        ;
                                col++;
                                while (--col >= 0 && !isspace(line[col]))
                                        if (col == 0 && line[0] == ':')
                                                break;
                                col++;
                                goto doerase;
                        }
                        if (ch == _tty.sg_kill && col > 0) {
                                col = 0;
                                if (line[0] == ':')
                                        col++;
                doerase:
                                move(CMDLINE, col);
                                clrtoeol();
                                continue;
                        }
                        if (isprint(ch) || ch == ' ') {
                                line[col] = ch;
                                mvaddch(CMDLINE, col, ch);
                                col++;
                        }
                } while (col == 0 || (ch != '\r' && ch != '\n'));
                line[col] = '\0';
		oldmask = sigblock(mask(SIGALRM));
                command(line + 1);
		sigsetmask(oldmask);
        }
	/*NOTREACHED*/
}
