#ifndef lint
static char sccsid[] = "@(#)keyboard.c	1.1 (Lucasfilm) %G%";
#endif

/*
 * Keyboard input support.
 */

#include "systat.h"

keyboard()
{
        char ch, line[80];

        for (;;) {
                col = 0;
                move(22, 0);
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
                                if (ch == CTRL(l)) {
					int oldmask = sigblock(mask(SIGALRM));

					wrefresh(curscr);
					sigsetmask(oldmask);
                                        continue;
                                }
				if (ch == CTRL(g)) {
					status();
					continue;
				}
                                if (ch != ':')
                                        continue;
                                move(22, 0);
                                clrtoeol();
                        }
                        if (ch == _tty.sg_erase && col > 0) {
                                if (col == 1 && line[0] == ':')
                                        continue;
                                col--;
                                goto doerase;
                        }
                        if (ch == CTRL(w) && col > 0) {
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
                                move(22, col);
                                clrtoeol();
                                continue;
                        }
                        if (isprint(ch)) {
                                line[col] = ch;
                                mvaddch(22, col, ch);
                                col++;
                        }
                } while (col == 0 || (ch != '\r' && ch != '\n'));
                line[col] = '\0';
                command(line + 1);
        }
	/*NOTREACHED*/
}
