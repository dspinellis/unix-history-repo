/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tstp.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <errno.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>

/*
 * tstp --
 *	Handle stop and start signals.
 */
void
tstp(signo)
	int signo;
{
	struct termios save;
	sigset_t set;

	/* Get the current terminal state. */
	if (tcgetattr(STDIN_FILENO, &save))
		return;

	/* Move the cursor to the end of the screen. */
	mvcur(0, COLS - 1, LINES - 1, 0);

	/* End the window. */
	endwin();

	/* Stop ourselves. */
	(void)sigemptyset(&set);
	(void)sigaddset(&set, SIGTSTP);
	(void)sigprocmask(SIG_UNBLOCK, &set, NULL);
	(void)signal(SIGTSTP, SIG_DFL);
	(void)kill(0, SIGTSTP);

	/* Time passes ... */

	/* Reset the signal handler. */
	(void)signal(SIGTSTP, tstp);

	/* Reset the terminal state. */
	(void)tcsetattr(STDIN_FILENO, TCSADRAIN, &save);

	/* Restart the screen. */
	wrefresh(curscr);
}
