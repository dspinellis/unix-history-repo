/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tstp.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <errno.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>

/*
 * stop_signal_handler --
 *	Handle stop signals.
 */
void
__stop_signal_handler(signo)
	int signo;
{
	struct termios save;
	sigset_t oset, set;

	/* Get the current terminal state. */
	if (tcgetattr(STDIN_FILENO, &save))
		return;

	/*
	 * Block every signal we can get our hands on.  This is because
	 * applications have timers going off that want to repaint the
	 * screen.
	 */
	(void)sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);
	
	/*
	 * End the window, which also resets the terminal state to the
	 * original modes.
	 */
	endwin();

	/* Unblock SIGTSTP. */
	(void)sigemptyset(&set);
	(void)sigaddset(&set, SIGTSTP);
	(void)sigprocmask(SIG_UNBLOCK, &set, NULL);

	/* Stop ourselves. */
	(void)signal(SIGTSTP, SIG_DFL);
	(void)kill(0, SIGTSTP);

	/* Time passes ... */

	/* Reset the curses SIGTSTP signal handler. */
	(void)signal(SIGTSTP, __stop_signal_handler);

	/* Reset the terminal state its mode when we stopped. */
	(void)tcsetattr(STDIN_FILENO, TCSADRAIN, &save);

	/* Restart the screen. */
	__startwin();

	/* Repaint the screen. */
	wrefresh(curscr);

	/* Reset the signals. */
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);
}
