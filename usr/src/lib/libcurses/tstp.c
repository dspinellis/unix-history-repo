/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tstp.c	8.1 (Berkeley) %G%";
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

	/* Get the current terminal state (which the user may have changed). */
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
	__restore_stophandler();
	(void)kill(0, SIGTSTP);

	/* Time passes ... */

	/* Reset the curses SIGTSTP signal handler. */
	__set_stophandler();

	/* save the new "default" terminal state */
	(void)tcgetattr(STDIN_FILENO, &__orig_termios);

	/* Reset the terminal state to the mode just before we stopped. */
	(void)tcsetattr(STDIN_FILENO, __tcaction, &save);

	/* Restart the screen. */
	__startwin();

	/* Repaint the screen. */
	wrefresh(curscr);

	/* Reset the signals. */
	(void)sigprocmask(SIG_SETMASK, &oset, NULL);
}

static void (*otstpfn)() = SIG_DFL;

/*
 * Set the TSTP handler.
 */
void
__set_stophandler()
{
	otstpfn = signal(SIGTSTP, __stop_signal_handler);
}

/*
 * Restore the TSTP handler.
 */
void
__restore_stophandler()
{
	(void)signal(SIGTSTP, otstpfn);
}
