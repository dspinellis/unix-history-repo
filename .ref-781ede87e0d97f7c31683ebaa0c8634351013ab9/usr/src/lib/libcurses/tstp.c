/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tstp.c	5.5 (Berkeley) %G%";
#endif /* not lint */

# include	<signal.h>

# include	"curses.ext"

/*
 * handle stop and start signals
 *
 * @(#)tstp.c	5.5 (Berkeley) %G%
 */
tstp() {

# ifdef SIGTSTP

	SGTTY	tty;
	int	omask;
# ifdef DEBUG
	if (outf)
		fflush(outf);
# endif
	tty = _tty;
	mvcur(0, COLS - 1, LINES - 1, 0);
	endwin();
	fflush(stdout);
	/* reset signal handler so kill below stops us */
	signal(SIGTSTP, SIG_DFL);
#define	mask(s)	(1 << ((s)-1))
	omask = sigsetmask(sigblock(0) &~ mask(SIGTSTP));
	kill(0, SIGTSTP);
	sigblock(mask(SIGTSTP));
	signal(SIGTSTP, tstp);
	_tty = tty;
	ioctl(_tty_ch, TIOCSETP, &_tty);
	wrefresh(curscr);
# endif	SIGTSTP
}
