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
static char sccsid[] = "@(#)timer.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>

#include <curses.h>
#include <setjmp.h>
#include <stdio.h>
#include <unistd.h>

#include "bog.h"
#include "extern.h"

static int waitch __P((long));

/*
 * Update the display of the remaining time while waiting for a character
 * If time runs out do a longjmp() to the game controlling routine, returning
 * non-zero; oth. return the character
 * Leave the cursor where it was initially
 */
int
timerch()
{
	extern int tlimit;
	extern long start_t;
	extern jmp_buf env;
	long prevt, t;
	int col, remaining, row;

	getyx(stdscr, row, col);
	prevt = 0L;
	for (;;) {
		if (waitch(1000L) == 1)
			break;
		time(&t);
		if (t == prevt)
			continue;
		prevt = t;
		remaining = tlimit - (int) (t - start_t);
		if (remaining < 0) {
			longjmp(env, 1);
			/*NOTREACHED*/
		}
		move(TIMER_LINE, TIMER_COL);
		printw("%d:%02d", remaining / 60, remaining % 60);
		move(row, col);
		refresh();
	}
	return (getch() & 0177);
}

/*
 * Wait up to 'delay' microseconds for input to appear
 * Returns 1 if input is ready, 0 oth.
 */
static int
waitch(delay)
	long delay;
{
	fd_set fdbits;
	struct timeval duration;

	duration.tv_sec = 0;
	duration.tv_usec = delay;
	FD_ZERO(&fdbits);
	FD_SET(STDIN_FILENO, &fdbits);
	return (select(32, &fdbits, NULL, NULL, &duration));
}

void
delay(tenths)
	int tenths;
{
	struct timeval duration;

	duration.tv_usec = (tenths % 10 ) * 100000L;
	duration.tv_sec = (long) (tenths / 10);
	select(32, 0, 0, 0, &duration);
}
