/* vi: set tabstop=4 : */

#include "bog.h"

#ifdef TIMER

#include <setjmp.h>
#include <curses.h>
#include <stdio.h>

static int waitch();

/*
 * Update the display of the remaining time while waiting for a character
 * If time runs out do a longjmp() to the game controlling routine, returning
 * non-zero; oth. return the character
 * Leave the cursor where it was initially
 */
timerch()
{
	int col, remaining, row;
	long prevt, t;
	extern int tlimit;
	extern long start_t;
	extern jmp_buf env;

	getyx(stdscr, row, col);
	prevt = 0L;
	while (1) {
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
	return(getch() & 0177);
}

/*
 * Wait up to 'delay' microseconds for input to appear
 * Returns 1 if input is ready, 0 oth.
 */

#ifdef BSD42

#include <sys/time.h>

static
waitch(delay)
long delay;
{
	int fdbits;
	struct timeval duration;

	duration.tv_sec = 0L;
	duration.tv_usec = delay;
	fdbits = 1;
	return(select(32, &fdbits, 0, 0, &duration));
}

delay(tenths)
int tenths;
{
	struct timeval duration;

	duration.tv_usec = (tenths % 10 ) * 100000L;
	duration.tv_sec = (long) (tenths / 10);
	select(32, 0, 0, 0, &duration);
}
#endif BSD42

#ifdef SYSV

#include <sys/ioctl.h>

/*
 * This is not too efficient...
 */
static
waitch(delay)
long delay;
{
	int nchars;

	if (ioctl(fileno(stdin), FIONREAD, &nchars) < 0) {
		perror("ioctl():");
		cleanup();
		exit(1);
	}
	return(nchars > 0);
}

/*
 * Do nothing for the given number of tenths of a second
 */
delay(tenths)
int tenths;
{
	int n;

	n = tenths / 10;
	if (n == 0)
		n == 1;
	sleep(n);
}

#endif SYSV

#ifdef ATARI

#include <osbind.h>

/*
 * The ST curses turns on the cursor only when a read is performed
 * Since there's nothing better to do at this point the cursor can
 * be enabled
 */
static
waitch(delay)
long delay;
{

	Bconout(2, '\033');
	Bconout(2, 'e');
	return(Cconis() == -1);
}

/*
 * Do nothing for the given number of tenths of a second
 */
delay(tenths)
int tenths;
{
	int n;

	n = tenths / 10;
	if (n == 0)
		n == 1;
	sleep(n);
}

#endif ATARI

#else !TIMER

/*
 * Do nothing for the given number of tenths of a second
 */
delay(tenths)
int tenths;
{
	int n;

	n = tenths / 10;
	if (n == 0)
		n == 1;
	sleep(n);
}

#endif TIMER

