/* @(#)isatty.c	4.2 (Berkeley) %G% */
/*
 * Returns 1 iff file is a tty
 */

#include <sgtty.h>

isatty(f)
{
	struct sgttyb ttyb;

	if (ioctl(f, TIOCGETP, &ttyb) < 0)
		return(0);
	return(1);
}
