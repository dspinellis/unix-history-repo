#ifndef lint
static char sccsid[] = "@(#)isatty.c	5.1 (Berkeley) %G%";
#endif not lint

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
