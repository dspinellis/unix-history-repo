/* @(#)isatty.c	4.1 (Berkeley) 12/21/80 */
/*
 * Returns 1 iff file is a tty
 */

#include <sgtty.h>

isatty(f)
{
	struct sgttyb ttyb;

	if (gtty(f, &ttyb) < 0)
		return(0);
	return(1);
}
