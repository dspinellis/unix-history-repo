/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)flush_in.c	8.1 (Berkeley) %G%";
#endif /* not lint */

# include	<curses.h>

/*
 * flush_in:
 *	Flush all pending input.
 */
flush_in()
{
# ifdef TIOCFLUSH
	ioctl(fileno(stdin), TIOCFLUSH, NULL);
# else TIOCFLUSH
	crmode();
# endif TIOCFLUSH
}
