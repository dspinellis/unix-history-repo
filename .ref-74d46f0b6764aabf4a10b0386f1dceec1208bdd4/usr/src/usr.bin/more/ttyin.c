/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ttyin.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines dealing with getting input from the keyboard (i.e. from the user).
 */

#include <less.h>

static int tty;

/*
 * Open keyboard for input.
 * (Just use file descriptor 2.)
 */
open_getchr()
{
	tty = 2;
}

/*
 * Get a character from the keyboard.
 */
getchr()
{
	char c;
	int result;

	do
	{
		result = iread(tty, &c, 1);
		if (result == READ_INTR)
			return (READ_INTR);
		if (result < 0)
		{
			/*
			 * Don't call error() here,
			 * because error calls getchr!
			 */
			quit();
		}
	} while (result != 1);
	return (c & 0177);
}
