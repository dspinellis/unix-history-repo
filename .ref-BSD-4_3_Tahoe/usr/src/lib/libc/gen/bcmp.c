/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bcmp.c	5.1 (Berkeley) 1/27/87";
#endif LIBC_SCCS and not lint

/*
 * bcmp -- vax cmpc3 instruction
 */
bcmp(b1, b2, length)
	register char *b1, *b2;
	register int length;
{

	if (length == 0)
		return (0);
	do
		if (*b1++ != *b2++)
			break;
	while (--length);
	return(length);
}
