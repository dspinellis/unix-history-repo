/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bzero.c	5.1 (Berkeley) 1/27/87";
#endif LIBC_SCCS and not lint

/*
 * bzero -- vax movc5 instruction
 */
bzero(b, length)
	register char *b;
	register int length;
{

	if (length)
		do
			*b++ = '\0';
		while (--length);
	return(length);
}
