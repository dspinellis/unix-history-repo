/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ffs.c	5.1 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

/*
 * ffs -- vax ffs instruction
 */
ffs(mask)
	register long mask;
{
	register int cnt;

	if (mask == 0)
		return(0);
	for (cnt = 1; !(mask & 1); cnt++)
		mask >>= 1;
	return(cnt);
}
