/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	8.1 (Berkeley) %G%";
#endif /* not lint */

label(s)
char *s;
{
	register i,c;
	putch(037);	/* alpha mode */
	for(i=0; c=s[i]; i++)
		putch(c);
}
