/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)fdate_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * Return date and time in an ASCII string.
 *
 * calling sequence:
 *	character*24 string
 * 	call fdate(string)
 * where:
 *	the 24 character string will be filled with the date & time in
 *	ascii form as described under ctime(3).
 *	No 'newline' or NULL will be included.
 */

fdate_(s, strlen)
char *s; long strlen;
{
	char *ctime(), *c;
	long time(), t;

	t = time(0);
	c = ctime(&t);
	c[24] = '\0';
	b_char(c, s, strlen);
}
