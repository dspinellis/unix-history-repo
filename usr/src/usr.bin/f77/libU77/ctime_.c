/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)ctime_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * convert system time to ascii string
 *
 * calling sequence:
 *	character*24 string, ctime
 *	integer clock
 *	string = ctime (clock)
 * where:
 *	string will receive the ascii equivalent of the integer clock time.
 */

char *ctime();

ctime_(str, len, clock)
char *str; long len, *clock;
{
	char *s = ctime(clock);
	s[24] = '\0';
	b_char(s, str, len);
}
