/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ctime_.c	5.1	%G%
 */

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
