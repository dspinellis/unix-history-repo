/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)fdate_.c	5.1	6/7/85
 */

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
