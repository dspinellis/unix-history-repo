/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)EXPO.c	1.3 (Berkeley) 4/9/90";
#endif /* not lint */

long
EXPO(value)

	double	value;
{
	register int retval;
	register char *cp;
	char sign, buf[30];
	extern char *index();

	if (value == 0.0)
		return 0;
	sprintf(buf, "%.1e", value);
	cp = index(buf, 'e') + 1;
	sign = *cp++;
	retval = 0;
	while (*cp)
		retval = retval * 10 + *cp++ - '0';
	return sign == '-' ? -retval : retval;
}
