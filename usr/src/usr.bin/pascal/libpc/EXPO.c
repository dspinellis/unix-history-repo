/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)EXPO.c	1.3 (Berkeley) %G%";
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
