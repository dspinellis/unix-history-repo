/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)EXPO.c 1.2 3/7/81";

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
