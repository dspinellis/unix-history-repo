/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBSCZ.c 1.5 %G%";

extern char ESUBSC[];	/* ESUBSC is defined in SUBSCZ.c */

long
SUBSCZ(value, upper)
	long		value;
	unsigned long	upper;
{
	if (value > upper) {
		ERROR(ESUBSC, value);
	}
	return value;
}
