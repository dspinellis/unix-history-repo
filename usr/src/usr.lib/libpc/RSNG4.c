/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RSNG4.c 1.4 11/12/82";

extern char ERANG[];	/* ERANG is defined in RANG4.c */

long
RSNG4(value, upper)
	long		value;
	unsigned long	upper;
{
	if (value > upper) {
		ERROR(ERANG, value);
		return;
	}
	return	value;
}
