/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ASRT.c 1.3 11/12/82";

char EASRT[] = "Assertion failed\n";

ASRT(cond)
	short	cond;
{
	if (cond)
		return;
	ERROR(EASRT, 0);
	return;
}
