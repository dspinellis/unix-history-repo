/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ASRT.c 1.2 6/10/81";

#define NULL 0

ASRT(cond, stmt)

	short	cond;
	char	*stmt;
{
	if (cond)
		return;
	if (stmt != NULL) {
		ERROR("Assertion failed: %s\n", stmt);
		return;
	} else {
		ERROR("Assertion failed\n", 0);
		return;
	}
}
