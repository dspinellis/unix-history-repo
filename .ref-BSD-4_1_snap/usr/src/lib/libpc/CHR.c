/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CHR.c 1.3 6/10/81";


char
CHR(value)

	long	value;
{
	if (value < 0 || value > 127) {
		ERROR("Argument to chr of %D is out of range\n", value);
		return;
	}
	return (char)value;
}
