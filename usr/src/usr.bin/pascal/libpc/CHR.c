/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CHR.c 1.5 %G%";

char ECHR[] = "Argument to chr of %D is out of range\n";

char
CHR(value)
	unsigned long	value;
{
	if (value > 127) {
		ERROR(ECHR, value);
	}
	return (char)value;
}
