/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)CHR.c	1.6 (Berkeley) %G%";
#endif /* not lint */

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
