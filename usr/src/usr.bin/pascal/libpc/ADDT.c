/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ADDT.c 1.1 %G%";

long *
ADDT(result0, left, right, size)

	long		*result0;
	register long	*left;
	register long	*right;
	register int	size;
{
	register long	*result = result0;

	do {
		*result++ = *left++ | *right++;
	} while (--size);
	return result0;
}
