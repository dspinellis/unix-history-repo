/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SUBT.c 1.2 3/7/81";

long *
SUBT(result0, left, right, siz)

	long		*result0;
	register long	*left;
	register long	*right;
	long		siz;
{
	register long	*result = result0;
	register int	size = siz;

	do {
		*result++ = *left++ & ~*right++;
	} while (--size);
	return result0;
}
