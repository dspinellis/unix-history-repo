/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)blkclr.c 1.1 3/7/81";

blkclr(siz, at)
	long		siz;
	register char	*at;
{
	register int	size = siz;

	while(size-- > 0)
		*at++ = 0;
}
