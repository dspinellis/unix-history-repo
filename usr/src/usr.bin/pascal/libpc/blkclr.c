/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)blkclr.c 1.2 %G%";

blkclr(at, siz)
	register char	*at;
	long		siz;
{
	register int	size = siz;

	while(size-- > 0)
		*at++ = 0;
}
