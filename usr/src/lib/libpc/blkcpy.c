/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)blkcpy.c 1.1 3/7/81";

blkcpy(siz, from, to)
	long		siz;
	register char	*from;
	register char	*to;
{
	register int	size = siz;

	if (from + size < to)
		while(size-- > 0)
			*to++ = *from++;
	else {
		to += size;
		from += size;
		while(size-- > 0)
			*--to = *--from;
	}
}
