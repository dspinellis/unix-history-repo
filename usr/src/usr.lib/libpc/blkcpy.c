/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)blkcpy.c 1.3 11/12/82";

blkcpy(from, to, siz)
	register char	*from;
	register char	*to;
	long		siz;
{
	register int	size = siz;

	if (to < from)
		while(size-- > 0)
			*to++ = *from++;
	else {
		to += size;
		from += size;
		while(size-- > 0)
			*--to = *--from;
	}
}
