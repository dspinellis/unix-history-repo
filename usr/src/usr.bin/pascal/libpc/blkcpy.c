/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)blkcpy.c 1.2 %G%";

blkcpy(siz, from, to)
	long		siz;
	register char	*from;
	register char	*to;
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
