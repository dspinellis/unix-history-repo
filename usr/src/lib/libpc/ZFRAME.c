/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ZFRAME.c 1.1 10/29/80";

ZFRAME(size, ptr)

	int		size;
	register long	*ptr;
{
	register long	*end;
	short		*sptr;

	end = ptr + (size / sizeof(long));
	for (; ptr < end; *ptr++ = 0)
		/* void */;
	if (size % sizeof(long)) {
		sptr = (short *)ptr;
		*sptr = 0;
	}
}
