/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ARGV.c 1.4 4/12/82";

#include "h00vars.h"

ARGV(subscript, var, siz)

	long		subscript;	/* subscript into argv */
	register char	*var;		/* pointer to pascal char array */
	long		siz;		/* sizeof(var) */
{
	register char	*cp;
	register int	size = siz;

	if ((unsigned)subscript >= _argc) {
		ERROR("Argument to argv of %D is out of range\n", subscript);
		return;
	}
	cp = _argv[subscript];
	do	{
		*var++ = *cp++;
	} while (--size && *cp);
	while (size--)
		*var++ = ' ';
}
