/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ARGV.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

ARGV(subscript, var, siz)

	long		subscript;	/* subscript into argv */
	register char	*var;		/* pointer to pascal char array */
	long		siz;		/* sizeof(var) */
{
	register char	*cp;
	register int	size = siz;

	if (subscript >= _argc) {
		ERROR(EARGV, subscript);
		return;
	}
	cp = _argv[subscript];
	do	{
		*var++ = *cp++;
	} while (--size && *cp);
	while (size--)
		*var++ = ' ';
}
