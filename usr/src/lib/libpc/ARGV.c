/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ARGV.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

ARGV(subscript, var, size)

	int		subscript;	/* subscript into argv */
	register char	*var;		/* pointer to pascal char array */
	register int	size;		/* sizeof(var) */
{
	register char	*cp;

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
