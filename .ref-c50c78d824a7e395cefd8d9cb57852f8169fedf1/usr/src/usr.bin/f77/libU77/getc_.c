/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)getc_.c	5.1	%G%
 */

/*
 * get a character from the standard input
 *
 * calling sequence:
 *	integer getc
 *	ierror = getc (char)
 * where:
 *	char will be read from the standard input, usually the terminal
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include	"../libI77/f_errno.h"
#include	"../libI77/fiodefs.h"

extern unit units[];	/* logical units table from iolib */

long getc_(c, clen)
char *c; long clen;
{
	int	i;
	unit	*lu;

	lu = &units[STDIN];
	if (!lu->ufd)
		return((long)(errno=F_ERNOPEN));
	if (lu->uwrt && ! nowreading(lu))
		return((long)errno);
	if ((i = getc (lu->ufd)) < 0)
	{
		if (feof(lu->ufd))
			return(-1L);
		i = errno;
		clearerr(lu->ufd);
		return((long)i);
	}
	*c = i;
	return(0L);
}
