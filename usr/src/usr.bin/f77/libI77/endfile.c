/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)endfile.c	5.2	%G%
 */

/*
 * endfile
 */

#include "fio.h"

static char	endf[]	= "endfile";

f_end (a)
alist	*a;
{
	unit	*b;
	int n;

	lfname = NULL;
	elist = NO;
	errflag = a->aerr;
	lunit = a->aunit;
	if (not_legal(lunit))
		err (errflag, F_ERUNIT, endf)
	b = &units[lunit];
	if(!b->ufd && (n = fk_open(READ, SEQ, FMT, (ftnint)lunit)) )
		err(errflag, n, endf);
	if (b->uend)
		return(0);
	lfname = b->ufnm;
	b->uend = YES;
	return ( t_runc (b, errflag, endf) );
}
