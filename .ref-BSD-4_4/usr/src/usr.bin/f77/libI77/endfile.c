/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)endfile.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

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
