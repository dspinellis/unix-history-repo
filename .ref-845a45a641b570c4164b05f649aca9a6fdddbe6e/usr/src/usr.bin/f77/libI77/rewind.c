/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rewind.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * rewind.c  -  f77 file rewind
 */

#include "fio.h"

static char	rwnd[]	= "rewind";

f_rew(a) alist *a;
{	int n;
	unit *b;

	lfname = NULL;
	elist = NO;
	external = YES;			/* for err */
	lunit = a->aunit;
	errflag = a->aerr;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,rwnd)
	b = &units[lunit];
	if(!b->ufd) return(OK);
	lfname = b->ufnm;
	if(!b->useek) err(errflag,F_ERNOBKSP,rwnd)
	b->uend = NO;
	if(b->uwrt)
		if(n=t_runc(b,errflag,rwnd)) return(n);
	rewind(b->ufd);
	return(OK);
}
