/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)due.c	5.2	%G%
 */

/*
 * direct unformatted external i/o
 */

#include "fio.h"

LOCAL char rdue[] = "read due";
LOCAL char wdue[] = "write due";

s_rdue(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_due(a,READ)) return(n);
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, rdue);
	return(OK);
}

s_wdue(a) cilist *a;
{
	int n;
	reading = NO;
	if(n=c_due(a,WRITE)) return(n);
	curunit->uend = NO;
	if(!curunit->uwrt && ! nowwriting(curunit)) err(errflag, errno, wdue)
	return(OK);
}

LOCAL
c_due(a,flg) cilist *a;
{	int n;
	lfname = NULL;
	elist = NO;
	sequential=formatted=NO;
	recpos = reclen = 0;
	external = YES;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,rdue+5);
	curunit = &units[lunit];
	if (!curunit->ufd && (n=fk_open(flg,DIR,UNF,(ftnint)lunit)) )
		err(errflag,n,rdue+5)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if (curunit->ufmt) err(errflag,F_ERNOUIO,rdue+5)
	if (!curunit->useek || !curunit->url) err(errflag,F_ERNODIO,rdue+5)
	if (fseek(cf, (long)((a->cirec-1)*curunit->url), 0) < 0)
		return(due_err(rdue+5));
	else
		return(OK);
}

e_rdue()
{
	return(OK);
}

e_wdue()
{/*	This is to ensure full records. It is really necessary. */
	int n = 0;
	if (curunit->url!=1 && recpos!=curunit->url &&
	    (fseek(cf, (long)(curunit->url-recpos-1), 1) < 0
		|| fwrite(&n, 1, 1, cf) != 1))
			return(due_err(rdue+5));
	return(OK);
}
