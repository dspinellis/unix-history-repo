/*
char id_due[] = "@(#)due.c	1.2";
 *
 * direct unformatted external i/o
 */

#include "fio.h"

char *due = "due";

s_rdue(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_due(a,READ)) return(n);
	if(curunit->uwrt) nowreading(curunit);
	return(OK);
}

s_wdue(a) cilist *a;
{
	int n;
	reading = NO;
	if(n=c_due(a,WRITE)) return(n);
	curunit->uend = NO;
	if(!curunit->uwrt) nowwriting(curunit);
	return(OK);
}

c_due(a,flag) cilist *a;
{	int n;
	lfname = NULL;
	elist = NO;
	sequential=formatted=NO;
	recpos = reclen = 0;
	external = YES;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,due);
	curunit = &units[lunit];
	if (!curunit->ufd && (n=fk_open(flag,DIR,UNF,(ftnint)lunit)) )
		err(errflag,n,due)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if (curunit->ufmt) err(errflag,F_ERNOUIO,due)
	if (!curunit->useek || !curunit->url) err(errflag,F_ERNODIO,due)
	if (fseek(cf, (long)((a->cirec-1)*curunit->url), 0) < 0)
		return(due_err(due));
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
			return(due_err(due));
	return(OK);
}
