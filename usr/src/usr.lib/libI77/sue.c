/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sue.c	5.1	6/7/85
 */

/*
 * sequential unformatted external read/write routines
 */

#include "fio.h"

extern int reclen;
LOCAL long recloc;
LOCAL char rsue[] = "read sue";
LOCAL char wsue[] = "write sue";

s_rsue(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_sue(a,READ)) return(n);
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, rsue)
	recpos = 0;
	if(fread(&reclen,sizeof(int),1,cf) == 1) return(OK);
	if(feof(cf))
	{	curunit->uend = YES;
		err(endflag, EOF, rsue)
	}
	clearerr(cf);
	err(errflag, errno, rsue)
}

s_wsue(a) cilist *a;
{
	int n;
	reading = NO;
	if(n=c_sue(a,WRITE)) return(n);
	if(!curunit->uwrt && ! nowwriting(curunit)) err(errflag, errno, wsue)
	reclen = 0;
	recloc=ftell(cf);
	fseek(cf,(long)sizeof(int),1);
	curunit->uend = NO;
	return(OK);
}

LOCAL
c_sue(a,flag) cilist *a;
{	int n;
	external = sequential = YES;
	formatted = NO;
	lfname = NULL;
	elist = NO;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,rsue+5)
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flag,SEQ,UNF,(ftnint)lunit)))
		err(errflag,n,rsue+5)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if(curunit->ufmt) err(errflag,F_ERNOUIO,rsue+5)
	if(curunit->url) err(errflag,F_ERNOSIO,rsue+5)
	if(!curunit->useek) err(errflag,F_ERSEEK,rsue+5)
	return(OK);
}

e_wsue()
{	long loc;
	fwrite(&reclen,sizeof(int),1,cf);
	loc=ftell(cf);
	fseek(cf,recloc,0);
	fwrite(&reclen,sizeof(int),1,cf);
	fseek(cf,loc,0);
	return(OK);
}

e_rsue()
{
	fseek(cf,(long)(reclen-recpos+sizeof(int)),1);
	return(OK);
}
