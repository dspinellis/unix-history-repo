/*
char id_sue[] = "@(#)sue.c	1.2";
 *
 * sequential unformatted external read/write routines
 */

#include "fio.h"

extern int reclen;
long recloc;
char *rsue = "read sue";
char *sue = "sue";

s_rsue(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_sue(a,READ)) return(n);
	if(curunit->uwrt) nowreading(curunit);
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
	if(!curunit->uwrt) nowwriting(curunit);
	reclen = 0;
	recloc=ftell(cf);
	fseek(cf,(long)sizeof(int),1);
	curunit->uend = NO;
	return(OK);
}

c_sue(a,flag) cilist *a;
{	int n;
	external = sequential = YES;
	formatted = NO;
	lfname = NULL;
	elist = NO;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,sue)
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flag,SEQ,UNF,(ftnint)lunit)))
		err(errflag,n,sue)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if(curunit->ufmt) err(errflag,F_ERNOUIO,sue)
	if(curunit->url) err(errflag,F_ERNOSIO,sue)
	if(!curunit->useek) err(errflag,F_ERSEEK,sue)
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
