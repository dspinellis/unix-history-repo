/*
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
	if(not_legal(lunit)) err(errflag,101,due);
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flag,DIR,UNF,lunit)) )
						err(errflag,n,due);
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if(curunit->ufmt) err(errflag,103,due)
	if(!curunit->useek || !curunit->url) err(errflag,104,due)
	fseek(cf,(long)(a->cirec-1)*curunit->url,0);
	return(OK);
}

e_rdue()
{
	if(curunit->url==1 || recpos==curunit->url) return(OK);
	fseek(cf,(long)(curunit->url-recpos),1);
	if(ftell(cf)%curunit->url) err(errflag,119,due)
	return(OK);
}

e_wdue()
{
	return(e_rdue());
}
