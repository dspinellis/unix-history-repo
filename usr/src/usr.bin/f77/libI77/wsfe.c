/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)wsfe.c	5.1	%G%
 */

/*
 * write sequential formatted external
 */

#include "fio.h"

LOCAL char wsfe[] = "write sfe";

extern int w_ed(),w_ned();
int x_putc(),pr_put(),x_wend(),x_wnew(),x_tab();
LOCAL ioflag new;

s_wsfe(a) cilist *a;	/*start*/
{	int n;
	reading = NO;
	sequential=FORMATTED;
	if(n=c_sfe(a,WRITE,SEQ,wsfe)) return(n);
	if(curunit->url) err(errflag,F_ERNOSIO,wsfe)
	if(!curunit->uwrt && ! nowwriting(curunit)) err(errflag, errno, wsfe)
	curunit->uend = NO;
	if (curunit->uprnt) putn = pr_put;
	else putn = x_putc;
	new = YES;
	doed= w_ed;
	doned= w_ned;
	doend = x_wend;
	dorevert = donewrec = x_wnew;
	dotab = x_tab;
	if(pars_f()) err(errflag,F_ERFMT,wsfe)
	fmt_bg();
	return(OK);
}

LOCAL
x_putc(c)
{
	if(c=='\n') recpos = reclen = cursor = 0;
	else recpos++;
	if (c) putc(c,cf);
	return(OK);
}

LOCAL
pr_put(c)
{
	if(c=='\n')
	{	new = YES;
		recpos = reclen = cursor = 0;
	}
	else if(new)
	{	new = NO;
		if(c=='0') c = '\n';
		else if(c=='1') c = '\f';
		else return(OK);
	}
	else recpos++;
	if (c) putc(c,cf);
	return(OK);
}

LOCAL
x_wnew()
{
	if(reclen>recpos) fseek(cf,(long)(reclen-recpos),1);
	return((*putn)('\n'));
}

LOCAL
x_wend(last) char last;
{
	if(reclen>recpos) fseek(cf,(long)(reclen-recpos),1);
	return((*putn)(last));
}

e_wsfe()
{	int n;
	n=en_fio();
	fmtbuf=NULL;
	return(n);
}
