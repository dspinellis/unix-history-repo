/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rdfe.c	5.1	6/7/85
 */

/*
 * read direct formatted external i/o
 */

#include "fio.h"

extern int rd_ed(),rd_ned();
int y_getc(),y_rnew(),y_tab();

LOCAL char rdfe[] = "read dfe";

s_rdfe(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_dfe(a,READ,rdfe)) return(n);
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, rdfe)
	getn = y_getc;
	doed = rd_ed;
	doned = rd_ned;
	dotab = y_tab;
	dorevert = doend = donewrec = y_rnew;
	if(pars_f()) err(errflag,F_ERFMT,rdfe)
	fmt_bg();
	return(OK);
}

e_rdfe()
{
	en_fio();
	return(OK);
}

LOCAL
y_getc()
{
	int ch;
	if(curunit->uend) return(EOF);
	if(curunit->url==1 || recpos++ < curunit->url)
	{
		if((ch=getc(cf))!=EOF)
		{
				return(ch);
		}
		if(feof(cf))
		{
			curunit->uend = YES;
			return(EOF);
		}
		err(errflag,errno,rdfe);
	}
	else return(' ');
}

/*
/*y_rev()
/*{	/*what about work done?*/
/*	if(curunit->url==1) return(0);
/*	while(recpos<curunit->url) (*putn)(' ');
/*	recpos=0;
/*	return(0);
/*}
/*
/*y_err()
/*{
/*	err(errflag, F_EREREC, rdfe+5);
/*}
*/

LOCAL
y_rnew()
{	if(curunit->url != 1)
	{	fseek(cf,(long)curunit->url*(++recnum),0);
		recpos = reclen = cursor = 0;
	}
	return(OK);
}
