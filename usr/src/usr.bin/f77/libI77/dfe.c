/*
char id_dfe[] = "@(#)dfe.c	1.6";
 *
 * direct formatted external i/o
 */

#include "fio.h"

extern int rd_ed(),rd_ned(),w_ed(),w_ned();
int y_getc(),y_putc(),y_rnew(),y_wnew(),y_tab();

LOCAL char rdfe[] = "read dfe";
LOCAL char wdfe[] = "write dfe";

s_rdfe(a) cilist *a;
{
	int n;
	reading = YES;
	if(n=c_dfe(a,READ)) return(n);
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

s_wdfe(a) cilist *a;
{
	int n;
	reading = NO;
	if(n=c_dfe(a,WRITE)) return(n);
	curunit->uend = NO;
	if(!curunit->uwrt && ! nowwriting(curunit)) err(errflag, errno, wdfe)
	putn = y_putc;
	doed = w_ed;
	doned = w_ned;
	dotab = y_tab;
	dorevert = doend = donewrec = y_wnew;
	if(pars_f()) err(errflag,F_ERFMT,wdfe)
	fmt_bg();
	return(OK);
}

e_rdfe()
{
	en_fio();
	return(OK);
}

e_wdfe()
{
	en_fio();
	return(OK);
}

LOCAL
c_dfe(a,flag) cilist *a;
{	int n;
	sequential = NO;
	external = formatted = FORMATTED;
	lfname = NULL;
	elist = NO;
	cursor=scale=recpos=reclen=0;
	radix = 10;
	signit = YES;
	fmtbuf = a->cifmt;
	errflag = a->cierr;
	endflag = a->ciend;
	lunit = a->ciunit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,rdfe+5);
	curunit = &units[lunit];
	if(!curunit->ufd && (n=fk_open(flag,DIR,FMT,(ftnint)lunit)))
		err(errflag,n,rdfe+5)
	cf = curunit->ufd;
	elist = YES;
	lfname = curunit->ufnm;
	if(!curunit->ufmt) err(errflag,F_ERNOFIO,rdfe+5)
	if(!curunit->useek || !curunit->url) err(errflag,F_ERNODIO,rdfe+5)
	recnum = a->cirec - 1;
	fseek(cf, (long)curunit->url * recnum, 0);
	cblank = curunit->ublnk;
	cplus = NO;
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

LOCAL
y_putc(c)
{
	if(curunit->url!=1 && recpos++ >= curunit->url) err(errflag,F_EREREC,wdfe)
	putc(c,cf);
	return(OK);
}

LOCAL
y_tab()
{	int n;
	if(curunit->url==1)
	{
		if(cursor < 0 && -cursor > ftell(cf)) rewind(cf);
		else	fseek(cf,(long)cursor,1);
		return(cursor=0);
	}
	else
	{	if(reclen < recpos) reclen = recpos;
		if((recpos + cursor) < 0) cursor = -recpos;	/* BOR */
		n = reclen - recpos;		/* n >= 0 */
		if(!reading && (cursor-n) > 0)
		{	recpos = reclen;
			cursor -= n;
			fseek(cf,(long)n,1);
			while(cursor--) if(n=(*putn)(' ')) return(n);
			return(cursor=0);
		}
		recpos += cursor;
		if(recpos >= curunit->url) err(errflag,F_EREREC,rdfe+5)
	}
	fseek(cf,(long)cursor,1);
	return(cursor=0);
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

LOCAL
y_wnew()
{	if(curunit->url != 1)
	{	if(reclen > recpos)
		{	fseek(cf,(long)(reclen-recpos),1);
			recpos = reclen;
		}
		while(recpos < curunit->url) (*putn)(' ');
		recnum++;
		recpos = reclen = cursor = 0;
	}
	return(OK);
}

LOCAL
y_rend()
{
	return(OK);
}

LOCAL
y_wend()
{
	return(y_wnew());
}
