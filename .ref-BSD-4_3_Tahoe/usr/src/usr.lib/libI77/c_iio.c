/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_iio.c	5.2	7/30/85
 */

/*
 * internal (character array) i/o: common portions
 */

#include "fio.h"
#include "lio.h"

LOCAL icilist *svic;		/* active internal io list */
LOCAL lio_nl;

int z_wnew();

z_getc()
{
	if(formatted == LISTDIRECTED )
	{
		if( lio_nl == YES )
		{
			recnum++;
			recpos = 0;
		}
		else if (recpos == svic->icirlen)
		{
			lio_nl = YES;
			return('\n');
		}
		lio_nl = NO;
	}

	if(icptr >= icend && !recpos)	/* new rec beyond eof */
	{	leof = EOF;
		return(EOF);
	}
	if(recpos++ < svic->icirlen) return(*icptr++);
	return(' ');
}

z_putc(c) char c;
{
	if(icptr < icend)
	{	if(c=='\n') return(z_wnew());
		if(recpos++ < svic->icirlen)
		{	*icptr++ = c;
			return(OK);
		}
		else err(errflag,F_EREREC,"iio")
	}
	leof = EOF;
#ifndef KOSHER
	err(endflag,EOF,"iio")   /* NOT STANDARD, end-of-file on writes */
#endif
#ifdef KOSHER
	err(errflag,F_EREREC,"iio")
#endif
}

z_ungetc(ch,cf) char ch;
{	
	if( lio_nl == YES )
	{
		lio_nl = NO;
		return(OK);
	}
	if(ch==EOF || --recpos >= svic->icirlen) return(OK);
	if(--icptr < svic->iciunit || recpos < 0) err(errflag,F_ERBREC,"ilio")
	*icptr = ch;
	return(OK);
}

LOCAL
c_fi(a) icilist *a;
{
	fmtbuf=a->icifmt;
	formatted = FORMATTED;
	external = NO;
	cblank=cplus=NO;
	scale=cursor=0;
	radix = 10;
	signit = YES;
	elist = YES;
	svic = a;
	recpos=reclen=0;
	icend = a->iciunit + a->icirnum*a->icirlen;
	errflag = a->icierr;
	endflag = a->iciend;
	return(OK);
}

c_si(a) icilist *a;
{
	sequential = YES;
	recnum = 0;
	icptr = a->iciunit;
	return(c_fi(a));
}

c_di(a) icilist *a;
{
	sequential = NO;
	recnum = a->icirec - 1;
	icptr = a->iciunit + recnum*a->icirlen;
	return(c_fi(a));
}

z_rnew()
{
	icptr = svic->iciunit + (++recnum)*svic->icirlen;
	recpos = reclen = cursor = 0;
	return(OK);
}

z_wnew()
{
	if(reclen > recpos)
	{	icptr += (reclen - recpos);
		recpos = reclen;
	}
	while(recpos < svic->icirlen) (*putn)(' ');
	recpos = reclen = cursor = 0;
	recnum++;
	return(OK);
}

z_tab()
{	int n;
	if(reclen < recpos) reclen = recpos;
	if((recpos + cursor) < 0) cursor = -recpos;	/* to BOR */
	n = reclen - recpos;
	if(!reading && (cursor-n) > 0)
	{	icptr += n;
		recpos = reclen;
		cursor -= n;
		while(cursor--) if(n=(*putn)(' ')) return(n);
	}
	else
	{	icptr += cursor;
		recpos += cursor;
	}
	return(cursor=0);
}

c_li(a) icilist *a;
{
	fmtbuf="int list io";
	sequential = YES;
	formatted = LISTDIRECTED;
	external = NO;
	elist = YES;
	svic = a;
	recnum = recpos = 0;
	cplus = cblank = NO;
	lio_nl = NO;
	icptr = a->iciunit;
	icend = icptr + a->icirlen * a->icirnum;
	errflag = a->icierr;
	endflag = a->iciend;
	leof = NO;
	return(OK);
}
