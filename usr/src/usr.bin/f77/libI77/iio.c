/*
char id_iio[] = "@(#)iio.c	1.1";
 *
 * internal (character array) i/o
 */

#include "fio.h"
#include "lio.h"

extern int rd_ed(),rd_ned(),w_ed(),w_ned();
extern int l_read(),l_write();
int z_wnew(),z_rnew(),z_tab();

z_getc()
{
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
		else err(errflag,110,"iio")
	}
	leof = EOF;
#ifndef KOSHER
	err(endflag,EOF,"iio")   /* NOT STANDARD, end-of-file on writes */
#endif
#ifdef KOSHER
	err(errflag,110,"iio")
#endif
}

z_ungetc(ch,cf) char ch;
{	if(ch==EOF || --recpos >= svic->icirlen) return(OK);
	if(--icptr < svic->iciunit || recpos < 0) err(errflag,107,"ilio")
	*icptr = ch;
	return(OK);
}

s_rsfi(a) icilist *a;
{
	reading = YES;
	doed=rd_ed;
	doned=rd_ned;
	getn=z_getc;
	doend = donewrec = z_rnew;
	dorevert = z_rnew;
	dotab = z_tab;
	return(c_si(a));
}

s_wsfi(a) icilist *a;
{
	reading = NO;
	doed=w_ed;
	doned=w_ned;
	putn=z_putc;
	doend = donewrec = z_wnew;
	dorevert = z_wnew;
	dotab = z_tab;
	return(c_si(a));
}

s_rdfi(a) icilist *a;
{
	reading = YES;
	doed = rd_ed;
	doned = rd_ned;
	getn = z_getc;
	donewrec = z_rnew;
	dorevert = doend = z_rnew;
	dotab = z_tab;
	return(c_di(a));
}

s_wdfi(a) icilist *a;
{
	reading = NO;
	doed = w_ed;
	doned = w_ned;
	putn = z_putc;
	donewrec = z_wnew;
	dorevert = doend = z_wnew;
	dotab = z_tab;
	return(c_di(a));
}

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
	if(pars_f(fmtbuf)) err(errflag,100,"ifio")
	fmt_bg();
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
	if((recpos + cursor) < 0) return(107);
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

e_rsfi()
{	int n;
	n = en_fio();
	fmtbuf = NULL;
	return(n);
}

e_wsfi()
{
	return(e_rsfi());
}

e_rdfi()
{
	return(e_rsfi());
}

e_wdfi()
{
	return(e_wsfi());
}

c_li(a) icilist *a;
{
	fmtbuf="int list io";
	sequential = formatted = LISTDIRECTED;
	external = NO;
	elist = YES;
	svic = a;
	recnum = recpos = 0;
	cplus = cblank = NO;
	icptr = a->iciunit;
	icend = icptr + a->icirlen * a->icirnum;
	errflag = a->icierr;
	endflag = a->iciend;
	leof = NO;
	return(OK);
}

s_rsli(a) icilist *a;
{
	reading = YES;
	lioproc = l_read;
	getn = z_getc;
	ungetn = z_ungetc;
	l_first = YES;
	lcount = 0;
	lquit = NO;
	return(c_li(a));
}

s_wsli(a) icilist *a;
{
	reading = NO;
	putn = z_putc;
	lioproc = l_write;
	line_len = a->icirlen;
	return(c_li(a));
}

e_rsli()
{	fmtbuf = NULL;
	return(OK);
}

e_wsli()
{	fmtbuf = NULL;
	reclen = recpos;
	return(z_wnew());
}

ftnint
iiorec_()
{	return(recnum);	}

ftnint
iiopos_()
{	return(recpos);	}
