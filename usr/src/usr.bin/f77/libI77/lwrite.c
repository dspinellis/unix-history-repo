/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lwrite.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * list directed write
 */

#include "fio.h"
#include "lio.h"

int l_write(), t_putc();
LOCAL char lwrt[] = "list write";

s_wsle(a) cilist *a;
{
	int n;
	reading = NO;
	formatted = LISTDIRECTED;
	fmtbuf = "ext list io";
	if(n=c_le(a,WRITE)) return(n);
	putn = t_putc;
	lioproc = l_write;
	line_len = LINE;
	curunit->uend = NO;
	leof = NO;
	if(!curunit->uwrt && ! nowwriting(curunit)) err(errflag, errno, lwrt)
	return(OK);
}

LOCAL
t_putc(c) char c;
{
	if(c=='\n') recpos=0;
	else recpos++;
	putc(c,cf);
	return(OK);
}

e_wsle()
{	int n;
	PUT('\n')
	return(OK);
}

l_write(number,ptr,len,type) ftnint *number,type; flex *ptr; ftnlen len;
{
	int i,n;
	ftnint x;
	float y,z;
	double yd,zd;
	float *xx;
	double *yy;
	for(i=0;i< *number; i++)
	{
		if( formatted == NAMELIST && i != 0 ) PUT(',');
		switch((int)type)
		{
		case TYSHORT:
			x=ptr->flshort;
			goto xint;
		case TYLONG:
			x=ptr->flint;
	xint:		ERRCHK(lwrt_I(x));
			break;
		case TYREAL:
			ERRCHK(lwrt_F(ptr->flreal));
			break;
		case TYDREAL:
			ERRCHK(lwrt_D(ptr->fldouble));
			break;
		case TYCOMPLEX:
			xx= &(ptr->flreal);
			y = *xx++;
			z = *xx;
			ERRCHK(lwrt_C(y,z));
			break;
		case TYDCOMPLEX:
			yy = &(ptr->fldouble);
			yd= *yy++;
			zd = *yy;
			ERRCHK(lwrt_DC(yd,zd));
			break;
		case TYLOGICAL:
			if(len == sizeof(short))
				x = ptr->flshort;
			else
				x = ptr->flint;
			ERRCHK(lwrt_L(x));
			break;
		case TYCHAR:
			ERRCHK(lwrt_A((char *)ptr,len));
			break;
		default:
			fatal(F_ERSYS,"unknown type in lwrite");
		}
		ptr = (flex *)((char *)ptr + len);
	}
	return(OK);

got_err:
	err( n>0?errflag:endflag,  n,
		formatted==LISTDIRECTED?"list io":"name list io");
}

LOCAL
lwrt_I(in) ftnint in;
{	int n;
	char buf[16],*p;
	sprintf(buf,"  %ld",(long)in);
	chk_len(LINTW);
	for(p=buf;*p;) PUT(*p++)
	return(OK);
}

LOCAL
lwrt_L(ln) ftnint ln;
{	int n;
	chk_len(LLOGW);
	return(wrt_L(&ln,LLOGW));
}

LOCAL
lwrt_A(p,len) char *p; ftnlen len;
{	int i,n;
	if(formatted == LISTDIRECTED)
	{
		chk_len(len);
		for(i=0;i<len;i++) PUT(*p++)
	}
	else
	{
		chk_len(len+2);
		PUT('\'')
		for(i=0;i<len;i++) PUT(*p++)
		PUT('\'')
	}
	return(OK);
}

LOCAL
lwrt_F(fn) float fn;
{	int d,n; float x; ufloat f;
	if(fn==0.0) return(lwrt_0());
	f.pf = fn;
	d = width(fn);
	chk_len(d);
	if(d==LFW)
	{
		scale = 0;
		for(d=LFD,x=abs(fn);x>=1.0;x/=10.0,d--);
		return(wrt_F(&f,LFW,d,(ftnlen)sizeof(float)));
	}
	else
	{
		scale = 1;
		return(wrt_E(&f,LEW,LED-scale,LEE,(ftnlen)sizeof(float),'e'));
	}
}

LOCAL
lwrt_D(dn) double dn;
{	int d,n; double x; ufloat f;
	if(dn==0.0) return(lwrt_0());
	f.pd = dn;
	d = dwidth(dn);
	chk_len(d);
	if(d==LDFW)
	{
		scale = 0;
		for(d=LDFD,x=abs(dn);x>=1.0;x/=10.0,d--);
		return(wrt_F(&f,LDFW,d,(ftnlen)sizeof(double)));
	}
	else
	{
		scale = 1;
		return(wrt_E(&f,LDEW,LDED-scale,LDEE,(ftnlen)sizeof(double),'d'));
	}
}

LOCAL
lwrt_C(a,b) float a,b;
{	int n;
	chk_len(LCW);
	PUT(' ')
	PUT(' ')
	PUT('(')
	if(n=lwrt_F(a)) return(n);
	PUT(',')
	if(n=lwrt_F(b)) return(n);
	PUT(')')
	return(OK);
}

LOCAL
lwrt_DC(a,b) double a,b;
{	int n;
	chk_len(LDCW);
	PUT(' ')
	PUT(' ')
	PUT('(')
	if(n=lwrt_D(a)) return(n);
	PUT(',')
	if(n=lwrt_D(b)) return(n);
	PUT(')')
	return(OK);
}

LOCAL
lwrt_0()
{	int n; char *z = "  0.";
	chk_len(4);
	while(*z) PUT(*z++)
	return(OK);
}
