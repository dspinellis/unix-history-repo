/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)wrtfmt.c	5.1	6/7/85
 */

/*
 * formatted write routines
 */

#include "fio.h"
#include "format.h"

extern char *icvt();
extern char *s_init;

#define abs(x) (x<0?-x:x)

w_ed(p,ptr,len) char *ptr; struct syl *p; ftnlen len;
{	int n;
	if(cursor && (n=wr_mvcur())) return(n);
	switch(p->op)
	{
	case I:
	case IM:
		return(wrt_IM(ptr,p->p1,p->p2,len));
	case L:
		return(wrt_L(ptr,p->p1,len));
	case A:
		return(wrt_AW(ptr,len,len));
	case AW:
		return(wrt_AW(ptr,p->p1,len));
	case D:
		return(wrt_E(ptr,p->p1,p->p2,2,len,'d'));
	case DE:
		return(wrt_E(ptr,p->p1,(p->p2)&0xff,((p->p2)>>8)&0xff,len,'d'));
	case E:
		return(wrt_E(ptr,p->p1,p->p2,2,len,'e'));
	case EE:
		return(wrt_E(ptr,p->p1,(p->p2)&0xff,((p->p2)>>8)&0xff,len,'e'));
	case G:
		return(wrt_G(ptr,p->p1,p->p2,2,len));
	case GE:
		return(wrt_G(ptr,p->p1,(p->p2)&0xff,((p->p2)>>8)&0xff,len));
	case F:
		return(wrt_F(ptr,p->p1,p->p2,len));
	default:
		return(errno=F_ERFMT);
	}
}

w_ned(p,ptr) char *ptr; struct syl *p;
{
	switch(p->op)
	{
	case SLASH:
		return((*donewrec)());
	case T:
		if(p->p1) cursor = p->p1 - recpos - 1;
#ifndef KOSHER
		else cursor = 8*p->p2 - recpos%8;	/* NOT STANDARD FORT */
#endif
		tab = YES;
		return(OK);
	case TL:
		cursor -= p->p1;
		if ((recpos + cursor) < 0) cursor = -recpos;	/* ANSI req'd */
		tab = YES;
		return(OK);
	case TR:
	case X:
		cursor += p->p1;
		/* tab = (p->op == TR); this would implement destructive X */
		tab = YES;
		return(OK);
	case APOS:
		return(wrt_AP(&s_init[p->p1]));
	case H:
		return(wrt_H(p->p1,&s_init[p->p2]));
	default:
		return(errno=F_ERFMT);
	}
}

LOCAL
wr_mvcur()
{	int n;
	if(tab) return((*dotab)());
	if (cursor < 0) return(errno=F_ERSEEK);
	while(cursor--) PUT(' ')
	return(cursor=0);
}

LOCAL
wrt_IM(ui,w,m,len) uint *ui; ftnlen len;
{	int ndigit,sign,spare,i,xsign,n;
	long x;
	char *ans;
	if(sizeof(short)==len) x=ui->is;
/*	else if(len == sizeof(char)) x = ui->ic; */
	else x=ui->il;
	if(x==0 && m==0)
	{	for(i=0;i<w;i++) PUT(' ')
		return(OK);
	}
	ans=icvt(x,&ndigit,&sign);
	if(sign || cplus) xsign=1;
	else xsign=0;
	if(ndigit+xsign>w || m+xsign>w)
	{	for(i=0;i<w;i++) PUT('*')
		return(OK);
	}
	if(ndigit>=m)
		spare=w-ndigit-xsign;
	else
		spare=w-m-xsign;
	for(i=0;i<spare;i++) PUT(' ')
	if(sign) PUT('-')
	else if(cplus) PUT('+')
	for(i=0;i<m-ndigit;i++) PUT('0')
	for(i=0;i<ndigit;i++) PUT(*ans++)
	return(OK);
}

LOCAL
wrt_AP(p)
{	char *s,quote;
	int n;
	if(cursor && (n=wr_mvcur())) return(n);
	s=(char *)p;
	quote = *s++;
	for(; *s; s++)
	{	if(*s!=quote) PUT(*s)
		else if(*++s==quote) PUT(*s)
		else return(OK);
	}
	return(OK);
}

LOCAL
wrt_H(a,b)
{	char *s=(char *)b;
	int n;
	if(cursor && (n=wr_mvcur())) return(n);
	while(a--) PUT(*s++)
	return(OK);
}

wrt_L(l,width,len) uint *l; ftnlen len;
{	int i,n;
	for(i=0;i<width-1;i++) PUT(' ')
	if(len == sizeof (short))
		i = l->is;
	else
		i = l->il;
	if(i) PUT('t')
	else PUT('f')
	return(OK);
}

LOCAL
wrt_AW(p,w,len) char * p; ftnlen len;
{	int n;
	while(w>len)
	{	w--;
		PUT(' ')
	}
	while(w-- > 0)
		PUT(*p++)
	return(OK);
}

wrt_E(p,w,d,e,len,expch) ufloat *p; ftnlen len; char expch;
{	char *s,ex[4];
	int dd,dp,sign,i,delta,pad,n;
	char *ecvt();

	if((len==sizeof(float)?p->pf:p->pd)==0.0)
	{
		n = cblank;
		cblank = 1;	/* force '0' fill */
		wrt_F(p,w-(e+2),d,len);
		cblank = n;
		PUT(expch)
		PUT('+')
/*		for(i=0;i<(e-1);i++)PUT(' ')
deleted		PUT('0')
 */
/* added */	for(i=0;i<e;i++) PUT('0')
		return(OK);
	}
	if (scale > 0) {	/* insane ANSI requirement */
		dd = d + 1;
		d = dd - scale;
	} else
		dd = d + scale;
	if (dd <= 0 || d < 0) goto E_badfield;
	s=ecvt( (len==sizeof(float)?(double)p->pf:p->pd) ,dd,&dp,&sign);
	delta = 3+e;
	if(sign||cplus) delta++;
	pad=w-(delta+d)-(scale>0? scale:0);
	if(pad<0) {
E_badfield:
		for(i=0;i<w;i++) PUT('*')
		return(OK);
	}
	for(i=0;i<(pad-(scale<=0?1:0));i++) PUT(' ')
	if(sign) PUT('-')
	else if(cplus) PUT('+')
	if(scale<=0 && pad) PUT('0')
	if(scale<0 && scale > -d)
	{
		PUT('.')
		for(i=0;i<-scale;i++)
			PUT('0')
		for(i=0;i<d+scale;i++)
			PUT(*s++)
	}
	else
	{
		if(scale>0)
			for(i=0;i<scale;i++)
				PUT(*s++)
		PUT('.')
		for(i=0;i<d;i++)
			PUT(*s++)
	}
	dp -= scale;
	sprintf(ex,"%d",abs(dp));
	if((pad=strlen(ex))>e)
	{	if(pad>(++e))
		{	PUT(expch)
			for(i=0;i<e;i++) PUT('*')
			return(OK);
		}
	}
	else PUT(expch)
	PUT(dp<0?'-':'+')
	for(i=0;i<(e-pad);i++) PUT('0')  /* was ' ' */
	s= &ex[0];
	while(*s) PUT(*s++)
	return(OK);
}

LOCAL
wrt_G(p,w,d,e,len) ufloat *p; ftnlen len;
{	double uplim = 1.0, x;
	int i,oldscale,n,j,ne;
	x=(len==sizeof(float)?(double)p->pf:p->pd);
	i=d;
	if(x==0.0) goto zero;
	x = abs(x);
	if(x>=0.1)
	{
		for(i=0; i<=d; i++, uplim*=10.0)
		{	if(x>=uplim) continue;
zero:			oldscale=scale;
			scale=0;
			ne = e+2;
			if(n = wrt_F(p,w-ne,d-i,len)) return(n);
			for(j=0; j<ne; j++) PUT(' ')
			scale=oldscale;
			return(OK);
		}
		/* falling off the bottom implies E format */
	}
	return(wrt_E(p,w,d,e,len,'e'));
}

wrt_F(p,w,d,len) ufloat *p; ftnlen len;
{	int i,delta,dp,sign,n,nf;
	double x;
	char *s,*fcvt();
	x= (len==sizeof(float)?(double)p->pf:p->pd);
	if(scale && x!=0.0)
	{	if(scale>0)
			for(i=0;i<scale;i++) x*=10;
		else	for(i=0;i<-scale;i++) x/=10;
	}
	s=fcvt(x,d,&dp,&sign);
/*	if(-dp>=d) sign=0; ?? */
	delta=1;
	if(sign || cplus) delta++;
	nf = w - (d + delta + (dp>0?dp:0));
	if(nf<0)
	{
		for(i=0;i<w;i++) PUT('*')
		return(OK);
	}
	if(nf>0) for(i=0; i<(nf-(dp<=0?1:0)); i++) PUT(' ')
	if(sign) PUT('-')
	else if(cplus) PUT('+')
	if(dp>0) for(i=0;i<dp;i++) PUT(*s++)
	else if(nf>0) PUT('0')
	PUT('.')
	for(i=0; i< -dp && i<d; i++) PUT('0')
	for(;i<d;i++)
	{	if(x==0.0 && !cblank) PUT(' ')	/* exactly zero */
		else if(*s) PUT(*s++)
		else PUT('0')
	}
	return(OK);
}
