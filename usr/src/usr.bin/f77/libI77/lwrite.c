/*
char id_lwrite[] = "@(#)lwrite.c	1.1";
 *
 * list directed write
 */

#include "fio.h"
#include "lio.h"

int l_write(), t_putc();

s_wsle(a) cilist *a;
{
	int n;
	reading = NO;
	if(n=c_le(a,WRITE)) return(n);
	putn = t_putc;
	lioproc = l_write;
	line_len = LINE;
	curunit->uend = NO;
	leof = NO;
	if(!curunit->uwrt) nowwriting(curunit);
	return(OK);
}

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
		switch((int)type)
		{
		case TYSHORT:
			x=ptr->flshort;
			goto xint;
		case TYLONG:
			x=ptr->flint;
	xint:		ERR(lwrt_I(x));
			break;
		case TYREAL:
			ERR(lwrt_F(ptr->flreal));
			break;
		case TYDREAL:
			ERR(lwrt_D(ptr->fldouble));
			break;
		case TYCOMPLEX:
			xx= &(ptr->flreal);
			y = *xx++;
			z = *xx;
			ERR(lwrt_C(y,z));
			break;
		case TYDCOMPLEX:
			yy = &(ptr->fldouble);
			yd= *yy++;
			zd = *yy;
			ERR(lwrt_DC(yd,zd));
			break;
		case TYLOGICAL:
			ERR(lwrt_L(ptr->flint));
			break;
		case TYCHAR:
			ERR(lwrt_A((char *)ptr,len));
			break;
		default:
			fatal(119,"unknown type in lwrite");
		}
		ptr = (flex *)((char *)ptr + len);
	}
	return(OK);
}

lwrt_I(in) ftnint in;
{	int n;
	char buf[16],*p;
	sprintf(buf,"  %ld",(long)in);
	if(n=chk_len(LINTW)) return(n);
	for(p=buf;*p;) PUT(*p++)
	return(OK);
}

lwrt_L(ln) ftnint ln;
{	int n;
	if(n=chk_len(LLOGW)) return(n);
	return(wrt_L(&ln,LLOGW));
}

lwrt_A(p,len) char *p; ftnlen len;
{	int i,n;
	if(n=chk_len(LSTRW)) return(n);
	PUT(' ')
	PUT(' ')
	for(i=0;i<len;i++) PUT(*p++)
	return(OK);
}

lwrt_F(fn) float fn;
{	int d,n; float x; ufloat f;
	if(fn==0.0) return(lwrt_0());
	f.pf = fn;
	d = width(fn);
	if(n=chk_len(d)) return(n);
	if(d==LFW)
	{
		scale = 0;
		for(d=LFD,x=abs(fn);x>=1.0;x/=10.0,d--);
		return(wrt_F(&f,LFW,d,(ftnlen)sizeof(float)));
	}
	else
	{
		scale = 1;
		return(wrt_E(&f,LEW,LED-scale,LEE,(ftnlen)sizeof(float)));
	}
}

lwrt_D(dn) double dn;
{	int d,n; double x; ufloat f;
	if(dn==0.0) return(lwrt_0());
	f.pd = dn;
	d = dwidth(dn);
	if(n=chk_len(d)) return(n);
	if(d==LDFW)
	{
		scale = 0;
		for(d=LDFD,x=abs(dn);x>=1.0;x/=10.0,d--);
		return(wrt_F(&f,LDFW,d,(ftnlen)sizeof(double)));
	}
	else
	{
		scale = 1;
		return(wrt_E(&f,LDEW,LDED-scale,LDEE,(ftnlen)sizeof(double)));
	}
}

lwrt_C(a,b) float a,b;
{	int n;
	if(n=chk_len(LCW)) return(n);
	PUT(' ')
	PUT(' ')
	PUT('(')
	if(n=lwrt_F(a)) return(n);
	PUT(',')
	if(n=lwrt_F(b)) return(n);
	PUT(')')
	return(OK);
}

lwrt_DC(a,b) double a,b;
{	int n;
	if(n=chk_len(LDCW)) return(n);
	PUT(' ')
	PUT(' ')
	PUT('(')
	if(n=lwrt_D(a)) return(n);
	PUT(',')
	if(n=lwrt_D(b)) return(n);
	PUT(')')
	return(OK);
}

lwrt_0()
{	int n; char *z = "  0.";
	if(n=chk_len(4)) return(n);
	while(*z) PUT(*z++)
	return(OK);
}

chk_len(w)
{	int n;
	if(recpos+w > line_len) PUT('\n')
	return(OK);
}
