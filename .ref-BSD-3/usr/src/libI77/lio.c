#include "fio.h"
#include "lio.h"
extern int l_write();
int t_putc();
s_wsle(a) cilist *a;
{
	int n;
	if(!init) f_init();
	if(n=c_le(a,WRITE)) return(n);
	reading=0;
	external=1;
	formatted=1;
	putn = t_putc;
	lioproc = l_write;
	if(!curunit->uwrt)
		return(nowwriting(curunit));
	else	return(0);
}
e_wsle()
{
	t_putc('\n');
	recpos=0;
	return(0);
}
t_putc(c)
{
	recpos++;
	putc(c,cf);
}
lwrt_I(n) ftnint n;
{
	char buf[LINTW],*p;
	sprintf(buf," %ld",(long)n);
	if(recpos+strlen(buf)>=LINE)
	{	t_putc('\n');
		recpos=0;
	}
	for(p=buf;*p;t_putc(*p++));
}
lwrt_L(n) ftnint n;
{
	if(recpos+LLOGW>=LINE)
	{	t_putc('\n');
		recpos=0;
	}
	wrt_L(&n,LLOGW);
}
lwrt_A(p,len) char *p; ftnlen len;
{
	int i;
	if(recpos+len>=LINE)
	{
		t_putc('\n');
		recpos=0;
	}
	t_putc(' ');
	for(i=0;i<len;i++) t_putc(*p++);
}
lwrt_F(n) double n;
{
	if(LLOW<=n && n<LHIGH)
	{
		if(recpos+LFW>=LINE)
		{
			t_putc('\n');
			recpos=0;
		}
		scale=0;
		wrt_F(&n,LFW,LFD,(ftnlen)sizeof(n));
	}
	else
	{
		if(recpos+LEW>=LINE)
		{	t_putc('\n');
			recpos=0;
		}
		wrt_E(&n,LEW,LED,LEE,(ftnlen)sizeof(n));
	}
}
lwrt_C(a,b) double a,b;
{
	if(recpos+2*LFW+3>=LINE)
	{	t_putc('\n');
		recpos=0;
	}
	t_putc(' ');
	t_putc('(');
	lwrt_F(a);
	t_putc(',');
	lwrt_F(b);
	t_putc(')');
}
l_write(number,ptr,len,type) ftnint *number,type; flex *ptr; ftnlen len;
{
	int i;
	ftnint x;
	double y,z;
	float *xx;
	double *yy;
	for(i=0;i< *number; i++)
	{
		switch((int)type)
		{
		default: fatal(204,"unknown type in lio");
		case TYSHORT: x=ptr->flshort;
			goto xint;
		case TYLONG: x=ptr->flint;
		xint: lwrt_I(x);
			break;
		case TYREAL: y=ptr->flreal;
			goto xfloat;
		case TYDREAL: y=ptr->fldouble;
		xfloat: lwrt_F(y);
			break;
		case TYCOMPLEX: xx= &(ptr->flreal);
			y = *xx++;
			z = *xx;
			goto xcomplex;
		case TYDCOMPLEX: yy = &(ptr->fldouble);
			y= *yy++;
			z = *yy;
		xcomplex: lwrt_C(y,z);
			break;
		case TYLOGICAL: lwrt_L(ptr->flint);
			break;
		case TYCHAR: lwrt_A((char *)ptr,len);
			break;
		}
		ptr = (char *)ptr + len;
	}
	return(0);
}
