#ifndef lint
static char sccsid[] = "@(#)mout.c	5.3 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <mp.h>
m_in(a,b,f) MINT *a; FILE *f;
{	MINT x,y,ten;
	int sign,c;
	short qten,qy;
	xfree(a);
	sign=1;
	ten.len=1;
	ten.val= &qten;
	qten=b;
	x.len=0;
	y.len=1;
	y.val= &qy;
	while((c=getc(f))!=EOF)
	switch(c)
	{
	case '\\':	(void)getc(f);
		continue;
	case '\t':
	case '\n': a->len *= sign;
		xfree(&x);
		return(0);
	case ' ':
		continue;
	case '-': sign = -sign;
		continue;
	default: if(c>='0' && c<= '9')
		{	qy=c-'0';
			mult(&x,&ten,a);
			madd(a,&y,a);
			move(a,&x);
			continue;
		}
		else
		{	VOID ungetc(c,stdin);
			a->len *= sign;
			return(0);
		}
	}
	return(EOF);
}
m_out(a,b,f) MINT *a; FILE *f;
{	int sign,xlen,i;
	short r;
	MINT x;
	char *obuf, *malloc();
	register char *bp;
	sign=1;
	xlen=a->len;
	if(xlen<0)
	{	xlen= -xlen;
		sign= -1;
	}
	if(xlen==0)
	{	fprintf(f,"0\n");
		return;
	}
	x.len=xlen;
	x.val=xalloc(xlen,"m_out");
	for(i=0;i<xlen;i++) x.val[i]=a->val[i];
	obuf=malloc(7*(unsigned)xlen);
	bp=obuf+7*xlen-1;
	*bp--=0;
	while(x.len>0)
	{	for(i=0;i<10&&x.len>0;i++)
		{	sdiv(&x,(short)b,&x,&r);
			*bp--=r+'0';
		}
		if(x.len>0) *bp--=' ';
	}
	if(sign==-1) *bp--='-';
	fprintf(f,"%s\n",bp+1);
	free(obuf);
	FREE(x)
	return;
}
sdiv(a,n,q,r) MINT *a,*q; short n; short *r;
{	MINT x,y;
	int sign;
	sign=1;
	x.len=a->len;
	x.val=a->val;
	if(n<0)
	{	sign= -sign;
		n= -n;
	}
	if(x.len<0)
	{	sign = -sign;
		x.len= -x.len;
	}
	s_div(&x,n,&y,r);
	xfree(q);
	q->val=y.val;
	q->len=sign*y.len;
	*r = *r*sign;
	return;
}
s_div(a,n,q,r) MINT *a,*q; short n; short *r;
{	int qlen,i;
	long int x;
	short *qval;
	x=0;
	qlen=a->len;
	qval=xalloc(qlen,"s_div");
	for(i=qlen-1;i>=0;i--)
	{
		x=x*0100000L+a->val[i];
		qval[i]=x/n;
		x=x%n;
	}
	*r=x;
	if(qlen && qval[qlen-1]==0) qlen--;
	q->len=qlen;
	q->val=qval;
	if(qlen==0) shfree(qval);
	return;
}
min(a) MINT *a;
{
	return(m_in(a,10,stdin));
}
omin(a) MINT *a;
{
	return(m_in(a,8,stdin));
}
mout(a) MINT *a;
{
	m_out(a,10,stdout);
}
omout(a) MINT *a;
{
	m_out(a,8,stdout);
}
fmout(a,f) MINT *a; FILE *f;
{	m_out(a,10,f);
}
fmin(a,f) MINT *a; FILE *f;
{
	return(m_in(a,10,f));
}
