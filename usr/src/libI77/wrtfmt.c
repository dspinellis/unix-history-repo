#include "fio.h"
#include "fmt.h"
extern int cursor;
mv_cur()
{	/*buggy, could move off front of record*/
	for(;cursor>0;cursor--) (*putn)(' ');
	if(cursor<0)
	{
		if(cursor+recpos<0) err(elist->cierr,110,"left off");
		if(curunit->useek) fseek(cf,(long)cursor,1);
		else err(elist->cierr,106,"fmt");
		cursor=0;
	}
	return(0);
}
w_ed(p,ptr,len) char *ptr; struct syl *p; ftnlen len;
{
	if(mv_cur()) return(mv_cur());
	switch(p->op)
	{
	default:
		fprintf(stderr,"w_ed, unexpected code: %d\n%s\n",
			p->op,fmtbuf);
		abort();
	case I:	return(wrt_I(ptr,p->p1,len));
	case IM:
		return(wrt_IM(ptr,p->p1,p->p2,len));
	case L:	return(wrt_L(ptr,p->p1));
	case A: return(wrt_A(ptr,len));
	case AW:
		return(wrt_AW(ptr,p->p1,len));
	case D:
	case E:
	case EE:
		return(wrt_E(ptr,p->p1,p->p2,p->p3,len));
	case G:
	case GE:
		return(wrt_G(ptr,p->p1,p->p2,p->p3,len));
	case F:	return(wrt_F(ptr,p->p1,p->p2,len));
	}
}
w_ned(p,ptr) char *ptr; struct syl *p;
{
	switch(p->op)
	{
	default: fprintf(stderr,"w_ned, unexpected code: %d\n%s\n",
			p->op,fmtbuf);
		abort();
	case SLASH:
		return((*donewrec)());
	case T: cursor = p->p1-recpos;
		return(1);
	case TL: cursor -= p->p1;
		return(1);
	case TR:
	case X:
		cursor += p->p1;
		return(1);
	case APOS:
		return(wrt_AP(p->p1));
	case H:
		return(wrt_H(p->p1,p->p2));
	}
}
wrt_I(n,w,len) uint *n; ftnlen len;
{	int ndigit,sign,spare,i;
	long x;
	char *ans;
	if(len==sizeof(short)) x=n->is;
	else if(len == sizeof(char)) x = n->ic;
	else x=n->il;
	ans=icvt(x,&ndigit,&sign);
	spare=w-ndigit;
	if(sign || cplus) spare--;
	if(spare<0)
		for(i=0;i<len;i++) (*putn)('*');
	else
	{	for(i=0;i<spare;i++) (*putn)(' ');
		if(sign) (*putn)('-');
		else if(cplus) (*putn)('+');
		for(i=0;i<ndigit;i++) (*putn)(*ans++);
	}
	return(0);
}
wrt_IM(n,w,m,len) uint *n; ftnlen len;
{	int ndigit,sign,spare,i,xsign;
	long x;
	char *ans;
	if(sizeof(short)==len) x=n->is;
	else if(len == sizeof(char)) x = n->ic;
	else x=n->il;
	ans=icvt(x,&ndigit,&sign);
	if(sign || cplus) xsign=1;
	else xsign=0;
	if(ndigit+xsign>w || m+xsign>w)
	{	for(i=0;i<w;i++) (*putn)('*');
		return(0);
	}
	if(x==0 && m==0)
	{	for(i=0;i<w;i++) (*putn)(' ');
		return(0);
	}
	if(ndigit>=m)
		spare=w-ndigit-xsign;
	else
		spare=w-m-xsign;
	for(i=0;i<spare;i++) (*putn)(' ');
	if(sign) (*putn)('-');
	else if(cplus) (*putn)('+');
	for(i=0;i<m-ndigit;i++) (*putn)('0');
	for(i=0;i<ndigit;i++) (*putn)(*ans++);
	return(0);
}
wrt_AP(n)
{	char *s,quote;
	if(mv_cur()) return(mv_cur());
	s=(char *)n;
	quote = *s++;
	for(;*s;s++)
	{	if(*s!=quote) (*putn)(*s);
		else if(*++s==quote) (*putn)(*s);
		else return(1);
	}
	return(1);
}
wrt_H(a,b)
{	char *s=(char *)b;
	if(mv_cur()) return(mv_cur());
	while(a--) (*putn)(*s++);
	return(1);
}
wrt_L(n,len) ftnint *n;
{	int i;
	for(i=0;i<len-1;i++)
		(*putn)(' ');
	if(*n) (*putn)('t');
	else (*putn)('f');
	return(0);
}
wrt_A(p,len) char *p; ftnlen len;
{
	while(len-- > 0) (*putn)(*p++);
	return(0);
}
wrt_AW(p,w,len) char * p; ftnlen len;
{
	while(w>len)
	{	w--;
		(*putn)(' ');
	}
	while(w-- > 0)
		(*putn)(*p++);
	return(0);
}
wrt_E(p,w,d,e,len) ufloat *p; ftnlen len;
{	char *s;
	int dp,sign,i,delta;
	char *ecvt();
	if(scale>0) d++;
	s=ecvt( (len==sizeof(float)?p->pf:p->pd) ,d,&dp,&sign);
	if(sign || cplus) delta=6;
	else delta=5;
	if(w<delta+d)
	{	for(i=0;i<w;i++) (*putn)('*');
		return(0);
	}
	for(i=0;i<w-(delta+d);i++) (*putn)(' ');
	if(sign) (*putn)('-');
	else if(cplus) (*putn)('+');
	if(scale<0 && scale > -d)
	{
		(*putn)('.');
		for(i=0;i<-scale;i++)
			(*putn)('0');
		for(i=0;i<d+scale;i++)
			(*putn)(*s++);
	}
	else if(scale>0 && scale<d+2)
	{	for(i=0;i<scale;i++)
			(*putn)(*s++);
		(*putn)('.');
		for(i=0;i<d-scale;i++)
			(*putn)(*s++);
	}
	else
	{	(*putn)('.');
		for(i=0;i<d;i++) (*putn)(*s++);
	}
	if(p->pf != 0) dp -= scale;
	else	dp = 0;
	if(dp < 100 && dp > -100) (*putn)('e');
	if(dp<0)
	{	(*putn)('-');
		dp = -dp;
	}
	else	(*putn)('+');
	if(e>=3 || dp >= 100)
	{	(*putn)(dp/100 + '0');
		dp = dp % 100;
	}
	if(e!=1) (*putn)(dp/10+'0');
	(*putn)(dp%10+'0');
	return(0);
}
wrt_G(p,w,d,e,len) ufloat *p; ftnlen len;
{	double up = 1,x;
	int i,oldscale=scale,n,j;
	x= len==sizeof(float)?p->pf:p->pd;
	if(x < 0 ) x = -x;
	if(x<.1) return(wrt_E(p,w,d,e,len));
	for(i=0;i<=d;i++,up*=10)
	{	if(x>up) continue;
		scale=0;
		if(e==0) n=4;
		else	n=e+2;
		i=wrt_F(p,w-n,d-i,len);
		for(j=0;j<n;j++) (*putn)(' ');
		scale=oldscale;
		return(i);
	}
	return(wrt_E(p,w,d,e,len));
}
wrt_F(p,w,d,len) ufloat *p; ftnlen len;
{	int i,delta,dp,sign,n;
	double x;
	char *s,*fcvt();
	x= (len==sizeof(float)?p->pf:p->pd);
	if(scale)
	{	if(scale>0)
			for(i=0;i<scale;i++) x*=10;
		else	for(i=0;i<-scale;i++) x/=10;
	}
	s=fcvt(x,d,&dp,&sign);
	if(-dp>=d) sign=0;
	if(sign || cplus) delta=2;
	else delta=1;
	n= w - (d+delta+(dp>0?dp:0));
	if(n<0)
	{
		for(i=0;i<w;i++) PUT('*');
		return(0);
	}
	for(i=0;i<n;i++) PUT(' ');
	if(sign) PUT('-');
	else if(cplus) PUT('+');
	for(i=0;i<dp;i++) PUT(*s++);
	PUT('.');
	for(i=0;i< -dp && i<d;i++) PUT('0');
	for(;i<d;i++)
	{	if(*s) PUT(*s++);
		else PUT('0');
	}
	return(0);
}
