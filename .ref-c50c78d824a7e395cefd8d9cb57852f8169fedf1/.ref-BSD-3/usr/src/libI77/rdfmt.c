#include "fio.h"
#include "fmt.h"
extern int cursor;
rd_ed(p,ptr,len) char *ptr; struct syl *p; ftnlen len;
{	int ch;
	for(;cursor>0;cursor--) if((ch=(*getn)())<0) return(ch);
	if(cursor<0)
	{	if(recpos+cursor < 0) err(elist->cierr,110,"fmt")
		if(curunit->useek) fseek(cf,(long) cursor,1);
		else err(elist->cierr,106,"fmt");
		cursor=0;
	}
	switch(p->op)
	{
	default: fprintf(stderr,"rd_ed, unexpected code: %d\n%s\n",
			p->op,fmtbuf);
		abort();
	case I: ch = (rd_I(ptr,p->p1,len, 10));
		break;
	case IM: ch = (rd_I(ptr,p->p1,len, 10));
		break;
	case O: ch = (rd_I(ptr, p->p1, len, 8));
		break;
	case L: ch = (rd_L(ptr,p->p1));
		break;
	case A:	ch = (rd_A(ptr,len));
		break;
	case AW:
		ch = (rd_AW(ptr,p->p1,len));
		break;
	case E: case EE:
	case D:
	case G:
	case GE:
	case F:	ch = (rd_F(ptr,p->p1,p->p2,len));
		break;
	}
	if(ch == 0) return(ch);
	else if(feof(cf)) return(EOF);
	clearerr(cf);
	return(errno);
}
rd_ned(p,ptr) char *ptr; struct syl *p;
{
	switch(p->op)
	{
	default: fprintf(stderr,"rd_ned, unexpected code: %d\n%s\n",
			p->op,fmtbuf);
		abort();
	case APOS:
		return(rd_POS(p->p1));
	case H:	return(rd_H(p->p1,p->p2));
	case SLASH: return((*donewrec)());
	case TR:
	case X:	cursor += p->p1;
		return(1);
	case T: cursor=p->p1-recpos - 1;
		return(1);
	case TL: cursor -= p->p1;
		return(1);
	}
}
rd_I(n,w,len, base) ftnlen len; uint *n; register int base;
{	long x=0;
	int i,sign=0,ch;
	for(i=0;i<w;i++)
	{
		if((ch=(*getn)())<0) return(ch);
		switch(ch)
		{
		default:
			return(errno=115);
		case ',': goto done;
		case '+': break;
		case '-':
			sign=1;
			break;
		case '\n':
		case ' ':
			if(cblank) x *= base;
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			x=base*x+ch-'0';
			break;
		}
	}
done:
	if(sign) x = -x;
	if(len==sizeof(short)) n->is=x;
	else if(len == sizeof(char)) n->ic = x;
	else n->il=x;
	return(0);
}
rd_L(n,w) ftnint *n;
{	int ch,i,v = -1;
	for(i=0;i<w;i++)
	{	if((ch=(*getn)())<0) return(ch);
		if(ch=='t' && v==-1) v=1;
		else if(ch=='f' && v==-1) v=0;
		else if(ch==',') return(0);
	}
	if(v==-1)
	{	errno=116;
		return(1);
	}
	*n=v;
	return(0);
}
rd_F(p,w,d,len) ftnlen len; ufloat *p;
{	double x,y;
	int i,sx,sz,ch,dot,ny,z,sawz;
	x=y=0;
	sawz=z=ny=dot=sx=sz=0;
	for(i=0;i<w;)
	{	i++;
		if((ch=(*getn)())<0) return(ch);
		else if(ch == ' ' && !cblank || ch == '+' && x == 0
			|| ch == '\n' && !cblank) continue;
		else if(ch=='-' && x==0) sx=1;
		else if(ch == '+' || ch == '-') goto expon;
		else if(ch<='9' && ch>='0')
			x=10*x+ch-'0';
		else if(ch=='e' || ch=='d' || ch=='.')
			break;
		else if(cblank && (ch==' ' || ch== '\n')) x*=10;
		else if(ch==',')
		{	i=w;
			break;
		}
		else return(errno = 115);
	}
	if(ch=='.') dot=1;
	while(i<w && ch!='e' && ch!='d' && ch!='+' && ch!='-')
	{	i++;
		if((ch=(*getn)())<0) return(ch);
		else if(ch<='9' && ch>='0')
			y=10*y+ch-'0';
		else if(cblank && (ch==' ' || ch == '\n'))
			y *= 10;
		else if(ch==',') {i=w; break;}
		else if(ch==' ') continue;
		else continue;
		ny++;
	}
expon:
	if(ch=='-') sz=1;
	while(i<w)
	{	i++;
		sawz=1;
		if((ch=(*getn)())<0) return(ch);
		else if(ch=='-') sz=1;
		else if(ch<='9' && ch>='0')
			z=10*z+ch-'0';
		else if(cblank && (ch==' ' || ch == '\n'))
			z *= 10;
		else if(ch==',') break;
		else if(ch==' ') continue;
		else if(ch=='+') continue;
		else if(ch!='\n') return(errno=115);
	}
	if(!dot)
		for(i=0;i<d;i++) x /= 10;
	for(i=0;i<ny;i++) y /= 10;
	x=x+y;
	if(sz)
		for(i=0;i<z;i++) x /=10;
	else	for(i=0;i<z;i++) x *= 10;
	if(sx) x = -x;
	if(!sawz)
	{
		for(i=scale;i>0;i--) x /= 10;
		for(i=scale;i<0;i++) x *= 10;
	}
	if(len==sizeof(float)) p->pf=x;
	else p->pd=x;
	return(0);
}
rd_A(p,len) char *p; ftnlen len;
{	int i,ch;
	for(i=0;i<len;i++)
	{	GET(ch);
		*p++=VAL(ch);
	}
	return(0);
}
rd_AW(p,w,len) char *p; ftnlen len;
{	int i,ch;
	if(w>=len)
	{	for(i=0;i<w-len;i++)
			GET(ch);
		for(i=0;i<len;i++)
		{	GET(ch);
			*p++=VAL(ch);
		}
		return(0);
	}
	for(i=0;i<w;i++)
	{	GET(ch);
		*p++=VAL(ch);
	}
	for(i=0;i<len-w;i++) *p++=' ';
	return(0);
}
rd_H(n,s) char *s;
{	int i,ch;
	for(i=0;i<n;i++)
		if((ch=(*getn)())<0) return(ch);
		else *s++ = ch=='\n'?' ':ch;
	return(1);
}
rd_POS(s) char *s;
{	char quote;
	int ch;
	quote= *s++;
	for(;*s;s++)
		if(*s==quote && *(s+1)!=quote) break;
		else if((ch=(*getn)())<0) return(ch);
		else *s = ch=='\n'?' ':ch;
	return(1);
}
