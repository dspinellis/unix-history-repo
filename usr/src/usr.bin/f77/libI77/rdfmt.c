/*
char id_rdfmt[] = "@(#)rdfmt.c	1.1";
 *
 * formatted read routines
 */

#include "fio.h"
#include "fmt.h"

#define isdigit(c)	(c>='0' && c<='9')
#define isalpha(c)	(c>='a' && c<='z')

rd_ed(p,ptr,len) char *ptr; struct syl *p; ftnlen len;
{	int n;
	if(cursor && (n=rd_mvcur())) return(n);
	switch(p->op)
	{
	case I:
	case IM:
		n = (rd_I(ptr,p->p1,len));
		break;
	case L:
		n = (rd_L(ptr,p->p1));
		break;
	case A:
		p->p1 = len;	/* cheap trick */
	case AW:
		n = (rd_AW(ptr,p->p1,len));
		break;
	case E:
	case EE:
	case D:
	case DE:
	case G:
	case GE:
	case F:
		n = (rd_F(ptr,p->p1,p->p2,len));
		break;
	default:
		return(errno=100);
	}
	if (n < 0)
	{
		if(feof(cf)) return(EOF);
		n = errno;
		clearerr(cf);
	}
	return(n);
}

rd_ned(p,ptr) char *ptr; struct syl *p;
{
	switch(p->op)
	{
/*	case APOS:
/*		return(rd_POS(p->p1));
/*	case H:
/*		return(rd_H(p->p1,p->p2));	*/
	case SLASH:
		return((*donewrec)());
	case TR:
	case X:
		cursor += p->p1;
		tab = (p->op==TR);
		return(OK);
	case T:
		if(p->p1) cursor = p->p1 - recpos - 1;
#ifndef KOSHER
		else cursor = 8*p->p2 - recpos%8;	/* NOT STANDARD FORT */
#endif
		tab = YES;
		return(OK);
	case TL:
		cursor -= p->p1;
		tab = YES;
		return(OK);
	default:
		return(errno=100);
	}
}

rd_mvcur()
{	int n;
	if(tab) return((*dotab)());
	while(cursor--) if((n=(*getn)()) < 0) return(n);
	return(cursor=0);
}

rd_I(n,w,len) ftnlen len; uint *n;
{	long x=0;
	int i,sign=0,ch,c;
	for(i=0;i<w;i++)
	{
		if((ch=(*getn)())<0) return(ch);
		switch(ch=lcase(ch))
		{
		case ',': goto done;
		case '+': break;
		case '-':
			sign=1;
			break;
		case ' ':
			if(cblank) x *= radix;
			break;
		case '\n':  goto done;
		default:
			if(isdigit(ch))
			{	if ((c=(ch-'0')) < radix)
				{	x = (x * radix) + c;
					break;
				}
			}
			else if(isalpha(ch))
			{	if ((c=(ch-'a'+10)) < radix)
				{	x = (x * radix) + c;
					break;
				}
			}
			return(errno=115);
		}
	}
done:
	if(sign) x = -x;
	if(len==sizeof(short)) n->is=x;
	else n->il=x;
	return(OK);
}

rd_L(n,w) ftnint *n;
{	int ch,i,v = -1;
	for(i=0;i<w;i++)
	{	if((ch=(*getn)()) < 0) return(ch);
		if((ch=lcase(ch))=='t' && v==-1) v=1;
		else if(ch=='f' && v==-1) v=0;
		else if(ch==',') break;
	}
	if(v==-1) return(errno=116);
	*n=v;
	return(OK);
}

rd_F(p,w,d,len) ftnlen len; ufloat *p;
{	double x,y;
	int i,sx,sz,ch,dot,ny,z,sawz;
	x=y=0;
	sawz=z=ny=dot=sx=sz=0;
	for(i=0;i<w;)
	{	i++;
		if((ch=(*getn)())<0) return(ch);
		ch=lcase(ch);
		if(ch==' ' && !cblank || ch=='+') continue;
		else if(ch=='-') sx=1;
		else if(ch<='9' && ch>='0')
			x=10*x+ch-'0';
		else if(ch=='e' || ch=='d' || ch=='.')
			break;
		else if(cblank && ch==' ') x*=10;
		else if(ch==',')
		{	i=w;
			break;
		}
		else if(ch!='\n') return(errno=115);
	}
	if(ch=='.') dot=1;
	while(i<w && ch!='e' && ch!='d' && ch!='+' && ch!='-')
	{	i++;
		if((ch=(*getn)())<0) return(ch);
		ch = lcase(ch);
		if(ch<='9' && ch>='0')
			y=10*y+ch-'0';
		else if(cblank && ch==' ')
			y *= 10;
		else if(ch==',') {i=w; break;}
		else if(ch==' ') continue;
		else continue;
		ny++;
	}
	if(ch=='-') sz=1;
	while(i<w)
	{	i++;
		sawz=1;
		if((ch=(*getn)())<0) return(ch);
		ch = lcase(ch);
		if(ch=='-') sz=1;
		else if(ch<='9' && ch>='0')
			z=10*z+ch-'0';
		else if(cblank && ch==' ')
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
	return(OK);
}

rd_AW(p,w,len) char *p; ftnlen len;
{	int i,ch;
	if(w >= len)
	{
		for(i=0;i<w-len;i++) GET(ch);
		for(i=0;i<len;i++)
		{	GET(ch);
			*p++=VAL(ch);
		}
	}
	else
	{
		for(i=0;i<w;i++)
		{	GET(ch);
			*p++=VAL(ch);
		}
		for(i=0;i<len-w;i++) *p++=' ';
	}
	return(OK);
}

/* THIS IS NOT ALLOWED IN THE NEW STANDARD 'CAUSE IT'S WEIRD */
/*rd_H(n,s) char *s;
/*{	int i,ch;
/*	for(i=0;i<n;i++)
/*		if((ch=(*getn)())<0) return(ch);
/*		else if(ch=='\n') for(;i<n;i++) *s++ = ' ';
/*		else *s++ = ch;
/*	return(OK);
/*}
*/
/*rd_POS(s) char *s;
/*{	char quote;
/*	int ch;
/*	quote= *s++;
/*	for(;*s;s++)
/*		if(*s==quote && *(s+1)!=quote) break;
/*		else if((ch=(*getn)())<0) return(ch);
/*		else *s = ch=='\n'?' ':ch;
/*	return(OK);
/*}
*/
