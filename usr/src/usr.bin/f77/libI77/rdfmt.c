/*
char id_rdfmt[] = "@(#)rdfmt.c	1.11";
 *
 * formatted read routines
 */

#include "fio.h"
#include "format.h"

extern char *s_init;
extern int low_case[256];
extern int used_data;

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
		n = (rd_L(ptr,p->p1,len));
		break;
	case A:
		n = (rd_AW(ptr,len,len));
		break;
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
		return(errno=F_ERFMT);
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
#ifndef	KOSHER
	case APOS:					/* NOT STANDARD F77 */
		return(rd_POS(&s_init[p->p1]));
	case H:						/* NOT STANDARD F77 */
		return(rd_H(p->p1,&s_init[p->p2]));
#endif
	case SLASH:
		return((*donewrec)());
	case TR:
	case X:
		cursor += p->p1;
		/* tab = (p->op==TR); This voids '..,tl6,1x,..' sequences */
		tab = YES;
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
		if ((recpos + cursor) < 0) cursor = -recpos;	/* ANSI req'd */
		tab = YES;
		return(OK);
	default:
		return(errno=F_ERFMT);
	}
}

LOCAL
rd_mvcur()
{	int n;
	if(tab) return((*dotab)());
	if (cursor < 0) return(errno=F_ERSEEK);
	while(cursor--) if((n=(*getn)()) < 0) return(n);
	return(cursor=0);
}

LOCAL
rd_I(n,w,len) ftnlen len; uint *n;
{	long x=0;
	int i,sign=0,ch,c,sign_ok=YES;
	for(i=0;i<w;i++)
	{
		if((ch=(*getn)())<0) return(ch);
		switch(ch)
		{
		case ',': goto done;
		case '-': sign=1;		/* and fall thru */
		case '+': if(sign_ok == NO) return(errno=F_ERRICHR);
			  sign_ok = NO;
			  break;
		case ' ':
			if(cblank) x *= radix;
			break;
		case '\n':  if(cblank) {
				x *= radix;
				break;
			    } else {
				goto done;
			    }
		default:
			sign_ok = NO;
			if( (c = ch-'0')>=0 && c<radix )
			{	x = (x * radix) + c;
				break;
			}
			else if( (c = low_case[ch]-'a'+10)>=0 && c<radix )
			{	x = (x * radix) + c;
				break;
			}
			return(errno=F_ERRICHR);
		}
	}
done:
	if(sign) x = -x;
	if(len==sizeof(short)) n->is=x;
	else n->il=x;
	return(OK);
}

LOCAL
rd_L(n,w,len) uint *n; ftnlen len;
{	int ch,i,v = -1;
	for(i=0;i<w;i++)
	{	if((ch=(*getn)()) < 0) return(ch);
		if((ch=low_case[ch])=='t' && v==-1) v=1;
		else if(ch=='f' && v==-1) v=0;
		else if(ch==',') break;
	}
	if(v==-1) return(errno=F_ERLOGIF);
	if(len==sizeof(short)) n->is=v;
	else n->il=v;
	return(OK);
}

LOCAL
rd_F(p,w,d,len) ftnlen len; ufloat *p;
{	double x,y;
	int i,sx,sz,ch,dot,ny,z,sawz,mode, sign_ok=YES;
	x=y=0;
	sawz=z=ny=dot=sx=sz=0;
	/* modes:	0 in initial blanks,
			2 blanks plus sign
			3 found a digit
	 */
	mode = 0;

	for(i=0;i<w;)
	{	i++;
		if((ch=(*getn)())<0) return(ch);

		if(ch==' ') {	/* blank */
			if(cblank && (mode==2)) x *= 10;
		} else if(ch<='9' && ch>='0') { /* digit */
			mode = 2;
			x=10*x+ch-'0';
		} else if(ch=='.') {
			break;
		} else if(ch=='e' || ch=='d' || ch=='E' || ch=='D') {
			goto exponent;
		} else if(ch=='+' || ch=='-') {
			if(mode==0) {  /* sign before digits */
				if(ch=='-') sx=1;
				mode = 1;
			} else if(mode==1) {  /* two signs before digits */
				return(errno=F_ERRFCHR);
			} else { /* sign after digits, weird but standard!
				    	means exponent without 'e' or 'd' */
				    goto exponent;
			}
		} else if(ch==',') {
			goto done;
		} else if(ch=='\n') {
			if(cblank && (mode==2)) x *= 10;
		} else {
			return(errno=F_ERRFCHR);
		}
	}
	/* get here if out of characters to scan or found a period */
	if(ch=='.') dot=1;
	while(i<w)
	{	i++;
		if((ch=(*getn)())<0) return(ch);

		if(ch<='9' && ch>='0') {
			y=10*y+ch-'0';
			ny++;
		} else if(ch==' ' || ch=='\n') {
			if(cblank) {
				y*= 10;
				ny++;
			}
		} else if(ch==',') {
			goto done;
		} else if(ch=='d' || ch=='e' || ch=='+' || ch=='-' || ch=='D' || ch=='E') {
			break;
		} else {
			return(errno=F_ERRFCHR);
		}
	}
	/*	now for the exponent.
	 *	mode=3 means seen digit or sign of exponent.
	 *	either out of characters to scan or 
	 *		ch is '+', '-', 'd', or 'e'.
	 */
exponent:
	if(ch=='-' || ch=='+') {
		if(ch=='-') sz=1;
		mode = 3;
	} else {
		mode = 2;
	}

	while(i<w)
	{	i++;
		sawz=1;
		if((ch=(*getn)())<0) return(ch);

		if(ch<='9' && ch>='0') {
			mode = 3;
			z=10*z+ch-'0';
		} else if(ch=='+' || ch=='-') {
			if(mode==3 ) return(errno=F_ERRFCHR);
			mode = 3;
			if(ch=='-') sz=1;
		} else if(ch == ' ' || ch=='\n') {
			if(cblank) z *=10;
		} else if(ch==',') {
			break;
		} else {
			return(errno=F_ERRFCHR);
		}
	}
done:
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

LOCAL
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
LOCAL
rd_H(n,s) char *s;
{	int i,ch = 0;

	used_data = YES;
	for(i=0;i<n;i++)
	{	if (ch != '\n')
			GET(ch);
		if (ch == '\n')
			*s++ = ' ';
		else
			*s++ = ch;
	}
	return(OK);
}

LOCAL
rd_POS(s) char *s;
{	char quote;
	int ch = 0;

	used_data = YES;
	quote = *s++;
	while(*s)
	{	if(*s==quote && *(s+1)!=quote)
			break;
		if (ch != '\n')
			GET(ch);
		if (ch == '\n')
			*s++ = ' ';
		else
			*s++ = ch;
	}
	return(OK);
}
