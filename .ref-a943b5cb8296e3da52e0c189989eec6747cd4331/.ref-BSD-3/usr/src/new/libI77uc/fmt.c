/*
 * fortran format parser
 */

#include "fio.h"
#include "fmt.h"

#define skip(s)		while(*s==' ') s++
#define isdigit(x)	(x>='0' && x<='9')

#ifdef interdata
#define SYLMX 300
#endif

#ifdef pdp11
#define SYLMX 300
#endif

#ifdef vax
#define SYLMX 300
#endif

struct syl syl[SYLMX];
int parenlvl,pc,revloc;
char *f_s(), *f_list(), *i_tem(), *gt_num(), *ap_end();

pars_f(s) char *s;
{
	parenlvl=revloc=pc=0;
	return((f_s(s,0)==FMTERR)? ERROR : OK);
}

char *f_s(s,curloc) char *s;
{
	skip(s);
	if(*s++!='(')
	{
		fmtptr = s;
		return(FMTERR);
	}
	if(parenlvl++ ==1) revloc=curloc;
	op_gen(RET,curloc,0,0,s);
	if((s=f_list(s))==FMTERR)
	{
		return(FMTERR);
	}
	skip(s);
	return(s);
}

char *f_list(s) char *s;
{
	while (*s)
	{	skip(s);
		if((s=i_tem(s))==FMTERR) return(FMTERR);
		skip(s);
		if(*s==',') s++;
		else if(*s==')')
		{	if(--parenlvl==0)
			{
				op_gen(REVERT,revloc,0,0,s);
			}
			else	op_gen(GOTO,0,0,0,s);
			return(++s);
		}
	}
	fmtptr = s;
	return(FMTERR);
}

char *i_tem(s) char *s;
{	char *t;
	int n,curloc;
	if(*s==')') return(s);
	if(ne_d(s,&t)) return(t);
	if(e_d(s,&t)) return(t);
	s=gt_num(s,&n);
	curloc = op_gen(STACK,n,0,0,s);
	return(f_s(s,curloc));
}

ne_d(s,p) char *s,**p;
{	int n,x,sign=0,pp1,pp2;
	switch(lcase(*s))
	{
	case ':': op_gen(COLON,(int)('\n'),0,0,s); break;
#ifndef KOSHER
	case '$': op_gen(DOLAR,(int)('\0'),0,0,s); break;  /*** NOT STANDARD FORTRAN ***/
#endif
	case 'b':
		switch(lcase(*(s+1)))
		{
			case 'z': s++; op_gen(BZ,1,0,0,s); break;
			case 'n': s++;
			default:  op_gen(BN,0,0,0,s); break;
		}
		break;
	case 's':
		switch(lcase(*(s+1)))
		{
			case 'p': s++; x=SP; pp1=1; pp2=1; break;
#ifndef KOSHER
			case 'u': s++; x=SU; pp1=0; pp2=0; break;  /*** NOT STANDARD FORTRAN ***/
#endif
			case 's': s++; x=SS; pp1=0; pp2=1; break;
			default:  x=S; pp1=0; pp2=1; break;
		}
		op_gen(x,pp1,pp2,0,s);
		break;
	case '/': op_gen(SLASH,0,0,0,s); break;
	case '-': sign=1; s++;	/*OUTRAGEOUS CODING TRICK*/
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		s=gt_num(s,&n);
		switch(lcase(*s))
		{
		case 'p': if(sign) n= -n; op_gen(P,n,0,0,s); break;
#ifndef KOSHER
		case 'r': if(n<=1)		/*** NOT STANDARD FORTRAN ***/
			{	fmtptr = s; return(FMTERR); }
			op_gen(R,n,0,0,s); break;
		case 't': op_gen(T,0,n,0,s); break;	/* NOT STANDARD FORT */
#endif
		case 'x': op_gen(X,n,0,0,s); break;
		case 'h': op_gen(H,n,(int)(s+1),0,s);
			s+=n;
			break;
		default: fmtptr = s; return(0);
		}
		break;
	case GLITCH:
	case '"':
	case '\'': op_gen(APOS,(int)s,0,0,s);
		*p = ap_end(s);
		return(FMTOK);
	case 't':
		switch(lcase(*(s+1)))
		{
			case 'l': s++; x=TL; break;
			case 'r': s++; x=TR; break;
			default:  x=T; break;
		}
		if(isdigit(*(s+1))) {s=gt_num(s+1,&n); s--;}
#ifndef KOSHER
		else n = 0;	/* NOT STANDARD FORTRAN, should be error */
#endif
#ifdef KOSHER
		fmtptr = s; return(FMTERR);
#endif
		op_gen(x,n,1,0,s);
		break;
	case 'x': op_gen(X,1,0,0,s); break;
	case 'p': op_gen(P,0,0,0,s); break;
#ifndef KOSHER
	case 'r': op_gen(R,10,1,0,s); break;  /*** NOT STANDARD FORTRAN ***/
#endif

	default: fmtptr = s; return(0);
	}
	s++;
	*p=s;
	return(FMTOK);
}

e_d(s,p) char *s,**p;
{	int n,w,d,e,x=0;
	char *sv=s;
	char c;
	s=gt_num(s,&n);
	op_gen(STACK,n,0,0,s);
	c = lcase(*s); s++;
	switch(c)
	{
	case 'd':
	case 'e':
	case 'g':
		s = gt_num(s, &w);
		if (w==0) break;
		if(*s=='.')
		{	s++;
			s=gt_num(s,&d);
		}
		else d=0;
		if(lcase(*s) == 'e'
#ifndef KOSHER
		|| *s == '.'		 /*** '.' is NOT STANDARD FORTRAN ***/
#endif
		)
		{	s++;
			s=gt_num(s,&e);
			if(c=='e') n=EE; else if(c=='d') n=DE; else n=GE;
		}
		else
		{	e=2;
			if(c=='e') n=E; else if(c=='d') n=D; else n=G;
		}
		op_gen(n,w,d,e,s);
		break;
	case 'l':
		s = gt_num(s, &w);
		if (w==0) break;
		op_gen(L,w,0,0,s);
		break;
	case 'a':
		skip(s);
		if(*s>='0' && *s<='9')
		{	s=gt_num(s,&w);
			if(w==0) break;
			op_gen(AW,w,0,0,s);
			break;
		}
		op_gen(A,0,0,0,s);
		break;
	case 'f':
		s = gt_num(s, &w);
		if (w==0) break;
		if(*s=='.')
		{	s++;
			s=gt_num(s,&d);
		}
		else d=0;
		op_gen(F,w,d,0,s);
		break;
	case 'i':
		s = gt_num(s, &w);
		if (w==0) break;
		if(*s =='.')
		{
			s++;
			s=gt_num(s,&d);
			x = IM;
		}
		else
		{	d = 1;
			x = I;
		}
		op_gen(x,w,d,0,s);
		break;
	default:
		pc--;	/* unSTACK */
		*p = sv;
		fmtptr = s;
		return(FMTERR);
	}
	*p = s;
	return(FMTOK);
}

op_gen(a,b,c,d,s) char *s;
{	struct syl *p= &syl[pc];
	if(pc>=SYLMX)
	{	fmtptr = s;
		fatal(100,"format too complex");
	}
#ifdef debug
	fprintf(stderr,"%3d opgen: %d %d %d %d %c\n",
		pc,a,b,c,d,*s==GLITCH?'"':*s); /* for debug */
#endif
	p->op=a;
	p->p1=b;
	p->p2=c;
	p->p3=d;
	return(pc++);
}

char *gt_num(s,n) char *s; int *n;
{	int m=0,a_digit=NO;
	skip(s);
	while(isdigit(*s))
	{
		m = 10*m + (*s++)-'0';
		a_digit = YES;
	}
	if(a_digit) *n=m;
	else *n=1;
	skip(s);
	return(s);
}

char *ap_end(s) char *s;
{
	char quote;
	quote = *s++;
	for(;*s;s++)
	{
		if(*s==quote && *++s!=quote) return(s);
	}
	fmtptr = s;
	fatal(100,"bad string");
}
