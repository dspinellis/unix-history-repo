/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)fmt.c	5.3	5/13/90
 */

/*
 * fortran format parser
 */

#include "fio.h"
#include "format.h"

#define isdigit(x)	(x>='0' && x<='9')
#define isspace(s)	(s==' ')
#define skip(s)		while(isspace(*s)) s++

#ifdef interdata
#define SYLMX 300
#endif

#ifdef pdp11
#define SYLMX 300
#endif

#ifdef vax
#define SYLMX 300
#endif

#ifdef tahoe
#define SYLMX 300
#endif

#ifdef hp300
#define SYLMX 300
#endif

LOCAL struct syl syl_vec[SYLMX];
struct syl *syl_ptr;
LOCAL int parenlvl,revloc;
int low_case[256];
short pc;
char *f_s(), *f_list(), *i_tem(), *gt_num(), *ap_end();
char *s_init;

pars_f()
{
	short *s_ptr;
	long  *l_ptr;
	int i;

	/* first time, initialize low_case[] */
	if( low_case[1] == 0 ) {
	    for(i = 0; i<256; i++) low_case[i]=i;
	    for(i = 'A'; i<='Z'; i++) low_case[i]=i-'A'+'a';
	}

	parenlvl=revloc=pc=0;

	s_ptr = (short *) fmtbuf;
	if( *s_ptr == FMT_COMP ) {
		/* already compiled - copy value of pc */
		pc = *(s_ptr+1);
		/* get address of the format */
		l_ptr = (long *) fmtbuf;
		fmtbuf = s_init = (char *) *(l_ptr+1);
		/* point syl_ptr to the compiled format */
		syl_ptr = (struct syl *) (l_ptr + 2);
		return(OK);
	} else {
		syl_ptr = syl_vec;
		s_init = fmtbuf;
		return((f_s(fmtbuf,0)==FMTERR)? ERROR : OK);
	}
}

LOCAL
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

LOCAL
char *f_list(s) char *s;
{
	while (*s)
	{	skip(s);
		if((s=i_tem(s))==FMTERR) return(FMTERR);
		skip(s);
		if(*s==',') s++;
		else if(*s==')')
		{	if(--parenlvl==0)
				op_gen(REVERT,revloc,0,0,s);
			else
				op_gen(GOTO,0,0,0,s);
			return(++s);
		}
	}
	fmtptr = s;
	return(FMTERR);
}

LOCAL
char *i_tem(s) char *s;
{	char *t;
	int n,curloc;
	if(*s==')') return(s);
	if ((n=ne_d(s,&t))==FMTOK)
		return(t);
	else if (n==FMTERR)
		return(FMTERR);
	if ((n=e_d(s,&t))==FMTOK)
		return(t);
	else if (n==FMTERR)
		return(FMTERR);
	s=gt_num(s,&n);
	if (n == 0) { fmtptr = s; return(FMTERR); }
	curloc = op_gen(STACK,n,0,0,s);
	return(f_s(s,curloc));
}

LOCAL
ne_d(s,p) char *s,**p;
{	int n,x,sign=0,pp1,pp2;
	switch(low_case[*s])
	{
	case ':': op_gen(COLON,(int)('\n'),0,0,s); break;
#ifndef KOSHER
	case '$': op_gen(DOLAR,(int)('\0'),0,0,s); break;  /*** NOT STANDARD FORTRAN ***/
#endif
	case 'b':
		switch(low_case[*(s+1)])
		{
			case 'n': s++; op_gen(BNZ,0,0,0,s); break;
			case 'z': s++; op_gen(BNZ,1,0,0,s); break;
#ifndef KOSHER
			default: op_gen(B,0,0,0,s); break;  /*** NOT STANDARD FORTRAN ***/
#else
			default: fmtptr = s; return(FMTUNKN);
#endif
		}
		break;
	case 's':
		switch(low_case[*(s+1)])
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

	case '-': sign=1;	/* OUTRAGEOUS CODING */
	case '+': s++;		/* OUTRAGEOUS CODING */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		s=gt_num(s,&n);
		switch(low_case[*s])
		{
		case 'p': if(sign) n= -n; op_gen(P,n,0,0,s); break;
#ifndef KOSHER
		case 'r': if(n<=1)		/*** NOT STANDARD FORTRAN ***/
			{	fmtptr = --s; return(FMTERR); }
			op_gen(R,n,0,0,s); break;
		case 't': op_gen(T,0,n,0,s); break;	/* NOT STANDARD FORT */
#endif
		case 'x': op_gen(X,n,0,0,s); break;
		case 'h': op_gen(H,n,(s+1)-s_init,0,s);
			s+=n;
			break;
		default: fmtptr = s; return(FMTUNKN);
		}
		break;
	case GLITCH:
	case '"':
	case '\'': op_gen(APOS,s-s_init,0,0,s);
		*p = ap_end(s);
		return(FMTOK);
	case 't':
		switch(low_case[*(s+1)])
		{
			case 'l': s++; x=TL; break;
			case 'r': s++; x=TR; break;
			default:  x=T; break;
		}
		if(isdigit(*(s+1))) {s=gt_num(s+1,&n); s--;}
#ifdef KOSHER
		else { fmtptr = s; return(FMTERR); }
#else
		else n = 0;	/* NOT STANDARD FORTRAN, should be error */
#endif
		op_gen(x,n,1,0,s);
		break;
	case 'x': op_gen(X,1,0,0,s); break;
	case 'p': op_gen(P,0,0,0,s); break;
#ifndef KOSHER
	case 'r': op_gen(R,10,1,0,s); break;  /*** NOT STANDARD FORTRAN ***/
#endif

	default: fmtptr = s; return(FMTUNKN);
	}
	s++;
	*p=s;
	return(FMTOK);
}

LOCAL
e_d(s,p) char *s,**p;
{	int n,w,d,e,x=0, rep_count;
	char *sv=s;
	char c;
	s=gt_num(s,&rep_count);
	if (rep_count == 0) goto ed_err;
	c = low_case[*s]; s++;
	switch(c)
	{
	case 'd':
	case 'e':
	case 'g':
		s = gt_num(s, &w);
		if (w==0) goto ed_err;
		if(*s=='.')
		{	s++;
			s=gt_num(s,&d);
		}
		else d=0;
		if(low_case[*s] == 'e'
#ifndef KOSHER
		|| *s == '.'		 /*** '.' is NOT STANDARD FORTRAN ***/
#endif
		)
		{	s++;
			s=gt_num(s,&e);
			if (e==0 || e>127 || d>127 ) goto ed_err;
			if(c=='e') n=EE; else if(c=='d') n=DE; else n=GE;
			op_gen(n,w,d + (e<<8),rep_count,s);
		}
		else
		{
			if(c=='e') n=E; else if(c=='d') n=D; else n=G;
			op_gen(n,w,d,rep_count,s);
		}
		break;
	case 'l':
		s = gt_num(s, &w);
		if (w==0) goto ed_err;
		op_gen(L,w,0,rep_count,s);
		break;
	case 'a':
		skip(s);
		if(isdigit(*s))
		{	s=gt_num(s,&w);
#ifdef	KOSHER
			if (w==0) goto ed_err;
#else
			if (w==0) op_gen(A,0,0,rep_count,s);
			else
#endif
			op_gen(AW,w,0,rep_count,s);
			break;
		}
		op_gen(A,0,0,rep_count,s);
		break;
	case 'f':
		s = gt_num(s, &w);
		if (w==0) goto ed_err;
		if(*s=='.')
		{	s++;
			s=gt_num(s,&d);
		}
		else d=0;
		op_gen(F,w,d,rep_count,s);
		break;
#ifndef	KOSHER
	case 'o':	/*** octal format - NOT STANDARD FORTRAN ***/
	case 'z':	/*** hex   format - NOT STANDARD FORTRAN ***/
#endif
	case 'i':
		s = gt_num(s, &w);
		if (w==0) goto ed_err;
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
#ifndef KOSHER
		if (c == 'o')
			op_gen(R,8,1,rep_count,s);
		else if (c == 'z')
			op_gen(R,16,1,rep_count,s);
#endif
		op_gen(x,w,d,rep_count,s);
#ifndef KOSHER
		if (c == 'o' || c == 'z')
			op_gen(R,10,1,rep_count,s);
#endif
		break;
	default:
		*p = sv;
		fmtptr = s;
		return(FMTUNKN);
	}
	*p = s;
	return(FMTOK);
ed_err:
	fmtptr = --s;
	return(FMTERR);
}

LOCAL
op_gen(a,b,c,rep,s) char *s;
{	struct syl *p= &syl_ptr[pc];
	if(pc>=SYLMX)
	{	fmtptr = s;
		fatal(F_ERFMT,"format too complex");
	}
	if( b>32767 || c>32767 || rep>32767 )
	{	fmtptr = s;
		fatal("field width or repeat count too large");
	}
#ifdef DEBUG
	fprintf(stderr,"%3d opgen: %d %d %d %d %c\n",
		pc,a,b,c,rep,*s==GLITCH?'"':*s); /* for debug */
#endif
	p->op=a;
	p->p1=b;
	p->p2=c;
	p->rpcnt=rep;
	return(pc++);
}

LOCAL
char *gt_num(s,n) char *s; int *n;
{	int m=0,a_digit=NO;
	skip(s);
	while(isdigit(*s) || isspace(*s))
	{
		if (isdigit(*s))
		{
			m = 10*m + (*s)-'0';
			a_digit = YES;
		}
		s++;
	}
	if(a_digit) *n=m;
	else *n=1;
	return(s);
}

LOCAL
char *ap_end(s) char *s;
{
	char quote;
	quote = *s++;
	for(;*s;s++)
	{
		if(*s==quote && *++s!=quote) return(s);
	}
	fmtptr = s;
	fatal(F_ERFMT,"bad string");
}
