#ifndef lint
static	char sccsid[] = "@(#)expr.c	1.2 (Berkeley) 7/25/86";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"

MSG		BADSYM;
MSG		BADVAR;
MSG		BADKET;
MSG		BADSYN;
MSG		NOCFN;
MSG		NOADR;
MSG		BADLOC;

ADDR		lastframe;
ADDR		savlastf;
ADDR		savframe;
ADDR		savpc;
ADDR		callpc;



CHAR		*lp;
INT		radix;
STRING		errflg;
ADDR		localval;
CHAR		isymbol[1024];

CHAR		lastc,peekc;

L_INT		dot;
L_INT		ditto;
INT		dotinc;
L_INT		var[];
L_INT		expv;




expr(a)
{	/* term | term dyadic expr |  */
	REG	rc;
	REG	L_INT	lhs;

	rdc(); lp--; rc=term(a);

	WHILE rc
	DO  lhs = expv;

	    switch ((int)readchar()) {

		    case '+':
			term(a|1); expv += lhs; break;

		    case '-':
			term(a|1); expv = lhs - expv; break;

		    case '#':
			term(a|1); expv = round(lhs,expv); break;

		    case '*':
			term(a|1); expv *= lhs; break;

		    case '%':
			term(a|1); expv = lhs/expv; break;

		    case '&':
			term(a|1); expv &= lhs; break;

		    case '|':
			term(a|1); expv |= lhs; break;

		    case ')':
			IF (a&2)==0 THEN error(BADKET); FI

		    default:
			lp--;
			return(rc);
	    }
	OD
	return(rc);
}

term(a)
{	/* item | monadic item | (expr) | */

	switch ((int)readchar()) {

		    case '*':
			term(a|1); expv=chkget(expv,DSP); return(1);

		    case '@':
			term(a|1); expv=chkget(expv,ISP); return(1);

		    case '-':
			term(a|1); expv = -expv; return(1);

		    case '~':
			term(a|1); expv = ~expv; return(1);

		    case '#':
			term(a|1); expv = !expv; return(1);

		    case '(':
			expr(2);
			IF *lp!=')'
			THEN	error(BADSYN);
			ELSE	lp++; return(1);
			FI

		    default:
			lp--;
			return(item(a));
	}
}

item(a)
{	/* name [ . local ] | number | . | ^ | <var | <register | 'x | | */
	REG		base, d, regptr;
	CHAR		savc;
	REG	L_INT		frame;
	register struct nlist *symp;

	readchar();
	IF symchar(0)
	THEN	readsym();
		IF lastc=='.'
		THEN	frame= *(ADDR *)(((ADDR)(&u))+FP); lastframe=0;
			callpc= *(ADDR *)(((ADDR)(&u))+PC);
			WHILE errflg==0
			DO  savpc=callpc;
				findsym(callpc,ISYM);
			    IF  eqsym(cursym->n_un.n_name,isymbol,'~')
			    THEN break;
			    FI
				callpc=get(frame-8, DSP);
			    lastframe=frame;
			    frame=get(frame, DSP)&ALIGN;
			    IF frame==0
			    THEN error(NOCFN);
			    FI
			OD
			savlastf=lastframe; savframe=frame;
			readchar();
			IF symchar(0)
			THEN	chkloc(expv=frame);
			FI
		ELIF (symp=lookup(isymbol))==0 THEN error(BADSYM);
		ELSE expv = symp->n_value;
		FI
		lp--;


	ELIF getnum()
	THEN ;
	ELIF lastc=='.'
	THEN	readchar();
		IF symchar(0)
		THEN	lastframe=savlastf; callpc=savpc;
			chkloc(savframe);
		ELSE	expv=dot;
		FI
		lp--;

	ELIF lastc=='"'
	THEN	expv=ditto;

	ELIF lastc=='+'
	THEN	expv=inkdot(dotinc);

	ELIF lastc=='^'
	THEN	expv=inkdot(-dotinc);

	ELIF lastc=='<'
	THEN	savc=rdc();
		IF (regptr=getreg(savc)) != -1
		THEN	IF kcore THEN expv = *(int *)regptr;
			ELSE expv= *(ADDR *)(((ADDR)(&u))+regptr); FI
		ELIF (base=varchk(savc)) != -1
		THEN	expv=var[base];
		ELSE	error(BADVAR);
		FI

	ELIF lastc=='\''
	THEN	d=4; expv=0;
		WHILE quotchar()
		DO  IF d--
		    THEN expv <<= 8;
			 expv |= lastc;
		    ELSE error(BADSYN);
		    FI
		OD

	ELIF a
	THEN	error(NOADR);
	ELSE lp--; return(0);
	FI
	return(1);
}

/* service routines for expression reading */
getnum()
{
	REG base,d,frpt;
	UNION{REAL r; L_INT i;} real;
	IF (base = radix) < 0 THEN base = -base; FI
	IF isdigit(lastc)
	THEN	expv = 0;
		WHILE (base>10 ? isxdigit(lastc) : isdigit(lastc))
		DO
		    REG m;
		    m = MAXINT/base;
		    if(expv>m)
			/* avoid overflow */
			expv = (expv-m)*base+m*base;
		    else
			expv *= base;
		    IF (d=convdig(lastc))>=base ORF d<0 THEN error(BADSYN); FI
		    expv += d; readchar();
		    IF expv==0
		    THEN IF (lastc=='x' ORF lastc=='X')
				 THEN base=16; readchar();
				 ELIF (lastc=='t' ORF lastc=='T')
			     THEN base=10; readchar();
		    	 ELIF (lastc=='o' ORF lastc=='O')
		    	 THEN base=8; readchar();
				 FI
		    FI
		OD
		IF lastc=='.' ANDF (base==10 ORF expv==0)
		THEN	real.r=expv; frpt=0; base=10;
			WHILE isdigit(readchar())
			DO	real.r *= base; frpt++;
				real.r += lastc-'0';
			OD
			WHILE frpt--
			DO	real.r /= base; OD
			expv = real.i;
		FI
		peekc=lastc;
		return(1);
	ELSE return(0);
	FI
}

readsym()
{
	REG char	*p;

	p = isymbol;
	REP IF p < &isymbol[sizeof(isymbol)-1]
	    THEN *p++ = lastc;
	    FI
	    readchar();
	PER symchar(1) DONE
	*p++ = 0;
}

convdig(c)
CHAR c;
{
	IF isdigit(c)
	THEN	return(c-'0');
	ELIF isxdigit(c)
	THEN	return(c-'a'+10);
	ELSE return(-1);
	FI
}

symchar(dig)
{
	IF lastc=='\\' THEN readchar(); return(TRUE); FI
	return ( isalpha(lastc) ORF lastc=='_' ORF dig ANDF isdigit(lastc) );
}

varchk(name)
REG name;
{
	IF isdigit(name) THEN return(name-'0'); FI
	IF isalpha(name) THEN return((name&037)-1+10); FI
	return(-1);
}

chkloc(frame)
L_INT		frame;
{
	readsym();
	REP IF localsym(frame)==0 THEN error(BADLOC); FI
	    expv=localval;
	PER !eqsym(cursym->n_un.n_name,isymbol,'~') DONE
}

eqsym(s1, s2, c)
	register char *s1, *s2;
{

	if (!strcmp(s1,s2))
		return (1);
	if (*s1 == c && !strcmp(s1+1, s2))
		return (1);
	return (0);
}
