#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)expr.c	2.5);


MSG		BADSYM;
MSG		BADVAR;
MSG		BADKET;
MSG		BADSYN;
MSG		NOCFN;
MSG		NOADR;
MSG		BADLOC;

SYMTAB		symbol;
ADDR		lastframe;
ADDR		savlastf;
ADDR		savframe;
ADDR		savpc;
ADDR		callpc;



CHAR		*lp;
INT		radix;
STRING		errflg;
L_INT		localval;
CHAR		isymbol[8];

CHAR		lastc,peekc;
char u[ctob(UPAGES)];	/* struct user u; */

L_INT		dot;
L_INT		ditto;
INT		dotinc;
L_INT		var[];
L_INT		expv;




expr(a)
{	/* term | term dyadic expr |  */
	INT		rc;
	L_INT		lhs;

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
	INT		base, d, regptr;
	CHAR		savc;
	BOOL		hex;
	L_INT		frame;
	SYMPTR		symp;

	hex=FALSE;

	readchar();
	IF symchar(0)
	THEN	readsym();
		IF lastc=='.'
		THEN	frame= *(ADDR *)(((ADDR)&u[0])+FP); lastframe=0;
			callpc= *(ADDR *)(((ADDR)&u[0])+PC);
			WHILE errflg==0
			DO  savpc=callpc;
				findsym(callpc,ISYM);
			    IF  eqsym(symbol.symc,isymbol,'~')
			    THEN break;
			    FI
				callpc=get(frame+16, DSP);
			    lastframe=frame;
			    frame=get(frame+12,DSP)&EVEN;
			    IF frame==0
			    THEN error(NOCFN);
			    FI
			OD
			savlastf=lastframe; savframe=frame;
			readchar();
			IF symchar(0)
			THEN	chkloc(expv=frame);
			FI
		ELIF (symp=lookupsym(isymbol))==0 THEN error(BADSYM);
		ELSE expv = symp->symv;
		FI
		lp--;


	ELIF getnum(readchar)
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
		IF regptr=getreg(savc)
		THEN	expv= * (ADDR *)(((ADDR)&u[0])+regptr);
		ELIF (base=varchk(savc)) != -1
		THEN	expv=var[base];
		ELSE	error(BADVAR);
		FI

	ELIF lastc=='\''
	THEN	d=4; expv=0;
		WHILE quotchar()
		DO  IF d--
		    THEN IF d==1 THEN expv <<=16; FI
			 expv |= ((d&1)?lastc:lastc<<8);
		    ELSE error(BADSYN);
		    FI
		OD

	ELIF a
	THEN	error(NOADR);
	ELSE	lp--; return(0);
	FI
	return(1);
}

/* service routines for expression reading */
getnum(rdf) int (*rdf)();
{
	INT base,d,frpt;
	BOOL hex;
	UNION{REAL r; L_INT i;} real;
	IF digit(lastc) ORF (hex=TRUE, lastc=='#' ANDF hexdigit((*rdf)()))
	THEN	expv = 0;
		base = (hex ? 16 : radix);
		WHILE (base>10 ? hexdigit(lastc) : digit(lastc))
		DO  expv = (base==16 ? expv<<4 : expv*base);
		    IF (d=convdig(lastc))>=base THEN error(BADSYN); FI
		    expv += d; (*rdf)();
		    IF expv==0
		    THEN IF (lastc=='x' ORF lastc=='X')
				 THEN hex=TRUE; base=16; (*rdf)();
				 ELIF (lastc=='t' ORF lastc=='T')
			     THEN hex=FALSE; base=10; (*rdf)();
		    	 ELIF (lastc=='o' ORF lastc=='O')
		    	 THEN hex=FALSE; base=8; (*rdf)();
				 FI
		    FI
		OD
		IF lastc=='.' ANDF (base==10 ORF expv==0) ANDF !hex
		THEN	real.r=expv; frpt=0; base=10;
			WHILE digit((*rdf)())
			DO	real.r *= base; frpt++;
				real.r += lastc-'0';
			OD
			WHILE frpt--
			DO	real.r /= base; OD
			expv = real.i;
		FI
		peekc=lastc;
/*		lp--; */
		return(1);
	ELSE return(0);
	FI
}

readsym()
{
	REG char	*p;

	p = isymbol;
	REP IF p < &isymbol[8]
	    THEN *p++ = lastc;
	    FI
	    readchar();
	PER symchar(1) DONE
	WHILE p < &isymbol[8] DO *p++ = 0; OD
}

SYMPTR	lookupsym(symstr)
STRING	symstr;
{
	SYMPTR		symp;
	symset();
	WHILE (symp=symget())
#ifndef EDDT
	DO IF (symp->symf&SYMCHK)==symp->symf
	   ANDF eqsym(symp->symc, symstr,'_')
#else
	DO	IF eqsym(symp->symc, symstr, '_')
#endif
	    THEN return(symp);
	    FI
	OD
	return(0);
}

hexdigit(c)
CHAR c;
{	return((c>='0' ANDF c<='9') ORF (c>='a' ANDF c<='f'));
}

convdig(c)
CHAR c;
{
	IF digit(c)
	THEN	return(c-'0');
	ELIF hexdigit(c)
	THEN	return(c-'a'+10);
	ELSE	return(17);
	FI
}

digit(c) char c;	{return(c>='0' ANDF c<='9');}

letter(c) char c;	{return(c>='a' ANDF c<='z' ORF c>='A' ANDF c<='Z');}

symchar(dig)
{
	IF lastc=='\\' THEN readchar(); return(TRUE); FI
	return( letter(lastc) ORF lastc=='_' ORF dig ANDF digit(lastc) );
}

varchk(name)
{
	IF digit(name) THEN return(name-'0'); FI
	IF letter(name) THEN return((name&037)-1+10); FI
	return(-1);
}

chkloc(frame)
L_INT		frame;
{
	readsym();
	REP IF localsym(frame)==0 THEN error(BADLOC); FI
	    expv=localval;
	PER !eqsym(symbol.symc,isymbol,'~') DONE
}

eqsym(s1, s2, c)
REG STRING	s1, s2;
CHAR		c;
{
	IF eqstr(s1,s2)
	THEN	return(TRUE);
	ELIF *s1==c
	THEN	CHAR		s3[8];
		REG INT		i;

		s3[0]=c;
		FOR i=1; i<8; i++
		DO s3[i] = *s2++; OD

		return(eqstr(s1,s3));
	FI
}
