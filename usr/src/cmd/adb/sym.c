#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


MSG		BADFIL;

SYMTAB		symbol;
BOOL		localok;
INT		lastframe;
SYMSLAVE		*symvec;
POS		maxoff;
L_INT		maxstor;

/* symbol management */
L_INT		symbas;
L_INT		symcnt;
L_INT		symnum;
L_INT		localval;
char		symrqd -1;
SYMTAB		symbuf[SYMSIZ];
SYMPTR		symnxt;
SYMPTR		symend;


INT		fsym;
STRING		errflg;
POS		findsym();


/* symbol table and file handling service routines */

longseek(f, a)
L_INT a;
{
	return(lseek(f,a,0) != -1);
}

valpr(v,idsp)
{
	POS		d;
	d = findsym(v,idsp);
	IF d < maxoff
	THEN	printf("%.8s", symbol.symc);
		IF d
		THEN	printf(OFFMODE, d);
		FI
	FI
}

localsym(cframe)
L_INT cframe;
{
	INT symflg;
	WHILE nextsym() ANDF localok
		ANDF symbol.symc[0]!='~'
		ANDF (symflg=symbol.symf)!=037
	DO IF symflg>=2 ANDF symflg<=4
	   THEN localval=symbol.symv;
		return(TRUE);
	   ELIF symflg==1
	   THEN localval=leng(shorten(cframe)+symbol.symv);
		return(TRUE);
	   ELIF symflg==20 ANDF lastframe
	   THEN localval=leng(lastframe+2*symbol.symv-10);
		return(TRUE);
	   FI
	OD
	return(FALSE);
}
psymoff(v,type,s)
L_INT v; int type; char *s;
{
	POS		w;
	w = findsym(shorten(v),type);
	IF w >= maxoff
	THEN printf(LPRMODE,v);
	ELSE printf("%.8s", symbol.symc);
	     IF w THEN printf(OFFMODE,w); FI
	FI
	printf(s);
}

POS	findsym(svalue,type)
POS	svalue;
INT	type;
{
	L_INT		diff, value, symval, offset;
	INT		symtyp;
	REG SYMSLAVE	*symptr;
	SYMSLAVE	*symsav;
	value=svalue; diff = 0377777L; symsav=0;
	IF type!=NSYM ANDF (symptr=symvec)
	THEN	WHILE diff ANDF (symtyp=symptr->typslave)!=ESYM
		DO  IF symtyp==type
		    THEN symval=leng(symptr->valslave);
			 IF value-symval<diff
			    ANDF value>=symval
			 THEN diff = value-symval;
			      symsav=symptr;
			 FI
		    FI
		    symptr++;
		OD
		IF symsav
		THEN	offset=leng(symsav-symvec);
			symcnt=symnum-offset;
			longseek(fsym, symbas+offset*SYMTABSIZ);
			read(fsym,&symbol,SYMTABSIZ);
		FI
	FI
	return(shorten(diff));
}

nextsym()
{
	IF (--symcnt)<0
	THEN	return(FALSE);
	ELSE	return(longseek(fsym, symbas+(symnum-symcnt)*SYMTABSIZ)!=0 ANDF
			read(fsym,&symbol,SYMTABSIZ)==SYMTABSIZ);
	FI
}



/* sequential search through file */
symset()
{
	symcnt = symnum;
	symnxt = symbuf;
	IF symrqd
	THEN	longseek(fsym, symbas);
		symread(); symrqd=FALSE;
	ELSE	longseek(fsym, symbas+sizeof symbuf);
	FI
}

SYMPTR	symget()
{
	REG INT	rc;
	IF symnxt >= symend
	THEN	rc=symread(); symrqd=TRUE;
	ELSE	rc=TRUE;
	FI
	IF --symcnt>0 ANDF rc==0 THEN errflg=BADFIL; FI
	return( (symcnt>=0 && rc) ? symnxt++ : 0);
}

symread()
{
	INT		symlen;

	IF (symlen=read(fsym,symbuf,sizeof symbuf))>=SYMTABSIZ
	THEN	symnxt = symbuf;
		symend = &symbuf[symlen/SYMTABSIZ];
		return(TRUE);
	ELSE	return(FALSE);
	FI
}
