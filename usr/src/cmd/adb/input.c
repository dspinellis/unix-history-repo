#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"

INT		mkfault;
CHAR		line[LINSIZ];
INT		infile;
CHAR		*lp;
CHAR		lastc EOR;
INT		eof;

/* input routines */

eol(c)
CHAR	c;
{
	return(c==EOR ORF c==';');
}

rdc()
{	REP	readchar();
	PER	lastc==SP ORF lastc==TB
	DONE
	return(lastc);
}

readchar()
{
	IF eof
	THEN	lastc=EOF;
	ELSE	IF lp==0
		THEN	lp=line;
			REP eof = read(infile,lp,1)==0;
			    IF mkfault THEN error(0); FI
			PER eof==0 ANDF *lp++!=EOR DONE
			*lp=0; lp=line;
		FI
		IF lastc = *lp THEN lp++; FI
	FI
	return(lastc);
}

nextchar()
{
	IF eol(rdc())
	THEN lp--; return(0);
	ELSE return(lastc);
	FI
}

quotchar()
{
	IF readchar()=='\\'
	THEN	return(readchar());
	ELIF lastc=='\''
	THEN	return(0);
	ELSE	return(lastc);
	FI
}

getformat(deformat)
STRING		deformat;
{
	REG STRING	fptr;
	REG BOOL	quote;
	fptr=deformat; quote=FALSE;
	WHILE (quote ? readchar()!=EOR : !eol(readchar()))
	DO  IF (*fptr++ = lastc)=='"'
	    THEN quote = ~quote;
	    FI
	OD
	lp--;
	IF fptr!=deformat THEN *fptr++ = '\0'; FI
}


