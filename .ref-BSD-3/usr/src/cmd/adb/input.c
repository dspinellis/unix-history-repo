#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)input.c	2.2);

INT		mkfault;
CHAR		line[LINSIZ];
INT		infile;
CHAR		*lp;
CHAR		peekc,lastc = EOR;
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
#ifdef EDDT
			getcon(lp); eof=0; while (*lp++!=EOR);
#else
			REP eof = read(infile,lp,1)==0;
			    IF mkfault THEN error(0); FI
			PER eof==0 ANDF *lp++!=EOR DONE
#endif
			*lp=0; lp=line;
		FI
		IF lastc = peekc THEN peekc=0;
		ELIF lastc = *lp THEN lp++;
		FI
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

#ifdef EDDT
getcon(cs) register char *cs; {
register char *p=cs;
for(;;) {
	putchar(*p++=getcnsl());
	if (p[-1]=='\r') {putchar(p[-1]='\n'); *p++='\0'; return;}
	if (p[-1]=='\\') {p[-1]=getcnsl(); continue;}
	if (p[-1]=='#') {p -= 2; continue;}
	if (p[-1]=='@') {putchar('\r'); putchar('\n'); p=cs; continue;}
}}
 
getcnsl() {
# define RXCS	32  /*  receiver control/staus */
# define RXDB  33  /*  receiver data */
# define RXCS_DONE  0x80  /*  receiver done */
while ((mfpr(RXCS) & RXCS_DONE)==0);
return (mfpr(RXDB) & 0177);
}
#endif
