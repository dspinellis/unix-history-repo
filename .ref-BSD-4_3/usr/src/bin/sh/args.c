#ifndef lint
static char sccsid[] = "@(#)args.c	4.4 7/31/85";
#endif

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"

PROC STRING *copyargs();
LOCAL DOLPTR	dolh;

CHAR	flagadr[10];

CHAR	flagchar[] = {
	'x',	'n',	'v',	't',	's',	'i',	'e',	'r',	'k',	'u',	0
};
INT	flagval[]  = {
	execpr,	noexec,	readpr,	oneflg,	stdflg,	intflg,	errflg,	rshflg,	keyflg,	setflg,	0
};

/* ========	option handling	======== */


INT	options(argc,argv)
	STRING		*argv;
	INT		argc;
{
	REG STRING	cp;
	REG STRING	*argp=argv;
	REG STRING	flagc;
	STRING		flagp;

	IF argc>1 ANDF *argp[1]=='-'
	THEN	cp=argp[1];
		flags &= ~(execpr|readpr);
		WHILE *++cp
		DO	flagc=flagchar;

			WHILE *flagc ANDF *flagc != *cp DO flagc++ OD
			IF *cp == *flagc
			THEN	flags |= flagval[flagc-flagchar];
			ELIF *cp=='c' ANDF argc>2 ANDF comdiv==0
			THEN	comdiv=argp[2];
				argp[1]=argp[0]; argp++; argc--;
			ELSE	failed(argv[1],badopt);
			FI
		OD
		argp[1]=argp[0]; argc--;
	FI

	/* set up $- */
	flagc=flagchar;
	flagp=flagadr;
	WHILE *flagc
	DO IF flags&flagval[flagc-flagchar]
	   THEN *flagp++ = *flagc;
	   FI
	   flagc++;
	OD
	*flagp++=0;

	return(argc);
}

VOID	setargs(argi)
	STRING		argi[];
{
	/* count args */
	REG STRING	*argp=argi;
	REG INT		argn=0;

	WHILE Rcheat(*argp++)!=ENDARGS DO argn++ OD

	/* free old ones unless on for loop chain */
	freeargs(dolh);
	dolh=copyargs(argi,argn);	/* sets dolv */
	assnum(&dolladr,dolc=argn-1);
}

freeargs(blk)
	DOLPTR		blk;
{
	REG STRING	*argp;
	REG DOLPTR	argr=0;
	REG DOLPTR	argblk;

	IF argblk=blk
	THEN	argr = argblk->dolnxt;
		IF (--argblk->doluse)==0
		THEN	FOR argp=argblk->dolarg; Rcheat(*argp)!=ENDARGS; argp++
			DO free(*argp) OD
			free(argblk);
		FI
	FI
	return(argr);
}

LOCAL STRING *	copyargs(from, n)
	STRING		from[];
{
	REG STRING *	np=alloc(sizeof(STRING*)*n+3*BYTESPERWORD);
	REG STRING *	fp=from;
	REG STRING *	pp=np;

	np->doluse=1;	/* use count */
	np=np->dolarg;
	dolv=np;

	WHILE n--
	DO *np++ = make(*fp++) OD
	*np++ = ENDARGS;
	return(pp);
}

clearup()
{
	/* force `for' $* lists to go away */
	WHILE argfor=freeargs(argfor) DONE

	/* clean up io files */
	WHILE pop() DONE
}

DOLPTR	useargs()
{
	IF dolh
	THEN	dolh->doluse++;
		dolh->dolnxt=argfor;
		return(argfor=dolh);
	ELSE	return(0);
	FI
}
