#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)main.c	2.2);


MSG		NOEOR;

INT		mkfault;
INT		executing;
INT		infile;
CHAR		*lp;
L_INT		maxoff;
L_INT		maxpos;
ADDR		sigint;
ADDR		sigqit;
INT		wtflag;
L_INT		maxfile;
L_INT		maxstor;
L_INT		txtsiz;
L_INT		datsiz;
L_INT		datbas;
L_INT		stksiz;
STRING		errflg;
L_INT		exitflg;
INT		magic;
L_INT		entrypt;

CHAR		lastc;
INT		eof;

INT		lastcom;
L_INT		var[36];
STRING		symfil;
STRING		corfil;
CHAR		printbuf[];
CHAR		*printptr;


L_INT
round(a,b)
REG L_INT a, b;
{
	REG L_INT w;
	w = (a/b)*b;
	IF a!=w THEN w += b; FI
	return(w);
}

/* error handling */

chkerr()
{
	IF errflg ORF mkfault
	THEN	error(errflg);
	FI
}

error(n)
	STRING		n;
{
	errflg=n;
	iclose(); oclose();
#ifndef EDDT
	reset();
#endif
}

fault(a)
{
	signal(a,fault);
#ifndef EDDT
	lseek(infile,0L,2);
#endif
	mkfault++;
}

/* set up files and initial address mappings */
INT argcount;

#ifndef EDDT
main(argc, argv)
#else
eddt(argc, argv)
#endif
REG STRING	*argv;
REG INT		argc;
{
	maxfile=1L<<24; maxstor=1L<<31;
	mkioptab();

	WHILE argc>1
	DO	IF eqstr("-w",argv[1])
		THEN	wtflag=2; argc--; argv++;
		ELSE	break;
		FI
	OD

	IF argc>1 THEN symfil = argv[1]; FI
	IF argc>2 THEN corfil = argv[2]; FI
	argcount=argc;
	setsym(); setcor();

	/* set up variables for user */
	maxoff=MAXOFF; maxpos=MAXPOS;
	var[VARB] = datbas;
	var[VARD] = datsiz;
	var[VARE] = entrypt;
	var[VARM] = magic;
	var[VARS] = stksiz;
	var[VART] = txtsiz;

#ifndef EDDT
	IF (sigint=signal(SIGINT,01))!=01
	THEN	sigint=fault; signal(SIGINT,fault);
	FI
	sigqit=signal(SIGQUIT,1);
	setexit();
#endif
	IF executing THEN delbp(); FI
	executing=FALSE;
#ifdef EDDT
	prints(DBNAME);
#endif

	LOOP	flushbuf();
		IF errflg
		THEN printf("%s\n",errflg);
		     exitflg=errflg;
		     errflg=0;
		FI
		IF mkfault
		THEN	mkfault=0; printc(EOR); prints(DBNAME);
		FI
		lp=0; rdc(); lp--;
		IF eof
		THEN	IF infile
#ifndef EDDT
			THEN	iclose(); eof=0; reset();
#else
			THEN	iclose(); eof=0;
#endif
			ELSE	done();
			FI
		ELSE	exitflg=0;
		FI
		command(0,lastcom);
		IF lp ANDF lastc!=EOR THEN error(NOEOR); FI
	POOL
}

#ifndef EDDT
done()
{
	endpcs();
	exit(exitflg);
}
#endif
