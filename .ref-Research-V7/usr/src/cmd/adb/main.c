#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


MSG		NOEOR;

INT		mkfault;
INT		executing;
INT		infile;
CHAR		*lp;
INT		maxoff;
INT		maxpos;
INT		sigint;
INT		sigqit;
INT		wtflag;
L_INT		maxfile;
L_INT		maxstor;
L_INT		txtsiz;
L_INT		datsiz;
L_INT		datbas;
L_INT		stksiz;
STRING		errflg;
INT		exitflg;
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


L_INT	round(a,b)
L_INT		a, b;
{
	L_INT		w;
	w = ((a+b-1)/b)*b;
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
	longjmp(erradb,1);
}

fault(a)
{
	signal(a,fault);
	lseek(infile,0L,2);
	mkfault++;
}

/* set up files and initial address mappings */
INT argcount;

main(argc, argv)
REG STRING	*argv;
REG INT		argc;
{
	maxfile=1L<<24; maxstor=1L<<16;

	gtty(0,&adbtty);
	gtty(0,&usrtty);
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

	IF (sigint=signal(SIGINT,01))!=01
	THEN	sigint=fault; signal(SIGINT,fault);
	FI
	sigqit=signal(SIGQUIT,1);
	setjmp(erradb);
	IF executing THEN delbp(); FI
	executing=FALSE;

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
			THEN	iclose(); eof=0; longjmp(erradb,1);
			ELSE	done();
			FI
		ELSE	exitflg=0;
		FI
		command(0,lastcom);
		IF lp ANDF lastc!=EOR THEN error(NOEOR); FI
	POOL
}

done()
{
	endpcs();
	exit(exitflg);
}

