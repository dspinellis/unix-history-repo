#
/*
 *
 *	UNIX debugger
 *
 */

#include	"head.h"

INT		mkfault;
INT		executing;
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
INT		magic;
L_INT		entrypt;

CHAR		lastc;

STRING		symfil;
STRING		corfil;
INT 		argcount;



main(argc, argv)
REG STRING	*argv;
REG INT		argc;
{
	register char *p;
	maxfile=1L<<24; maxstor=1L<<31;

	setbuf(stdout, NULL);
	setbuf(stderr, NULL);

	WHILE argc>1
	DO	IF eqstr("-w",argv[1])
		THEN	wtflag=2; argc--; argv++;
		ELSE	break;
		FI
	OD

	IF argc>1 THEN symfil = argv[1]; FI
	IF argc>2 THEN corfil = argv[2]; FI
	fp = filework;
	if (argc > 3) {
		for (p = argv[3]; *p; *fp++ = *p++) ;
		*fp++ = '/';
	}

	argcount=argc;
	setsym(); setcor();
	initfp();

	/* set up variables for user */
	maxoff=MAXOFF; maxpos=MAXPOS;

	IF (sigint=signal(SIGINT,01))!=01
	THEN	sigint= (ADDR) fault; signal(SIGINT,fault);
	FI
	sigqit=signal(SIGQUIT,1);
	setexit();
	if (debug) printf("Sdb restarted\n");
	IF executing THEN delbp(); FI
	executing=FALSE;

	for (;;) {
		mkfault = 0;
		printf("*");
		if (decode() == 1) {
			printf("Error; try again\n");
			continue;
		}

	if (debug) {
		printf("cmd %c:\n", cmd);
		printf("%s:%s\n", proc, var);
		printf("args-%s;re-%s;integ-%d\n", args, re, integ);
		printf("scallf-%d;reflg-%d\n\n", scallf, reflag);
 	}

		docommand();
	}
}


fault(a)
{
	signal(a,fault);
	mkfault++;
	printf("\n");
	reset();
}
