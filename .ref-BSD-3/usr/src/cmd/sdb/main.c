#
/*
 *
 *	UNIX debugger
 *
 */

#include	"head.h"
#include	<signal.h>
#include	<stdio.h>
#include	<sys/stat.h>

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
int	fpe();



#ifndef STD
char *mktemp(), *tfn;
int tfd;
#endif

main(argc, argv)
REG STRING	*argv;
REG INT		argc;
{
	register char *p;
	struct stat stbuf;

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

	if (stat(symfil, &stbuf) == -1) {
		printf("`%s' does not exist\n", symfil);
		exit(4);
	}
	symtime = stbuf.st_mtime;
	if (stat(corfil, &stbuf) != -1) {
		if (symtime > stbuf.st_mtime)
			printf("Warning: `%s' newer than `%s'\n",
				symfil, corfil);
	}

	setsym(); setcor();
	initfp();

	/* set up variables for user */
	maxoff=MAXOFF; maxpos=MAXPOS;
	gtty(2, &sdbttym);
	IF (sigint= (ADDR) signal(SIGINT,01))!=01
	THEN	sigint= (ADDR) fault; signal(SIGINT,fault);
	FI
	sigqit= (ADDR) signal(SIGQUIT,1);
	signal(SIGILL, fpe);
#ifdef VAX135
	tfn = "/usr/hpk/sdblog/XXXXXX";
#endif
#ifdef UCBVAX
	tfn = "/usr/staff/hpk/sdblog/XXXXXX";
#endif
#ifndef STD
	tfn = mktemp(tfn);
	tfd = open(tfn);
	if (tfd < 0)
		tfd = creat(tfn, 0666);
	lseek(tfd, 0l, 2);
#endif

	setjmp(env);
	if (debug) printf("Sdb restarted\n");
	gtty(2, &userttym);
	if (sdbttym.sg_flags != userttym.sg_flags)
		stty(2, &sdbttym);
	IF executing THEN delbp(); FI
	executing=FALSE;

	for (;;) {
		mkfault = 0;
		printf("*");
		if (decode(readline(stdin)) == 1) {
			printf("Error; try again\n");
			continue;
		}

	if (debug) {
		printf("cmd %c:\n", cmd);
		printf("%s:%s\n", proc, var);
		printf("args-%s;re-%s;integ-%d\n", args, re, integ);
		printf("scallf-%d;reflg-%d\n\n", scallf, reflag);
 	}

#ifndef STD
		write(tfd, &cmd, 1);
#endif

		docommand();
	}
}


fault(a)
{
	signal(a,fault);
	mkfault++;
	printf("\n");
	longjmp(env, 0);
}

fpe() {
	signal(SIGILL, fpe);
	error("Illegal floating constant");
	longjmp(env, 0);
}
