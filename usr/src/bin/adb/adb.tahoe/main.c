#ifndef lint
static	char sccsid[] = "@(#)main.c	1.1 (Berkeley) 2/25/86";
#endif
/*
 * adb - main command loop and error/interrupt handling
 */
#include "defs.h"

MSG		NOEOR;

INT		mkfault;
INT		executing;
INT		infile;
CHAR		*lp;
L_INT		maxpos;
SIG		sigint;
SIG		sigqit;
INT		wtflag;
L_INT		maxfile;
STRING		errflg;
L_INT		exitflg;

CHAR		lastc;
INT		eof;

INT		lastcom;

ADDR	maxoff = MAXOFF;
L_INT	maxpos = MAXPOS;
char	*Ipath = "/usr/lib/adb";

main(argc, argv)
	register char **argv;
	int argc;
{

	mkioptab();
another:
	if (argc>1) {
		if (eqstr("-w", argv[1])) {
			wtflag = 2;		/* suitable for open() */
			argc--, argv++;
			goto another;
		}
		if (eqstr("-k", argv[1])) {
			kernel = 1;
			argc--, argv++;
			goto another;
		}
		if (argv[1][0] == '-' && argv[1][1] == 'I') {
			Ipath = argv[1]+2;
			argc--, argv++;
		}
	}
	if (argc > 1)
		symfil = argv[1];
	if (argc > 2)
		corfil = argv[2];
	xargc = argc;
	setsym(); setcor(); setvar();

	if ((sigint=signal(SIGINT,SIG_IGN)) != SIG_IGN) {
		sigint = fault;
		signal(SIGINT, fault);
	}
	sigqit = signal(SIGQUIT, SIG_IGN);
	setexit();
	if (executing)
		delbp();
	executing = 0;
	for (;;) {
		flushbuf();
		if (errflg) {
			printf("%s\n", errflg);
			exitflg = 1;
			errflg = 0;
		}
		if (mkfault) {
			mkfault=0;
			printc('\n');
			printf(DBNAME);
		}
		lp=0; rdc(); lp--;
		if (eof) {
			if (infile) {
				iclose(-1, 0); eof=0; reset();
			} else
				done();
		} else
			exitflg = 0;
		command(0, lastcom);
		if (lp && lastc!='\n')
			error(NOEOR);
	}
}

done()
{
	endpcs();
	exit(exitflg);
}

L_INT
round(a,b)
REG L_INT a, b;
{
	REG L_INT w;
	w = (a/b)*b;
	IF a!=w THEN w += b; FI
	return(w);
}

/*
 * If there has been an error or a fault, take the error.
 */
chkerr()
{
	if (errflg || mkfault)
		error(errflg);
}

/*
 * An error occurred; save the message for later printing,
 * close open files, and reset to main command loop.
 */
error(n)
	char *n;
{
	errflg = n;
	iclose(0, 1); oclose();
	reset();
}

/*
 * An interrupt occurred; reset the interrupt
 * catch, seek to the end of the current file
 * and remember that there was a fault.
 */
fault(a)
{
	signal(a, fault);
	lseek(infile, 0L, 2);
	mkfault++;
}
