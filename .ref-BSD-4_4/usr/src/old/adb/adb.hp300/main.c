static	char sccsid[] = "@(#)main.c 4.3 4/1/82";
/*
 * adb - main command loop and error/interrupt handling
 */
#include "defs.h"
#include <setjmp.h>

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
STRING		errflg;
L_INT		exitflg;

CHAR		lastc;
INT		eof;

INT		lastcom;

long	maxoff = MAXOFF;
long	maxpos = MAXPOS;
char	*Ipath = "/usr/old/libdata/adb";
jmp_buf	env;
int	useentry = 0;

main(argc, argv)
	register char **argv;
	int argc;
{
	char *ip, *getenv();
	
	mkioptab();
	if ((ip = getenv("ADBPATH")) != NULL)
		Ipath = ip;
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
		if (eqstr("-e", argv[1])) {
			useentry = 1;
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
	(void) setjmp(env);
	if (executing)
		delbp();
	executing = 0;
	for (;;) {
		flushbuf();
		if (errflg) {
			printf("%s\n", errflg);
			exitflg = errflg;
			errflg = 0;
		}
		if (mkfault) {
			mkfault=0;
			printc('\n');
			prints(DBNAME);
		}
		lp=0; rdc(); lp--;
		if (eof) {
			if (infile) {
				iclose(-1, 0); eof=0; longjmp(env, 1);
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
REG u_int a, b;
{
	REG u_int w;
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
	longjmp(env, 1);
}

/*
 * An interrupt occurred; reset the interrupt
 * catch, seek to the end of the current file
 * and remember that there was a fault.
 */
fault(a)
{
	signal(a, fault);
	lseek(infile, (off_t)0, 2);
	mkfault++;
}

#include <sys/stat.h>
/*
 * Use Ipath to look for the file name. Return nonzero if we were
 * able to locate the file, with the successful path in path.
 */
findifile(name, path)
	char *name, path[];
{
	char *cp, *tp;
	struct stat stbuf;
	
	for (cp = Ipath; cp && *cp; cp = tp) {
		tp = index(cp, ':');
		if (tp) {
			if (tp == cp) {
				sprintf(path, "%s", name);
			}
			else {
				sprintf(path, "%.*s/%s", tp-cp, cp, name);
			}
			tp++;
		} else {
			sprintf(path, "%s/%s", cp, name);
		}
		if (stat(path, &stbuf) >= 0) {
			return (1);
		}
	}
	return (0);
}

