/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <signal.h>
#include <fcntl.h>
#include "shell.h"
#include "main.h"
#include "mail.h"
#include "options.h"
#include "output.h"
#include "parser.h"
#include "nodes.h"
#include "eval.h"
#include "jobs.h"
#include "input.h"
#include "trap.h"
#include "var.h"
#include "memalloc.h"
#include "error.h"
#include "init.h"
#include "mystring.h"

#define PROFILE 0

int rootpid;
int rootshell;
STATIC union node *curcmd;
STATIC union node *prevcmd;
extern int errno;
#if PROFILE
short profile_buf[16384];
extern int etext();
#endif

#ifdef __STDC__
STATIC void read_profile(char *);
char *getenv(char *);
#else
STATIC void read_profile();
char *getenv();
#endif


/*
 * Main routine.  We initialize things, parse the arguments, execute
 * profiles if we're a login shell, and then call cmdloop to execute
 * commands.  The setjmp call sets up the location to jump to when an
 * exception occurs.  When an exception occurs the variable "state"
 * is used to figure out how far we had gotten.
 */

main(argc, argv)  char **argv; {
	struct jmploc jmploc;
	struct stackmark smark;
	volatile int state;
	char *shinit;

#if PROFILE
	monitor(4, etext, profile_buf, sizeof profile_buf, 50);
#endif
	state = 0;
	if (setjmp(jmploc.loc)) {
		/*
		 * When a shell procedure is executed, we raise the
		 * exception EXSHELLPROC to clean up before executing
		 * the shell procedure.
		 */
		if (exception == EXSHELLPROC) {
			rootpid = getpid();
			rootshell = 1;
			minusc = NULL;
			state = 3;
		} else if (state == 0 || iflag == 0 || ! rootshell)
			exitshell(2);
		reset();
		if (exception == EXINT
#if ATTY
		 && (! attyset() || equal(termval(), "emacs"))
#endif
		 ) {
			out2c('\n');
			flushout(&errout);
		}
		popstackmark(&smark);
		FORCEINTON;				/* enable interrupts */
		if (state == 1)
			goto state1;
		else if (state == 2)
			goto state2;
		else if (state == 3)
			goto state3;
		else
			goto state4;
	}
	handler = &jmploc;
#ifdef DEBUG
	opentrace();
	trputs("Shell args:  ");  trargs(argv);
#endif
	rootpid = getpid();
	rootshell = 1;
	init();
	setstackmark(&smark);
	procargs(argc, argv);
	if (argv[0] && argv[0][0] == '-') {
		state = 1;
		read_profile("/etc/profile");
state1:
		state = 2;
		read_profile(".profile");
	} 
state2:
	state = 3;
	if ((shinit = lookupvar("ENV")) != NULL &&
	     *shinit != '\0') {
		state = 3;
		read_profile(shinit);
	}
state3:
	state = 4;
	if (minusc) {
		evalstring(minusc);
	}
	if (sflag || minusc == NULL) {
state4:	/* XXX ??? - why isn't this before the "if" statement */
		cmdloop(1);
	}
#if PROFILE
	monitor(0);
#endif
	exitshell(exitstatus);
}


/*
 * Read and execute commands.  "Top" is nonzero for the top level command
 * loop; it turns on prompting if the shell is interactive.
 */

void
cmdloop(top) {
	union node *n;
	struct stackmark smark;
	int inter;
	int numeof = 0;

	TRACE(("cmdloop(%d) called\n", top));
	setstackmark(&smark);
	for (;;) {
		if (pendingsigs)
			dotrap();
		inter = 0;
		if (iflag && top) {
			inter++;
			showjobs(1);
			chkmail(0);
			flushout(&output);
		}
		n = parsecmd(inter);
		/* showtree(n); DEBUG */
		if (n == NEOF) {
			if (!top || numeof >= 50)
				break;
			if (!stoppedjobs()) {
				if (!Iflag)
					break;
				out2str("\nUse \"exit\" to leave shell.\n");
			}
			numeof++;
		} else if (n != NULL && nflag == 0) {
			job_warning = (job_warning == 2) ? 1 : 0;
			numeof = 0;
			evaltree(n, 0);
		}
		popstackmark(&smark);
	}
	popstackmark(&smark);		/* unnecessary */
}



/*
 * Read /etc/profile or .profile.  Return on error.
 */

STATIC void
read_profile(name)
	char *name;
	{
	int fd;

	INTOFF;
	if ((fd = open(name, O_RDONLY)) >= 0)
		setinputfd(fd, 1);
	INTON;
	if (fd < 0)
		return;
	cmdloop(0);
	popfile();
}



/*
 * Read a file containing shell functions.
 */

void
readcmdfile(name)
	char *name;
	{
	int fd;

	INTOFF;
	if ((fd = open(name, O_RDONLY)) >= 0)
		setinputfd(fd, 1);
	else
		error("Can't open %s", name);
	INTON;
	cmdloop(0);
	popfile();
}



/*
 * Take commands from a file.  To be compatable we should do a path
 * search for the file, but a path search doesn't make any sense.
 */

dotcmd(argc, argv)  char **argv; {
	exitstatus = 0;
	if (argc >= 2) {		/* That's what SVR2 does */
		setinputfile(argv[1], 1);
		commandname = argv[1];
		cmdloop(0);
		popfile();
	}
	return exitstatus;
}


exitcmd(argc, argv)  char **argv; {
	if (stoppedjobs())
		return;
	if (argc > 1)
		exitstatus = number(argv[1]);
	exitshell(exitstatus);
}


#ifdef notdef
/*
 * Should never be called.
 */

void
exit(exitstatus) {
	_exit(exitstatus);
}
#endif
