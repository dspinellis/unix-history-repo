/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)main.c 1.1 %G%";

/*
 * Debugger main routine.
 */

#include "defs.h"
#include <setjmp.h>
#include <signal.h>
#include "main.h"
#include "command.h"
#include "process.h"
#include "object.h"

#define FIRST_TIME 0		/* initial value setjmp returns */
#define isinteractive()		(isatty(fileno(stdin)))

LOCAL jmp_buf env;
LOCAL catchintr();

main(argc, argv)
int argc;
char **argv;
{
	FILE *fp;

	cmdname = argv[0];
	catcherrs();
	scanargs(argc, argv);
	if ((fp = fopen(objname, "r")) == NIL) {
		panic("can't read %s", objname);
	} else {
		fclose(fp);
	}
	if (option('r')) {
		if (setjmp(env) == FIRST_TIME) {
			arginit();
			run();
			/* NOTREACHED */
		} else {
			option('r') = FALSE;
			if (isinteractive()) {
				printf("> ");
				fflush(stdout);
			}
		}
	} else {
		start(NIL, NIL, NIL);
		prompt();
		init();
	}
	setjmp(env);
	signal(SIGINT, &catchintr);
	yyparse();
	putchar('\n');
}

/*
 * Initialize the world, including setting initial input file
 * if the file exists.
 */

init()
{
	initinput();
	readobj(objname);
	lexinit();
}

/*
 * After a non-fatal error we jump back to command parsing.
 */

erecover()
{
	gobble();
	prompt();
	longjmp(env, 1);
}

/*
 * This routine is called when an interrupt occurs.
 */

LOCAL catchintr()
{
	putchar('\n');
	prompt();
	longjmp(env, 1);
}

/*
 * scan the argument list
 */

LOCAL scanargs(argc, argv)
int argc;
char **argv;
{
	register int i, j;
	BOOLEAN foundfile;

	foundfile = FALSE;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (j = 1; argv[i][j] != '\0'; j++) {
				setoption(argv[i][j]);
			}
		} else if (!foundfile) {
			objname = argv[i];
		} else {
			panic("extraneous argument %s", argv[i]);
		}
	}
}

/*
 * take appropriate action for recognized command argument
 */

LOCAL setoption(c)
register char c;
{
	switch(c) {
		case 'r':	/* run program before accepting commands */
		case 'b':	/* trace internal breakpoints (for debugging) */
		case 'e':	/* trace execution (for debugging) */
			option(c) = TRUE;
			break;

		default:
			panic("unknown option '%c'", c);
	}
}
