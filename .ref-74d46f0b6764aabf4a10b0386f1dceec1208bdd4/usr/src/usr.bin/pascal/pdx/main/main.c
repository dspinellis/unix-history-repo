/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) %G%";
#endif /* not lint */

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

#define FIRST_TIME 0        /* initial value setjmp returns */

LOCAL int firstarg;
LOCAL jmp_buf env;
LOCAL catchintr();

main(argc, argv)
int argc;
char **argv;
{
    FILE *fp;
    int i;

#ifdef lint
    syserr();
#endif
    catchsigs();
    scanargs(argc, argv);
    cmdname = argv[0];
    if ((fp = fopen(objname, "r")) == NIL) {
	panic("can't read %s", objname);
    } else {
	fclose(fp);
    }
    if (option('r')) {
	if (setjmp(env) == FIRST_TIME) {
	    arginit();
	    for (i = firstarg; i < argc; i++) {
		newarg(argv[i]);
	    }
	    run();
	    /* NOTREACHED */
	} else {
	    option('r') = FALSE;
	}
    } else {
	initstart();
	prompt();
	init();
    }
    setjmp(env);
    signal(SIGINT, catchintr);
    yyparse();
    putchar('\n');
    quit(0);
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
    BOOLEAN done;

    if (streq(argv[0], "pxhdr") || streq(argv[0], "pix")) {
	objname = argv[1];
	option('r') = TRUE;
	option('t') = TRUE;
	if (streq(argv[0], "pxhdr")) {
	    setargs("pdx", argv[2]);
	    firstarg = 3;
	} else {
	    setargs("pix", NIL);
	    firstarg = 2;
	}
	argv[0] = "pdx";
    } else {
	done = FALSE;
	i = 1;
	while (i < argc && !done) {
	    if (argv[i][0] == '-') {
		for (j = 1; argv[i][j] != '\0'; j++) {
		    switch (argv[i][j]) {
			case 'r':   /* run program before accepting commands */
			case 'i':   /* assume input is a terminal */
			case 'b':   /* (internal) trace breakpoints */
			case 'e':   /* (internal) trace execution */
			case 'h':   /* (internal) display header information */
			    option(argv[i][j]) = TRUE;
			    break;

		    default:
			panic("bad option \"%c\"", argv[i]);
		    }
		}
	    } else {
		objname = argv[i];
		done = TRUE;
	    }
	    i++;
	}
	firstarg = i;
	setargs("pdx", objname);
    }
}

/*
 * Terminate program.  In the case of the -t option, we must remove
 * the object file because it's a tmp file.
 */

quit(r)
int r;
{
    if (option('t')) {
	unlink(objname);
    }
    exit(r);
}

LOCAL catchsigs()
{
    signal(SIGHUP, quit);
    signal(SIGQUIT, quit);
}
