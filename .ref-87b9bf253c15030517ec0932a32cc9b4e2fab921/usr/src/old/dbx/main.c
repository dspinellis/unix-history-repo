/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)main.c 1.3 %G%";

/*
 * Debugger main routine.
 */

#include "defs.h"
#include <setjmp.h>
#include <signal.h>
#include <errno.h>
#include "main.h"
#include "scanner.h"
#include "process.h"
#include "source.h"
#include "object.h"

#ifndef public

#define isterm(file)	(interactive or isatty(fileno(file)))

#endif

public Boolean coredump;		/* true if using a core dump */
public Boolean runfirst;		/* run program immediately */
public Boolean interactive;		/* standard input IS a terminal */
public Boolean lexdebug;		/* trace yylex return values */
public Boolean tracebpts;		/* trace create/delete breakpoints */
public Boolean traceexec;		/* trace process execution */
public Boolean tracesyms;		/* print symbols as their read */

public File corefile;			/* File id of core dump */

#define FIRST_TIME 0			/* initial value setjmp returns */

private Boolean initdone = false;	/* true if initialization done */
private jmp_buf env;			/* setjmp/longjmp data */
private char outbuf[BUFSIZ];		/* standard output buffer */
private char namebuf[512];		/* possible name of object file */
private int firstarg;			/* first program argument (for -r) */

private catchintr();

/*
 * Main program.
 */

main(argc, argv)
int argc;
String argv[];
{
    register Integer i;

    cmdname = argv[0];
    catcherrs();
    onsyserr(EINTR, nil);
    setbuf(stdout, outbuf);
    scanargs(argc, argv);
    language_init();
    process_init();
    if (runfirst) {
	if (setjmp(env) == FIRST_TIME) {
	    arginit();
	    for (i = firstarg; i < argc; i++) {
		newarg(argv[i]);
	    }
	    run();
	    /* NOTREACHED */
	} else {
	    runfirst = false;
	}
    } else {
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

public init()
{
    File f;
    String home;
    char buf[100];
    extern String getenv();

    enterkeywords();
    scanner_init();
    if (not coredump and not runfirst) {
	start(nil, nil, nil);
    }
    readobj(objname);
    curfunc = program;
    bpinit();
    f = fopen(initfile, "r");
    if (f != nil) {
	fclose(f);
	setinput(initfile);
    } else {
	home = getenv("HOME");
	if (home != nil) {
	    sprintf(buf, "%s/%s", home, initfile);
	    f = fopen(buf, "r");
	    if (f != nil) {
		fclose(f);
		setinput(strdup(buf));
	    }
	}
    }
    initdone = true;
}

/*
 * Re-initialize the world, first de-allocating all storage.
 * This is necessary when the symbol information must be re-read
 * from the object file when it has changed.
 *
 * Before "forgetting" things, we save the current tracing/breakpoint
 * information to a temp file.  Then after re-creating the world,
 * we read the temp file as commands.  This isn't always the right thing;
 * if a procedure that was being traced is deleted, an error message
 * will be generated.
 *
 * If the argument vector is not nil, then this is re-initialize is being
 * done in preparation for running the program.  Since we want to process
 * the commands in the temp file before running the program, we add the
 * run command at the end of the temp file.  In this case, reinit longjmps
 * back to parsing rather than returning.
 */

public reinit(argv, infile, outfile)
String *argv;
String infile;
String outfile;
{
    register Integer i;
    String tmpfile;
    extern String mktemp();

    tmpfile = mktemp("/tmp/dbxXXXX");
    setout(tmpfile);
    status();
    print_alias(nil);
    if (argv != nil) {
	printf("run");
	for (i = 1; argv[i] != nil; i++) {
	    printf(" %s", argv[i]);
	}
	if (infile != nil) {
	    printf(" < %s", infile);
	}
	if (outfile != nil) {
	    printf(" > %s", outfile);
	}
	putchar('\n');
    }
    unsetout();
    bpfree();
    objfree();
    process_init();
    enterkeywords();
    scanner_init();
    readobj(objname);
    bpinit();
    fflush(stdout);
    setinput(tmpfile);
    unlink(tmpfile);
    if (argv != nil) {
	longjmp(env, 1);
	/* NOTREACHED */
    }
}

/*
 * After a non-fatal error we jump back to command parsing.
 */

public erecover()
{
    if (initdone) {
	gobble();
	longjmp(env, 1);
    }
}

/*
 * This routine is called when an interrupt occurs.
 */

private catchintr()
{
    putchar('\n');
    longjmp(env, 1);
}

/*
 * Scan the argument list.
 */

private scanargs(argc, argv)
int argc;
String argv[];
{
    register int i, j;
    register Boolean foundfile;
    register File f;
    char *tmp;

    runfirst = false;
    interactive = false;
    lexdebug = false;
    tracebpts = false;
    traceexec = false;
    tracesyms = false;
    foundfile = false;
    corefile = nil;
    coredump = true;
    sourcepath = list_alloc();
    list_append(list_item("."), nil, sourcepath);
    i = 1;
    while (i < argc and (not foundfile or corefile == nil)) {
	if (argv[i][0] == '-') {
	    if (streq(argv[i], "-I")) {
		++i;
		if (i >= argc) {
		    fatal("missing directory for -I");
		}
		list_append(list_item(argv[i]), nil, sourcepath);
	    } else {
		for (j = 1; argv[i][j] != '\0'; j++) {
		    setoption(argv[i][j]);
		}
	    }
	} else if (not foundfile) {
	    objname = argv[i];
	    foundfile = true;
	} else if (coredump and corefile == nil) {
	    corefile = fopen(argv[i], "r");
	    if (corefile == nil) {
		coredump = false;
	    }
	}
	++i;
    }
    if (i < argc and not runfirst) {
	fatal("extraneous argument %s", argv[i]);
    }
    firstarg = i;
    if (not foundfile and isatty(0)) {
	printf("enter object file name (default is `%s'): ", objname);
	fflush(stdout);
	gets(namebuf);
	if (namebuf[0] != '\0') {
	    objname = namebuf;
	}
    }
    f = fopen(objname, "r");
    if (f == nil) {
	fatal("can't read %s", objname);
    } else {
	fclose(f);
    }
    if (rindex(objname, '/') != nil) {
	tmp = strdup(objname);
	*(rindex(tmp, '/')) = '\0';
	list_append(list_item(tmp), nil, sourcepath);
    }
    if (coredump and corefile == nil) {
	corefile = fopen("core", "r");
	if (corefile == nil) {
	    coredump = false;
	}
    }
}

/*
 * Take appropriate action for recognized command argument.
 */

private setoption(c)
char c;
{
    switch (c) {
	case 'r':   /* run program before accepting commands */
	    runfirst = true;
	    coredump = false;
	    break;

	case 'i':
	    interactive = true;
	    break;

	case 'b':
	    tracebpts = true;
	    break;

	case 'e':
	    traceexec = true;
	    break;

	case 's':
	    tracesyms = true;
	    break;

	case 'l':
#   	    ifdef LEXDEBUG
		lexdebug = true;
#	    else
		fatal("\"-l\" only applicable when compiled with LEXDEBUG");
#	    endif
	    break;

	default:
	    fatal("unknown option '%c'", c);
    }
}

/*
 * Exit gracefully.
 */

public quit(r)
Integer r;
{
    exit(r);
}
