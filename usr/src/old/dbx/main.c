/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * Debugger main routine.
 */

#include "defs.h"
#include <setjmp.h>
#include <signal.h>
#include <errno.h>
#include "main.h"
#include "tree.h"
#include "eval.h"
#include "debug.h"
#include "symbols.h"
#include "scanner.h"
#include "keywords.h"
#include "process.h"
#include "runtime.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "coredump.h"
#include "pathnames.h"

#ifndef public

#define isterm(file)	(interactive or isatty(fileno(file)))

#ifdef IRIS
#   include <termio.h>

    typedef struct termio Ttyinfo;
#else
#   include <sgtty.h>
#   include <fcntl.h>

    typedef struct {
	struct sgttyb sg;		/* standard sgttyb structure */
	struct tchars tc;		/* terminal characters */
	struct ltchars ltc;		/* local special characters */
	integer ldisc;			/* line discipline */
	integer local;			/* TIOCLGET */
	integer fcflags;		/* fcntl(2) F_GETFL, F_SETFL */
    } Ttyinfo;
#endif

#endif

public boolean coredump;		/* true if using a core dump */
public boolean runfirst;		/* run program immediately */
public boolean interactive;		/* standard input IS a terminal */
public boolean lexdebug;		/* trace scanner return values */
public boolean tracebpts;		/* trace create/delete breakpoints */
public boolean traceexec;		/* trace execution */
public boolean tracesyms;		/* print symbols are they are read */
public boolean traceblocks;		/* trace blocks while reading symbols */
public boolean vaddrs;			/* map addresses through page tables */
public boolean quiet;			/* don't print heading */
public boolean autostrip;		/* strip C++ prefixes */

public File corefile;			/* File id of core dump */

public integer versionNumber = 4;

#define FIRST_TIME 0			/* initial value setjmp returns */

private Boolean initdone = false;	/* true if initialization done */
private jmp_buf env;			/* setjmp/longjmp data */
private char outbuf[BUFSIZ];		/* standard output buffer */
private char namebuf[512];		/* possible name of object file */

private Ttyinfo ttyinfo;
private String corename;		/* name of core file */

private catchintr();
private char **scanargs();

/*
 * Main program.
 */

main(argc, argv)
int argc;
String argv[];
{
    extern integer versionNumber;
    char **scanargs();

    if (!(cmdname = rindex(*argv, '/')))
	cmdname = *argv;
    else
	++cmdname;

    catcherrs();
    onsyserr(EINTR, nil);
    onsyserr(EADDRINUSE, nil);
    onsyserr(ENXIO, nil);
    setbuf(stdout, outbuf);
    argv = scanargs(argc, argv);
    if (not runfirst and not quiet) {
	printheading();
    }
    openfiles();
    language_init();
    symbols_init();
    process_init();
    optab_init();
    if (runfirst) {
	if (setjmp(env) == FIRST_TIME) {
	    arginit();
	    while (*argv)
		newarg(*argv++);
	    run();
	    /* NOTREACHED */
	} else {
	    runfirst = false;
	}
    } else {
	init();
    }
    if (setjmp(env) != FIRST_TIME) {
	restoretty(stdout, &ttyinfo);
    }
    signal(SIGINT, catchintr);
    yyparse();
    putchar('\n');
    quit(0);
}

public printheading ()
{
    extern String date;

    printf("dbx version 3.%d of %s.\nType 'help' for help.\n",
	versionNumber, date
    );
    fflush(stdout);
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

    savetty(stdout, &ttyinfo);
    enterkeywords();
    scanner_init();
    if (not coredump and not runfirst) {
	start(nil, nil, nil);
    }
    printf("reading symbolic information ...");
    fflush(stdout);
    readobj(objname);
    printf("\n");
    fflush(stdout);
    if (coredump) {
	printf("[using memory image in %s]\n", corename);
	if (vaddrs) {
	    coredump_getkerinfo();
	}
	getsrcpos();
	setcurfunc(whatblock(pc));
    } else {
	setcurfunc(program);
    }
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

    tmpfile = mktemp(_PATH_TMP);
    setout(tmpfile);
    status();
    alias(nil, nil, nil);
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
    symbols_init();
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
 * After a non-fatal error we skip the rest of the current input line, and
 * jump back to command parsing.
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
    if (isredirected()) {
	fflush(stdout);
	unsetout();
    }
    putchar('\n');
    longjmp(env, 1);
}

/*
 * Scan the argument list.
 */

private char **scanargs (argc, argv)
int argc;
String argv[];
{
    extern char *optarg;
    extern integer optind;
    integer ch;

    runfirst = false;
    interactive = false;
    lexdebug = false;
    tracebpts = false;
    traceexec = false;
    tracesyms = false;
    traceblocks = false;
    vaddrs = false;
    quiet = false;
    autostrip = true;
    corefile = nil;
    coredump = true;
    sourcepath = list_alloc();
    list_append(list_item("."), nil, sourcepath);

    while ((ch = getopt(argc, argv, "I:abc:eiklnqrs")) != EOF)
    switch((char)ch) {
	case 'I':
		list_append(list_item(optarg), nil, sourcepath);
		break;
	case 'a':
		autostrip = false;
		break;
	case 'b':
		tracebpts = true;
		break;
	case 'c':
		initfile = optarg;
		break;
	case 'e':
		traceexec = true;
		break;
	case 'i':
		interactive = true;
		break;
	case 'k':
		vaddrs = true;
		break;
	case 'l':
#ifdef LEXDEBUG
		lexdebug = true;
#else
		fatal("\"-l\" only applicable when compiled with LEXDEBUG");
#endif
		break;
	case 'n':
		traceblocks = true;
		break;
	case 'q':
		quiet = true;
		break;
	case 'r':	/* run program before accepting commands */
		runfirst = true;
		coredump = false;
		break;
	case 's':
		tracesyms = true;
		break;
	case '?':
	default:
		fatal("unknown option");
    }
    argv += optind;
    if (*argv) {
	objname = *argv;
	if (*++argv && coredump) {
		corename = *argv;
		corefile = fopen(*argv, "r");
		if (corefile == nil)
			coredump = false;
		++argv;
	}
    }
    if (*argv and not runfirst) {
	fatal("extraneous argument %s", *argv);
    }
    return argv;
}

private openfiles ()
{
    File f;
    char *tmp;

    if (objname == nil and isatty(0)) {
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
	if (vaddrs) {
	    corename = _PATH_MEM;
	    corefile = fopen(corename, "r");
	    if (corefile == nil) {
		panic("can't open %s", _PATH_MEM);
	    }
	} else {
	    corename = "core";
	    corefile = fopen(corename, "r");
	    if (corefile == nil) {
		coredump = false;
	    }
	}
    }
}

/*
 * Save/restore the state of a tty.
 */

public savetty(f, t)
File f;
Ttyinfo *t;
{
#   ifdef IRIS
	ioctl(fileno(f), TCGETA, t);
#   else
	ioctl(fileno(f), TIOCGETP, &(t->sg));
	ioctl(fileno(f), TIOCGETC, &(t->tc));
	ioctl(fileno(f), TIOCGLTC, &(t->ltc));
	ioctl(fileno(f), TIOCGETD, &(t->ldisc));
	ioctl(fileno(f), TIOCLGET, &(t->local));
	t->fcflags = fcntl(fileno(f), F_GETFL, 0);
	if ((t->fcflags&FASYNC) != 0) {
	    /* fprintf(stderr, "[async i/o found set -- reset]\n"); */
	    t->fcflags &= ~FASYNC;
	}
#   endif
}

public restoretty(f, t)
File f;
Ttyinfo *t;
{
#   ifdef IRIS
	ioctl(fileno(f), TCSETA, t);
#   else
	ioctl(fileno(f), TIOCSETN, &(t->sg));
	ioctl(fileno(f), TIOCSETC, &(t->tc));
	ioctl(fileno(f), TIOCSLTC, &(t->ltc));
	ioctl(fileno(f), TIOCSETD, &(t->ldisc));
	ioctl(fileno(f), TIOCLSET, &(t->local));
	if ((t->fcflags&FASYNC) != 0) {
	    /* fprintf(stderr, "[async i/o not set]\n"); */
	    t->fcflags &= ~FASYNC;
	}
	(void) fcntl(fileno(f), F_SETFL, t->fcflags);
#   endif
}

/*
 * Exit gracefully.
 */

public quit(r)
Integer r;
{
    pterm(process);
    exit(r);
}
