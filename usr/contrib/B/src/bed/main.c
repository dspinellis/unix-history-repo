/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: main.c,v 2.5 85/08/22 16:05:00 timo Exp $";

/*
 * B editor -- Main program (init/exit processing), error handling.
 */

/*
 * The B editor is a structured editor for a programming language
 * for beginners and non-professional computer users.
 * [L.G.L.T. Meertens: Draft Proposal for the B programming language,
 * Mathematical Centre, Amsterdam, 1982, ISBN 90 6169 238 2.]
 * Note that `B' is only a provisional name for the language.
 * The editor uses a subset of the run-time system for the B
 * interpreter, so that they may be linked together in a later stage.
 * Also the sharing strategy of the B run-time routines makes a very
 * elegant and powerful UNDO-mechanism possible.
 */

#include "b.h" /* Contains definitions like string, etc. */
#include "feat.h"
#include "bobj.h"


#ifdef SAVEPOS
#define SAVEPOSFILE ".Bed_pos" /* Last focus position database */
#define MAXSAVE 50 /* Maximum number of entries kept in SAVEPOSFILE */
#endif SAVEPOS


/* Command line flags */

bool dflag; /* -d: debugging output wanted */

bool slowterminal;
	/* -s: the terminal is so slow that long messages are annoying */

bool hushbaby; /* -h: no bells are to be heard */

#ifdef COMMENTED_OUT /* Lower levels don't respond to this */
bool nostandout; /* -n: inhibit use of standout */
#endif COMMENTED_OUT


/*
 * Main program -- call module initializations, do some work, 
 * call module shut-off code, exit.
 */

Visible Procedure
main(argc, argv)
	int argc;
	string *argv;
{
	bool initdone = No;
	bool status = Yes;
	int lineno = 0;
	string arg0 = argv[0];
	string cp;
	string filename;
	extern string malloc();

	cp = rindex(arg0, '/');
	if (cp)
		arg0 = cp+1;

	/* Process UNIX command line options */
	for (; argc > 1 && argv[1][0] == '-'; --argc, ++argv) {
		switch (argv[1][1]) {

#ifndef NDEBUG
		case 'd':
			dflag = Yes;
			break;
#endif NDEBUG

		case 'h':
			hushbaby = Yes;
			break;

#ifdef COMMENTED_OUT /* Lower levels don't respond to this */
		case 'n':
			nostandout = Yes;
			break;
#endif COMMENTED_OUT

		case 's':
			slowterminal = Yes;
			break;

		default:
			fprintf(stderr,
				"*** Usage: %s [-h] [-s] %s\n",
				arg0,
#ifdef FILEARGS
				"[ [+lineno] file ] ...");
#else !FILEARGS
				"");
#endif !FILEARGS
			exit(1);

		}
	}

	/* Setbuf must be called before any output is produced! */
	setbuf(stdout, malloc((unsigned)BUFSIZ));

#ifdef FILEARGS
	for (; status && argc > 1; --argc, ++argv) {
		if (argv[1][0] == '+') { /* +lineno option */
			lineno = atoi(argv[1] + 1);
		}
		else {
			filename = argv[1];
			if (!initdone) {
				initall();
				initdone = Yes;
			}
			status = demo(filename, lineno);
			lineno = 0;
		}
	}
#endif FILEARGS
	if (!initdone) {
#ifdef BTOP
		initall();
		mainloop();
#else BTOP
#ifndef FILEARGS
		Deliberate error. You should define at least one of BTOP and FILEARGS;
#endif !FILEARGS
		fprintf(stderr, "*** No file edited\n");
		exit(0);
#endif BTOP
	}
	endall();
	objstats();
	if (status)
		objcheck();
	else
		objdump();
	return !status;
}


/*
 * Module initializations -- for each module xxxx that needs dynamic
 * initialization, call a routine named initxxxx.
 * The order is determined by the inter-module dependencies.
 * Also note that all terminal- and screen-related initializations are called
 * indirectly by initterm().
 */

Hidden Procedure
initall()
{
#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** initall();\n\r");
#endif NDEBUG
	initfile();
	initkeys();
	initgram();
#ifdef USERSUGG
	initsugg();
#endif USERSUGG
	initunix();
	initterm();
}


/*
 * Module shut-off code -- for each module xxxx that needs dynamic
 * shut-off code (what is the inverse of `initialization'?),
 * call a routine named endxxxx.
 * Endall is also called (from module "unix") when a signal or interrupt
 * causes termination.
 */

Visible Procedure
endall()
{
	if (dflag)
		fprintf(stderr, "*** endall();\n\r");
	endterm();
	enddemo();
	endunix();
	enderro();
#ifdef USERSUGG
	endsugg();
#endif USERSUGG
}



/*
 * System error -- abort the editor with a short error message.
 * Should only be called for catastrophic, unrecoverable errors
 * or those that `cannot happen'.
 */

/* VARARGS 1 */
Visible Procedure
syserr(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	string fmt;
{
#ifdef BTOP
	termchild();
#endif BTOP
	endall();
	fprintf(stderr, "*** System error: ");
	fprintf(stderr, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
	fprintf(stderr, "\n");
#ifndef NDEBUG
	fprintf(stderr, "*** Core dump for B guru: ");
	fflush(stderr);
	abort();
#else
	fflush(stderr);
	_exit(1);
#endif
	/* NOTREACHED */
}


/*
 * Assertion error.
 * Call syserr with information about where something was wrong.
 * (Sorry, WHAT was wrong must be dug out of the core dump.)
 */

Visible Procedure
asserr(file, line)
	string file;
	int line;
{
	syserr("Assertion failed: file %s, line %d", file, line);
	/* NOTREACHED */
}
