/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ar.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/errno.h>
#include <dirent.h>
#include <stdio.h>
#include <ar.h>
#include <string.h>
#include <stdlib.h>
#include <paths.h>
#include "archive.h"
#include "extern.h"

CHDR chdr;
u_int options;
char *archive, *envtmp, *posarg, *posname;
static void badoptions(), usage();

/*
 * main --
 *	main basically uses getopt to parse options and calls the appropriate
 *	functions.  Some hacks that let us be backward compatible with 4.3 ar
 *	option parsing and sanity checking.
 */
main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	int c;
	char *p;
	int (*fcall)(), append(), contents(), delete(), extract(),
	    move(), print(), replace();

	if (argc < 3)
		usage();

	/*
	 * Historic versions didn't require a '-' in front of the options.
	 * Fix it, if necessary.
	*/
	if (*argv[1] != '-') {
		if (!(p = malloc((u_int)(strlen(argv[1]) + 2)))) {
			(void)fprintf(stderr, "ar: %s.\n", strerror(errno));
			exit(1);
		}
		*p = '-';
		(void)strcpy(p + 1, argv[1]);
		argv[1] = p;
	}

	while ((c = getopt(argc, argv, "abcdilmopqrTtuvx")) != EOF) {
		switch(c) {
		case 'a':
			options |= AR_A;
			break;
		case 'b':
		case 'i':
			options |= AR_B;
			break;
		case 'c':
			options |= AR_C;
			break;
		case 'd':
			options |= AR_D;
			fcall = delete;
			break;
		case 'l':		/* not documented, compatibility only */
			envtmp = ".";
			break;
		case 'm':
			options |= AR_M;
			fcall = move;
			break;
		case 'o':
			options |= AR_O;
			break;
		case 'p':
			options |= AR_P;
			fcall = print;
			break;
		case 'q':
			options |= AR_Q;
			fcall = append;
			break;
		case 'r':
			options |= AR_R;
			fcall = replace;
			break;
		case 'T':
			options |= AR_TR;
			break;
		case 't':
			options |= AR_T;
			fcall = contents;
			break;
		case 'u':
			options |= AR_U;
			break;
		case 'v':
			options |= AR_V;
			break;
		case 'x':
			options |= AR_X;
			fcall = extract;
			break;
		default:
			usage();
		}
	}

	argv += optind;
	argc -= optind;

	/* One of -dmpqrtx required. */
	if (!(options & (AR_D|AR_M|AR_P|AR_Q|AR_R|AR_T|AR_X))) {
		(void)fprintf(stderr,
		    "ar: one of options -dmpqrtx is required.\n");
		usage();
	}
	/* Only one of -a and -bi allowed. */
	if (options & AR_A && options & AR_B) {
		(void)fprintf(stderr,
		    "ar: only one of -a and -[bi] options allowed.\n");
		usage();
	}
	/* -ab require a position argument. */
	if (options & (AR_A|AR_B)) {
		if (!(posarg = *argv++)) {
			(void)fprintf(stderr,
			    "ar: no position operand specified.\n");
			usage();
		}
		posname = rname(posarg);
	}
	/* -d only valid with -Tv. */
	if (options & AR_D && options & ~(AR_D|AR_TR|AR_V))
		badoptions("-d");
	/* -m only valid with -abiTv. */
	if (options & AR_M && options & ~(AR_A|AR_B|AR_M|AR_TR|AR_V))
		badoptions("-m");
	/* -p only valid with -Tv. */
	if (options & AR_P && options & ~(AR_P|AR_TR|AR_V))
		badoptions("-p");
	/* -q only valid with -cTv. */
	if (options & AR_Q && options & ~(AR_C|AR_Q|AR_TR|AR_V))
		badoptions("-q");
	/* -r only valid with -abcuTv. */
	if (options & AR_R && options & ~(AR_A|AR_B|AR_C|AR_R|AR_U|AR_TR|AR_V))
		badoptions("-r");
	/* -t only valid with -Tv. */
	if (options & AR_T && options & ~(AR_T|AR_TR|AR_V))
		badoptions("-t");
	/* -x only valid with -ouTv. */
	if (options & AR_X && options & ~(AR_O|AR_U|AR_TR|AR_V|AR_X))
		badoptions("-x");

	if (!(archive = *argv++)) {
		(void)fprintf(stderr, "ar: no archive specified.\n");
		usage();
	}

	/* -dmqr require a list of archive elements. */
	if (options & (AR_D|AR_M|AR_Q|AR_R) && !*argv) {
		(void)fprintf(stderr, "ar: no archive members specified.\n");
		usage();
	}

	exit((*fcall)(argv));
}

static void
badoptions(arg)
	char *arg;
{
	(void)fprintf(stderr,
	    "ar: illegal option combination for %s.\n", arg);
	usage();
}

static void
usage()
{
	(void)fprintf(stderr, "usage:  ar -d [-Tv] archive file ...\n");
	(void)fprintf(stderr, "\tar -m [-Tv] archive file ...\n");
	(void)fprintf(stderr, "\tar -m [-abiTv] position archive file ...\n");
	(void)fprintf(stderr, "\tar -p [-Tv] archive [file ...]\n");
	(void)fprintf(stderr, "\tar -q [-cTv] archive file ...\n");
	(void)fprintf(stderr, "\tar -r [-cuTv] archive file ...\n");
	(void)fprintf(stderr, "\tar -r [-abciuTv] position archive file ...\n");
	(void)fprintf(stderr, "\tar -t [-Tv] archive [file ...]\n");
	(void)fprintf(stderr, "\tar -x [-ouTv] archive [file ...]\n");
	exit(1);
}	
