/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Case Larsen.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)uniq.c	5.2 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>

int cflag, dflag, uflag;
int numchars, numfields, repeats;

#define	MAXLINELEN	(2048 + 1)

main (argc,argv)
	int argc;
	char **argv;
{
	extern int optind;
	FILE *ifp, *ofp, *file();
	int ch;
	register char *t1, *t2;
	char *prevline, *thisline, *malloc(), *skip();

	while ((ch = getopt(argc, argv, "-cdu123456789")) != EOF)
		switch (ch) {
		case '-':
			--optind;
			goto done;
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'u':
			uflag = 1;
			break;
		/*
		 * since -n is a valid option that could be picked up by
		 * getopt, but is better handled by the +n and -n code, we
		 * break out.
		 */
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			--optind;
			goto done;
		case '?':
		default:
			usage();
	}

done:	argc -= optind;
	argv +=optind;

	/* if no flags are set, default is -d -u */
	if (cflag) {
		if (dflag || uflag)
			usage();
	} else if (!dflag && !uflag)
		dflag = uflag = 1;

	/* because of the +, getopt is messed up */
	for (; **argv == '+' || **argv == '-'; ++argv, --argc)
		switch (**argv) {
		case '+':
			if ((numchars = atoi(*argv + 1)) < 0)
				goto negerr;
			break;
		case '-':
			if ((numfields = atoi(*argv + 1)) < 0) {
negerr:				(void)fprintf(stderr,
				    "uniq: negative field/char skip value.\n");
				usage();
			}
			break;
		}
    
	switch(argc) {
	case 0:
		ifp = stdin;
		ofp = stdout;
		break;
	case 1:
		ifp = file(argv[0], "r");
		ofp = stdout;
		break;
	case 2:
		ifp = file(argv[0], "r");
		ofp = file(argv[1], "w");
		break;
	default:
		usage();
	}

	prevline = malloc(MAXLINELEN);
	thisline = malloc(MAXLINELEN);
	(void)fgets(prevline, MAXLINELEN, ifp);

	while (fgets(thisline, MAXLINELEN, ifp)) {
		/* if requested get the chosen fields + character offsets */
		if (numfields || numchars) {
			t1 = skip(thisline);
			t2 = skip(prevline);
		} else {
			t1 = thisline;
			t2 = prevline;
		}

		/* if different, print; set previous to new value */
		if (strcmp(t1, t2)) {
			show(ofp, prevline);
			t1 = prevline;
			prevline = thisline;
			thisline = t1;
			repeats = 0;
		}
		else
			++repeats;
	}
	show(ofp, prevline);
	exit(0);
}

/*
 * show --
 *	output a line depending on the flags and number of repetitions
 *	of the line.
 */
show(ofp, str)
	FILE *ofp;
	char *str;
{
	if (cflag)
		(void)fprintf(ofp, "%4d %s", repeats + 1, str);
	if (dflag && repeats || uflag && !repeats)
		(void)fprintf(ofp, "%s", str);
}

char *
skip(str)
	register char *str;
{
	register int infield, nchars, nfields;

	for (nfields = numfields, infield = 0; nfields && *str; ++str)
		if (isspace(*str)) {
			if (infield) {
				infield = 0;
				--nfields;
			}
		} else if (!infield)
			infield = 1;
	for (nchars = numchars; nchars-- && *str; ++str);
	return(str);
}

FILE *
file(name, mode)
	char *name, *mode;
{
	FILE *fp;

	if (!(fp = fopen(name, mode))) {
		(void)fprintf(stderr, "uniq: can't open %s.\n", name);
		exit(1);
	}
	return(fp);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: uniq [-c | -du] [- #fields] [+ #chars] [input [output]]\n");
	exit(1);
}
