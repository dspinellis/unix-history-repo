/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char copyright[] =
"@(#) Copyright (c) 1980, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

/*
 * This program is described in detail in the "PXP 1.0 Implementation Notes"
 *
 * The structure of pxp is very similar to that of the translator pi.
 * The major new pieces here are a set of profile data maintenance
 * routines in the file pmon.c and a set of pretty printing utility
 * routines in the file pp.c.
 * The semantic routines of pi have been rewritten to do a simple
 * reformatting tree walk, the parsing and scanning remains
 * the same.
 *
 * This version does not place more than one statement per line and
 * is not very intelligent about folding long lines, with only
 * an ad hoc way of folding case label list and enumerated type
 * declarations being implemented.
 */

char	usagestr[] =
	"pxp [ -acdefjntuw_ ] [ -23456789 ] [ -z [ name ... ] ] name.p";
char	*howfile =	"/usr/lib/how_pxp";
char	stdoutn[20] =	"Standard output";

int	unit =	4;

FILE	*ibuf;
extern	char errout;

/*
 * Main program for pxp.
 * Process options, then call yymain
 * to do all the real work.
 */
FILE *ibp;
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	register c;

	if (argv[0][0] == 'a')
		howfile += 9;
	argc--, argv++;
	if (argc == 0) {
		execl("/bin/cat", "cat", howfile, 0);
		goto usage;
	}
	while (argc > 0) {
		cp = argv[0];
		if (*cp++ != '-')
			break;
		while (c = *cp++) switch (c) {
#ifdef DEBUG
			case 'T':
				typetest++;
				continue;
			case 'A':
				testtrace++;
			case 'F':
				fulltrace++;
			case 'E':
				errtrace++;
				continue;
			case 'C':
				yycosts();
				pexit(NOSTART);
			case 'U':
				yyunique++;
				continue;
#endif
			case 'a':
				all++;
				continue;
			case 'c':
				core++;
				continue;
			case 'd':
				nodecl++;
				continue;
			case 'e':
				noinclude = -1;
				continue;
			case 'f':
				full++;
				continue;
			case 'j':
				justify++;
				continue;
			case 'l':
			case 'n':
				togopt(c);
				continue;
			case 'o':
				onefile++;
				continue;
			case 's':
				stripcomm++;
				continue;
			case 't':
				table++;
				continue;
			case 'u':
			case 'w':
				togopt(c);
				continue;
			case 'z':
				profile++;
				pflist = argv + 1;
				pflstc = 0;
				while (argc > 1) {
					if (dotted(argv[1], 'p'))
						break;
					pflstc++, argc--, argv++;
				}
				if (pflstc == 0)
					togopt(c);
				else
					nojunk++;
				continue;
			case '_':
				underline++;
				continue;
#			ifdef RMOTHERS
			case 'O':
				rmothers++;
				continue;
#			endif RMOTHERS
			default:
				if (c >= '2' && c <= '9') {
					unit = c - '0';
					continue;
				}
usage:
				Perror("Usage", usagestr);
				exit(1);
		}
		argc--, argv++;
	}
	if (core && !profile && !table)
		profile++;
	if (argc == 0 || argc > 2)
		goto usage;
	if (profile || table) {
		noinclude = 0;
		if (argc == 2) {
			argc--;
			getit(argv[1]);
		} else
			getit(core ? "core" : "pmon.out");
	} else
		noinclude++;
	if (argc != 1)
		goto usage;
	firstname = filename = argv[0];
	if (dotted(filename, 'i')) {
		if (profile || table)
			goto usage;
		noinclude = 1;
		bracket++;
	} else if (!dotted(filename, 'p')) {
		Perror(filename, "Name must end in '.p'");
		exit(1);
	}
	if ((ibuf = fopen(filename, "r")) == NULL)
		perror(filename), pexit(NOSTART);
	ibp = ibuf;
	if (onefile) {
		int onintr();

		cp = strcpy(stdoutn, "/tmp/pxp00000") + 13;
		signal(2, onintr);
		for (c = getpid(); c; c /= 10)
			*--cp |= (c % 10);
		if (freopen(stdoutn, "w", stdout) == NULL)
bad:
			perror(stdoutn), exit(1);
	}
	if (profile || opt('l')) {
		opt('n')++;
		yysetfile(filename);
		opt('n')--;
	} else
		lastname = filename;
	errout = 2;
	yymain();
	/* No return */
}

/*
 * Put a header on a top of a page
 */
header()
{
	extern char version[];
	static char reenter;
	extern int outcol;

	gettime(filename);
	if (reenter) {
		if (outcol)
			putchar('\n');
		putchar('\f');
	}
	reenter++;
	if (profile || table) {
		printf("Berkeley Pascal PXP -- Version %s\n\n%s  %s\n\n",
			version, myctime(&tvec), filename);
		printf("Profiled %s\n\n", myctime(&ptvec));
	}
}

char	ugh[] =	"Fatal error in pxp\n";
/*
 * Exit from the Pascal system.
 * We throw in an ungraceful termination
 * message if c > 1 indicating a severe
 * error such as running out of memory
 * or an internal inconsistency.
 */
pexit(c)
	int c;
{
	register char *cp;
	extern int outcol;

	if (stdoutn[0] == '/')
		unlink(stdoutn);
	if (outcol)
		putchar('\n');
	flush();
	if (c == DIED)
		write(2, ugh, sizeof ugh);
	exit(c);
}

onintr()
{

	pexit(DIED);
}

puthedr()
{

	yysetfile(filename);
}
