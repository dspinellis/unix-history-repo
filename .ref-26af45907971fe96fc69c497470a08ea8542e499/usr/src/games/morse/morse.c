/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)morse.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>

static char
	*digit[] = {
	"-----",
	".----",
	"..---",
	"...--",
	"....-",
	".....",
	"-....",
	"--...",
	"---..",
	"----.",
},
	*alph[] = {
	".-",
	"-...",
	"-.-.",
	"-..",
	".",
	"..-.",
	"--.",
	"....",
	"..",
	".---",
	"-.-",
	".-..",
	"--",
	"-.",
	"---",
	".--.",
	"--.-",
	".-.",
	"...",
	"-",
	"..-",
	"...-",
	".--",
	"-..-",
	"-.--",
	"--..",
};

static int sflag;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register int ch;
	register char *p;

	while ((ch = getopt(argc, argv, "s")) != EOF)
		switch((char)ch) {
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: morse [string ...]");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		do {
			for (p = *argv; *p; ++p)
				morse((int)*p);
		} while (*++argv);
	else while ((ch = getchar()) != EOF)
		morse(ch);
}

morse(c)
	register int c;
{
	if (isalpha(c))
		show(alph[c - (isupper(c) ? 'A' : 'a')]);
	else if (isdigit(c))
		show(digit[c - '0']);
	else if (c == ',')
		show("--..--");
	else if (c == '.')
		show(".-.-.-");
	else if (isspace(c))
		show(" ...\n");
}

show(s)
	register char *s;
{
	if (sflag)
		printf(" %s", s);
	else for (; *s; ++s)
		printf(" %s", *s == '.' ? "dit" : "daw");
	printf(",\n");
}
