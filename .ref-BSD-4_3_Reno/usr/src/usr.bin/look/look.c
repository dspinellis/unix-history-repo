/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)look.c	4.7 (Berkeley) 5/11/89";
#endif not lint

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

#define	EOS		'\0'
#define	MAXLINELEN	250
#define	YES		1

static int	fold, dict, len;

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	static char	*filename = _PATH_WORDS;
	register off_t	bot, mid, top;
	register int	c;
	struct stat	sb;
	char	entry[MAXLINELEN], copy[MAXLINELEN];

	while ((c = getopt(argc, argv, "df")) != EOF)
		switch((char)c) {
		case 'd':
			dict = YES;
			break;
		case 'f':
			fold = YES;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	switch(argc) {
	case 1:	/* if default file, set to dictionary order and folding */
		dict = fold = YES;
		break;
	case 2:
		filename = argv[1];
		break;
	default:
		usage();
	}

	if (!freopen(filename, "r", stdin)) {
		fprintf(stderr,"look: can't read %s.\n", filename);
		exit(2);
	}
	if (fstat(fileno(stdin), &sb)) {
		perror("look: fstat");
		exit(2);
	}

	len = strlen(*argv);
	canon(*argv, *argv);
	len = strlen(*argv);		/* may have changed */
	if (len > MAXLINELEN - 1) {
		fputs("look: search string is too long.\n", stderr);
		exit(2);
	}

	for (bot = 0, top = sb.st_size;;) {
		mid = (top + bot) / 2;
		(void)fseek(stdin, mid, L_SET);

		for (++mid; (c = getchar()) != EOF && c != '\n'; ++mid);
		if (!getline(entry))
			break;
		canon(entry, copy);
		if (strncmp(*argv, copy, len) <= 0) {
			if (top <= mid)
				break;
			top = mid;
		}
		else
			bot = mid;
	}
	(void)fseek(stdin, bot, L_SET);
	while (ftell(stdin) < top) {
		register int val;

		if (!getline(entry))
			exit(0);
		canon(entry, copy);
		if (!(val = strncmp(*argv, copy, len))) {
			puts(entry);
			break;
		}
		if (val < 0)
			exit(0);
	}
	while (getline(entry)) {
		canon(entry, copy);
		if (strncmp(*argv, copy, len))
			break;
		puts(entry);
	}
	exit(0);
}

/*
 * getline --
 *	get a line
 */
static
getline(buf)
	register char	*buf;
{
	register int	c;

	for (;;) {
		if ((c = getchar()) == EOF)
			return(0);
		if (c == '\n')
			break;
		*buf++ = c;
	}
	*buf = EOS;
	return(1);
}

/*
 * canon --
 *	create canonical version of word
 */
static
canon(src, copy)
	register char	*src, *copy;
{
	register int	cnt;
	register char	c;

	for (cnt = len + 1; (c = *src++) && cnt; --cnt)
		if (!dict || isalnum(c))
			*copy++ = fold && isupper(c) ? tolower(c) : c;
	*copy = EOS;
}

/*
 * usage --
 *	print a usage message and die
 */
static
usage()
{
	fputs("usage: look [-df] string [file]\n", stderr);
	exit(1);
}
