/*
 * Copyright (c) 1980, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)from.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <ctype.h>
#include <pwd.h>
#include <stdio.h>
#include <paths.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	struct passwd *pwd, *getpwuid();
	int ch, newline;
	char *file, *sender, *p;
#if MAXPATHLEN > BUFSIZ
	char buf[MAXPATHLEN];
#else
	char buf[BUFSIZ];
#endif

	file = sender = NULL;
	while ((ch = getopt(argc, argv, "f:s:")) != EOF)
		switch((char)ch) {
		case 'f':
			file = optarg;
			break;
		case 's':
			sender = optarg;
			for (p = sender; *p; ++p)
				if (isupper(*p))
					*p = tolower(*p);
			break;
		case '?':
		default:
			fprintf(stderr, "usage: from [-f file] [-s sender] [user]\n");
			exit(1);
		}
	argv += optind;

	if (!file) {
		if (!(file = *argv)) {
			if (!(pwd = getpwuid(getuid()))) {
				fprintf(stderr,
				    "from: no password file entry for you.\n");
				exit(1);
			}
			file = pwd->pw_name;
		}
		(void)sprintf(buf, "%s/%s", _PATH_MAILDIR, file);
		file = buf;
	}
	if (!freopen(file, "r", stdin)) {
		fprintf(stderr, "from: can't read %s.\n", file);
		exit(1);
	}
	for (newline = 1; fgets(buf, sizeof(buf), stdin);) {
		if (*buf == '\n') {
			newline = 1;
			continue;
		}
		if (newline && !strncmp(buf, "From ", 5) &&
		    (!sender || match(buf + 5, sender)))
			printf("%s", buf);
		newline = 0;
	}
	exit(0);
}

match(line, sender)
	register char *line, *sender;
{
	register char ch, pch, first, *p, *t;

	for (first = *sender++;;) {
		if (isspace(ch = *line))
			return(0);
		++line;
		if (isupper(ch))
			ch = tolower(ch);
		if (ch != first)
			continue;
		for (p = sender, t = line;;) {
			if (!(pch = *p++))
				return(1);
			if (isupper(ch = *t++))
				ch = tolower(ch);
			if (ch != pch)
				break;
		}
	}
	/* NOTREACHED */
}
