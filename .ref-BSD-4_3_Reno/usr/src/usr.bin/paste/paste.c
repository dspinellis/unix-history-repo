/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam S. Moskowitz of Menlo Consulting.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)paste.c	5.6 (Berkeley) 6/24/90";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

char *delim;
int delimcnt;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch, seq;

	seq = 0;
	while ((ch = getopt(argc, argv, "d:s")) != EOF)
		switch(ch) {
		case 'd':
			delimcnt = tr(delim = optarg);
			break;
		case 's':
			seq = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (!delim) {
		delimcnt = 1;
		delim = "\t";
	}

	if (seq)
		sequential(argv);
	else
		parallel(argv);
	exit(0);
}

typedef struct _list {
	struct _list *next;
	FILE *fp;
	int cnt;
	char *name;
} LIST;

parallel(argv)
	char **argv;
{
	register LIST *lp;
	register int cnt;
	register char ch, *p;
	LIST *head, *tmp;
	int opencnt, output;
	char buf[_BSD_LINE_MAX + 1], *malloc();

	for (cnt = 0, head = NULL; p = *argv; ++argv, ++cnt) {
		if (!(lp = (LIST *)malloc((u_int)sizeof(LIST)))) {
			(void)fprintf(stderr, "paste: %s.\n", strerror(ENOMEM));
			exit(1);
		}
		if (p[0] == '-' && !p[1])
			lp->fp = stdin;
		else if (!(lp->fp = fopen(p, "r"))) {
			(void)fprintf(stderr, "paste: %s: %s.\n", p,
			    strerror(errno));
			exit(1);
		}
		lp->next = NULL;
		lp->cnt = cnt;
		lp->name = p;
		if (!head)
			head = tmp = lp;
		else {
			tmp->next = lp;
			tmp = lp;
		}
	}

	for (opencnt = cnt; opencnt;) {
		for (output = 0, lp = head; lp; lp = lp->next) {
			if (!lp->fp) {
				if (output && lp->cnt &&
				    (ch = delim[(lp->cnt - 1) % delimcnt]))
					putchar(ch);
				continue;
			}
			if (!fgets(buf, sizeof(buf), lp->fp)) {
				if (!--opencnt)
					break;
				lp->fp = NULL;
				if (output && lp->cnt &&
				    (ch = delim[(lp->cnt - 1) % delimcnt]))
					putchar(ch);
				continue;
			}
			if (!(p = index(buf, '\n'))) {
				(void)fprintf(stderr,
				    "paste: %s: input line too long.\n",
				    lp->name);
				exit(1);
			}
			*p = '\0';
			/*
			 * make sure that we don't print any delimiters
			 * unless there's a non-empty file.
			 */
			if (!output) {
				output = 1;
				for (cnt = 0; cnt < lp->cnt; ++cnt)
					if (ch = delim[cnt % delimcnt])
						putchar(ch);
			} else if (ch = delim[(lp->cnt - 1) % delimcnt])
				putchar(ch);
			(void)printf("%s", buf);
		}
		if (output)
			putchar('\n');
	}
}

sequential(argv)
	char **argv;
{
	register FILE *fp;
	register int cnt;
	register char ch, *p, *dp;
	char buf[_BSD_LINE_MAX + 1];

	for (; p = *argv; ++argv) {
		if (p[0] == '-' && !p[1])
			fp = stdin;
		else if (!(fp = fopen(p, "r"))) {
			(void)fprintf(stderr, "paste: %s: %s.\n", p,
			    strerror(errno));
			continue;
		}
		if (fgets(buf, sizeof(buf), fp)) {
			for (cnt = 0, dp = delim;;) {
				if (!(p = index(buf, '\n'))) {
					(void)fprintf(stderr,
					    "paste: %s: input line too long.\n",
					    *argv);
					exit(1);
				}
				*p = '\0';
				(void)printf("%s", buf);
				if (!fgets(buf, sizeof(buf), fp))
					break;
				if (ch = *dp++)
					putchar(ch);
				if (++cnt == delimcnt) {
					dp = delim;
					cnt = 0;
				}
			}
			putchar('\n');
		}
		if (fp != stdin)
			(void)fclose(fp);
	}
}

tr(arg)
	char *arg;
{
	register int cnt;
	register char ch, *p;

	for (p = arg, cnt = 0; (ch = *p++); ++arg, ++cnt)
		if (ch == '\\')
			switch(ch = *p++) {
			case 'n':
				*arg = '\n';
				break;
			case 't':
				*arg = '\t';
				break;
			case '0':
				*arg = '\0';
				break;
			default:
				*arg = ch;
				break;
		} else
			*arg = ch;

	if (!cnt) {
		(void)fprintf(stderr, "paste: no delimiters specified.\n");
		exit(1);
	}
	return(cnt);
}

usage()
{
	(void)fprintf(stderr, "paste: [-s] [-d delimiters] file ...\n");
	exit(1);
}
