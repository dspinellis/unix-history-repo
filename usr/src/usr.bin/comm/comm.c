/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Case Larsen.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)comm.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <fcntl.h>
#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define	MAXLINELEN	(LINE_MAX + 1)

char *tabs[] = { "", "\t", "\t\t" };

FILE   *file __P((char *));
void	show __P((FILE *, char *, char *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int comp, file1done, file2done, read1, read2;
	int ch, flag1, flag2, flag3;
	FILE *fp1, *fp2;
	char *col1, *col2, *col3;
	char **p, line1[MAXLINELEN], line2[MAXLINELEN];

	flag1 = flag2 = flag3 = 1;
	while ((ch = getopt(argc, argv, "-123")) != EOF)
		switch(ch) {
		case '-':
			--optind;
			goto done;
		case '1':
			flag1 = 0;
			break;
		case '2':
			flag2 = 0;
			break;
		case '3':
			flag3 = 0;
			break;
		case '?':
		default:
			usage();
		}
done:	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	fp1 = file(argv[0]);
	fp2 = file(argv[1]);

	/* for each column printed, add another tab offset */
	p = tabs;
	col1 = col2 = col3 = NULL;
	if (flag1)
		col1 = *p++;
	if (flag2)
		col2 = *p++;
	if (flag3)
		col3 = *p;

	for (read1 = read2 = 1;;) {
		/* read next line, check for EOF */
		if (read1)
			file1done = !fgets(line1, MAXLINELEN, fp1);
		if (read2)
			file2done = !fgets(line2, MAXLINELEN, fp2);

		/* if one file done, display the rest of the other file */
		if (file1done) {
			if (!file2done && col2)
				show(fp2, col2, line2);
			break;
		}
		if (file2done) {
			if (!file1done && col1)
				show(fp1, col1, line1);
			break;
		}

		/* lines are the same */
		if (!(comp = strcmp(line1, line2))) {
			read1 = read2 = 1;
			if (col3)
				(void)printf("%s%s", col3, line1);
			continue;
		}

		/* lines are different */
		if (comp < 0) {
			read1 = 1;
			read2 = 0;
			if (col1)
				(void)printf("%s%s", col1, line1);
		} else {
			read1 = 0;
			read2 = 1;
			if (col2)
				(void)printf("%s%s", col2, line2);
		}
	}
	exit(0);
}

void
show(fp, offset, buf)
	FILE *fp;
	char *offset, *buf;
{

	do {
		(void)printf("%s%s", offset, buf);
	} while (fgets(buf, MAXLINELEN, fp));
}

FILE *
file(name)
	char *name;
{
	FILE *fp;

	if (!strcmp(name, "-"))
		return (stdin);
	if ((fp = fopen(name, "r")) == NULL) {
		(void)fprintf(stderr, "comm: %s: %s\n", name, strerror(errno));
		exit(1);
	}
	return (fp);
}

void
usage()
{

	(void)fprintf(stderr, "usage: comm [-123] file1 file2\n");
	exit(1);
}
