/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Case Larsen.
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
static char sccsid[] = "@(#)comm.c	5.5 (Berkeley) 6/25/90";
#endif /* not lint */

#include <sys/file.h>
#include <limits.h>
#include <stdio.h>

#define	MAXLINELEN	(_BSD_LINE_MAX + 1)

char *tabs[] = { "", "\t", "\t\t" };

main(argc,argv)
	int argc;
	char *argv[];
{
	register int comp, file1done, file2done, read1, read2;
	register char *col1, *col2, *col3;
	int ch, flag1, flag2, flag3;
	FILE *fp1, *fp2, *file();
	char **p, line1[MAXLINELEN], line2[MAXLINELEN];
	extern int optind;

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
	if (flag1)
		col1 = *p++;
	if (flag2)
		col2 = *p++;
	if (flag3)
		col3 = *p++;

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
		return(stdin);
	if (!(fp = fopen(name, "r"))) {
		(void)fprintf(stderr, "comm: can't read %s.\n", name);
		exit(1);
	}
	return(fp);
}

usage()
{
	(void)fprintf(stderr, "usage: comm [-123] [ - ] file1 file2\n");
	exit(1);
}
