/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)chgbars.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Derived from original program by A. Dain Samples.
 * 
 * The program chgbars will accept the output from a diff comparison of
 * two versions of a file.  It will then read the new version of the file
 * and insert the appropriate troff commands to put change bars in the
 * right margin. Typing 'chgbars' without any arguments will give you
 * some documentation and an example.
 * 
 * Caveat: If you make a change inside an equation or table, the
 * preprocessors eqn and tbl may not like what chgbars does to the file.
 * You may have to go into the output from chgbars to remove or rearrange
 * some of the lines of the form '.mc |  \"open' or '.mc  \"close' in
 * order to get through tbl or eqn.
 * 
 * Unfortunately, users of RCS will be disappointed: one cannot use rcsdiff.
 * Rcsdiff compares the files in the wrong order.
 * 
 * There is a relatively easy way to do the job with the tools sed and awk.
 * However, sed does not allow enough commands to process large documents.
 * In the true spirit of a filter/tool, chgbars is limited only by the
 * amount of memory on the machine, and is fast and useful.
 * 
 * The modifications necessary are outlined:
 * 
 * FORM OF DIFF OUTPUT	COMMENT			NEW SED COMMANDS
 * ================================================================
 * 100a			means those lines	100a\\
 * L1			were deleted from	.mc |\\
 * L2			oldfile			.mc
 * :
 * Ln
 * .
 * ----------------------------------------------------------------
 * 100c			
 * L1		
 * L2	
 * : 
 * Ln						
 * .
 *	means that line 100 in newfile replaced all those lines in oldfile.
 *		100a
 *		.mc
 *		99 a
 *		.mc |
 * ----------------------------------------------------------------
 * 100d			that line was added	100a\\
 *			to oldfile		.mc
 *						99a\\
 *						.mc |
 * ----------------------------------------------------------------
 * 100,200d		those lines were	200a\\
 *			added to oldfile		.mc
 *						99a\\
 *						.mc |
 * ----------------------------------------------------------------
 * 100,200c		
 * L1		
 * L2	
 * :
 * Ln
 * .
 *	means lines 100 to 200 of newfile replaced all the following lines
 *	in oldfile.
 *		200a
 *		.mc
 *		99a
 *		.mc |
 * ----------------------------------------------------------------
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#define dbg(s)  /* fprintf(stderr,"s\n") */
#define none 0
#define open 1
#define close 2
#define both 3

char (*action)[];
FILE *file1, *file2;
char linebuf[1024];
char nextch;
int num1, num2, t, line;
char lineact;

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

main(argc, argv)
	int argc;
	char *argv[];
{
	register char  *p;
	register int    i;

	if (argc < 2)
		usage();
	/* open $1 */
	if (strcmp(argv[1], "-") == 0) {
		file1 = stdin;
		if (argc <= 2)
			usage();
	} else if ((file1 = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr, "error: can't open %s\n", argv[1]);
		exit(1);
	}
	/*
	 * read each entry setting the appropriate entry in action[]
	 *
	 * get the first line number: since diff -e puts the numbers out
	 * in reverse order, this tells us how big to make the file
	 */
	readline();
	if (lineact == 'a')
		t = num1;
	else
		t = num2;
	action = (char (*)[]) malloc(t + 1);
	for (p = (char *) action, i = 0; i < t; i++)
		*p++ = (char) none;
	while (!feof(file1)) {
		if (lineact == 'a') {
			(*action)[num1] = both;
			skiptilldot();
		} else {
			(*action)[num1 - 1] = open;
			(*action)[num2] = close;
			if (lineact == 'c')
				skiptilldot();
			else
				skiptilleol();
		}
		readline();
	}
	fclose(file1);
	/* open $2 */
	if (argc == 2)
		file2 = stdin;
	else
		file2 = fopen(argv[2], "r");
	if (file2 == NULL) {
		fprintf(stderr, "can't open %2\n", argv[2]);
		exit(1);
	}
	line = 0;
	while (!feof(file2)) {
		if (line != 0)
			fputs(linebuf, stdout);
		if (line <= t) {
			switch ((*action)[line]) {
			case open:
				printf(".mc |   \\\"open\n");
				break;
			case close:
				printf(".mc     \\\"close\n");
				break;
			case both:
				printf(".mc |   \\\"both\n");
				printf(".mc\n");
				break;
			default:;
			}
		}
		fgets(linebuf, sizeof(linebuf), file2);
		line++;
	}
	if (line <= t) {
		fprintf(stderr, "oops: number of lines read does not match\n");
		fprintf(stderr, "number of lines expected\n");
		exit(1);
	}
	exit(0);
}

usage()
{

	fprintf(stderr, "Usage:\n");
	fprintf(stderr, "\tchgbars diff.out\n");
	fprintf(stderr, "\t\t\treads the output from diff, and expects the\n");
	fprintf(stderr, "\t\t\tfile to be modified on stdin\n");
	fprintf(stderr, "\tchgbars diff.out file.tbm\n");
	fprintf(stderr, "\t\t\tboth the output from diff and the file to be\n");
	fprintf(stderr, "\t\t\tmodified are stated explicitly\n");
	fprintf(stderr, "\tchgbars - file.tbm\n");
	fprintf(stderr, "\t\t\treads the output from diff on stdin\n");
	fprintf(stderr, "\nE.g.\n");
	fprintf(stderr, "diff -b -e newfile oldfile | chgbars - newfile | vtroff -ms\n");
	fprintf(stderr, "\t(note the order of the files in the diff command!)\n");
	fprintf(stderr, "\n\nBe forewarned: chgbars does not know about tables or equations.\n");
	fprintf(stderr, "If any part of a table or equation is changed, chgbars will insert the\n");
	fprintf(stderr, ".mc commands, whether tbl or eqn likes it or not.\n");
	fprintf(stderr, "This means that you may have to do some hand editing of the output of\n");
	fprintf(stderr, "chgbars to make it acceptable to one or both of these preprocessors.\n");

	exit(1);
}

readnum()
{
	int num;

	dbg(readnum);
	num = 0;
	nextch = getc(file1);
	while (isdigit(nextch)) {
		num = num * 10 + nextch - '0';
		nextch = getc(file1);
	}
	return num;
}

readline()
{

	dbg(readline);
	num1 = readnum();
	if (nextch == ',')
		num2 = readnum();
	else
		num2 = num1;
	lineact = nextch;
}

skiptilleol()
{

	dbg(skiptilleol);
	while (nextch != '\n')
		nextch = getc(file1);
}

skiptilldot()
{

	dbg(skiptilldot);
	do {
		if (fgets(linebuf, sizeof(linebuf), file1) == NULL) {
			fprintf(stderr, "error reading file1\n");
			exit(1);
		}
	} while (strcmp(linebuf, ".\n") != 0);
}
