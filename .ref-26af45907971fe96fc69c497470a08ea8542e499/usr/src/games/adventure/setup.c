/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jim Gillogly at The Rand Corporation.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)setup.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Setup: keep the structure of the original Adventure port, but use an
 * internal copy of the data file, serving as a sort of virtual disk.  It's
 * lightly encrypted to prevent casual snooping of the executable.
 *
 * Also do appropriate things to tabs so that bogus editors will do the right
 * thing with the data file.
 *
 */

#define SIG1 " *      Jim Gillogly"
#define SIG2 " *      Sterday, 6 Thrimidge S.R. 1993, 15:24"

#include <stdio.h>
#include "hdr.h"        /* SEED lives in there; keep them coordinated. */

#define USAGE "Usage: setup file > data.c (file is typically glorkz)\n"

#define YES 1
#define NO  0

void fatal();

#define LINE 10         /* How many values do we get on a line? */

main(argc, argv)
int argc;
char *argv[];
{
	FILE *infile;
	int c, count, linestart;

	if (argc != 2) fatal(USAGE);

	if ((infile = fopen(argv[1], "r")) == NULL)
		fatal("Can't read file %s.\n", argv[1]);
	puts("/*\n * data.c: created by setup from the ascii data file.");
	puts(SIG1);
	puts(SIG2);
	puts(" */");
	printf("\n\nchar data_file[] =\n{");
	srandom(SEED);
	count = 0;
	linestart = YES;

	while ((c = getc(infile)) != EOF)
	{
		if (linestart && c == ' ') /* Convert first spaces to tab */
		{
			printf("0x%02x,", ('\t' ^ random()) & 0xFF);
			while ((c = getc(infile)) == ' ' && c != EOF);
			/* Drop the non-whitespace character through */
			linestart = NO;
		}
		switch(c)
		{
		    case '\t':
			linestart = NO; /* Don't need to convert spaces */
			break;
		    case '\n':
			linestart = YES; /* Ready to convert spaces again */
			break;
		}
		if (count++ % LINE == 0)   /* Finished a line? */
			printf("\n\t");
		printf("0x%02x,", (c ^ random()) & 0xFF);
	}
	puts("\n\t0\n};");
	fclose(infile);
	exit(0);
}


void fatal(format, arg)
char *format;
{
	fprintf(stderr, format, arg);
	exit(1);
}
