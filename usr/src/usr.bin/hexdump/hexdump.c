/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)hexdump.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "hexdump.h"

enum _vflag vflag = FIRST;		/* display duplicate lines */
FS *fshead;				/* head of format strings */
off_t skip;				/* bytes to skip */
int blocksize;				/* data block size */
int exitval;				/* final exit value */
int length;				/* max bytes to read */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	extern char *optarg;
	register FS *tfs;
	int ch;
	char *p, *rindex();

	length = -1;
	while ((ch = getopt(argc, argv, "bcde:f:n:os:vx")) != EOF)
		switch (ch) {
		case 'b':
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 16/1 \"%03o \" \"\\n\"");
			break;
		case 'c':
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 16/1 \"%3_c \" \"\\n\"");
			break;
		case 'd':
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 8/2 \"%05u \" \"\\n\"");
			break;
		case 'e':
			add(optarg);
			break;
		case 'f':
			addfile(optarg);
			break;
		case 'n':
			if ((length = atoi(optarg)) < 0) {
				(void)fprintf(stderr,
				    "hexdump: bad length value.\n");
				exit(1);
			}
			break;
		case 'o':
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 8/2 \"%06o \" \"\\n\"");
			break;
		case 's':
			if ((skip = strtol(optarg, &p, 0)) < 0) {
				(void)fprintf(stderr,
				    "hexdump: bad skip value.\n");
				exit(1);
			}
			switch(*p) {
			case 'b':
				skip *= 512;
				break;
			case 'k':
				skip *= 1024;
				break;
			case 'm':
				skip *= 1048576;
				break;
			}
			break;
		case 'v':
			vflag = ALL;
			break;
		case 'x':
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 8/2 \"%04x \" \"\\n\"");
			break;
		case '?':
			usage();
			exit(1);
		}

	if (!fshead) {
		p = rindex(argv[0], 'o');
		if (p && !strcmp(p, "od")) {
			add("\"%07.7_Ao\n\"");
			add("\"%07.7_ao  \" 8/2 \"%06o \" \"\\n\"");
		} else {
			add("\"%07.7_Ax\n\"");
			add("\"%07.7_ax \" 8/2 \"%04x \" \"\\n\"");
		}
	}

	argv += optind;
	argc -= optind;

	/* figure out the data block size */
	for (blocksize = 0, tfs = fshead; tfs; tfs = tfs->nextfs) {
		tfs->bcnt = size(tfs);
		if (blocksize < tfs->bcnt)
			blocksize = tfs->bcnt;
	}
	/* rewrite the rules, do syntax checking */
	for (tfs = fshead; tfs; tfs = tfs->nextfs)
		rewrite(tfs);

	(void)next(argv);
	display();
	exit(exitval);
}

usage()
{
	(void)fprintf(stderr,
"hexdump: [-bcdovx] [-e format] [-f file] [-n length] [-s skip] [file ...]\n");
	exit(1);
}
