/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hexsyntax.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "hexdump.h"

off_t skip;				/* bytes to skip */

newsyntax(argc, argvp)
	int argc;
	char ***argvp;
{
	extern enum _vflag vflag;
	extern FS *fshead;
	extern char *optarg;
	extern int length, optind;
	int ch;
	char *p, **argv;

	argv = *argvp;
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
			add("\"%07.7_ax \" 8/2 \"  %05u \" \"\\n\"");
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
			add("\"%07.7_ax \" 8/2 \" %06o \" \"\\n\"");
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
			add("\"%07.7_ax \" 8/2 \"   %04x \" \"\\n\"");
			break;
		case '?':
			usage();
			exit(1);
		}

	if (!fshead) {
		add("\"%07.7_Ax\n\"");
		add("\"%07.7_ax \" 8/2 \"%04x \" \"\\n\"");
	}

	*argvp += optind;
}

usage()
{
	(void)fprintf(stderr,
"hexdump: [-bcdovx] [-e fmt] [-f fmt_file] [-n length] [-s skip] [file ...]\n");
	exit(1);
}
