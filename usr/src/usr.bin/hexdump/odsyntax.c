/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)odsyntax.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include "hexdump.h"

int deprecated;

oldsyntax(argc, argvp)
	int argc;
	char ***argvp;
{
	extern enum _vflag vflag;
	extern FS *fshead;
	extern char *optarg;
	extern int length, optind;
	int ch, first;
	char **argv;

	deprecated = 1;
	first = 0;
	argv = *argvp;
	while ((ch = getopt(argc, argv, "aBbcDdeFfHhIiLlOoPpswvXx")) != EOF)
		switch (ch) {
		case 'a':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("16/1 \"%3_u \" \"\\n\"");
			break;
		case 'B':
		case 'o':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("8/2 \" %06o \" \"\\n\"");
			break;
		case 'b':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("16/1 \"%03o \" \"\\n\"");
			break;
		case 'c':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("16/1 \"%3_c \" \"\\n\"");
			break;
		case 'd':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("8/2 \"  %05u \" \"\\n\"");
			break;
		case 'D':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("4/4 \"     %010u \" \"\\n\"");
			break;
		case 'e':		/* undocumented in od */
		case 'F':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("2/8 \"          %21.14e \" \"\\n\"");
			break;
			
		case 'f':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("4/4 \" %14.7e \" \"\\n\"");
			break;
		case 'H':
		case 'X':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("4/4 \"       %08x \" \"\\n\"");
			break;
		case 'h':
		case 'x':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("8/2 \"   %04x \" \"\\n\"");
			break;
		case 'I':
		case 'L':
		case 'l':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("4/4 \"    %11d \" \"\\n\"");
			break;
		case 'i':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("8/2 \" %6d \" \"\\n\"");
			break;
		case 'O':
			if (!first++) {
				add("\"%07.7_Ao\n\"");
				add("\"%07.7_ao  \"");
			} else
				add("\"         \"");
			add("4/4 \"    %011o \" \"\\n\"");
			break;
		case 'v':
			vflag = ALL;
			break;
		case 'P':
		case 'p':
		case 's':
		case 'w':
		case '?':
		default:
			(void)fprintf(stderr,
			    "od: od(1) has been deprecated for hexdump(1).\n");
			if (ch != '?')
				(void)fprintf(stderr,
"od: hexdump(1) compatibility doesn't support the -%c option%s\n",
				    ch, ch == 's' ? "; see strings(1)." : ".");
			usage();
		}

	if (!fshead) {
		add("\"%07.7_Ao\n\"");
		add("\"%07.7_ao  \" 8/2 \"%06o \" \"\\n\"");
	}

	argc -= optind;
	*argvp += optind;

	odoffset(argc, argvp);
}

#define	ishexdigit(c) \
	(c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F')

odoffset(argc, argvp)
	int argc;
	char ***argvp;
{
	extern off_t skip;
	register char *num, *p;
	int base;
	char *end;

	/*
	 * The offset syntax of od(1) was genuinely bizarre.  First, if
	 * it started with a plus it had to be an offset.  Otherwise, if
	 * there were at least two arguments, a number or lower-case 'x'
	 * followed by a number makes it an offset.  By default it was
	 * octal; if it started with 'x' or '0x' it was hex.  If it ended
	 * in a '.', it was decimal.  If a 'b' or 'B' was appended, it
	 * multiplied the number by 512 or 1024 byte units.  There was
	 * no way to assign a block count to a hex offset.
	 *
	 * We assumes it's a file if the offset is bad.
	 */
	p = **argvp;
	if (*p != '+' && (argc < 2 ||
	    (!isdigit(p[0]) && (p[0] != 'x' || !ishexdigit(p[1])))))
		return;

	base = 0;
	/*
	 * skip over leading '+', 'x[0-9a-fA-f]' or '0x', and
	 * set base.
	 */
	if (p[0] == '+')
		++p;
	if (p[0] == 'x' && ishexdigit(p[1])) {
		++p;
		base = 16;
	} else if (p[0] == '0' && p[1] == 'x') {
		p += 2;
		base = 16;
	}

	/* skip over the number */
	if (base == 16)
		for (num = p; ishexdigit(*p); ++p);
	else
		for (num = p; isdigit(*p); ++p);

	/* check for no number */
	if (num == p)
		return;

	/* if terminates with a '.', base is decimal */
	if (*p == '.') {
		if (base)
			return;
		base = 10;
	}

	skip = strtol(num, &end, base ? base : 8);

	/* if end isn't the same as p, we got a non-octal digit */
	if (end != p)
		skip = 0;
	else {
		if (*p) {
			if (*p == 'b')
				skip *= 512;
			else if (*p == 'B')
				skip *= 1024;
			++p;
		}
		if (*p)
			skip = 0;
		else {
			++*argvp;
			/*
			 * If the offset uses a non-octal base, the base of
			 * the offset is changed as well.  This isn't pretty,
			 * but it's easy.
			 */
#define	TYPE_OFFSET	7
			if (base == 16) {
				fshead->nextfu->fmt[TYPE_OFFSET] = 'x';
				fshead->nextfs->nextfu->fmt[TYPE_OFFSET] = 'x';
			} else if (base == 10) {
				fshead->nextfu->fmt[TYPE_OFFSET] = 'd';
				fshead->nextfs->nextfu->fmt[TYPE_OFFSET] = 'd';
			}
		}
	}
}
