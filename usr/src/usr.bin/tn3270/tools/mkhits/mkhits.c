/*
 *	Copyright (c) 1984-1987 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)mkhits.c	3.1 (Berkeley) %G%";
#endif	/* not lint */

/*
 * This program scans a file which describes a keyboard.  The output
 * of the program is a series of 'C' declarations which describe a
 * mapping between (scancode, shiftstate, altstate) and 3270 functions,
 * characters, and AIDs.
 *
 * The format of the input file is as follows:
 *
 * keynumber [ scancode [ unshifted [ shifted [ alted [ shiftalted ] ] ] ] ]
 *
 * keynumber is in decimal, and starts in column 1.
 * scancode is hexadecimal.
 * unshifted, etc. - these are either a single ascii character,
 *			or the name of a function or an AID-generating key.
 *
 * all fields are separated by a single space.
 */

#include <stdio.h>
#if	defined(unix)
#include <strings.h>
#else	/* defined(unix) */
#include <string.h>
#endif	/* defined(unix) */
#include <ctype.h>
#include "../ctlr/function.h"

#include "dohits.h"


int
main(argc, argv)
int	argc;
char	*argv[];
{
    int scancode;
    int empty;
    int i;
    struct hits *ph;
    struct Hits *Ph;
    char *aidfile = 0, *fcnfile = 0;

    if (argc > 1) {
	if (argv[1][0] != '-') {
	    aidfile = argv[1];
	}
    }
    if (argc > 2) {
	if (argv[2][0] != '-') {
	    fcnfile = argv[2];
	}
    }

    dohits(aidfile, fcnfile);		/* Set up "Hits" */

    printf("struct hits hits[] = {\n");
    empty = 0;
    scancode = -1;
    for (Ph = Hits; Ph < Hits+(sizeof Hits/sizeof Hits[0]); Ph++) {
	ph = &Ph->hits;
	scancode++;
	if ((ph->hit[0].ctlrfcn == undefined)
		&& (ph->hit[1].ctlrfcn == undefined)
		&& (ph->hit[2].ctlrfcn == undefined)
		&& (ph->hit[3].ctlrfcn == undefined)) {
	    empty++;
	    continue;
	} else {
	    while (empty) {
		printf("\t{ 0, {  {undefined}, {undefined}");
		printf(", {undefined}, {undefined}  } },\n");
		empty--;
	    }
	}
	printf("\t{ %d, {\t/* 0x%02x */\n\t", ph->keynumber, scancode);
	for (i = 0; i < 4; i++) {
	    printf("\t{ ");
	    switch (ph->hit[i].ctlrfcn) {
	    case undefined:
		printf("undefined");
		break;
	    case FCN_CHARACTER:
		printf("FCN_CHARACTER, 0x%02x", ph->hit[i].code);
		break;
	    case FCN_AID:
		printf("FCN_AID, %s", Ph->name[i]);
		break;
	    case FCN_NULL:
	    default:
		if ((Ph->name[i] != 0)
				    && (strcmp(Ph->name[i], "FCN_NULL") != 0)) {
		    printf("%s", Ph->name[i]);
		} else {
		    printf("undefined");
		}
		break;
	    }
	    printf(" },\n\t");
	}
	printf("} },\n");
    }
    printf("};\n");
    return 0;
}
