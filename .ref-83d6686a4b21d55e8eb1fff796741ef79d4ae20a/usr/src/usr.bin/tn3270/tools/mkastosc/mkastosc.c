/*
 * Copyright (c) 1988 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkastosc.c	4.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#if	defined(unix)
#include <strings.h>
#else	/* defined(unix) */
#include <string.h>
#endif	/* defined(unix) */
#include <ctype.h>

#include "../general/general.h"
#include "../ctlr/function.h"

#include "dohits.h"

static struct tbl {
    unsigned char
	scancode,
	used;
    char
	*shiftstate;
} tbl[128];

int
main(argc, argv)
int	argc;
char	*argv[];
{
    int scancode;
    int asciicode;
    int empty;
    int i;
    int c;
    int found;
    struct hits *ph;
    struct Hits *Ph;
    struct thing *this;
    struct thing **attable;
    struct tbl *Pt;
    static char *shiftof[] =
	    { "0", "SHIFT_UPSHIFT", "SHIFT_ALT", "SHIFT_ALT|SHIFT_UPSHIFT" };
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

    printf("/*\n");
    printf(" * Ascii to scancode conversion table.  First\n");
    printf(" * 128 bytes (0-127) correspond with actual Ascii\n");
    printf(" * characters; the rest are functions from ctrl/function.h\n");
    printf(" */\n");
    /* Build the ascii part of the table. */
    for (Ph = Hits, scancode = 0; Ph <= Hits+highestof(Hits);
							Ph++, scancode++) {
	ph = &Ph->hits;
	for (i = 0; i < 4; i++) {
	    if (ph->hit[i].ctlrfcn == FCN_CHARACTER) {
		c = Ph->name[i][0];	/* "name" of this one */
		if (tbl[c].used == 0) {
		    tbl[c].used = 1;
		    tbl[c].shiftstate = shiftof[i];
		    tbl[c].scancode = scancode;
		}
	    }
	}
    }
    /* Now, output the table */
    for (Pt = tbl, asciicode = 0; Pt <= tbl+highestof(tbl); Pt++, asciicode++) {
	if (Pt->used == 0) {
	    if (isprint(asciicode) && (asciicode != ' ')) {
		fprintf(stderr, "Unable to produce scancode sequence for");
		fprintf(stderr, " ASCII character [%c]!\n", asciicode);
	    }
	    printf("\t{ 0, 0, undefined, 0 },\t");
	} else {
	    printf("\t{ 0x%02x, %s, FCN_CHARACTER, 0 },",
					Pt->scancode, Pt->shiftstate);
	}
	printf("\t/* 0x%x", asciicode);
	if (isprint(asciicode)) {
	    printf(" [%c]", asciicode);
	}
	printf(" */\n");
    }
		

    for (attable = &table[0]; attable <= &table[highestof(table)]; attable++) {
	for (this = *attable; this; this = this->next) {
	    Ph = this->hits;
	    if (Ph == 0) {
		continue;
	    }
	    for (i = 0; i < 4; i++) {
		if ((Ph->name[i] != 0) &&
			(Ph->name[i][0] == this->name[0]) &&
			(strcmp(Ph->name[i], this->name) == 0)) {
		    printf("\t{ 0x%02x, %s, ",
				Ph-Hits, shiftof[i]);
		    if (memcmp("AID_", this->name, 4) == 0) {	/* AID key */
			printf("FCN_AID, ");
		    } else {
			printf("%s, ", Ph->name[i]);
		    }
		    if (memcmp("PF", this->name+4, 2) == 0) {
			printf("\"PFK%s\" },\n", Ph->name[i]+4+2);
		    } else {
			printf("\"%s\" },\n", Ph->name[i]+4);
		    }
		}
	    }
	}
    }

    return 0;
}
