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
#include <string.h>
#include <ctype.h>
#include "../ascii/ascebc.h"
#include "../ctlr/ebc_disp.h"
#include "../ctlr/function.h"

#include "dohits.h"


void
main()
{
    int scancode;
    int empty;
    int i;
    struct hits *ph;
    struct Hits *Ph;

    dohits();		/* Set up "Hits" */

    printf("struct hits hits[] = {\n");
    empty = 0;
    scancode = -1;
    for (Ph = Hits; Ph < Hits+(sizeof Hits/sizeof Hits[0]); Ph++) {
	ph = &Ph->hits;
	scancode++;
	if ((ph->hit[0].type == undefined)
		&& (ph->hit[1].type == undefined)
		&& (ph->hit[2].type == undefined)
		&& (ph->hit[3].type == undefined)) {
	    empty++;
	    continue;
	} else {
	    while (empty) {
		printf("\t{ 0, {\t{ illegal },\t{ illegal }");
		printf(",\t{ illegal },\t{ illegal } } },\n");
		empty--;
	    }
	}
	printf("\t{ %d, {\t/* 0x%02x */\n\t", ph->keynumber, scancode);
	for (i = 0; i < 4; i++) {
	    printf("\t{ ");
	    switch (ph->hit[i].type) {
	    case undefined:
	    case illegal:
		printf("illegal");
		break;
	    case character:
		printf("character, 0x%02x", ph->hit[i].code);
		break;
	    case function:
		printf("function, %s", Ph->name[i]);
		break;
	    case aid:
		printf("aid, %s", Ph->name[i]);
		break;
	    }
	    printf(" },\n\t");
	}
	printf("} },\n");
    }
    printf("};\n");
}
