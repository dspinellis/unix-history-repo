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

#define	LETS_SEE_ASCII
#include "../m4.out"
#undef	LETS_SEE_ASCII

#include "../ascebc.h"
#include "../ebc_disp.h"
#include "../kbd3270.h"

#include "dohits.h"


void
main()
{
    int scancode;
    int empty;
    int i;
    int found;
    struct hits *ph;
    struct Hits *Ph;
    TC_Ascii_t *TC;
    struct thing *this;

    dohits();		/* Set up "Hits" */

    printf("struct tctokbd {\n\tenum { cantdo, normal, shifted, alted,");
    printf(" shiftalted } shift;\n\tunsigned char scancode;");
    printf("\n} tctokbd[] = {\n");
    for (TC = &TC_Ascii[TC_LOWEST_USER-TC_LOWEST];
		TC <= &TC_Ascii[TC_HIGHEST-TC_LOWEST]; TC++) {
	/* Hack for "PFK" names (which should be "PF") */
	if (memcmp(TC->tc_name, "PFK", 3) == 0) {
	    static char PFonly[100] = "PF";

	    strcpy(PFonly+2, TC->tc_name+3);
	    TC->tc_name = PFonly;
	}
	found = 0;
	for (this = firstentry(TC->tc_name); (!found) && this;
							this = this->next) {
	    if ((this->name[4] == TC->tc_name[0])
			&& (strcmp(this->name+4, TC->tc_name) == 0)) {
		/* this is the entry */
		/* What we have is a TC entry matching a scancode entry */
		Ph = this->hits;		/* now, get hits */
		if (Ph == 0) {
		    continue;
		}
		for (i = 0; i < 4; i++) {
		    if ((Ph->name[i][4] == TC->tc_name[0])
			    && (strcmp(Ph->name[i]+4, TC->tc_name) == 0)) {
			/* This is THE hit! */
			found = 1;
			printf("\t{ ");
			switch (i) {
			case 0:
			    printf("normal, ");
			    break;
			case 1:
			    printf("shifted, ");
			    break;
			case 2:
			    printf("alted, ");
			    break;
			case 3:
			    printf("shitfalted, ");
			    break;
			}
			printf("0x%x },\n", Ph-Hits);
		    }
		}
	    }
	}
	if (!found) {
	    printf("\t{ cantdo, 0 },\n");
	    fprintf(stderr, "Unable to produce TC_%s with scan codes!\n",
				TC->tc_name);
	}
    }
    printf("};\n");
}
