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
#include <strings.h>
#include <ctype.h>

#define	LETS_SEE_ASCII
#include "../keyboard/m4.out"
#undef	LETS_SEE_ASCII

#include "../ascii/ascebc.h"
#include "../ctlr/ebc_disp.h"
#include "../ctlr/function.h"

#include "dohits.h"


void
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
    TC_Ascii_t *TC;
    struct thing *this;
    struct {
	char *shift;
	int	scancode;
    } tbl[128], *Pt;
    static char *shiftof[] = { "normal", "shifted", "alted", "shiftalted" };
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
    printf(" * characters; the rest are TC types from termcodes.m4\n");
    printf(" * (actually, from m4.out).\n");
    printf(" */\n");
    printf("struct asctosc {\n");
    printf("\tenum shiftvalue { cantdo, normal, shifted, alted,");
    printf(" shiftalted } shift;\n\tunsigned char scancode;");
    printf("\n} asctosc[] = {\n");
    /* Build the ascii part of the table. */
    for (Ph = Hits, scancode = 0; Ph <= Hits+highestof(Hits);
							Ph++, scancode++) {
	ph = &Ph->hits;
	for (i = 0; i < 4; i++) {
	    if (ph->hit[i].ctlrfcn == FCN_CHARACTER) {
		c = Ph->name[i][0];	/* "name" of this one */
		if ((tbl[c].shift == 0) || (tbl[c].shift[0] == 0)) {
		    tbl[c].shift = shiftof[i];
		    tbl[c].scancode = scancode;
		}
	    }
	}
    }
    /* Now, output the table */
    for (Pt = tbl, asciicode = 0; Pt <= tbl+highestof(tbl); Pt++, asciicode++) {
	if (Pt->shift[0] == 0) {
	    if (isprint(asciicode) && (asciicode != ' ')) {
		fprintf(stderr, "Unable to produce scancode sequence for");
		fprintf(stderr, " ASCII character [%c]!\n", asciicode);
	    }
	    printf("\t{ cantdo, 0 },\t");
	} else {
	    printf("\t{ %s, 0x%x },", Pt->shift, Pt->scancode);
	}
	printf("\t/* 0x%x", asciicode);
	if (isprint(asciicode)) {
	    printf(" [%c]", asciicode);
	}
	printf(" */\n");
    }
		

    for (TC = &TC_Ascii[TC_LOWEST-TC_LOWEST];
		TC < &TC_Ascii[TC_LOWEST_USER-TC_LOWEST]; TC++, asciicode++) {
	printf("\t{ cantdo, 0 },\t");
	printf("\t/* 0x%x */\n", asciicode);
    }
    for (TC = &TC_Ascii[TC_LOWEST_USER-TC_LOWEST];
		TC <= &TC_Ascii[TC_HIGHEST-TC_LOWEST]; TC++, asciicode++) {
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
			printf("0x%02x },", Ph-Hits);
			break;
		    }
		}
	    }
	}
	if (!found) {
	    printf("\t{ cantdo, 0 },\t");
	    fprintf(stderr, "Unable to produce TC_%s with scan codes!\n",
				TC->tc_name);
	}
	printf("\t/* 0x%x - %s */\n", asciicode, TC->tc_name);
    }
    printf("};\n");
}
