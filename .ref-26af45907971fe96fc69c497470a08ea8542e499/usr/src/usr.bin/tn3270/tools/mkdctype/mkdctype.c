/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdctype.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include "../api/ebc_disp.h"
#include "ectype.h"


extern unsigned char ectype[256];


void
main()
{
    static unsigned char dctype[192] = { 0 };
    int i;
    char *orbar;
    int type;

    for (i = 0; i < sizeof ectype; i++) {
	dctype[ebc_disp[i]] = ectype[i];
    }

    for (i = 0; i < sizeof dctype; i++) {
	if ((i%16) == 0) {
	    printf("/*%02x*/\n", i);
	}
	printf("\t");
	type = dctype[i];
	orbar = "";
	if (type & E_UPPER) {
	    printf("E_UPPER");
	    orbar = "|";
	}
	if (type & E_LOWER) {
	    printf("%sD_LOWER", orbar);
	    orbar = "|";
	}
	if (type & E_DIGIT) {
	    printf("%sD_DIGIT", orbar);
	    orbar = "|";
	}
	if (type & E_SPACE) {
	    printf("%sD_SPACE", orbar);
	    orbar = "|";
	}
	if (type & E_PUNCT) {
	    printf("%sD_PUNCT", orbar);
	    orbar = "|";
	}
	if (type & E_PRINT) {
	    printf("%sD_PRINT", orbar);
	    orbar = "|";
	}
	if (orbar[0] == 0) {
	    printf("0");
	}
	printf(",\n");
    }
}
