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
static char sccsid[] = "@(#)mkdctype.c	4.1 (Berkeley) 12/4/88";
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
