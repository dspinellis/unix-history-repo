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
static char sccsid[] = "@(#)mkdctype.c	1.5 (Berkeley) %G%";
#endif	/* not lint */

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
