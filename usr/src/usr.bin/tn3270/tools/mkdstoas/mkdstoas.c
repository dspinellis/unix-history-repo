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
static char sccsid[] = "@(#)mkdstoas.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#if	defined(unix)
#include <strings.h>
#else	/* defined(unix) */
#include <string.h>
#endif	/* defined(unix) */
#include <ctype.h>
#include "../api/asc_ebc.h"
#include "../api/ebc_disp.h"


int
main()
{
    int i;

    /* For each display code, find the ascii code that matches */

    printf("unsigned char disp_asc[256] = {");
    for (i = 0; i < sizeof disp_ebc; i++) {
	if ((i%8) == 0) {
	    printf("\n");
	}
	printf("\t0x%02x,", ebc_asc[disp_ebc[i]]);
    }
    for (i = sizeof disp_ebc; i < 256; i++) {
	if ((i%8) == 0) {
	    printf("\n");
	}
	printf("\t0x%02x,", ' ');
    }
    printf("\n};\n");

    return 0;
}
