/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkastods.c	3.2 (Berkeley) 3/28/88";
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

    /* For each ascii code, find the display code that matches */

    printf("unsigned char asc_disp[256] = {");
    for (i = 0; i < NASCII; i++) {
	if ((i%8) == 0) {
	    printf("\n");
	}
	printf("\t0x%02x,", ebc_disp[asc_ebc[i]]);
    }
    for (i = sizeof disp_ebc; i < 256; i++) {
	if ((i%8) == 0) {
	    printf("\n");
	}
	printf("\t0x%02x,", 0);
    }
    printf("\n};\n");

    return 0;
}
