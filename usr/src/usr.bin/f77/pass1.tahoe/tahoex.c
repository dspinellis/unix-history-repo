/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)tahoex.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

#include <stdio.h>
#include "defines.h"
#include "machdefs.h"



prchars(fp, s)
FILEP fp;
int *s;
{

fprintf(fp, ".byte 0%o,0%o\n", s[0], s[1]);
}



pruse(fp, s)
FILEP fp;
char *s;
{
fprintf(fp, "\t%s\n", s);
}



prskip(fp, k)
FILEP fp;
ftnint k;
{
fprintf(fp, "\t.space\t%ld\n", k);
}





prcomblock(fp, name)
FILEP fp;
char *name;
{
fprintf(fp, LABELFMT, name);
}
