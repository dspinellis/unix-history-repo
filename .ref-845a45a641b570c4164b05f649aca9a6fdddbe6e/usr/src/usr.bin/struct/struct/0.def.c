/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)0.def.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "def.h"

int routnum;
FILE *debfd;
LOGICAL routerr;
int nodenum, accessnum;
int **graph;
int progtype;
VERT stopvert, retvert;
VERT START;
