/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)yyerror.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

yyerror(msg)
char *msg;
{
	(void)fprintf(stderr, "%s\n", msg);
	return(0);
}
