/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "hp7221.h"

label(s)
char *s;
{
	printf("~'%s", s);
	putchar( ENDOFSTRING );
}
