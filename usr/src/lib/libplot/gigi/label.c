/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "gigi.h"

label(s)
char *s;
{
	printf("T(S0 H2 D0 I0) \"");
	for(;*s!='\0';s++) {
		putchar(*s);
		if (*s == '"') putchar('"');
	}
	putchar('"');
}
