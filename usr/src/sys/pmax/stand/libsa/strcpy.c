/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)strcpy.c	7.1 (Berkeley) %G%
 */

#include <pmax/stand/dec_prom.h>

char *
strcpy(s1, s2)
	char *s1, *s2;
{
	return (callv->strcpy(s1, s2));
}
