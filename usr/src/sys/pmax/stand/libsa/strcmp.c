/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)strcmp.c	7.1 (Berkeley) %G%
 */

#include <pmax/stand/dec_prom.h>

strcmp(s1, s2)
	char *s1, *s2;
{
	return (callv->strcmp(s1, s2));
}
