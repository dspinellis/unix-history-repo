/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwputc.c	3.8 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	(void) wwwrite(w, &c, sizeof c);
}
