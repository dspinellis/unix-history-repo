/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwputs.c	3.7 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

wwputs(s, w)
register char *s;
struct ww *w;
{
	register char *p = s;

	while (*p++)
		;
	(void) wwwrite(w, s, p - s - 1);
}
