/*-
 * Copyright (c) 1992, 1993, 1994 Henry Spencer.
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regfree.c	8.2 (Berkeley) %G%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)regfree.c	8.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

#include "utils.h"
#include "regex2.h"

/*
 - regfree - free everything
 = extern void regfree(regex_t *);
 */
void
regfree(preg)
regex_t *preg;
{
	register struct re_guts *g;

	if (preg->re_magic != MAGIC1)	/* oops */
		return;			/* nice to complain, but hard */

	g = preg->re_g;
	if (g == NULL || g->magic != MAGIC2)	/* oops again */
		return;
	preg->re_magic = 0;		/* mark it invalid */
	g->magic = 0;			/* mark it invalid */

	if (g->strip != NULL)
		free((char *)g->strip);
	if (g->sets != NULL)
		free((char *)g->sets);
	if (g->setbits != NULL)
		free((char *)g->setbits);
	if (g->must != NULL)
		free(g->must);
	free((char *)g);
}
