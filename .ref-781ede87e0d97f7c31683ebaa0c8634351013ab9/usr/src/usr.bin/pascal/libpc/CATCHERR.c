/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)CATCHERR.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

CATCHERR(err, todo)
	long err;		/* error to be caught */
	struct formalrtn todo;	/* procedure to call to catch error */
{
	if (todo.fbn == 1)
		_entry[err].fentryaddr = todo.fentryaddr;
	else
		fputs("procedure not at level 1", stderr);
}
