/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)CATCHERR.c 1.1 %G%";

#include "h00vars.h"

CATCHERR(err, todo)
	long err;		/* error to be caught */
	struct formalrtn todo;	/* procedure to call to catch error */
{
	if (todo.cbn == 1)
		_entry[err].entryaddr = todo.entryaddr;
	else
		fputs("procedure not at level 1", stderr);
}
