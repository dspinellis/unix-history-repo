/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)i.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h" 
#include "extern.h"

/*
 * Set things up for the central input routine to correctly place
 * the text as per the insert command.
 */
void
i(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
#ifdef POSIX
	LINE *l_address;
#endif

	if ((End_default == 1) && (current != NULL))
		Start = End = current->above;
	else {
		if (End == NULL) {
			strcpy(help_msg, "illegal address for command i");
			*errnum = -1;
			return;
		} else
			End = End->above;
	}
	Start_default = End_default = 0;
#ifdef POSIX
	l_address = End;
#endif

	/*
	 * 'i' is just a variation on 'a': completely true with BSD; with
	 * POSIX we have to fake the location of "current" in a special case.
	 */
	a(inputt, errnum);
#ifdef POSIX
	if (l_address->above)
		current = l_address->below;
#endif
}
