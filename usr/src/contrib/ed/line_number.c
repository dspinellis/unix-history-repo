/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)line_number.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>

#include "ed.h"
#include "extern.h"

/*
 * Converts a LINE to a line number (int) that can be printed to
 * the device the user is at. Used by n.
 */
int
line_number(line_addr)
	LINE *line_addr;
{
	LINE *l_temp1;
	int l_cnt = 1;		/* yes! =1 */

	l_temp1 = top;
	if ((line_addr == NULL) && (top == NULL))
		return (0);

	for (;;) {
		if (sigint_flag)
			SIGINT_ACTION;
		if (line_addr == l_temp1)
			break;
		l_temp1 = l_temp1->below;
		l_cnt++;
	}
	return (l_cnt);
}
