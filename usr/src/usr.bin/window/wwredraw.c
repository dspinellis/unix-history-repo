/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwredraw.c	3.12 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	xxclear();
	for (i = 0; i < wwnrow; i++) {
		wwtouched[i] = WWU_TOUCHED;
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
