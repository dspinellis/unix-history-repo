/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwredraw.c	3.9 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		wwtouched[i] = WWU_TOUCHED;
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
