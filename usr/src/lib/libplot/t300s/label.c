/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include "con.h"
label(s)
char *s;
{
	int i,c;
		while((c = *s++) != '\0'){
			xnow += HORZRES;
			spew(c);
		}
		return;
}
