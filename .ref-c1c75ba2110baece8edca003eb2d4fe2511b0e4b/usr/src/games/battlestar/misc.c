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
static char sccsid[] = "@(#)misc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "externs.h"

card(array, size)		/* for beenthere, injuries */
	register char *array;
	int size;
{
	register char *end = array + size;
	register int i = 0;

	while (array < end)
		if (*array++)
			i++;
	return (i);
}

ucard(array)
	register unsigned *array;
{
	register int j = 0, n;

	for (n = 0; n < NUMOFOBJECTS; n++)
		if (testbit(array, n))
			    j++;
	return (j);
}
