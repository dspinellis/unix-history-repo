/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)bpad.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "hexdump.h"

bpad(fu, pr)
	FU *fu;
	PR *pr;
{
	register char *p1, *p2;

	/*
	 * the format string has no more data to print out.  Go through
	 * the rest of the format string, and, for all non-%s conversions,
	 * replace with %s and remove any conversion flags that don't
	 * apply to %s.  This should replace any output with an equivalent
	 * number of spaces.
	 */
	for (;;) {
		if (fu->flags&F_IGNORE)
			continue;
		for (; pr; pr = pr->nextpr) {
			if (pr->flags == F_TEXT)
				continue;
			pr->flags = F_BPAD;
			*pr->cchar = 's';
			/* remove conversion flags %s can't handle. */
			for (p1 = p2 = pr->fmt; *p1; *p2++ = *p1++)
				if (*p1 == '%') {
					static char *spec1 = "-0+ #";
					static char *spec2 = ".0123456789";

					*p2++ = *p1;
					while (index(spec1, *++p1))
						if (*p1 == '-')
							*p2++ = '-';
					while (index(spec2, *p2++ = *p1++));
				}
			*p2 = '\0';
		}
		if (!(fu = fu->nextfu))
			break;
		pr = fu->nextpr;
	}
}
