/*
 * Copyright (c) 1988 The Regents of the University of California.
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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyslot.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <ttyent.h>
#include <stdio.h>

ttyslot()
{
	register struct ttyent *ttyp;
	register int slot;
	register char *p;
	int cnt;
	char *name, *rindex(), *ttyname();

	for (cnt = 0; cnt < 3; ++cnt) 
		if (name = ttyname(cnt)) {
			if (p = rindex(name, '/')) 
				++p;
			else
				p = name;
			for (slot = 1; ttyp = getttyent(); ++slot)
				if (!strcmp(ttyp->ty_name, p)) {
					endttyent();
					return(slot);
				}
			break;
		}
	endttyent();
	return(0);
}
