/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyslot.c	5.5 (Berkeley) 6/1/90";
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

	setttyent();
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
