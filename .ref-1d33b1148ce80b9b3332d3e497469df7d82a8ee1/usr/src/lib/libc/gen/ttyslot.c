/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyslot.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <ttyent.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int
ttyslot()
{
	register struct ttyent *ttyp;
	register int slot;
	register char *p;
	int cnt;
	char *name;

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
