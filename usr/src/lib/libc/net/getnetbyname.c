/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getnetbyname.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <netdb.h>
#include <string.h>

extern int _net_stayopen;

struct netent *
getnetbyname(name)
	register const char *name;
{
	register struct netent *p;
	register char **cp;

	setnetent(_net_stayopen);
	while (p = getnetent()) {
		if (strcmp(p->n_name, name) == 0)
			break;
		for (cp = p->n_aliases; *cp != 0; cp++)
			if (strcmp(*cp, name) == 0)
				goto found;
	}
found:
	if (!_net_stayopen)
		endnetent();
	return (p);
}
