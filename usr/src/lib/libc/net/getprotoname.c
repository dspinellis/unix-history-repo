/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getprotoname.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <netdb.h>
#include <string.h>

extern int _proto_stayopen;

struct protoent *
getprotobyname(name)
	register const char *name;
{
	register struct protoent *p;
	register char **cp;

	setprotoent(_proto_stayopen);
	while (p = getprotoent()) {
		if (strcmp(p->p_name, name) == 0)
			break;
		for (cp = p->p_aliases; *cp != 0; cp++)
			if (strcmp(*cp, name) == 0)
				goto found;
	}
found:
	if (!_proto_stayopen)
		endprotoent();
	return (p);
}
