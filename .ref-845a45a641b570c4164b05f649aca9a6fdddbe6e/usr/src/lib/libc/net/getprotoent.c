/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getprotoent.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define	MAXALIASES	35

static FILE *protof = NULL;
static char line[BUFSIZ+1];
static struct protoent proto;
static char *proto_aliases[MAXALIASES];
int _proto_stayopen;

void
setprotoent(f)
	int f;
{
	if (protof == NULL)
		protof = fopen(_PATH_PROTOCOLS, "r" );
	else
		rewind(protof);
	_proto_stayopen |= f;
}

void
endprotoent()
{
	if (protof) {
		fclose(protof);
		protof = NULL;
	}
	_proto_stayopen = 0;
}

struct protoent *
getprotoent()
{
	char *p;
	register char *cp, **q;

	if (protof == NULL && (protof = fopen(_PATH_PROTOCOLS, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, protof)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = strpbrk(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	proto.p_name = p;
	cp = strpbrk(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	while (*cp == ' ' || *cp == '\t')
		cp++;
	p = strpbrk(cp, " \t");
	if (p != NULL)
		*p++ = '\0';
	proto.p_proto = atoi(cp);
	q = proto.p_aliases = proto_aliases;
	if (p != NULL) {
		cp = p;
		while (cp && *cp) {
			if (*cp == ' ' || *cp == '\t') {
				cp++;
				continue;
			}
			if (q < &proto_aliases[MAXALIASES - 1])
				*q++ = cp;
			cp = strpbrk(cp, " \t");
			if (cp != NULL)
				*cp++ = '\0';
		}
	}
	*q = NULL;
	return (&proto);
}
