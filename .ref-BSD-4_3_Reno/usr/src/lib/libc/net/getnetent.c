/*
 * Copyright (c) 1983 Regents of the University of California.
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
static char sccsid[] = "@(#)getnetent.c	5.7 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static FILE *netf = NULL;
static char line[BUFSIZ+1];
static struct netent net;
static char *net_aliases[MAXALIASES];
int _net_stayopen;
static char *any();

setnetent(f)
	int f;
{
	if (netf == NULL)
		netf = fopen(_PATH_NETWORKS, "r" );
	else
		rewind(netf);
	_net_stayopen |= f;
}

endnetent()
{
	if (netf) {
		fclose(netf);
		netf = NULL;
	}
	_net_stayopen = 0;
}

struct netent *
getnetent()
{
	char *p;
	register char *cp, **q;

	if (netf == NULL && (netf = fopen(_PATH_NETWORKS, "r" )) == NULL)
		return (NULL);
again:
	p = fgets(line, BUFSIZ, netf);
	if (p == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	net.n_name = p;
	cp = any(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	while (*cp == ' ' || *cp == '\t')
		cp++;
	p = any(cp, " \t");
	if (p != NULL)
		*p++ = '\0';
	net.n_net = inet_network(cp);
	net.n_addrtype = AF_INET;
	q = net.n_aliases = net_aliases;
	if (p != NULL) 
		cp = p;
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &net_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&net);
}

static char *
any(cp, match)
	register char *cp;
	char *match;
{
	register char *mp, c;

	while (c = *cp) {
		for (mp = match; *mp; mp++)
			if (*mp == c)
				return (cp);
		cp++;
	}
	return ((char *)0);
}
