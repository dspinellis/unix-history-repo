/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gethostnamadr.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Copyright (c) 1985 Regents of the University of California
 *	All Rights Reserved
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <nameser.h>
#include <resolv.h>

#define	MAXALIASES	35

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];

static struct hostent *
getanswer(msg, msglen, iquery)
	char *msg;
	int msglen, iquery;
{
	register HEADER *hp;
	register char *cp;
	register int n;
	char answer[PACKETSZ];
	char *eom, *bp, **ap;
	int type, class, ancount, buflen;

	n = res_send(msg, msglen, answer, sizeof(answer));
	if (n < 0) {
		if (_res.options & RES_DEBUG)
			printf("res_send failed\n");
		return (NULL);
	}
	eom = answer + n;
	/*
	 * find first satisfactory answer
	 */
	hp = (HEADER *) answer;
	ancount = ntohs(hp->ancount);
	if (hp->rcode != NOERROR || ancount == 0) {
		if (_res.options & RES_DEBUG)
			printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
		return (NULL);
	}
	bp = hostbuf;
	buflen = sizeof(hostbuf);
	ap = host_aliases;
	cp = answer + sizeof(HEADER);
	if (hp->qdcount) {
		if (iquery) {
			if ((n = dn_expand(answer, cp, bp, buflen)) < 0)
				return (NULL);
			cp += n + QFIXEDSZ;
			host.h_name = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
		} else
			cp += dn_skip(cp) + QFIXEDSZ;
	} else if (iquery)
		return (NULL);
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand(answer, cp, bp, buflen)) < 0)
			return (NULL);
		cp += n;
		type = getshort(cp);
 		cp += sizeof(u_short);
		class = getshort(cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		n = getshort(cp);
		cp += sizeof(u_short);
		if (type == T_CNAME) {
			cp += n;
			if (ap >= &host_aliases[MAXALIASES-1])
				continue;
			*ap++ = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
			continue;
		}
		if (type != T_A || n != 4) {
			if (_res.options & RES_DEBUG)
				printf("unexpected answer type %d, size %d\n",
					type, n);
			continue;
		}
		if (!iquery) {
			host.h_name = bp;
			bp += strlen(bp) + 1;
		}
		*ap = NULL;
		host.h_aliases = host_aliases;
		host.h_addrtype = class == C_IN ? AF_INET : AF_UNSPEC;
		if (bp + n >= &hostbuf[sizeof(hostbuf)]) {
			if (_res.options & RES_DEBUG)
				printf("size (%d) too big\n", n);
			return (NULL);
		}
		bcopy(cp, host.h_addr = bp, host.h_length = n);
		return (&host);
	}
	return (NULL);
}

struct hostent *
gethostbyname(name)
	char *name;
{
	int n;

	n = res_mkquery(QUERY, name, C_ANY, T_A, NULL, 0, NULL,
		hostbuf, sizeof(hostbuf));
	if (n < 0) {
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
		return (NULL);
	}
	return (getanswer(hostbuf, n, 0));
}

struct hostent *
gethostbyaddr(addr, len, type)
	char *addr;
	int len, type;
{
	int n;

	if (type != AF_INET)
		return (NULL);
	n = res_mkquery(IQUERY, NULL, C_IN, T_A, addr, len, NULL,
		hostbuf, sizeof(hostbuf));
	if (n < 0) {
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
		return (NULL);
	}
	return (getanswer(hostbuf, n, 1));
}
