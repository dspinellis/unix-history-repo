/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gethostnamadr.c	6.3 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include <arpa/resolv.h>

#define	MAXALIASES	35
#define MAXADDRS	35

static char *h_addr_ptrs[MAXADDRS + 1];

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];
static struct in_addr host_addr;

typedef union {
    HEADER qb1;
    char qb2[PACKETSZ];
} querybuf;

static union {
    long al;
    char ac;
} align;


int h_errno;

static struct hostent *
getanswer(msg, msglen, iquery)
	char *msg;
	int msglen, iquery;
{
	register HEADER *hp;
	register char *cp;
	register int n;
	querybuf answer;
	char *eom, *bp, **ap;
	int type, class, ancount, qdcount, buflen;
	int haveanswer, getclass;
	char **hap;

	n = res_send(msg, msglen, (char *)&answer, sizeof(answer));
	if (n < 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("res_send failed\n");
#endif
		h_errno = TRY_AGAIN;
		return (NULL);
	}
	eom = (char *)&answer + n;
	/*
	 * find first satisfactory answer
	 */
	hp = (HEADER *) &answer;
	ancount = ntohs(hp->ancount);
	qdcount = ntohs(hp->qdcount);
	if (hp->rcode != NOERROR || ancount == 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
#endif
		switch (hp->rcode) {
			case NXDOMAIN:
				/* Check if it's an authoritive answer */
				if (hp->aa)
					h_errno = HOST_NOT_FOUND;
				else
					h_errno = TRY_AGAIN;
				break;
			case SERVFAIL:
				h_errno = TRY_AGAIN;
				break;
			case NOERROR:
				h_errno = NO_ADDRESS;
				break;
			case FORMERR:
			case NOTIMP:
			case REFUSED:
				h_errno = NO_RECOVERY;
		}
		return (NULL);
	}
	bp = hostbuf;
	buflen = sizeof(hostbuf);
	cp = (char *)&answer + sizeof(HEADER);
	if (qdcount) {
		if (iquery) {
			if ((n = dn_expand((char *)&answer, cp, bp, buflen)) < 0) {
				h_errno = NO_RECOVERY;
				return (NULL);
			}
			cp += n + QFIXEDSZ;
			host.h_name = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
		} else
			cp += dn_skip(cp) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skip(cp) + QFIXEDSZ;
	} else if (iquery) {
		if (hp->aa)
			h_errno = HOST_NOT_FOUND;
		else
			h_errno = TRY_AGAIN;
		return (NULL);
	}
	ap = host_aliases;
	host.h_aliases = host_aliases;
	hap = h_addr_ptrs;
	host.h_addr_list = h_addr_ptrs;
	haveanswer = 0;
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand((char *)&answer, cp, bp, buflen)) < 0)
			break;
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
		if (type == T_PTR) {
			if ((n = dn_expand((char *)&answer, cp, bp, buflen)) < 
0) {
				cp += n;
				continue;
			}
			cp += n;
			host.h_name = bp;
			return(&host);
		}
		if (type != T_A)  {
#ifdef DEBUG
			if (_res.options & RES_DEBUG)
				printf("unexpected answer type %d, size %d\n",
					type, n);
#endif
			cp += n;
			continue;
		}
		if (haveanswer) {
			if (n != host.h_length) {
				cp += n;
				continue;
			}
			if (class != getclass) {
				cp += n;
				continue;
			}
		} else {
			host.h_length = n;
			getclass = class;
			host.h_addrtype = (class == C_IN) ? AF_INET : AF_UNSPEC;
			if (!iquery) {
				host.h_name = bp;
				bp += strlen(bp) + 1;
			}
		}

		bp += ((u_long)bp % sizeof(align));

		if (bp + n >= &hostbuf[sizeof(hostbuf)]) {
#ifdef DEBUG
			if (_res.options & RES_DEBUG)
				printf("size (%d) too big\n", n);
#endif
			break;
		}
		bcopy(cp, *hap++ = bp, n);
		bp +=n;
		cp += n;
		haveanswer++;
	}
	if (haveanswer) {
		*ap = NULL;
		*hap = NULL;
		return (&host);
	} else {
		h_errno = TRY_AGAIN;
		return (NULL);
	}
}

struct hostent *
gethostbyname(name)
	char *name;
{
	int n;
	querybuf buf;

	n = res_mkquery(QUERY, name, C_ANY, T_A, (char *)NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		return (NULL);
	}
	return(getanswer((char *)&buf, n, 0));
}

struct hostent *
gethostbyaddr(addr, len, type)
	char *addr;
	int len, type;
{
	int n;
	querybuf buf;
	register struct hostent *hp;
	char qbuf[MAXDNAME];

	if (type != AF_INET)
		return (NULL);
	(void)sprintf(qbuf, "%d.%d.%d.%d.in-addr.arpa",
		((unsigned)addr[3] & 0xff),
		((unsigned)addr[2] & 0xff),
		((unsigned)addr[1] & 0xff),
		((unsigned)addr[0] & 0xff));
	n = res_mkquery(QUERY, qbuf, C_IN, T_PTR, NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		return (NULL);
	}
	if ((hp = getanswer((char *)&buf, n, 1)) == NULL)
		return(NULL);
	hp->h_addrtype = type;
	hp->h_length = len;
	h_addr_ptrs[0] = (char *)&host_addr;
	h_addr_ptrs[1] = (char *)0;
	host_addr = *(struct in_addr *)addr;
	return(hp);
}

