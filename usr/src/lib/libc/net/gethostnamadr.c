/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gethostnamadr.c	6.26 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <ctype.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <resolv.h>

#define	MAXALIASES	35
#define MAXADDRS	35

static char *h_addr_ptrs[MAXADDRS + 1];

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];
static struct in_addr host_addr;
static char HOSTDB[] = "/etc/hosts";
static FILE *hostf = NULL;
static char line[BUFSIZ+1];
static char hostaddr[MAXADDRS];
static char *host_addrs[2];
static int stayopen = 0;
static char *any();

#if PACKETSZ > 1024
#define MAXPACKET	PACKETSZ
#else
#define MAXPACKET	1024
#endif

typedef union {
    HEADER qb1;
    char qb2[MAXPACKET];
} querybuf;

static union {
    long al;
    char ac;
} align;


int h_errno;
extern errno;

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
	int type, class, buflen, ancount, qdcount;
	int haveanswer, getclass = C_ANY;
	char **hap;

	n = res_send(msg, msglen, (char *)&answer, sizeof(answer));
	if (n < 0) {
#ifdef DEBUG
		int terrno;
		terrno = errno;
		if (_res.options & RES_DEBUG)
			printf("res_send failed\n");
		errno = terrno;
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
				if (hp->aa)
					h_errno = NO_ADDRESS;
				else
					h_errno = TRY_AGAIN;
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
			if ((n = dn_expand((char *)&answer, eom,
			     cp, bp, buflen)) < 0) {
				h_errno = NO_RECOVERY;
				return (NULL);
			}
			cp += n + QFIXEDSZ;
			host.h_name = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
		} else
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
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
#if BSD >= 43
	host.h_addr_list = h_addr_ptrs;
#endif
	haveanswer = 0;
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		type = _getshort(cp);
 		cp += sizeof(u_short);
		class = _getshort(cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		n = _getshort(cp);
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
			if ((n = dn_expand((char *)&answer, eom,
			    cp, bp, buflen)) < 0) {
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

		bp += sizeof(align) - ((u_long)bp % sizeof(align));

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
#if BSD >= 43
		*hap = NULL;
#else
		host.h_addr = h_addr_ptrs[0];
#endif
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
	register char *cp, **domain;
	int n;
	struct hostent *hp, *gethostdomain();
	char *hostalias();
	extern struct hostent *_gethtbyname();

	if (!(_res.options & RES_INIT) && res_init() == -1)
		return (NULL);
	if (isdigit(name[0])) {
		h_errno = HOST_NOT_FOUND;
		return (NULL);
	
	}
	errno = 0;
	for (cp = name, n = 0; *cp; cp++)
		if (*cp == '.')
			n++;
	if ((n && *--cp == '.') || (_res.options & RES_DEFNAMES) == 0) {
		int defflag = _res.options & RES_DEFNAMES;

		_res.options &= ~RES_DEFNAMES;			/* XXX */
		if (n && *cp == '.')
			*cp = 0;
		hp = gethostdomain(name, (char *)NULL);
		if (n && *cp == 0)
			*cp = '.';
		if (defflag)
			_res.options |= RES_DEFNAMES;
		return (hp);
	}
	if (n == 0 && (cp = hostalias(name)))
		return (gethostdomain(cp, (char *)NULL));
	for (domain = _res.dnsrch; *domain; domain++) {
		hp = gethostdomain(name, *domain);
		if (hp)
			return (hp);
		/*
		 * If no server present, use host table.
		 * If host isn't found in this domain,
		 * keep trying higher domains in the search list
		 * (if that's enabled).
		 * On a NO_ADDRESS error, keep trying,
		 * or a wildcard MX entry could keep us from finding
		 * host entries higher in the domain.
		 */
		if (errno == ECONNREFUSED)
			return (_gethtbyname(name));
		if ((h_errno != HOST_NOT_FOUND &&
		    h_errno != NO_ADDRESS) ||
		    (_res.options & RES_DNSRCH) == 0)
			return (NULL);
		h_errno = 0;
	}
	return (gethostdomain(name, (char *)NULL));
}

static struct hostent *
gethostdomain(name, domain)
	char *name, *domain;
{
	querybuf buf;
	char nbuf[2*MAXDNAME+2];
	int n;

	if (domain == NULL)
		(void)sprintf(nbuf, "%.*s", MAXDNAME, name);
	else
		(void)sprintf(nbuf, "%.*s.%.*s",
		    MAXDNAME, name, MAXDNAME, domain);
	n = res_mkquery(QUERY, nbuf, C_IN, T_A, (char *)NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		return (NULL);
	}
	return (getanswer((char *)&buf, n, 0));
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
	extern struct hostent *_gethtbyaddr();
	
	if (type != AF_INET)
		return (NULL);
	(void)sprintf(qbuf, "%d.%d.%d.%d.in-addr.arpa",
		((unsigned)addr[3] & 0xff),
		((unsigned)addr[2] & 0xff),
		((unsigned)addr[1] & 0xff),
		((unsigned)addr[0] & 0xff));
	n = res_mkquery(QUERY, qbuf, C_IN, T_PTR, (char *)NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		return (NULL);
	}
	hp = getanswer((char *)&buf, n, 1);
	if (hp == NULL && errno == ECONNREFUSED)
		hp = _gethtbyaddr(addr, len, type);
	if (hp == NULL)
		return(NULL);
	hp->h_addrtype = type;
	hp->h_length = len;
	h_addr_ptrs[0] = (char *)&host_addr;
	h_addr_ptrs[1] = (char *)0;
	host_addr = *(struct in_addr *)addr;
	return(hp);
}

char *
hostalias(name)
	register char *name;
{
	register char *C1, *C2;
	FILE *fp;
	char *file, *getenv(), *strcpy(), *strncpy();
	char buf[BUFSIZ];
	static char abuf[MAXDNAME];

	file = getenv("HOSTALIASES");
	if (file == NULL || (fp = fopen(file, "r")) == NULL)
		return (NULL);
	buf[sizeof(buf) - 1] = '\0';
	while (fgets(buf, sizeof(buf), fp)) {
		for (C1 = buf; *C1 && !isspace(*C1); ++C1);
		if (!*C1)
			break;
		*C1 = '\0';
		if (!strcasecmp(buf, name)) {
			while (isspace(*++C1));
			if (!*C1)
				break;
			for (C2 = C1 + 1; *C2 && !isspace(*C2); ++C2);
			abuf[sizeof(abuf) - 1] = *C2 = '\0';
			(void)strncpy(abuf, C1, sizeof(abuf) - 1);
			fclose(fp);
			return (abuf);
		}
	}
	fclose(fp);
	return (NULL);
}

_sethtent(f)
	int f;
{
	if (hostf == NULL)
		hostf = fopen(HOSTDB, "r" );
	else
		rewind(hostf);
	stayopen |= f;
}

_endhtent()
{
	if (hostf && !stayopen) {
		(void) fclose(hostf);
		hostf = NULL;
	}
}

struct hostent *
_gethtent()
{
	char *p;
	register char *cp, **q;

	if (hostf == NULL && (hostf = fopen(HOSTDB, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, hostf)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	cp = any(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
#if	BSD >= 43
	host.h_addr_list = host_addrs;
#endif
	host.h_addr = hostaddr;
	*((u_long *)host.h_addr) = inet_addr(p);
	host.h_length = sizeof (u_long);
	host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	q = host.h_aliases = host_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&host);
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

struct hostent *
_gethtbyname(name)
	char *name;
{
	register struct hostent *p;
	register char **cp;
	
	_sethtent(0);
	while (p = _gethtent()) {
		if (strcasecmp(p->h_name, name) == 0)
			break;
		for (cp = p->h_aliases; *cp != 0; cp++)
			if (strcasecmp(*cp, name) == 0)
				goto found;
	}
found:
	_endhtent();
	return (p);
}

struct hostent *
_gethtbyaddr(addr, len, type)
	char *addr;
	int len, type;
{
	register struct hostent *p;

	_sethtent(0);
	while (p = _gethtent())
		if (p->h_addrtype == type && !bcmp(p->h_addr, addr, len))
			break;
	_endhtent();
	return (p);
}
