/*
 * Copyright (c) 1986 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "sendmail.h"

#ifndef lint
#ifdef NAMED_BIND
static char sccsid[] = "@(#)domain.c	5.22 (Berkeley) 6/1/90 (with name server)";
#else
static char sccsid[] = "@(#)domain.c	5.22 (Berkeley) 6/1/90 (without name server)";
#endif
#endif /* not lint */

#ifdef NAMED_BIND

#include <sys/param.h>
#include <errno.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <netdb.h>

typedef union {
	HEADER qb1;
	char qb2[PACKETSZ];
} querybuf;

static char hostbuf[MAXMXHOSTS*PACKETSZ];

getmxrr(host, mxhosts, localhost, rcode)
	char *host, **mxhosts, *localhost;
	int *rcode;
{
	extern int h_errno;
	register u_char *eom, *cp;
	register int i, j, n, nmx;
	register char *bp;
	HEADER *hp;
	querybuf answer;
	int ancount, qdcount, buflen, seenlocal;
	u_short pref, localpref, type, prefer[MAXMXHOSTS];

	errno = 0;
	n = res_search(host, C_IN, T_MX, (char *)&answer, sizeof(answer));
	if (n < 0)
	{
		if (tTd(8, 1))
			printf("getmxrr: res_search failed (errno=%d, h_errno=%d)\n",
			    errno, h_errno);
		switch (h_errno)
		{
		  case NO_DATA:
		  case NO_RECOVERY:
			/* no MX data on this host */
			goto punt;

		  case HOST_NOT_FOUND:
			/* the host just doesn't exist */
			*rcode = EX_NOHOST;
			break;

		  case TRY_AGAIN:
			/* couldn't connect to the name server */
			if (!UseNameServer && errno == ECONNREFUSED)
				goto punt;

			/* it might come up later; better queue it up */
			*rcode = EX_TEMPFAIL;
			break;
		}

		/* irreconcilable differences */
		return (-1);
	}

	/* find first satisfactory answer */
	hp = (HEADER *)&answer;
	cp = (u_char *)&answer + sizeof(HEADER);
	eom = (u_char *)&answer + n;
	for (qdcount = ntohs(hp->qdcount); qdcount--; cp += n + QFIXEDSZ)
		if ((n = dn_skipname(cp, eom)) < 0)
			goto punt;
	nmx = 0;
	seenlocal = 0;
	buflen = sizeof(hostbuf);
	bp = hostbuf;
	ancount = ntohs(hp->ancount);
	while (--ancount >= 0 && cp < eom && nmx < MAXMXHOSTS) {
		if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		GETSHORT(type, cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		GETSHORT(n, cp);
		if (type != T_MX)  {
			if (tTd(8, 1) || _res.options & RES_DEBUG)
				printf("unexpected answer type %d, size %d\n",
				    type, n);
			cp += n;
			continue;
		}
		GETSHORT(pref, cp);
		if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		if (!strcasecmp(bp, localhost)) {
			if (seenlocal == 0 || pref < localpref)
				localpref = pref;
			seenlocal = 1;
			continue;
		}
		prefer[nmx] = pref;
		mxhosts[nmx++] = bp;
		n = strlen(bp) + 1;
		bp += n;
		buflen -= n;
	}
	if (nmx == 0) {
punt:		mxhosts[0] = strcpy(hostbuf, host);
		return(1);
	}

	/* sort the records */
	for (i = 0; i < nmx; i++) {
		for (j = i + 1; j < nmx; j++) {
			if (prefer[i] > prefer[j] ||
			    (prefer[i] == prefer[j] && rand() % 1 == 0)) {
				register int temp;
				register char *temp1;

				temp = prefer[i];
				prefer[i] = prefer[j];
				prefer[j] = temp;
				temp1 = mxhosts[i];
				mxhosts[i] = mxhosts[j];
				mxhosts[j] = temp1;
			}
		}
		if (seenlocal && prefer[i] >= localpref) {
			/*
			 * truncate higher pref part of list; if we're
			 * the best choice left, we should have realized
			 * awhile ago that this was a local delivery.
			 */
			if (i == 0) {
				*rcode = EX_CONFIG;
				return(-1);
			}
			nmx = i;
			break;
		}
	}
	return(nmx);
}

getcanonname(host, hbsize)
	char *host;
	int hbsize;
{
	extern int h_errno;
	register u_char *eom, *cp;
	register int n; 
	HEADER *hp;
	querybuf answer;
	u_short type;
	int first, ancount, qdcount, loopcnt;
	char nbuf[PACKETSZ];

	loopcnt = 0;
loop:
	/*
	 * Use query type of ANY if possible (NO_WILDCARD_MX), which will
	 * find types CNAME, A, and MX, and will cause all existing records
	 * to be cached by our local server.  If there is (might be) a
	 * wildcard MX record in the local domain or its parents that are
	 * searched, we can't use ANY; it would cause fully-qualified names
	 * to match as names in a local domain.
	 */
# ifdef NO_WILDCARD_MX
	n = res_search(host, C_IN, T_ANY, (char *)&answer, sizeof(answer));
# else
	n = res_search(host, C_IN, T_CNAME, (char *)&answer, sizeof(answer));
# endif
	if (n < 0) {
		if (tTd(8, 1))
			printf("getcanonname:  res_search failed (errno=%d, h_errno=%d)\n",
			    errno, h_errno);
		return;
	}

	/* find first satisfactory answer */
	hp = (HEADER *)&answer;
	ancount = ntohs(hp->ancount);

	/* we don't care about errors here, only if we got an answer */
	if (ancount == 0) {
		if (tTd(8, 1))
			printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
		return;
	}
	cp = (u_char *)&answer + sizeof(HEADER);
	eom = (u_char *)&answer + n;
	for (qdcount = ntohs(hp->qdcount); qdcount--; cp += n + QFIXEDSZ)
		if ((n = dn_skipname(cp, eom)) < 0)
			return;

	/*
	 * just in case someone puts a CNAME record after another record,
	 * check all records for CNAME; otherwise, just take the first
	 * name found.
	 */
	for (first = 1; --ancount >= 0 && cp < eom; cp += n) {
		if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
		    sizeof(nbuf))) < 0)
			break;
		if (first) {			/* XXX */
			(void)strncpy(host, nbuf, hbsize);
			host[hbsize - 1] = '\0';
			first = 0;
		}
		cp += n;
		GETSHORT(type, cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		GETSHORT(n, cp);
		if (type == T_CNAME)  {
			/*
			 * assume that only one cname will be found.  More
			 * than one is undefined.  Copy so that if dn_expand
			 * fails, `host' is still okay.
			 */
			if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
			    sizeof(nbuf))) < 0)
				break;
			(void)strncpy(host, nbuf, hbsize); /* XXX */
			host[hbsize - 1] = '\0';
			if (++loopcnt > 8)	/* never be more than 1 */
				return;
			goto loop;
		}
	}
}

#else /* not NAMED_BIND */

#include <netdb.h>

getcanonname(host, hbsize)
	char *host;
	int hbsize;
{
	struct hostent *hp;

	hp = gethostbyname(host);
	if (hp == NULL)
		return;

	if (strlen(hp->h_name) >= hbsize)
		return;

	(void) strcpy(host, hp->h_name);
}

#endif /* not NAMED_BIND */
