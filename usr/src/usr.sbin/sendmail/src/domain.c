/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *  Sendmail
 *  Copyright (c) 1986  Eric P. Allman
 *  Berkeley, California
 */

#include "sendmail.h"

#ifndef lint
#ifdef MXDOMAIN
static char sccsid[] = "@(#)domain.c	5.10 (Berkeley) %G% (with MXDOMAIN)";
#else
static char sccsid[] = "@(#)domain.c	5.10 (Berkeley) %G% (without MXDOMAIN)";
#endif
#endif /* not lint */

#ifdef MXDOMAIN
# include <sys/param.h>
# include <arpa/nameser.h>
# include <resolv.h>
# include <netdb.h>

typedef union {
	HEADER qb1;
	char qb2[PACKETSZ];
} querybuf;

static char	hostbuf[BUFSIZ];
int		h_errno;
extern u_short	_getshort();

getmxrr(host, mxhosts, maxmx, localhost)
	char *host, **mxhosts;
	int maxmx;
	char *localhost;
{

	HEADER *hp;
	char *eom, *bp, *cp;
	querybuf buf, answer;
	int n, n1, i, j, nmx, ancount, qdcount, buflen;
	int seenlocal;
	u_short prefer[BUFSIZ];
	u_short pref, localpref, type, class;

	n = res_mkquery(QUERY, host, C_IN, T_MX, (char *)NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		h_errno = NO_RECOVERY;
		return(-2);
	}
	n = res_send((char *)&buf, n, (char *)&answer, sizeof(answer));
	if (n < 0) {
#ifdef DEBUG
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("res_send failed\n");
#endif
		h_errno = TRY_AGAIN;
		return (-1);
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
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
#endif
		switch (hp->rcode) {
			case NXDOMAIN:
				/* Check if it's an authoritive answer */
				if (hp->aa) {
					h_errno = HOST_NOT_FOUND;
					return(-3);
				} else {
					h_errno = TRY_AGAIN;
					return(-1);
				}
			case SERVFAIL:
				h_errno = TRY_AGAIN;
				return(-1);
#ifdef OLDJEEVES
			/*
			 * Jeeves (TOPS-20 server) still does not
			 * support MX records.  For the time being,
			 * we must accept FORMERRs as the same as
			 * NOERROR.
			 */
			case FORMERR:
#endif OLDJEEVES
			case NOERROR:
				(void) strcpy(hostbuf, host);
				mxhosts[0] = hostbuf;
				return(1);
#ifndef OLDJEEVES
			case FORMERR:
#endif OLDJEEVES
			case NOTIMP:
			case REFUSED:
				h_errno = NO_RECOVERY;
				return(-2);
		}
		return (-1);
	}
	bp = hostbuf;
	nmx = 0;
	seenlocal = 0;
	buflen = sizeof(hostbuf);
	cp = (char *)&answer + sizeof(HEADER);
	if (qdcount) {
		cp += dn_skipname(cp, eom) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
	}
	while (--ancount >= 0 && cp < eom && nmx < maxmx) {
		if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		type = _getshort(cp);
 		cp += sizeof(u_short);
		/*
		class = _getshort(cp);
		*/
 		cp += sizeof(u_short) + sizeof(u_long);
		n = _getshort(cp);
		cp += sizeof(u_short);
		if (type != T_MX)  {
#ifdef DEBUG
			if (tTd(8, 1) || _res.options & RES_DEBUG)
				printf("unexpected answer type %d, size %d\n",
					type, n);
#endif
			cp += n;
			continue;
		}
		pref = _getshort(cp);
		cp += sizeof(u_short);
		if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		if (!strcasecmp(bp, localhost))
		{
			seenlocal = 1;
			localpref = pref;
			continue;
		}
		prefer[nmx] = pref;
		mxhosts[nmx++] = bp;
		n1 = strlen(bp)+1;
		bp += n1;
		buflen -= n1;
	}
	if (nmx == 0) {
		(void) strcpy(hostbuf, host);
		mxhosts[0] = hostbuf;
		return(1);
	}
	/* sort the records */
	for (i = 0; i < nmx; i++) {
		for (j = i + 1; j < nmx; j++) {
			if (prefer[i] > prefer[j]) {
				int temp;
				char *temp1;

				temp = prefer[i];
				prefer[i] = prefer[j];
				prefer[j] = temp;
				temp1 = mxhosts[i];
				mxhosts[i] = mxhosts[j];
				mxhosts[j] = temp1;
			}
		}
		if (seenlocal && (prefer[i] >= localpref))
		{
			nmx = i;
			/*
			 * We are the first MX, might as well try delivering
			 * since nobody is supposed to have more info.
			 */
			if (nmx == 0)
			{
				(void) strcpy(hostbuf, host);
				mxhosts[0] = hostbuf;
				return(1);
			}
			break;
		}
	}
	return(nmx);
}


getcanonname(host, hbsize)
	char *host;
	int hbsize;
{

	HEADER *hp;
	char *eom, *cp;
	querybuf buf, answer;
	int n, ancount, qdcount;
	u_short type;
	char nbuf[BUFSIZ];
	int first;

	n = res_mkquery(QUERY, host, C_IN, T_ANY, (char *)NULL, 0, NULL,
		(char *)&buf, sizeof(buf));
	if (n < 0) {
#ifdef DEBUG
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("res_mkquery failed\n");
#endif
		h_errno = NO_RECOVERY;
		return;
	}
	n = res_send((char *)&buf, n, (char *)&answer, sizeof(answer));
	if (n < 0) {
#ifdef DEBUG
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("res_send failed\n");
#endif
		h_errno = TRY_AGAIN;
		return;
	}
	eom = (char *)&answer + n;
	/*
	 * find first satisfactory answer
	 */
	hp = (HEADER *) &answer;
	ancount = ntohs(hp->ancount);
	qdcount = ntohs(hp->qdcount);
	/*
	 * We don't care about errors here, only if we got an answer
	 */
	if (ancount == 0) {
#ifdef DEBUG
		if (tTd(8, 1) || _res.options & RES_DEBUG)
			printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
#endif
		return;
	}
	cp = (char *)&answer + sizeof(HEADER);
	if (qdcount) {
		cp += dn_skipname(cp, eom) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
	}
	first = 1;
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
		    sizeof(nbuf))) < 0)
			break;
		if (first) {
			(void)strncpy(host, nbuf, hbsize);
			host[hbsize - 1] = '\0';
			first = 0;
		}
		cp += n;
		type = _getshort(cp);
 		cp += sizeof(u_short);
 		cp += sizeof(u_short) + sizeof(u_long);
		n = _getshort(cp);
		cp += sizeof(u_short);
		if (type == T_CNAME)  {
			/*
			 * Assume that only one cname will be found.  More
			 * than one is undefined.
			 */
			if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
			    sizeof(nbuf))) < 0)
				break;
			(void)strncpy(host, nbuf, hbsize);
			host[hbsize - 1] = '\0';
			getcanonname(host, hbsize);
			break;
		}
		cp += n;
	}
	return;
}
#endif MXDOMAIN
