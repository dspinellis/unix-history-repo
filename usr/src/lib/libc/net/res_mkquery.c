/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)res_mkquery.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <nameser.h>
#include <resolv.h>

/*
 * Form all types of queries.
 * Returns the size of the result or -1.
 */
res_mkquery(op, dname, class, type, data, datalen, newrr, buf, buflen)
	int op;			/* opcode of query */
	char *dname;		/* domain name */
	int class, type;	/* class and type of query */
	char *data;		/* resource record data */
	int datalen;		/* length of data */
	struct rrec *newrr;	/* new rr for modify or append */
	char *buf;		/* buffer to put query */
	int buflen;		/* size of buffer */
{
	register HEADER *hp;
	register char *cp;
	register int n;
	char dnbuf[MAXDNAME];
	char *dnptrs[10], **dpp, **lastdnptr;
	extern char *index();

	if (_res.options & RES_DEBUG)
		printf("res_mkquery(%d, %s, %d, %d)\n", op, dname, class, type);
	/*
	 * Initialize header fields.
	 */
	hp = (HEADER *) buf;
	hp->id = htons(++_res.id);
	hp->opcode = op;
	hp->qr = hp->aa = hp->tc = hp->ra = 0;
	hp->pr = (_res.options & RES_PRIMARY) != 0;
	hp->rd = (_res.options & RES_RECURSE) != 0;
	hp->rcode = NOERROR;
	hp->qdcount = 0;
	hp->ancount = 0;
	hp->nscount = 0;
	hp->arcount = 0;
	cp = buf + sizeof(HEADER);
	buflen -= sizeof(HEADER);
	dpp = dnptrs;
	*dpp++ = buf;
	*dpp++ = NULL;
	lastdnptr = dnptrs + sizeof(dnptrs)/sizeof(dnptrs[0]);
	/*
	 * If the domain name contains no dots (single label), then
	 * append the default domain name to the one given.
	 */
	if ((_res.options & RES_DEFNAMES) && dname[0] != '\0' &&
	    index(dname, '.') == NULL) {
		if (!(_res.options & RES_INIT))
			res_init();
		if (_res.defdname[0] != '\0')
			dname = sprintf(dnbuf, "%s.%s", dname, _res.defdname);
	}
	/*
	 * perform opcode specific processing
	 */
	switch (op) {
	case QUERY:
	case CQUERYM:
	case CQUERYU:
		buflen -= QFIXEDSZ;
		if ((n = dn_comp(dname, cp, buflen, dnptrs, lastdnptr)) < 0)
			return (-1);
		cp += n;
		buflen -= n;
		putshort(type, cp);
		cp += sizeof(u_short);
		putshort(class, cp);
		cp += sizeof(u_short);
		hp->qdcount = HTONS(1);
		if (op == QUERY || data == NULL)
			break;
		/*
		 * Make an additional record for completion domain.
		 */
		buflen -= RRFIXEDSZ;
		if ((n = dn_comp(data, cp, buflen, dnptrs, lastdnptr)) < 0)
			return (-1);
		cp += n;
		buflen -= n;
		putshort(T_NULL, cp);
		cp += sizeof(u_short);
		putshort(class, cp);
		cp += sizeof(u_short);
		putlong(0, cp);
		cp += sizeof(u_long);
		putshort(0, cp);
		cp += sizeof(u_short);
		hp->arcount = HTONS(1);
		break;

	case IQUERY:
		/*
		 * Initialize answer section
		 */
		if (buflen < 1 + RRFIXEDSZ + datalen)
			return (-1);
		*cp++ = '\0';	/* no domain name */
		putshort(type, cp);
		cp += sizeof(u_short);
		putshort(class, cp);
		cp += sizeof(u_short);
		putlong(0, cp);
		cp += sizeof(u_long);
		putshort(datalen, cp);
		cp += sizeof(u_short);
		if (datalen) {
			bcopy(data, cp, datalen);
			cp += datalen;
		}
		hp->ancount = HTONS(1);
		break;

#ifdef notdef
	case UPDATED:
		/*
		 * Put record to be added or deleted in additional section
		 */
		buflen -= RRFIXEDSZ + datalen;
		if ((n = dn_comp(dname, cp, buflen, NULL, NULL)) < 0)
			return (-1);
		cp += n;
		*((u_short *)cp) = htons(type);
		cp += sizeof(u_short);
		*((u_short *)cp) = htons(class);
		cp += sizeof(u_short);
		*((u_long *)cp) = 0;
		cp += sizeof(u_long);
		*((u_short *)cp) = htons(datalen);
		cp += sizeof(u_short);
		if (datalen) {
			bcopy(data, cp, datalen);
			cp += datalen;
		}
		break;

	case UPDATEM:
		/*
		 * Record to be modified followed by its replacement
		 */
		buflen -= RRFIXEDSZ + datalen;
		if ((n = dn_comp(dname, cp, buflen, dnptrs, lastdnptr)) < 0)
			return (-1);
		cp += n;
		*((u_short *)cp) = htons(type);
		cp += sizeof(u_short);
		*((u_short *)cp) = htons(class);
		cp += sizeof(u_short);
		*((u_long *)cp) = 0;
		cp += sizeof(u_long);
		*((u_short *)cp) = htons(datalen);
		cp += sizeof(u_short);
		if (datalen) {
			bcopy(data, cp, datalen);
			cp += datalen;
		}

	case UPDATEA:
		buflen -= RRFIXEDSZ + newrr->r_size;
		if ((n = dn_comp(dname, cp, buflen, dnptrs, lastdnptr)) < 0)
			return (-1);
		cp += n;
		*((u_short *)cp) = htons(newrr->r_type);
		cp += sizeof(u_short);
		*((u_short *)cp) = htons(newrr->r_type);
		cp += sizeof(u_short);
		*((u_long *)cp) = htonl(newrr->r_ttl);
		cp += sizeof(u_long);
		*((u_short *)cp) = htons(newrr->r_size);
		cp += sizeof(u_short);
		if (newrr->r_size) {
			bcopy(newrr->r_data, cp, newrr->r_size);
			cp += newrr->r_size;
		}
		break;
#endif
	}
	return (cp - buf);
}
