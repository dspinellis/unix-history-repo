/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ns_sort.c	4.5 (Berkeley) 1/14/89";
#endif /* not lint */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <syslog.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

extern	char *p_type(), *p_class();

extern	int	debug;
extern  FILE	*ddt;

struct netinfo* 
local(from)
	struct sockaddr_in *from;
{
	extern struct netinfo *nettab, netloop;
	struct netinfo *ntp;

	if (from->sin_addr.s_addr == netloop.my_addr.s_addr)
		return( &netloop);
	for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
		if (ntp->net == (from->sin_addr.s_addr & ntp->mask))
			return(ntp);
	}
	return(NULL);
}


sort_response(cp, ancount, lp, eom)
	register char *cp;
	register int ancount;
	struct netinfo *lp;
	u_char *eom;
{
	register struct netinfo *ntp;
	extern struct netinfo *nettab;

#ifdef DEBUG
	if (debug > 2)
	    fprintf(ddt,"sort_response(%d)\n", ancount);
#endif DEBUG
	if (ancount > 1) {
		if (sort_rr(cp, ancount, lp, eom))
			return;
		for (ntp = nettab; ntp != NULL; ntp = ntp->next) {
			if ((ntp->net == lp->net) && (ntp->mask == lp->mask))
				continue;
			if (sort_rr(cp, ancount, ntp, eom))
				break;
		}
	}
}

int
sort_rr(cp, count, ntp, eom)
	register u_char *cp;
	int count;
	register struct netinfo *ntp;
	u_char *eom;
{
	int type, class, dlen, n, c;
	struct in_addr inaddr;
	u_char *rr1;

#ifdef DEBUG
	if (debug > 2) {
	    inaddr.s_addr = ntp->net;
	    fprintf(ddt,"sort_rr( x%x, %d, %s)\n",cp, count,
		inet_ntoa(inaddr));
	}
#endif DEBUG
	rr1 = NULL;
	for (c = count; c > 0; --c) {
	    n = dn_skipname(cp, eom);
	    if (n < 0)
		return (1);		/* bogus, stop processing */
	    cp += n;
	    if (cp + QFIXEDSZ > eom)
		return (1);
	    GETSHORT(type, cp);
	    GETSHORT(class, cp);
	    cp += sizeof(u_long);
	    GETSHORT(dlen, cp);
	    if (dlen > eom - cp)
		return (1);		/* bogus, stop processing */
	    switch (type) {
	    case T_A:
	    	switch (class) {
	    	case C_IN:
	    		bcopy(cp, (char *)&inaddr, sizeof(inaddr));
			if (rr1 == NULL)
				rr1 = cp;
			if ((ntp->mask & inaddr.s_addr) == ntp->net) {
#ifdef DEBUG
			    if (debug > 1) {
	    			fprintf(ddt,"net %s best choice\n",
					inet_ntoa(inaddr));
			    }
#endif DEBUG
			    if (rr1 != cp) {
	    		        bcopy(rr1, cp, sizeof(inaddr));
	    		        bcopy((char *)&inaddr, rr1, sizeof(inaddr));
			    }
			    return(1);
			}
	    		break;
	    	}
	    	break;
	    }
	    cp += dlen;
	}
	return(0);
}
