/*
 * Copyright (c) 1986, 1990 Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)ns_sort.c	4.8 (Berkeley) 6/1/90";
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
	extern struct netinfo *nettab, netloop, **enettab;
	struct netinfo *ntp;

	if (from->sin_addr.s_addr == netloop.my_addr.s_addr)
		return( &netloop);
	for (ntp = nettab; ntp != *enettab; ntp = ntp->next) {
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
	    	case C_HS:
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
