/*
 * Copyright (c) 1985 Regents of the University of California.
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
static char sccsid[] = "@(#)skip.c	5.8 (Berkeley) 6/21/90";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  skip.c --
 *
 *	Routines to skip over portions of a query buffer.
 *
 *	Note: this file has been submitted for inclusion in
 *	BIND resolver library. When this has been done, this file
 *	is no longer necessary (assuming there haven't been any
 *	changes).
 *
 *	Adapted from 4.3BSD BIND res_debug.c
 *
 *******************************************************************************
 */

#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>

char *res_skip_rr();


/*
 *******************************************************************************
 *
 *  res_skip --
 *
 * 	Skip the contents of a query.
 *
 * 	Interpretation of numFieldsToSkip argument:
 *            res_skip returns pointer to:
 *    	1 ->  start of question records.
 *    	2 ->  start of authoritative answer records.
 *    	3 ->  start of additional records.
 *    	4 ->  first byte after end of additional records.
 *
 *   Results:
 *	(address)	- success operation.
 *  	NULL 		- a resource record had an incorrect format.
 *
 *******************************************************************************
 */

char *
res_skip(msg, numFieldsToSkip, eom)
	char *msg;
	int numFieldsToSkip;
	char *eom;
{
	register char *cp;
	register HEADER *hp;
	register int tmp;
	register int n;

	/*
	 * Skip the header fields.
	 */
	hp = (HEADER *)msg;
	cp = msg + sizeof(HEADER);

	/*
	 * skip question records.
	 */
	if (n = ntohs(hp->qdcount) ) {
		while (--n >= 0 && cp < eom) {
			tmp = dn_skipname(cp, eom);
			if (tmp == -1) return(NULL);
			cp += tmp;
			cp += sizeof(u_short);	/* type 	*/
			cp += sizeof(u_short);	/* class 	*/
		}
	}
	if (--numFieldsToSkip <= 0) return(cp);

	/*
	 * skip authoritative answer records
	 */
	if (n = ntohs(hp->ancount)) {
		while (--n >= 0 && cp < eom) {
			cp = res_skip_rr(cp, eom);
			if (cp == NULL) return(NULL);
		}
	}
	if (--numFieldsToSkip == 0) return(cp);

	/*
	 * skip name server records
	 */
	if (n = ntohs(hp->nscount)) {
		while (--n >= 0 && cp < eom) {
			cp = res_skip_rr(cp, eom);
			if (cp == NULL) return(NULL);
		}
	}
	if (--numFieldsToSkip == 0) return(cp);

	/*
	 * skip additional records
	 */
	if (n = ntohs(hp->arcount)) {
		while (--n >= 0 && cp < eom) {
			cp = res_skip_rr(cp, eom);
			if (cp == NULL) return(NULL);
		}
	}

	return(cp);
}


/*
 *******************************************************************************
 *
 *  res_skip_rr --
 *
 * 	Skip over resource record fields.
 *
 *   Results:
 *	(address)	- success operation.
 *  	NULL 		- a resource record had an incorrect format.
 *******************************************************************************
 */

char *
res_skip_rr(cp, eom)
	char *cp;
	char *eom;
{
	int tmp;
	int dlen;

	if ((tmp = dn_skipname(cp, eom)) == -1)
		return (NULL);			/* compression error */
	cp += tmp;
	if ((cp + RRFIXEDSZ) > eom)
		return (NULL);
	cp += sizeof(u_short);	/* 	type 	*/
	cp += sizeof(u_short);	/* 	class 	*/
	cp += sizeof(u_long);	/* 	ttl 	*/
	dlen = _getshort(cp);
	cp += sizeof(u_short);	/* 	dlen 	*/
	cp += dlen;
	if ((cp + dlen) > eom)
		return (NULL);
	return (cp);
}
