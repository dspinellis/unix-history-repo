/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rcv.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "main.h"

/*
 * Recieve a message from a customer,
 * put in data structures and return message request type
 */
int 
rcvrequest(sock, cp, opts, optlen, rfdp) 
	int sock ;
	struct conversation *cp ;
	char **opts;
	int *optlen, *rfdp ;
	
{
	int rv ;
	struct iovec iov[4];
	int rqstfmt;
	struct msghdr msg ;
	struct connectdomain *cdp;

	cdp = &cp->co_cd ;
	msg.msg_name = "";		/* optional address */
	msg.msg_namelen = 0 ;		/* size of address */
	iov[0].iov_base = (caddr_t) &rqstfmt ;
	iov[0].iov_len = sizeof (rqstfmt) ;
	iov[1].iov_base = (caddr_t) cdp ;
	iov[1].iov_len = sizeof(cp->co_optionsbuf) + sizeof (cp->co_cd) ;
	msg.msg_iov = iov;
	msg.msg_iovlen = 2;
	msg.msg_accrights = (caddr_t) rfdp ;
	msg.msg_accrightslen = 4;

	if ((rv = recvmsg (sock, &msg, 0)) <= 0 ) {
		perror("connection request message recieve") ;
		return (-1) ;
	}

	if (iov[1].iov_len > CDSIZE(cdp))  {
		*optlen = iov[1].iov_len - CDSIZE(cdp) ;
		*opts = iov[1].iov_base + CDSIZE(cdp);
	} else	*optlen = 0 ;

	if (msg.msg_accrightslen != 4) *rfdp = -1 ;

	return (rqstfmt) ;
}
