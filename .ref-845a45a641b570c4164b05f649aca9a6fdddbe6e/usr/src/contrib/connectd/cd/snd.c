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
static char sccsid[] = "@(#)snd.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "main.h"

/*
 * Send a message back to a customer,
 * from data structures and return error status 
 */
int 
sendrequest(sock, rqst, cp, opts, optlen, fd) 
	int sock, rqst ;
	struct conversation *cp ;
	char *opts;
	int optlen, fd ;
{
	int rv ;
	struct iovec iov[4];
	struct msghdr msg ;

	/* send message to user application containing fd */
	msg.msg_name = "" ;
	msg.msg_namelen = 0 ;		/* size of address */
	iov[0].iov_base = (caddr_t) &rqst ;
	iov[0].iov_len = sizeof (rqst) ;
	iov[1].iov_base = (caddr_t) &cp->co_constatus ;
	iov[1].iov_len = sizeof(int) ;
	msg.msg_iov = iov;
	msg.msg_iovlen = 2;
	if (opts) {
		iov[2].iov_base = (caddr_t) opts;
		iov[2].iov_len = optlen ;
		msg.msg_iovlen = 3;
	}
	if (fd >= 0) {
		msg.msg_accrights = (caddr_t) &fd ;
		msg.msg_accrightslen = sizeof(int) ;
	} else	{
		msg.msg_accrightslen = 0 ;
		msg.msg_accrights = 0 ;
	}

	rv = sendmsg(sock, &msg, 0) ; 
	if (rv < 0) {
		perror("snd:") ;
	}
	return (rv) ;
}
