/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)rcv.c	5.2 (Berkeley) 5/29/93";
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
