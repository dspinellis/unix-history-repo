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
static char sccsid[] = "@(#)snd.c	5.2 (Berkeley) 5/29/93";
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
