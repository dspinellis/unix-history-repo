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
 *
 *	@(#)connect.h	5.2 (Berkeley) 5/29/93
 */

/*
 * Unix Connection daemon
 * 
 * service request structures and subroutine definitions
 * for connection requests.
 */

#define	MAXCONNECTS	10	/* maximum number of simultaineous
					connects per process */
struct connectdomain {
	short	cd_family ;
	short	cd_alen ;
	char	cd_address[100] ;
} ;

#define	CDSIZE(s)	(sizeof(s->cd_family) + sizeof (s->cd_alen) + (s->cd_alen))

int	externalconnect(), externalabort(), externalfinish(), externaloption() ;

#define	CDNEWREQUEST	  1	/* request a new connection from client */
#define	CDNEWRESPONSE	  2	/* response from server daemon */
#define	CDCANCELREQUEST	  3	/* cancellation message from client */
#define	CDFINISHREQUEST	  5	/* finshed, pass back file descriptor to srvr */
#define	CDFINISHRESPONSE  6	/* finish acked from server to client */
#define	CDOPTIONREQUEST	  7	/* pass option request to be performed
					on descriptor by srvr */
#define	CDOPTIONRESPONSE  8	/* option acked from server to client */
#define	CDRESOFFEREQUEST  9	/* other daemon offers resources of a given type
					to srvr */
#define	CDRESOFFERESPONSE 10	/* offer acked from server to client */
#define	CDWITHDRNREQUEST  11	/* other daemon withdraws resources frm srvr */
#define	CDWITHDRNRESPONSE 12	/* resp to withdraw resources frm srvr */
#define	CDSGRANTREQUEST   13	/* connect daemon demand resource frm srvr */
#define	CDSGRANTRESPONSE  14	/* resp to demand resource frm srvr */

#define	ISCDREQUEST(s)	(s&1)	/* is this a request or response */
