/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)connect.h	5.2 (Berkeley) %G%
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
