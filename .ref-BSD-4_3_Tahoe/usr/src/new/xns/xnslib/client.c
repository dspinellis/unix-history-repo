/*
 * This file implements client functions for the XNS courier library
 */

/*
 $Log:	client.c,v $
 * Revision 3.0  87/01/14  14:40:36  ed
 * release containing Xerox (Webster Research Center) modifications
 * 
 * Revision 2.0  85/11/21  07:22:04  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:36:46  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:36:46  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:10  jqj
 * finished but undebugged version
 * 
 * Revision 1.1  84/12/30  10:14:30  jqj
 * Initial revision -- Mogul's tcp-based version
 */

#ifndef lint
static char rcsid[] = "$Header: client.c,v 3.0 87/01/14 14:40:36 ed Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>		/* for xn.h, and socket.h */
#include <sys/socket.h>
#include <netns/ns.h>		/* for XNS addresses and courierconnection.h */
#include <netns/sp.h>		/* for XNS addresses and courierconnection.h */
#include "courier.h"
#include "realcourierconnection.h"
#include <except.h>

#if DEBUG
int CourierClientDebuggingFlag = 0;
#endif



CourierConnection *
CourierOpen( addr )
	struct ns_addr *addr;
{
	extern char *malloc();
	CourierConnection *conn;

	conn = (CourierConnection*) malloc(sizeof(CourierConnection));
	conn->host.sns_family = AF_NS;
	conn->host.sns_addr = *addr;
	/* unknown socket? */
	if (conn->host.sns_addr.x_port == 0)
		conn->host.sns_addr.x_port = htons(IDPPORT_COURIER);
#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr,
			"[CourierOpen: connect to %x#%x.%x.%x.%x.%x.%x#%x]\n",
			ns_netof(conn->host.sns_addr),
			conn->host.sns_addr.x_host.c_host[0],
			conn->host.sns_addr.x_host.c_host[1],
			conn->host.sns_addr.x_host.c_host[2],
			conn->host.sns_addr.x_host.c_host[3],
			conn->host.sns_addr.x_host.c_host[4],
			conn->host.sns_addr.x_host.c_host[5],
			ntohs(conn->host.sns_addr.x_port));
#endif
	if ((conn->fd = openSPPConnection(&conn->host)) >= 0) {
		conn->abortseen = FALSE;
		conn->bdtstate = wantdata;
		conn->state = wantversion;
		conn->sphdrOpts.sp_dt = 0;
		conn->sphdrOpts.sp_cc = 0;
		return(conn);
	}
	else {
		free((char*)conn);
		return(NULL);
	}
}


SendCallMessage(f, program, version, procedure, nwords, arguments)
	CourierConnection *f;	/* socket descriptor */
	LongCardinal program;	/* Courier program number */
	Cardinal version,	/* Courier program version */
		procedure,	/* Courier procedure number */
		nwords;		/* number of words in argument buffer */
	Unspecified *arguments;	/* prepacked argument buffer */
{
	static Cardinal ourCVersion = COURIERVERSION;
	static Cardinal msgtype = 0;
	static Unspecified tid = 0;
#define HDRLEN 8
	Unspecified *_bp, hdrbuf[HDRLEN];

#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[SendCallMessage program %D procedure %d, arglen %d]\n",
			program, procedure, nwords);
#endif
	_bp = hdrbuf;
	if (f->state == wantversion) {
		/* exchange version numbers */
		_bp += externalize_Cardinal(&ourCVersion, _bp);
		_bp += externalize_Cardinal(&ourCVersion, _bp);
	}
	_bp += externalize_Cardinal(&msgtype, _bp);	/* call message type */
	_bp += externalize_Unspecified(&tid, _bp);	/* transaction id */
	_bp += externalize_LongCardinal(&program, _bp);
	_bp += externalize_Cardinal(&version, _bp);
	_bp += externalize_Cardinal(&procedure, _bp);
	(void) CheckEND(f);
	CourierWrite(f, (_bp-hdrbuf), hdrbuf, nwords, arguments);
	if (f->state == calldone)
		f->state = inprogress;
}


Unspecified *
ReceiveReturnMessage(f, abortedFlag)
	CourierConnection *f;		/* socket descriptor */
	Boolean *abortedFlag;		/* TRUE iff abort received */
/* returns a pointer to a block of data to be freed after unpacking
 * procResults or [errorValue, errorArgs]
 */
{
#define RETHDRLEN 2
	Unspecified *bp, *buf, hdrbuf[RETHDRLEN];
	Cardinal msgtype, tid;
	rejectionDetails *rdetails;
	extern char *malloc();

	buf = ReadMessage(f, hdrbuf, RETHDRLEN);
	bp = hdrbuf;
	f->state = calldone;
	bp += internalize_Cardinal(&msgtype, bp);
	bp += internalize_Unspecified(&tid, bp);
#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[ReceiveReturnMessage type %d]\n", msgtype);
#endif
	switch (msgtype) {
	case RETURN:
		*abortedFlag = FALSE;
		break;
	case REJECT:
		bp = buf;
		rdetails = (rejectionDetails*)malloc(sizeof(rejectionDetails));
		bp += internalize_enumeration(&(rdetails->designator), bp);
		if (rdetails->designator == noSuchVersionNumber) {
			bp += internalize_Cardinal( &(rdetails->noSuchVersionNumber_case.lowest), bp);
			bp += internalize_Cardinal( &(rdetails->noSuchVersionNumber_case.highest), bp);
		}
		Deallocate(buf);
		raise(REJECT_ERROR, (char*) rdetails);
		/*NOTREACHED*/
	case ABORT:
		*abortedFlag = TRUE;
		break;
	}
	return(buf);
}


MaybeCallBDTHandler(f, BDTproc)
/* called by RPC stub from Foo_client.c */
	CourierConnection *f;	/* socket descriptor */
	int (*BDTproc)();
{
/* can't look ahead here, since server may be expecting us to send
 * lots of data before he does anything
 */
	f->abortseen = FALSE;
	/* ### setup interrupt handler for URGENT messages */
	if (BDTproc != NULL)
		(*BDTproc)(f);
	/* ### clear interrupt handler */
	f->bdtstate = wantdata;
}

