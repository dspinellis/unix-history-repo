/*
 * This file implements server functions for the XNS courier library
 */

/*
 $Log:	server.c,v $
 * Revision 2.2  86/12/10  14:21:10  ed
 * Reset _serverConnection->abortseen in ReceiveCallMessage
 * 
 * Revision 2.1  86/11/11  09:33:42  jqj
 * In ReceiveCallMessage, set state to inprogress so server BDT will work
 * right.
 * 
 * Revision 2.0  85/11/21  07:22:19  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:37:39  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:37:39  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:43  jqj
 * finished but undebugged version
 * 
 * Revision 1.1  85/1/4  2:40:00  jqj
 * Initial revision -- Mogul's tcp-based version
 */
#ifndef lint
static char rcsid[] = "$Header: server.c,v 2.2 86/12/10 14:21:10 ed Exp $";
#endif

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>		/* for ns.h */
#include <sys/socket.h>
#include <netns/ns.h>		/* for XNS addresses and courierconnectin.h */
#include <netns/sp.h>		/* for spphdr */
#include "courier.h"
#include "realcourierconnection.h"
#include <except.h>
#include <ctype.h>

#if DEBUG
int CourierServerDebuggingFlag = 0;
#endif

/*
 * Message stream handle.
 */
CourierConnection *_serverConnection = 0;
Unspecified tid;				/* transaction ID */


/* CALL, transaction id, prognumh, prognuml, version, procedurenum */
#define CALLHDRLEN 6


Unspecified *
ReceiveCallMessage(procp, skipcount, skippedwords)
	Cardinal *procp;
	int skipcount;
	Unspecified *skippedwords;
{
	Cardinal msgtype, version;
	LongCardinal programnumber;
	Unspecified *buf, *bp, hdrbuf[CALLHDRLEN];
	int i;

	if (skipcount > 1 && _serverConnection->state == wantversion) {
		skipcount -= 2;
		_serverConnection->state = inprogress;	/* per Ed Flint */
		skippedwords += 2;
	}

	if (skipcount > CALLHDRLEN) {
		fprintf(stderr,"ReceiveCallMessage:  skipcount=%d, too big\n",
			skipcount);
		exit(1);
	}

	_serverConnection->abortseen= FALSE;		/* reset */

	for (i=0; i < skipcount; i++)
		hdrbuf[i] = skippedwords[i];
	buf = ReadMessage(_serverConnection, hdrbuf+skipcount,
			  CALLHDRLEN-skipcount);
	bp = hdrbuf;
	bp += internalize_Cardinal(&msgtype, bp);
	bp += internalize_Unspecified(&tid, bp);
	bp += internalize_LongCardinal(&programnumber, bp);
	bp += internalize_Cardinal(&version, bp);
	bp += internalize_Cardinal(procp, bp);
#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[ReceiveCallMessage %D %d %d]\n",
				programnumber, version, *procp);
#endif
	return(buf);
}


SendReturnMessage(nwords, results)
	Cardinal nwords;
	Unspecified *results;
{
#define RETHDRLEN 2
	Unspecified *bp, buf[RETHDRLEN];
	static Cardinal msgtype = RETURN;

#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[SendReturnMessage %d]\n", nwords);
#endif
	bp = buf;
	bp += externalize_Cardinal(&msgtype, bp);
	bp += externalize_Unspecified(&tid, bp);
	CourierWrite(_serverConnection, (bp-buf), buf, nwords, results);
	_serverConnection->bdtstate = wantdata;
}


static int
ServerInit(argc, argv, skippedwords)
	int argc;
	char *argv[];
	Unspecified skippedwords[];
{
	extern char *malloc();
	int skipcount;
#if DEBUG
	int namelen;
#endif
	int i;

	_serverConnection = (CourierConnection *)
			malloc(sizeof(CourierConnection));
	_serverConnection->bdtstate = wantdata;
	/* we normally don't bother to set up host, since the server will
	 * never reopen a closed connection
	 */
#if DEBUG
	namelen = sizeof(struct sockaddr_ns);
	getpeername(_serverConnection->fd, &_serverConnection->host, &namelen);
	fprintf(stderr,"[ServerInit: argc=%d]\n",argc);
	for (i=0; i<argc; i++) fprintf(stderr,"\targv[%d]=%s\n", i,argv[i]);

#endif
	skipcount = -1;
	while (argc-- > 0) {
#if DEBUG
		if (strcmp(argv[0],"-d") == 0)
			CourierServerDebuggingFlag = 1;
		else
#endif
		if (isdigit(*argv[0])) {
			if (skipcount < 0) {
				_serverConnection->fd = atoi(argv[0]);
				skipcount++;
			}
			else if (skipcount < 8)
				skippedwords[skipcount++] = atoi(argv[0]);
		}
		argv++;
	}
	if (skipcount < 0 || skipcount == 1) {
		fprintf(stderr,"in ServerInit, skipcount=%d\n",skipcount);
		exit(1);
	}
	_serverConnection->state = wantversion;
	return(skipcount);
}


main(argc, argv)
	int argc;
	char *argv[];
{
	/*
	 * The caller may need to read a packet before getting to the
	 * program/version which it needs for dispatching.  Data so read
	 * is passed in the argv list, and used to set skipcount and
	 * skippedwords.
	 */
	int skipcount;	/* actual length of skippedwords */
	Unspecified skippedwords[8];

	/* ServerInit() contains server-independent startup code */
	skipcount = ServerInit(argc, argv, skippedwords);

	/* Server() may terminate in 2 ways:
	 * (1)	normally, with a return(0) and a closed connection,
	 *	either from our timeout or from END sent by client.
	 * (2)	abnormally, with an exit(1) indicating a protocol
	 *	violation.  We do not currently close down the
	 *	connection in all such cases, but we should.
	 * Note that Server may also exec() a different server if
	 *	a remote procedure for a different program arrives.
	 */
	Server(skipcount, skippedwords);
	exit(0);
}
