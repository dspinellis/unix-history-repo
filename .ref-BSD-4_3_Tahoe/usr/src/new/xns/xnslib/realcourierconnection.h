/* $Header: realcourierconnection.h,v 2.0 85/11/21 07:22:17 jqj Exp $ */
/*
 $Log:	realcourierconnection.h,v $
 * Revision 2.0  85/11/21  07:22:17  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:37:04  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:37:04  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:24  jqj
 * finished but undebugged version
 * 
 */

/*
 * entries in the database of active Courier connections
 * This file replaces the definition of ``#define CourierConnection int''
 * in courier.h
 */


#ifndef CourierConnectionHeader
#define CourierConnectionHeader

enum connectionstate {
	closed,			/* no SPP connection */
	wantversion,		/* need to receive Courier version */
	inprogress,		/* got version, but no RETURN yet */
	calldone		/* transaction completed.  Waiting for call */
	};
enum bdtconnectionstate {
	wantdata,		/* call, but no reply on BDT connection */
	established,		/* in middle of BDT transfer */
	bdteomseen		/* BDT data done */
	};

typedef struct {
	int fd;
	int abortseen;
	enum connectionstate state;
	enum bdtconnectionstate bdtstate;
	struct sockaddr_ns host;
	struct sphdr sphdrOpts;
} CourierConnection;

extern CourierConnection *CourierOpen();

#endif
