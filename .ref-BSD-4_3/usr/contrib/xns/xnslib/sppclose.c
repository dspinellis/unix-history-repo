/*
 * Routines for closing an SPP connection
 */

/*
 $Log:	sppclose.c,v $
 * Revision 2.0  85/11/21  07:22:21  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:37:50  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:37:50  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:52  jqj
 * finished but undebugged version
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: sppclose.c,v 2.0 85/11/21 07:22:21 jqj Exp $";
#endif

/* #include <stdio.h> */
#include <sys/types.h>		/* for ns.h */
#include <sys/socket.h>
#include <sys/time.h>
#include <netns/ns.h>		/* for misc. constants */
#include <netns/sp.h>		/* for spphdr */
#include "courier.h"

#ifndef SPPMAXDATA
#define SPPMAXDATA 534
#endif

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#define NULL ((char*)0)
#endif

int
sppclose(s)
/* try to close an SPP connection by sending an END packet. */
/* return TRUE on normal close, FALSE on abnormal close */
	int s;
{
	int fdmask;
	static struct timeval timeout = {15,0};
	struct {
		struct sphdr hdr;
		char data[SPPMAXDATA];
	} packbuf;

		/* streamtype=254, EOM=FALSE, Attn=FALSE */
	packbuf.hdr.sp_dt = SPPSST_END;
	packbuf.hdr.sp_cc = 0;
	fdmask = 1<<s;
	if (write(s, &packbuf, sizeof(packbuf.hdr)) >= 0
	    && select(s+1,&fdmask,(int*)NULL,(int*)NULL,&timeout) > 0
	    && read(s,(char*)&packbuf,sizeof(packbuf)) > 0) {
		if (packbuf.hdr.sp_dt == SPPSST_ENDREPLY) {
			/* normal close */
			/* SetSPPoptions(s, SPPSST_ENDREPLY, 0, 0);
				/* streamtype=255, EOM=FALSE, Attn=FALSE */
			packbuf.hdr.sp_dt = SPPSST_ENDREPLY;
			packbuf.hdr.sp_cc = 0;
			if (write(s, &packbuf, sizeof(packbuf.hdr)) >= 0
			    && shutdown(s,0) >= 0) {
				/* don't read any more, but */
				/*  try to get ENDREPLY out */
				(void) close(s);
				return(TRUE);
			}
			/* fall out of if to abnormal close */
		}
		else if (packbuf.hdr.sp_dt == SPPSST_END) {
			/* simultaneous close */
			return(sppclosereply(s));
		}
		/* else must have been a data packet -- abnormal close */
		/* fall through */
	}
	/* timer expired, data packet arrived, or write failed */
	(void) shutdown(s,2);		/* throw away all data */
	(void) close(s);
	return(FALSE);
}

int
sppclosereply(s)
/* handle receipt of a packet of type END, by closing down the SPP
 * connection.  Returns TRUE on normal close, FALSE otherwise.
 */
	int s;		/* spp socket */
{
	int fdmask;
	static struct timeval timeout = {10,0};
	struct {
		struct sphdr hdr;
		char data[SPPMAXDATA];
	} packbuf;

	fdmask = 1<<s;
	/* SetSPPoptions(s, SPPSST_ENDREPLY, 0, 0);
		/* streamtype=255, EOM=FALSE, Attn=FALSE */
	packbuf.hdr.sp_dt = SPPSST_ENDREPLY;
	packbuf.hdr.sp_cc = 0;
	if (write(s, &packbuf, sizeof(packbuf.hdr))
	    && select(s+1, &fdmask, (int*)NULL, (int*)NULL, &timeout) > 0
	    && read(s, (char*)&packbuf, sizeof(packbuf)) > 0
	    && shutdown(s,2) == 0) {
		close(s);
		return(packbuf.hdr.sp_dt == SPPSST_ENDREPLY);
	}
	/* write failed, timeout expired, or error occured */
	(void) shutdown(s,2);	/* throw away any data */
	(void) close(s);
	return(FALSE);
}
