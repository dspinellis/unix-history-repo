/*
 * This file contains routines useful to the applications developer who
 * must read or write BDT data.
 */

/*
 $Log:	bdt.c,v $
 * Revision 3.0  87/01/14  14:39:02  ed
 * release containing Xerox (Webster Research Center) modifications
 * 
 * Revision 2.1  86/09/07  09:36:29  jqj
 * return correct number of bytes written by BDTwrite, or -1 on error.
 * 
 * Revision 2.0  85/11/21  07:22:02  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.4  85/03/11  16:36:38  jqj
 * *** empty log message ***
 * 
 * Revision 1.4  85/03/11  16:36:38  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.3  85/03/11  16:34:19  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:06  jqj
 * finished but undebugged version
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: bdt.c,v 3.0 87/01/14 14:39:02 ed Exp $";
#endif

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>		/* for socket.h and xn.h */
#include <sys/socket.h>
#include <sys/uio.h>		/* for scatter/gather io */
#include <netns/ns.h>		/* for XNS addresses and courierconnection.h */
#include <netns/idp.h>
#include <netns/sp.h>		/* for spphdr */
#include "courier.h"
#include "realcourierconnection.h"

#define MAKEVEC(idx, addr, len) our_iovec[idx].iov_base = (caddr_t)addr;\
				our_iovec[idx].iov_len = len;



int
BDTwrite(f,buffer,nbytes)
/* Call with CourierConnection*, not *(CourierConnection*) */
/* Semantics are much like write(), except that it returns -1
 * if a BDT abort message arrives from receiver.
 * Returns # of bytes actually written, or -1 if an error occurs (some
 * data may have been transferred!).
 */
	register CourierConnection *f;
	char *buffer;
	int nbytes;
{
	register int n, w;
	struct iovec our_iovec[2];

	MAKEVEC(0, &(f->sphdrOpts), sizeof(f->sphdrOpts));
	MAKEVEC(1, buffer, SPPMAXDATA);

	if (f->bdtstate == wantdata) {
			/* stream=BDT, EOM=FALSE, Attn=FALSE */
		f->sphdrOpts.sp_dt = SPPSST_BDT;
		f->sphdrOpts.sp_cc &= ~SP_EM;
		f->bdtstate = established;
	}
	if (BDTabortSeen(f)) {
		BDTabort(f);	/* send end (abort) */
		f->abortseen = FALSE;	/* clear abort */
		f->bdtstate = bdteomseen;
		return(-1);	/* truncate the stream */
	}
	/* ### if nbytes > SPPMAXDATA, do something intelligent? */
	for(n = nbytes; n > SPPMAXDATA; n -= SPPMAXDATA) {
		w = writev(f->fd, our_iovec, 2) - sizeof(f->sphdrOpts);
		if (w < 0) return(-1);
		if(w < SPPMAXDATA)
			return( w + nbytes - n);
		our_iovec[1].iov_base += SPPMAXDATA;
	}
	our_iovec[1].iov_len = n;
	w = writev(f->fd, our_iovec, 2) - sizeof(f->sphdrOpts);
	if (w < 0) return(-1);
	return( w + nbytes - n);
}


int
BDTclosewrite(f)
/* call with CourierConnection*, not *(CourierConnection*) */
/* End a BDT connection.  Returns 0 on success, -1 on failure.
 */
	register CourierConnection *f;
{

	f->bdtstate = bdteomseen;
	if (BDTabortSeen(f)) {
		BDTabort(f);
		f->abortseen = FALSE;
		return(-1);
	}
	/* stream=BDT, EOM=TRUE, Attn=FALSE */
	f->sphdrOpts.sp_dt = SPPSST_BDT;
	f->sphdrOpts.sp_cc |= SP_EM;
	/* finally, send normal end in a packet of its own */
	write(f->fd,(char*)&f->sphdrOpts,sizeof(struct sphdr));
	return(0);
}


int
BDTread(f, buffer, nbytes)
/* Call with CourierConnection*, not *(CourierConnection*) */
/* Semantics are much like read(), except that it returns -1 on
 * more conditions.  Returns number of characters actually read,
 * or 0 on end of message.
 */
	register CourierConnection *f;
	char *buffer;
	int nbytes;
{
	register int count;
	struct {
		struct sphdr hdr;
		char data[SPPMAXDATA];
	} packbuf;
	struct iovec our_iovec[2];

	switch (f->state) {
	case closed:
	case calldone:
		fprintf(stderr,"BDTread() called while connection state = %s\n",
			(f->state == closed) ? "closed" : "calldone");
		exit(1);
		/* NOTREACHED */
	case wantversion:
		count = recv(f->fd, (char*) &packbuf, sizeof(packbuf), MSG_PEEK)
				- sizeof(struct sphdr);
		while (count == 0
		       && packbuf.hdr.sp_dt == SPPSST_RPC) {
			read(f->fd, (char*) &packbuf, sizeof(packbuf));
			count = recv(f->fd, (char*) &packbuf, sizeof(packbuf),
					MSG_PEEK)
					- sizeof(struct sphdr);
		}
		if (count == 0)
			/* streamtype != SPPSST_RPC, so we can't */
			/* have a version number */
			break;
			/* fall out of switch, still wantversion */
		/* ### N.B. we don't handle count==2 */
		else if (count != (2*sizeof(Cardinal)))
			/* must be a REJECT or ABORT message */
			/* let someone else handle it! */
			return(-1);
		else {
			/* must be a Courier version number */
			/* read it and throw it away */
			read(f->fd, (char*) &packbuf, sizeof(packbuf));
			f->state = inprogress;
			/* fall into case inprogress */
		}
	case inprogress:
		switch (f->bdtstate) {
		case wantdata:
			count = recv(f->fd, (char*) &packbuf, sizeof(packbuf),
					MSG_PEEK)
					- sizeof(struct sphdr);
			if (packbuf.hdr.sp_dt == SPPSST_RPC)
				return(-1);
			f->bdtstate = established;
			/* fall through to case established */
		case established:
			break;
			/* fall out of inner (and outer!) switch */
		case bdteomseen:
			return(0);
		}
		break;
	}
	MAKEVEC(0,&packbuf.hdr,sizeof(struct sphdr));
	MAKEVEC(1,buffer,nbytes);
	count = readv(f->fd,our_iovec,2) - sizeof(struct sphdr);
	/* at this point, we've read a packet that isn't SPPSST_RPC */
	while (TRUE) {
		if (packbuf.hdr.sp_dt == SPPSST_END) {
			(void) sppclosereply(f->fd);
			f->state = closed;
			fprintf(stderr,"SPP END received during BDT\n");
			exit(1);
		}
		if (packbuf.hdr.sp_dt != SPPSST_BDT) {
			fprintf(stderr,
				"wrong stream type, %d, seen during BDT\n",
				packbuf.hdr.sp_dt);
			exit(1);
			/* NOTREACHED */
		}
		if (f->abortseen || (packbuf.hdr.sp_cc & SP_OB)) {
			f->abortseen = TRUE;
			return(-1);
		}
		if (packbuf.hdr.sp_cc & SP_EM) {
			f->bdtstate = bdteomseen;
			/* next read will return 0 */
			return(count);
		}
		if (count > 0)
			return(count);
		count = readv(f->fd,our_iovec,2) - sizeof(struct sphdr);
	}
}

BDTabort(f)
	register CourierConnection *f;
{
	static struct handy {
		struct sphdr hdr;
		char value;
	} data;
	/* stream=BDT, EOM=FALSE, Attn=TRUE */
	data.hdr.sp_dt = SPPSST_BDT;
	data.hdr.sp_cc = SP_EM;
	data.value = 1;	/* BDT abort data value */
	send(f->fd, &data, sizeof(data), MSG_OOB);
	f->bdtstate = bdteomseen;
	f->abortseen = TRUE;
}


BDTabortSeen(f)
	register CourierConnection *f;
{
	struct {
		struct sphdr hdr;
		Unspecified data[MAXWORDS];
	} packbuf;
	int fdmask;
	register int count;
	static struct timeval timeout = {0,0};

	fdmask = 1<<(f->fd);
	/* ### code here for OOB signalling! */
	while (select(f->fd+1,&fdmask,(int*)NULL,(int*)NULL,&timeout) > 0
	    && (count = recv(f->fd,(char*)&packbuf, sizeof(packbuf),
	    			MSG_PEEK) - sizeof(struct sphdr)) > 0) {
		if (packbuf.hdr.sp_dt == SPPSST_BDT
		    && (packbuf.hdr.sp_dt & SP_OB)
		    && count == 1) {
			read(f->fd, (char*)&packbuf, sizeof(packbuf));
			f->abortseen = TRUE;
			return(TRUE);
		}
		else if (count == 0)
			read(f->fd, (char*)&packbuf, sizeof(packbuf));
		else return(FALSE);
	}
	return(FALSE);
}
