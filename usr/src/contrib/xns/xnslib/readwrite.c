/*
 * This file implements functions used by both client and servers in the
 * XNS courier library
 */

/*
 $Log:	readwrite.c,v $
 * Revision 2.5  87/04/12  14:01:18  jqj
 * typo in previous
 * 
 * Revision 2.3  87/04/12  13:52:27  jqj
 * don't print silly message if remote system is down -- let higher level
 * software say so.
 * 
 * Revision 2.2  86/11/07  15:58:05  jqj
 * Fixes for very long messages from wiebe@wally.cs.washington.edu.
 * 
 * Revision 2.1  86/09/07  07:31:59  jqj
 * OpenSPPconnection should return -1 on failure.
 * 
 * Revision 2.0  85/11/21  07:22:15  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.8  85/10/21  13:01:17  jqj
 * Gould version.
 * 
 * Revision 1.7  85/10/17  07:22:53  jqj
 * Fix to previous edit.
 * 
 * Revision 1.6  85/10/17  07:07:02  jqj
 * ReadMessage had a typo which Gould compiler caught:  bug in case of
 * message with Courier header split across several SPP packets.
 * 
 * Revision 1.5  85/09/27  16:01:23  jqj
 * added error checking to read in ReadMessage to bomb on closed connections.
 * 
 * Revision 1.4  85/03/11  16:37:24  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.3  85/02/22  09:27:40  bill
 * Almost working version.  Am about to change 
 * ReadMessage to match what really shows up from the Xerox stuff.
 * 
 * Revision 1.2  85/01/27  07:37:39  jqj
 * finished but undebugged version
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: readwrite.c,v 2.5 87/04/12 14:01:18 jqj Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>		/* for ns.h and socket.h */
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/uio.h>		/* for scatter/gather io */
#include <netns/ns.h>		/* for XNS addresses and courierconnection.h */
#include <netns/idp.h>
#include <netns/sp.h>		/* for spphdr */
#include <errno.h>		/* for EPROTOTYPE */
#include "courier.h"
#include "realcourierconnection.h"

#define MAKEVEC(idx, addr, len) our_iovec[idx].iov_base = (caddr_t)addr;\
				our_iovec[idx].iov_len = len;


CourierWrite(f, hdrlen, hdrbuf, nwords, arguments)
/* write a 2-block message possibly consisting of several packets */
	register CourierConnection *f;
	int hdrlen;			/* length of hdrbuf, in words */
	Unspecified *hdrbuf;
	register Cardinal nwords;	/* length of arguments, in words */
	register Unspecified *arguments;
{
	struct iovec our_iovec[3];

	if (f->state == closed) {
		f->abortseen = FALSE;
		if ((f->fd = openSPPConnection(&(f->host))) >= 0) {
			f->state = wantversion;
		}
		else {
			fprintf(stderr,"(Courier) Can't reopen SPP connection\n");
			exit(1);
			/* NOTREACHED */
		}
	}
	MAKEVEC(0, &(f->sphdrOpts), sizeof(f->sphdrOpts));
	MAKEVEC(1, hdrbuf, (hdrlen*sizeof(Unspecified)) );
	if (nwords <= MAXWORDS-hdrlen) {
		/* SetSPPoptions(f->fd, SPPSST_RPC, 1, 0);
			 datastream=0, EOM=TRUE, Attn=FALSE */
		f->sphdrOpts.sp_dt = SPPSST_RPC;
		f->sphdrOpts.sp_cc |= SP_EM;
		MAKEVEC(2, arguments, nwords*sizeof(Unspecified));
		if (writev(f->fd, our_iovec, 3) < 0) {
			perror("(Courier) writev");
			exit(1);
		}
	
	}
	else {
		MAKEVEC(2, arguments, (MAXWORDS-hdrlen)*sizeof(Unspecified));
		/* SetSPPoptions(f->fd, SPPSST_RPC, 0, 0);
			/* datastream=0, EOM=FALSE, Attn=FALSE */
		f->sphdrOpts.sp_dt = SPPSST_RPC;
		f->sphdrOpts.sp_cc &=  ~SP_EM;
		nwords -= MAXWORDS-hdrlen;  arguments += MAXWORDS-hdrlen;
		if (writev(f->fd, our_iovec, 3) < 0) {
			perror("(Courier) writev");
			exit(1);
		}
		MAKEVEC(1, (char *)arguments, MAXWORDS*sizeof(Unspecified));
		while (nwords > MAXWORDS) {
			writev(f->fd, our_iovec, 2);
			nwords -= MAXWORDS;  arguments += MAXWORDS;
			our_iovec[1].iov_base = (char *)arguments;
		}
		f->sphdrOpts.sp_cc |=  SP_EM;
		/* SetSPPoptions(f->fd, SPPSST_RPC, 1, 0);
			/* datastream=0, EOM=TRUE, Attn=FALSE */
		our_iovec[1].iov_len = nwords*sizeof(Unspecified);
		writev(f->fd, our_iovec, 2);
	}

}



Unspecified *
ReadMessage(f, firstbuf, firstlength)
	register CourierConnection *f;	/* socket descriptor */
	Unspecified *firstbuf;
	Cardinal firstlength;
/* Read a complete Courier message from SPP socket f->fd, skipping packets
 * with the wrong datastream type.
 * If firstbuf is specified with a non-zero length (in Unspecifieds), then it 
 * is filled before the malloced packet.
 * Return a pointer to beginning of a malloced packet (caller is responsible
 * for freeing it), and a length in *retlength
 * Returns NULL if connection closes prematurely.
 */
{
	char *buf;			/* ptr to message buffer */
	LongCardinal length,		/* current message length, bytes */
		bufsize,		/* current buffer size, bytes */
		nextincrement;		/* amt of space to try for next */
	register int count;		/* data bytes read by current readv() */
	struct iovec our_iovec[3];
	struct {
		struct sphdr hdr;
		Cardinal version[2];
	} hdrbuf;
	Cardinal versionl, 		/* version numbers received */
		versionh;
	int verbyteswanted;
	extern char *malloc(), *realloc();
	extern free();
	int cc;

	/* spp & idp header */
	MAKEVEC(0, &hdrbuf.hdr, sizeof(struct sphdr));
	/* conn id, etc... */
	if (firstbuf == NULL)
		firstlength = 0;
	else
		firstlength *= sizeof(Unspecified);	/* length in bytes */
	MAKEVEC(1, firstbuf, firstlength);
	/* data */
	buf = malloc(SPPMAXDATA);
	MAKEVEC(2, buf, SPPMAXDATA);

	bufsize = SPPMAXDATA;
	/*
	 * flush Courier version number if necessary
	 */
	if (f->state != wantversion) {
		/* we don't have to look for a version number this time! */
		count = readv(f->fd, our_iovec, 3) - sizeof(struct sphdr);
		if (count < 0 || hdrbuf.hdr.sp_dt == SPPSST_END) {
			if (count >= 0) (void) sppclosereply(f->fd);
			f->state = closed;
			free(buf);
			return(NULL);
		}
	} else {
		/* stick version range in with header */
		verbyteswanted = 2*sizeof(Cardinal);
		our_iovec[0].iov_len += verbyteswanted;
		while (verbyteswanted > 0) {
			count = readv(f->fd, our_iovec, 3) 
					- sizeof(struct sphdr);
			if (count < 0 || hdrbuf.hdr.sp_dt == SPPSST_END) {
				if (count >= 0) (void) sppclosereply(f->fd);
				f->state = closed;
				free(buf);
				return(NULL);
			}
			/* we don't bother to check for matching */
			/* Courier version */
			if (count >= verbyteswanted) {
				count -= verbyteswanted;
				our_iovec[0].iov_len -= verbyteswanted;
				verbyteswanted = 0;
			}
			else {
				verbyteswanted -= count;
				our_iovec[0].iov_len -= count;
				count = 0;
			}
		}
		f->state = inprogress;
		while (count == 0) {
			/* read either RPC reply or BDT garbage */
			count = readv(f->fd, our_iovec, 3)
					- sizeof(struct sphdr);
			if (count < 0 || hdrbuf.hdr.sp_dt == SPPSST_END) {
				if (count >= 0) (void) sppclosereply(f->fd);
				f->state = closed;
				free(buf);
				return(NULL);
			}
		}
		/* {version-packet, null-0-packet, bdt-packet, reply-packet}, 
		 * is handled, but I don't think it's legal */
	}
	/*
	 * we've flushed any version number that might be present,
	 * and have read the first packet -- which may be garbage.
	 * Throw away any further garbage (e.g. BDT data) too.  
	 */
	while (hdrbuf.hdr.sp_dt != SPPSST_RPC) {
		count = readv(f->fd, our_iovec, 3) - sizeof(struct sphdr);
		if (count < 0 || hdrbuf.hdr.sp_dt == SPPSST_END) {
			if (count >= 0) (void) sppclosereply(f->fd);
			f->state = closed;
			free(buf);
			return(NULL);
		}
	}
	/*
	 * Now we have a real RPC data packet, which we hope is the reply
	 */
	length = count;
	nextincrement = SPPMAXDATA;
	while ( ! (hdrbuf.hdr.sp_cc & SP_EM)) {
		/* Not to end of message yet, so read another packet */
		if (length+SPPMAXDATA-firstlength > bufsize) {
			/* not enough space for next packet.  Make room. */
			bufsize += nextincrement;
			buf = realloc(buf, (unsigned) bufsize);
			/* do order(log(messagelength)) reallocs */
			nextincrement += nextincrement;
		}
		if (length >= firstlength) {
			MAKEVEC(1,NULL,0);
			MAKEVEC(2,buf+length-firstlength,bufsize+firstlength-length);
		}
		else {
			firstbuf += length/sizeof(Unspecified);
			firstlength -= length;
			MAKEVEC(1, firstbuf, firstlength);
		}
		count = readv(f->fd, our_iovec, 3) - sizeof(struct sphdr);
		if (count < 0 || hdrbuf.hdr.sp_dt == SPPSST_END) {
			if (count >= 0) (void) sppclosereply(f->fd);
			f->state = closed;
			free(buf);
			return(NULL);
		}
		if (hdrbuf.hdr.sp_dt != SPPSST_RPC) {
			fprintf(stderr,"(Courier) Stream type changed from %d to %d during message\n",
				SPPSST_RPC, hdrbuf.hdr.sp_dt);
			exit(1);
			/* NOTREACHED */
		}
		length += count;
	}
	return((Unspecified*) buf);
}



CheckEND(f)
/* look ahead on courier connection, checking for an END packet.
 * If seen, set state to closed.
 */
	CourierConnection *f;
{
	struct {
		struct sphdr hdr;
		char data[SPPMAXDATA];
	} packbuf;
	int count;
	int fdmask;
	static struct timeval timeout = {0,0};

	fdmask = 1<<(f->fd);
	while (select(f->fd+1,&fdmask,(int*)NULL,(int*)NULL,&timeout) > 0
	    && (count = recv(f->fd,(char*)&packbuf, sizeof(packbuf),
	    			MSG_PEEK)) > 0) {
		if (packbuf.hdr.sp_dt == SPPSST_END) {
			read(f->fd, (char*)&packbuf, sizeof(packbuf));
			(void) sppclosereply(f->fd);
			f->state = closed;
			return(TRUE);
		}
		else if (count == sizeof(struct sphdr))
			read(f->fd, (char*)&packbuf, sizeof(packbuf));
		else return(FALSE);
	}
	return(FALSE);
}


CourierClose(conn)
	CourierConnection * conn;
{
	(void) sppclose(conn->fd);
	free((char*) conn);
}


/* returns either a socket or -1 on error */
int
openSPPConnection(dst)
	struct sockaddr_ns *dst;
{
	int s;
	extern int errno;

	if ((s = socket(dst->sns_family, SOCK_SEQPACKET, 0)) < 0) {
		perror("(Courier) socket");
		return(-1);
		/*NOTREACHED*/
	}
	if (connect(s, (struct sockaddr*)dst, sizeof(struct sockaddr_ns)) < 0) {
		if ((errno != ETIMEDOUT) && (errno != ECONNREFUSED))
			perror("(Courier) connect");
		return(-1);
		/*NOTREACHED*/
	}
	return(s);
}
