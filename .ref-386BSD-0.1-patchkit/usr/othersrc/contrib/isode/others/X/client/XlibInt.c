/*
 * $XConsortium: XlibInt.c,v 11.90 88/09/30 17:25:18 jim Exp $
 */

#include "copyright.h"
/* Copyright    Massachusetts Institute of Technology    1985, 1986, 1987 */

/*
 *	XlibInternal.c - Internal support routines for the C subroutine
 *	interface library (Xlib) to the X Window System Protocol V11.0.
 */
#define NEED_EVENTS
#define NEED_REPLIES

#include <stdio.h>
#include "Xlibint.h"

#ifdef ISOCONN
#include <isode/psap.h>
#include <isode/tsap.h>
#include <isode/isoservent.h>

#endif /* ISOCONN */

#ifdef CRAY

/*
 * Cray UniCOS does not have readv and writev so we emulate
 */
#include <sys/socket.h>

static int readv (fd, iov, iovcnt)
int fd;
struct iovec *iov;
int iovcnt;
{
	struct msghdr hdr;

	hdr.msg_iov = iov;
	hdr.msg_iovlen = iovcnt;
	hdr.msg_accrights = 0;
	hdr.msg_accrightslen = 0;
	hdr.msg_name = 0;
	hdr.msg_namelen = 0;

	return (recvmsg (fd, &hdr, 0));
}

static int writev (fd, iov, iovcnt)
int fd;
struct iovec *iov;
int iovcnt;
{
	struct msghdr hdr;

	hdr.msg_iov = iov;
	hdr.msg_iovlen = iovcnt;
	hdr.msg_accrights = 0;
	hdr.msg_accrightslen = 0;
	hdr.msg_name = 0;
	hdr.msg_namelen = 0;

	return (sendmsg (fd, &hdr, 0));
}

#endif /* CRAY */

#ifdef ISOCONN
/*
 * Need these Convenience routines to map IO to T-service 
 * XXX
 * Should map error returns in td->td_reason into
 * errno's appropriately...
 */

/*
 * Check if any bytes queued that could be read...
 */
TBytesReadable(fd, ptr)
int fd;
long *ptr;
{
	struct TSAPdisconnect tds;
        struct TSAPdisconnect *td = &tds;
	int ret = TSelectOctets (fd, ptr, td);
	if (ret == NOTOK) {
		    fprintf(stderr, "Client TBytesReadable: %s\n", 
			TErrString(td->td_reason));
	}
	return ret;
}

/*
 * need followinf for arg mismatch
 */
UBytesReadable(fd, ptr)
int fd;
long *ptr;
{
	return ioctl(fd, FIONREAD, ptr);
}

/* 
 * Simple read from transport cx, client
 */
TReadFromServer(fd, data, size)
int fd;
unsigned size;
char *data;
{
	char *aptr = data;
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
static struct TSAPdata txs;
static struct TSAPdata *tx = &txs;
static struct qbuf *qb;
static char *qptr;
static int ingot, qcpy, result = 0;
	int q2data, ret;

#ifdef ISODEBUG
	if (isodexbug) {
		fprintf(stderr, "TReadFromServer %d want %d (%d buffered)\n", 
			fd, size, result);
	}
#endif /* ISODEBUG */
	if (result == 0) {
		if ((ret = TReadRequest(fd, tx, OK, td)) == NOTOK) {
#ifdef ISODEBUG
		    if (errno == EWOULDBLOCK) {
			fprintf(stderr, "Client TReadReq would block: %s\n", 
				TErrString(td->td_reason));
			if (!DR_FATAL(td->td_reason))
				errno = EWOULDBLOCK;
			else
				errno = EBADF;
			return ret;
		    }
		    if (isodexbug)
			    fprintf(stderr, "Client TReadReq: %s\n", 
				TErrString(td->td_reason));
#endif /* ISODEBUG */
/*
 * map problems here - eg fTimeOut...
 */
		    if (td->td_reason == DR_TIMER)
			errno = EWOULDBLOCK;
		    return ret;
		}
		result = tx->tx_cc;
		qb = &(tx->tx_qbuf);
		qptr = qb->qb_data;
#ifdef ISODEBUG
		if (isodexbug)
			fprintf(stderr, "TReadRequest want %d got %d\n", 
				size, result);
#endif
	} 
#ifdef ISODEBUG
	else {
		if (isodexbug)
			fprintf(stderr, "TReadFromServer want %d buffered %d\n",
				 size, result);
	}
#endif
/*
 * Buffer it
 */
        ingot = 0;
        aptr = data;
	for(ingot = 0, aptr = data, q2data = min(size, result); 
		ingot<q2data;
		aptr += qcpy, ingot+= qcpy) {
	    int aleft = q2data - ingot;
	    if (qb->qb_len > aleft) {
		    qcpy = aleft;
		    bcopy(qptr, aptr, qcpy);
		    qptr += aleft;
	    } else {
		    qcpy = qb->qb_len;
		    bcopy(qb->qb_data, aptr, qcpy);
		    if ((qb = qb->qb_forw) == NULL)
			break;
		    qptr = qb->qb_data;
	    }
        }
	if ((result -= ingot) <= 0) {
		result = 0;
		TXFREE(tx);
	}
	return ingot;

}

/*
 * Simple write on transport descriptor client
 */
TWriteToServer(fd, data, size)
int fd;
unsigned size;
char *data;
{
	struct TSAPdisconnect tds;
        struct TSAPdisconnect *td = &tds;

#ifdef ISODEBUG
	if (isodexbug)
		fprintf(stderr, "TWriteToServer %d: %d\n", fd, size);
#endif
	if (TDataRequest(fd, data, size, td) == NOTOK) {
		if (errno != EWOULDBLOCK)
			fprintf(stderr, "Client TDataReq: %s\n", 
				TErrString(td->td_reason));
		return -1;
	} else
		return size;
}

/*
 * This is really disgusting, as we do 2 copies - one qbuf into data,
 * one data into iovecs...should really do something utterly neater or
 * ask mtr to provide another T-Service interface for pre-alloced
 * bufs - ideally iovec style
 *
 * or change the structure of X to do async...
 */
TReadvFromServer(fd, iov, iovcnt)
int fd, iovcnt;
struct iovec *iov;
{
	int i, size, result, left, bcp; 
	char *data, *dp;
	struct iovec *iovp;

	for(i=0, size = 0, iovp = iov; i < iovcnt; i++, iovp++)
		size += iovp->iov_len;

#ifdef ISODEBUG
	if (isodexbug)
		fprintf(stderr, "TReadvFromServer %d, want %d\n", fd, size);
#endif
	if ((data = Xmalloc(size)) == NULL) {
#ifdef ISODEBUG
	    if (isodexbug)
		    fprintf(stderr, "TReadvFromServer, malloc failed\n");
#endif
/*
 * Could map to EWOULDBLOCK...?
 */
	    return(-1);
	}

/*
 * Note, TReadFromServer is written to *NOT* return more than size
 */
	if ((result = TReadFromServer(fd, data, size)) == NOTOK) {
	    if (errno != EWOULDBLOCK)
		    fprintf(stderr, "TReadvReq err\n");
	    return(-1);
	}

	left = result;
	dp = data;
	while (left > 0) {
	    bcp = iov->iov_len;
	    if (bcp > left )
		bcp = left;
	    bcopy(dp, iov->iov_base, bcp);
	    if (bcp < left)
		iov++;
	    dp += bcp;
	    left -= bcp;
	}
	Xfree(data);
	return result;
}

/*
 * scatter gather write to transport descriptor
 */
TWritevToServer(fd, iov, iovcnt)
int fd, iovcnt;
struct iovec *iov;
{
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	struct udvec uv[64], *uvp;
	int i, ret, tot = 0;

/*
 * Yuck needs dynamicising, or else rely on
 * iov's being same as uv's
 */
	if (iovcnt >= 64) {
		fprintf(stderr, "Very Bad News i am afraid\n");
		return -99;
	}
	for (i=0, uvp = uv; i<iovcnt; uvp++, iov++, i++) {
		uvp->uv_base = iov->iov_base;
		uvp->uv_len = iov->iov_len;
		tot += uvp->uv_len;
	}
#ifdef ISODEBUG
	if (isodexbug)
		fprintf(stderr, "TWritevToServer %d: %d\n",fd, tot);
#endif
	uv[iovcnt].uv_base = NULLCP;
	uv[iovcnt].uv_len = 0;
	if ((ret = TWriteRequest(fd, uv, td)) == NOTOK) {
	    if (errno != EWOULDBLOCK)
		    fprintf(stderr, "Client TReadReq: %s\n", TErrString(td->td_reason));
	    return -1;	
	}
	return tot;
}

TDiscFromServer(fd)
int fd;
{
	struct TSAPdisconnect tds;
        struct TSAPdisconnect *td = &tds;

        if (TDiscRequest(fd, NULLCP, 0, td) == NOTOK)
                fprintf(stderr, "TDR failed %s\n", TErrString(td->td_reason));
}

#endif /* ISOCONN */

/*
 * The following routines are internal routines used by Xlib for protocol
 * packet transmission and reception.
 *
 * XIOError(Display *) will be called if any sort of system call error occurs.
 * This is assumed to be a fatal condition, i.e., XIOError should not return.
 *
 * XError(Display *, XErrorEvent *) will be called whenever an X_Error event is
 * received.  This is not assumed to be a fatal condition, i.e., it is
 * acceptable for this procedure to return.  However, XError should NOT
 * perform any operations (directly or indirectly) on the DISPLAY.
 *
 * Routines declared with a return type of 'Status' return 0 on failure,
 * and non 0 on success.  Routines with no declared return type don't 
 * return anything.  Whenever possible routines that create objects return
 * the object they have created.
 */

_XQEvent *_qfree = NULL;			/* NULL _XQEvent. */

static int padlength[4] = {0, 3, 2, 1};
    /* lookup table for adding padding bytes to data that is read from
    	or written to the X socket.  */

static xReq _dummy_request = {
	0, 0, 0
};
/*
 * _XFlush - Flush the X request buffer.  If the buffer is empty, no
 * action is taken.  This routine correctly handles incremental writes.
 * This routine may have to be reworked if int < long.
 */
_XFlush (dpy)
	register Display *dpy;
{
	register long size, todo;
	register int write_stat;
	register char *bufindex;

	size = todo = dpy->bufptr - dpy->buffer;
	bufindex = dpy->bufptr = dpy->buffer;
	/*
	 * While write has not written the entire buffer, keep looping
	 * until the entire buffer is written.  bufindex will be incremented
	 * and size decremented as buffer is written out.
	 */
	while (size) {
	    errno = 0;
	    write_stat = WriteToServer(dpy->fd, bufindex, (int) todo);
	    if (write_stat >= 0) {
		size -= write_stat;
		todo = size;
		bufindex += write_stat;
#ifdef EWOULDBLOCK
	    } else if (errno == EWOULDBLOCK) {
		_XWaitForWritable(dpy);
#endif
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_XWaitForWritable(dpy);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		todo >>= 1;
#endif
	    } else {
		/* Write failed! */
		/* errno set by write system call. */
		(*_XIOErrorFunction)(dpy);
	    }
	}
	dpy->last_req = (char *)&_dummy_request;
}

int
_XEventsQueued (dpy, mode)
    register Display *dpy;
    int mode;
{
	register int len;
	int pend;
	char buf[BUFSIZE];
	register xReply *rep;
	
	if (mode == QueuedAfterFlush)
	    _XFlush(dpy);
	if (BytesReadable(dpy->fd, (char *) &pend) < 0)
	    (*_XIOErrorFunction)(dpy);
	if ((len = pend) < SIZEOF(xReply))
	    return(dpy->qlen);	/* _XFlush can enqueue events */
	else if (len > BUFSIZE)
	    len = BUFSIZE;
	len /= SIZEOF(xReply);
	pend = len * SIZEOF(xReply);
	_XRead (dpy, buf, (long) pend);

	/* no space between comma and type or else macro will die */
	STARTITERATE (rep,xReply, buf, (len > 0), len--) {
	    if (rep->generic.type == X_Error)
		_XError(dpy, (xError *)rep);
	    else   /* must be an event packet */
		_XEnq(dpy, (xEvent *) rep);
	}
	ENDITERATE
	return(dpy->qlen);
}

/* _XReadEvents - Flush the output queue,
 * then read as many events as possible (but at least 1) and enqueue them
 */
_XReadEvents(dpy)
	register Display *dpy;
{
	char buf[BUFSIZE];
	long pend_not_register; /* because can't "&" a register variable */
	register long pend;
	register xEvent *ev;
	Bool not_yet_flushed = True;

	do {
	    /* find out how much data can be read */
	    if (BytesReadable(dpy->fd, (char *) &pend_not_register) < 0)
	    	(*_XIOErrorFunction)(dpy);
	    pend = pend_not_register;

	    /* must read at least one xEvent; if none is pending, then
	       we'll just flush and block waiting for it */
	    if (pend < SIZEOF(xEvent)) {
	    	pend = SIZEOF(xEvent);
		/* don't flush until we block the first time */
		if (not_yet_flushed) {
		    int qlen = dpy->qlen;
		    _XFlush (dpy);
		    if (qlen != dpy->qlen) return;
		    not_yet_flushed = False;
		}
	    }
		
	    /* but we won't read more than the max buffer size */
	    if (pend > BUFSIZE)
	    	pend = BUFSIZE;

	    /* round down to an integral number of XReps */
	    pend = (pend / SIZEOF(xEvent)) * SIZEOF(xEvent);

	    _XRead (dpy, buf, pend);

	    /* no space between comma and type or else macro will die */
	    STARTITERATE (ev,xEvent, buf, (pend > 0),
			  pend -= SIZEOF(xEvent)) {
		if (ev->u.u.type == X_Error)
		    _XError (dpy, (xError *) ev);
		else  /* it's an event packet; enqueue it */
		    _XEnq (dpy, ev);
	    }
	    ENDITERATE
	} while (dpy->head == NULL);
}

/* 
 * _XRead - Read bytes from the socket taking into account incomplete
 * reads.  This routine may have to be reworked if int < long.
 */
_XRead (dpy, data, size)
	register Display *dpy;
	register char *data;
	register long size;
{
	register long bytes_read;

	if (size == 0) return;
	errno = 0;
	while ((bytes_read = ReadFromServer(dpy->fd, data, (int)size))
		!= size) {

	    	if (bytes_read > 0) {
		    size -= bytes_read;
		    data += bytes_read;
		    }
#ifdef EWOULDBLOCK
		else if (errno == EWOULDBLOCK) {
		    _XWaitForReadable(dpy);
		    errno = 0;
		}
#endif		
#ifdef SUNSYSV
		else if (errno == 0) {
		    _XWaitForReadable(dpy);
		}
#endif
		else if (bytes_read == 0) {
		    /* Read failed because of end of file! */
		    errno = EPIPE;
		    (*_XIOErrorFunction)(dpy);
		    }

		else  /* bytes_read is less than 0; presumably -1 */ {
		    /* If it's a system call interrupt, it's not an error. */
		    if (errno != EINTR)
		    	(*_XIOErrorFunction)(dpy);
		    }
	    	 }
}

#ifdef WORD64

/*
 * XXX This is a *really* stupid way of doing this....
 */

#define PACKBUFFERSIZE 4096


/*
 * _XRead32 - Read bytes from the socket unpacking each 32 bits
 *            into a long (64 bits on a CRAY computer).
 * 
 */
static _doXRead32 (dpy, data, size, packbuffer)
        register Display *dpy;
        register long *data;
        register long size;
	register char *packbuffer;
{
 long *lpack,*lp;
 long mask32 = 0x00000000ffffffff;
 long maskw, nwords, i, bits;

        _XReadPad (dpy, packbuffer, size);

        lp = data;
        lpack = (long *) packbuffer;
        nwords = size >> 2;
        bits = 32;

        for(i=0;i<nwords;i++){
            maskw = mask32 << bits;
           *lp++ = ( *lpack & maskw ) >> bits;
            bits = bits ^32;
            if(bits){
               lpack++;
            }
        }
}

_XRead32 (dpy, data, len)
    Display *dpy;
    long *data;
    long len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nwords = (PACKBUFFERSIZE >> 2);	/* bytes to CARD32 */

    for (; len > nwords; len -= nwords, data += nwords) {
	_doXRead32 (dpy, data, nwords, packbuffer);
    }
    _doXRead32 (dpy, data, len, packbuffer);
}



/*
 * _XRead16 - Read bytes from the socket unpacking each 16 bits
 *            into a long (64 bits on a CRAY computer).
 *
 */
static _doXRead16 (dpy, data, size, packbuffer)
        register Display *dpy;
        register short *data;
        register long size;
	char *packbuffer;
{
	long *lpack,*lp;
	long mask16 = 0x000000000000ffff;
	long maskw, nwords, i, bits;

        _XRead(dpy,packbuffer,size);	/* don't do a padded read... */

        lp = (long *) data;
        lpack = (long *) packbuffer;
        nwords = size >> 1;  /* number of 16 bit words to be unpacked */
        bits = 48;
        for(i=0;i<nwords;i++){
            maskw = mask16 << bits;
           *lp++ = ( *lpack & maskw ) >> bits;
            bits -= 16;
            if(bits < 0){
               lpack++;
               bits = 48;
            }
        }
}

_XRead16 (dpy, data, len)
    Display *dpy;
    short *data;
    long len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nwords = (PACKBUFFERSIZE >> 1);	/* bytes to CARD16 */

    for (; len > nwords; len -= nwords, data += nwords) {
	_doXRead16 (dpy, data, nwords, packbuffer);
    }
    _doXRead16 (dpy, data, len, packbuffer);
}

_XRead16Pad (dpy, data, size)
    Display *dpy;
    short *data;
    long size;
{
    int slop = (size & 3);
    short slopbuf[3];

    _XRead16 (dpy, data, size);
    if (slop > 0) {
	_XRead16 (dpy, slopbuf, 4 - slop);
    }
}
#endif /* WORD64 */


/*
 * _XReadPad - Read bytes from the socket taking into account incomplete
 * reads.  If the number of bytes is not 0 mod 32, read additional pad
 * bytes. This routine may have to be reworked if int < long.
 */
_XReadPad (dpy, data, size)
    	register Display *dpy;	
	register char *data;
	register long size;
{
    	register long bytes_read;
	struct iovec iov[2];
	char pad[3];

	if (size == 0) return;
	iov[0].iov_len = (int)size;
	iov[0].iov_base = data;
	/* 
	 * The following hack is used to provide 32 bit long-word
	 * aligned padding.  The [1] vector is of length 0, 1, 2, or 3,
	 * whatever is needed.
	 */

	iov[1].iov_len = padlength[size & 3];
	iov[1].iov_base = pad;
	size += iov[1].iov_len;

	errno = 0;
	while ((bytes_read = ReadvFromServer (dpy->fd, iov, 2)) != size) {

	    if (bytes_read > 0) {
		size -= bytes_read;
	    	if ((iov[0].iov_len -= bytes_read) < 0) {
		    iov[1].iov_len += iov[0].iov_len;
		    iov[1].iov_base -= iov[0].iov_len;
		    iov[0].iov_len = 0;
		    }
	    	else
	    	    iov[0].iov_base += bytes_read;
	    	}
#ifdef EWOULDBLOCK
	    else if (errno == EWOULDBLOCK) {
		_XWaitForReadable(dpy);
		errno = 0;
	    }
#endif
#ifdef SUNSYSV
	    else if (errno == 0) {
		_XWaitForReadable(dpy);
	    }
#endif
	    else if (bytes_read == 0) {
		/* Read failed because of end of file! */
		errno = EPIPE;
		(*_XIOErrorFunction)(dpy);
		}
	    
	    else  /* bytes_read is less than 0; presumably -1 */ {
		/* If it's a system call interrupt, it's not an error. */
		if (errno != EINTR)
		    (*_XIOErrorFunction)(dpy);
		}
	    }

}

/*
 * _XSend - Flush the buffer and send the client data. 32 bit word aligned
 * transmission is used, if size is not 0 mod 4, extra bytes are transmitted.
 * This routine may have to be reworked if int < long;
 */
_XSend (dpy, data, size)
	register Display *dpy;
	char *data;
	register long size;
{
	struct iovec iov[3];
	static char pad[3] = {0, 0, 0};
           /* XText8 and XText16 require that the padding bytes be zero! */

	long skip = 0;
	long total = (dpy->bufptr - dpy->buffer) + ((size + 3) & ~3);
	long todo = total;

	while (total) {
	    long before = skip;
	    long remain = todo;
	    int i = 0;
	    long len;

	/* You could be very general here and have "in" and "out" iovecs
	 * and write a loop without using a macro, but what the heck
	 */

#define InsertIOV(pointer, length) \
	    len = (length) - before; \
	    if (len > remain) \
		len = remain; \
	    if (len <= 0) { \
		before = -len; \
	    } else { \
		iov[i].iov_len = len; \
		iov[i].iov_base = (pointer) + before; \
		i++; \
		remain -= len; \
		before = 0; \
	    }

	    InsertIOV(dpy->buffer, dpy->bufptr - dpy->buffer)
	    InsertIOV(data, size)
	    /* Provide 32-bit aligned padding as necessary */
	    InsertIOV(pad, padlength[size & 3])
    
	    errno = 0;
	    if ((len = WritevToServer(dpy->fd, iov, i)) >= 0) {
		skip += len;
		total -= len;
		todo = total;
#ifdef EWOULDBLOCK
	    } else if (errno == EWOULDBLOCK) {
		_XWaitForWritable(dpy);
#endif
#ifdef SUNSYSV
	    } else if (errno == 0) {
		_XWaitForWritable(dpy);
#endif
#ifdef EMSGSIZE
	    } else if (errno == EMSGSIZE) {
		todo = todo >> 1;
#endif
	    } else {
		(*_XIOErrorFunction)(dpy);
	    }
	}

	dpy->bufptr = dpy->buffer;
	dpy->last_req = (char *) & _dummy_request;
}

/*
 * _XAllocID - normal resource ID allocation routine.  A client
 * can roll his own and instatantiate it if he wants, but must
 * follow the rules.
 */
XID _XAllocID(dpy)
register Display *dpy;
{
   return (dpy->resource_base + (dpy->resource_id++ << dpy->resource_shift));
}

/*
 * The hard part about this is that we only get 16 bits from a reply.  Well,
 * then, we have three values that will march along, with the following
 * invariant:
 *	dpy->last_request_read <= rep->sequenceNumber <= dpy->request
 * The right choice for rep->sequenceNumber is the largest that
 * still meets these constraints.
 */
static unsigned long
_SetLastRequestRead(dpy, rep)
    register Display *dpy;
    register xGenericReply *rep;
{
    register unsigned long	newseq, lastseq;

    /*
     * KeymapNotify has no sequence number, but is always guaranteed
     * to immediately follow another event, except when generated via
     * SendEvent (hmmm).
     */
    if ((rep->type & 0x7f) == KeymapNotify)
	return(dpy->last_request_read);

    newseq = (dpy->last_request_read & ~((unsigned long)0xffff)) |
	     rep->sequenceNumber;
    lastseq = dpy->last_request_read;
    while (newseq < lastseq) {
	newseq += 0x10000;
	if (newseq > dpy->request) {
	    (void) fprintf (stderr, 
	    "Xlib:  sequence lost (0x%lx > 0x%lx) in reply type 0x%x!\n",
	                           newseq, dpy->request, 
				   (unsigned int) rep->type);
	    newseq -= 0x10000;
	    break;
	}
    }

    dpy->last_request_read = newseq;
    return(newseq);
}

/*
 * _XReply - Wait for a reply packet and copy its contents into the
 * specified rep.  Mean while we must handle error and event packets that
 * we may encounter.
 */
Status _XReply (dpy, rep, extra, discard)
    register Display *dpy;
    register xReply *rep;
    int extra;		/* number of 32-bit words expected after the reply */
    Bool discard;	/* should I discard data followind "extra" words? */
{
    /* Pull out the serial number now, so that (currently illegal) requests
     * generated by an error handler don't confuse us.
     */
    unsigned long cur_request = dpy->request;

    _XFlush(dpy);
    while (1) {
	_XRead(dpy, (char *)rep, (long)SIZEOF(xReply));
	switch ((int)rep->generic.type) {

	    case X_Reply:
	        /* Reply received.  Fast update for synchronous replies,
		 * but deal with multiple outstanding replies.
		 */
	        if (rep->generic.sequenceNumber == (cur_request & 0xffff))
		    dpy->last_request_read = cur_request;
		else
		    (void) _SetLastRequestRead(dpy, &rep->generic);
		if (extra == 0) {
		    if (discard && (rep->generic.length > 0))
		       /* unexpectedly long reply! */
		       _EatData (dpy, rep->generic.length);
		    return (1);
		    }
		if (extra == rep->generic.length) {
		    /* 
		     * Read the extra data into storage immediately following
		     * the GenericReply structure. 
		     */
		    _XRead (dpy, NEXTPTR(rep,xReply), ((long)extra) << 2);
		    return (1);
		    }
		if (extra < rep->generic.length) {
		    /* Actual reply is longer than "extra" */
		    _XRead (dpy, NEXTPTR(rep,xReply), ((long)extra) << 2);
		    if (discard)
		        _EatData (dpy, rep->generic.length - extra);
		    return (1);
		    }
		/* 
		 *if we get here, then extra > rep->generic.length--meaning we
		 * read a reply that's shorter than we expected.  This is an 
		 * error,  but we still need to figure out how to handle it...
		 */
		_XRead (dpy, NEXTPTR(rep,xReply),
			((long) rep->generic.length) << 2);
		(*_XIOErrorFunction) (dpy);
		return (0);

    	    case X_Error:
	    	{
	        register _XExtension *ext;
		register Bool ret = False;
		int ret_code;
		xError *err = (xError *) rep;
		unsigned long serial;

		serial = _SetLastRequestRead(dpy, (xGenericReply *)rep);
		if (serial == cur_request)
			/* do not die on "no such font", "can't allocate",
			   "can't grab" failures */
			switch ((int)err->errorCode) {
			case BadName:
			    switch (err->majorCode) {
				case X_OpenFont:
				case X_LookupColor:
				case X_AllocNamedColor:
				    return(0);
			    }
			    break;
			case BadFont:
			    if (err->majorCode == X_QueryFont)
				return (0);
			    break;
			case BadAlloc:
			case BadAccess:
				return (0);
			/* 
			 * we better see if there is an extension who may
			 * want to suppress the error.
			 */
			default:
			    ext = dpy->ext_procs;
			    while (ext) {
				if (ext->error != NULL) 
				   ret = (*ext->error)
					(dpy, err, &ext->codes, &ret_code);
				ext = ext->next;
				}
			    if (ret) return (ret_code);
			    break;
			}
		_XError(dpy, err);
		if (serial == cur_request)
		    return(0);
		}
		break;
	    default:
		_XEnq(dpy, (xEvent *) rep);
		break;
	    }
	}
}   


/* Read and discard "n" 32-bit words. */

static _EatData (dpy, n)
    Display *dpy;
    unsigned long n;
    {
    unsigned int bufsize;
    char *buf;
    n <<= 2;  /* convert to number of bytes */
    buf = Xmalloc (bufsize = (n > 2048) ? 2048 : n);
    while (n) {
	long bytes_read = (n > bufsize) ? bufsize : n;
	_XRead (dpy, buf, bytes_read);
	n -= bytes_read;
	}
    Xfree (buf);
    }

/*
 * _XEnq - Place event packets on the display's queue.
 * note that no squishing of move events in V11, since there
 * is pointer motion hints....
 */
_XEnq (dpy, event)
	register Display *dpy;
	register xEvent *event;
{
	register _XQEvent *qelt;

/*NOSTRICT*/
	if (qelt = _qfree) {
		/* If _qfree is non-NULL do this, else malloc a new one. */
		_qfree = qelt->next;
	}
	else if ((qelt = 
	    (_XQEvent *) Xmalloc((unsigned)sizeof(_XQEvent))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		(*_XIOErrorFunction)(dpy);
	}
	qelt->next = NULL;
	/* go call through display to find proper event reformatter */
	if ((*dpy->event_vec[event->u.u.type & 0177])(dpy, &qelt->event, event)) {
	    if (dpy->tail)	dpy->tail->next = qelt;
	    else 		dpy->head = qelt;
    
	    dpy->tail = qelt;
	    dpy->qlen++;
	} else {
	    /* ignored, or stashed away for many-to-one compression */
	    qelt->next = _qfree;
	    _qfree = qelt;
	}
}
/*
 * EventToWire in seperate file in that often not needed.
 */

/*ARGSUSED*/
Bool
_XUnknownWireEvent(dpy, re, event)
register Display *dpy;	/* pointer to display structure */
register XEvent *re;	/* pointer to where event should be reformatted */
register xEvent *event;	/* wire protocol event */
{
#ifdef notdef
	(void) fprintf(stderr, 
	    "Xlib: unhandled wire event! event number = %d, display = %x\n.",
			event->u.u.type, dpy);
#endif
	return(False);
}

/*ARGSUSED*/
Status
_XUnknownNativeEvent(dpy, re, event)
register Display *dpy;	/* pointer to display structure */
register XEvent *re;	/* pointer to where event should be reformatted */
register xEvent *event;	/* wire protocol event */
{
#ifdef notdef
	(void) fprintf(stderr, 
 	   "Xlib: unhandled native event! event number = %d, display = %x\n.",
			re->type, dpy);
#endif
	return(0);
}
/*
 * reformat a wire event into an XEvent structure of the right type.
 */
Bool
_XWireToEvent(dpy, re, event)
register Display *dpy;	/* pointer to display structure */
register XEvent *re;	/* pointer to where event should be reformatted */
register xEvent *event;	/* wire protocol event */
{

	re->type = event->u.u.type & 0x7f;
	((XAnyEvent *)re)->serial = _SetLastRequestRead(dpy,
					(xGenericReply *)event);
	((XAnyEvent *)re)->send_event = ((event->u.u.type & 0x80) != 0);
	((XAnyEvent *)re)->display = dpy;
	
	/* Ignore the leading bit of the event type since it is set when a
		client sends an event rather than the server. */

	switch (event-> u.u.type & 0177) {
	      case KeyPress:
	      case KeyRelease:
	        {
			register XKeyEvent *ev = (XKeyEvent*) re;
			ev->root 	= event->u.keyButtonPointer.root;
			ev->window 	= event->u.keyButtonPointer.event;
			ev->subwindow 	= event->u.keyButtonPointer.child;
			ev->time 	= event->u.keyButtonPointer.time;
			ev->x 		= event->u.keyButtonPointer.eventX;
			ev->y 		= event->u.keyButtonPointer.eventY;
			ev->x_root 	= event->u.keyButtonPointer.rootX;
			ev->y_root 	= event->u.keyButtonPointer.rootY;
			ev->state	= event->u.keyButtonPointer.state;
			ev->same_screen	= event->u.keyButtonPointer.sameScreen;
			ev->keycode 	= event->u.u.detail;
		}
	      	break;
	      case ButtonPress:
	      case ButtonRelease:
	        {
			register XButtonEvent *ev =  (XButtonEvent *) re;
			ev->root 	= event->u.keyButtonPointer.root;
			ev->window 	= event->u.keyButtonPointer.event;
			ev->subwindow 	= event->u.keyButtonPointer.child;
			ev->time 	= event->u.keyButtonPointer.time;
			ev->x 		= event->u.keyButtonPointer.eventX;
			ev->y 		= event->u.keyButtonPointer.eventY;
			ev->x_root 	= event->u.keyButtonPointer.rootX;
			ev->y_root 	= event->u.keyButtonPointer.rootY;
			ev->state	= event->u.keyButtonPointer.state;
			ev->same_screen	= event->u.keyButtonPointer.sameScreen;
			ev->button 	= event->u.u.detail;
		}
	        break;
	      case MotionNotify:
	        {
			register XMotionEvent *ev =   (XMotionEvent *)re;
			ev->root 	= event->u.keyButtonPointer.root;
			ev->window 	= event->u.keyButtonPointer.event;
			ev->subwindow 	= event->u.keyButtonPointer.child;
			ev->time 	= event->u.keyButtonPointer.time;
			ev->x 		= event->u.keyButtonPointer.eventX;
			ev->y 		= event->u.keyButtonPointer.eventY;
			ev->x_root 	= event->u.keyButtonPointer.rootX;
			ev->y_root 	= event->u.keyButtonPointer.rootY;
			ev->state	= event->u.keyButtonPointer.state;
			ev->same_screen	= event->u.keyButtonPointer.sameScreen;
			ev->is_hint 	= event->u.u.detail;
		}
	        break;
	      case EnterNotify:
	      case LeaveNotify:
		{
			register XCrossingEvent *ev   = (XCrossingEvent *) re;
			ev->root	= event->u.enterLeave.root;
			ev->window	= event->u.enterLeave.event;
			ev->subwindow	= event->u.enterLeave.child;
			ev->time	= event->u.enterLeave.time;
			ev->x		= event->u.enterLeave.eventX;
			ev->y		= event->u.enterLeave.eventY;
			ev->x_root	= event->u.enterLeave.rootX;
			ev->y_root	= event->u.enterLeave.rootY;
			ev->state	= event->u.enterLeave.state;
			ev->mode	= event->u.enterLeave.mode;
			ev->same_screen = (event->u.enterLeave.flags & 
				ELFlagSameScreen) && True;
			ev->focus	= (event->u.enterLeave.flags &
			  	ELFlagFocus) && True;
			ev->detail	= event->u.u.detail;
		}
		  break;
	      case FocusIn:
	      case FocusOut:
		{
			register XFocusChangeEvent *ev = (XFocusChangeEvent *) re;
			ev->window 	= event->u.focus.window;
			ev->mode	= event->u.focus.mode;
			ev->detail	= event->u.u.detail;
		}
		  break;
	      case KeymapNotify:
		{
			register XKeymapEvent *ev = (XKeymapEvent *) re;
			ev->window	= dpy->current;
			bcopy ((char *)((xKeymapEvent *) event)->map,
			       &ev->key_vector[1], 
			       sizeof (((xKeymapEvent *) event)->map));
		}
		break;
	      case Expose:
		{
			register XExposeEvent *ev = (XExposeEvent *) re;
			ev->window	= event->u.expose.window;
			ev->x		= event->u.expose.x;
			ev->y		= event->u.expose.y;
			ev->width	= event->u.expose.width;
			ev->height	= event->u.expose.height;
			ev->count	= event->u.expose.count;
		}
		break;
	      case GraphicsExpose:
		{
		    register XGraphicsExposeEvent *ev =
			(XGraphicsExposeEvent *) re;
		    ev->drawable	= event->u.graphicsExposure.drawable;
		    ev->x		= event->u.graphicsExposure.x;
		    ev->y		= event->u.graphicsExposure.y;
		    ev->width		= event->u.graphicsExposure.width;
		    ev->height		= event->u.graphicsExposure.height;
		    ev->count		= event->u.graphicsExposure.count;
		    ev->major_code	= event->u.graphicsExposure.majorEvent;
		    ev->minor_code	= event->u.graphicsExposure.minorEvent;
		}
		break;
	      case NoExpose:
		{
		    register XNoExposeEvent *ev = (XNoExposeEvent *) re;
		    ev->drawable	= event->u.noExposure.drawable;
		    ev->major_code	= event->u.noExposure.majorEvent;
		    ev->minor_code	= event->u.noExposure.minorEvent;
		}
		break;
	      case VisibilityNotify:
		{
		    register XVisibilityEvent *ev = (XVisibilityEvent *) re;
		    ev->window		= event->u.visibility.window;
		    ev->state		= event->u.visibility.state;
		}
		break;
	      case CreateNotify:
		{
		    register XCreateWindowEvent *ev =
			 (XCreateWindowEvent *) re;
		    ev->window		= event->u.createNotify.window;
		    ev->parent		= event->u.createNotify.parent;
		    ev->x		= event->u.createNotify.x;
		    ev->y		= event->u.createNotify.y;
		    ev->width		= event->u.createNotify.width;
		    ev->height		= event->u.createNotify.height;
		    ev->border_width	= event->u.createNotify.borderWidth;
		    ev->override_redirect	= event->u.createNotify.override;
		}
		break;
	      case DestroyNotify:
		{
		    register XDestroyWindowEvent *ev =
				(XDestroyWindowEvent *) re;
		    ev->window		= event->u.destroyNotify.window;
		    ev->event		= event->u.destroyNotify.event;
		}
		break;
	      case UnmapNotify:
		{
		    register XUnmapEvent *ev = (XUnmapEvent *) re;
		    ev->window		= event->u.unmapNotify.window;
		    ev->event		= event->u.unmapNotify.event;
		    ev->from_configure	= event->u.unmapNotify.fromConfigure;
		}
		break;
	      case MapNotify:
		{
		    register XMapEvent *ev = (XMapEvent *) re;
		    ev->window		= event->u.mapNotify.window;
		    ev->event		= event->u.mapNotify.event;
		    ev->override_redirect	= event->u.mapNotify.override;
		}
		break;
	      case MapRequest:
		{
		    register XMapRequestEvent *ev = (XMapRequestEvent *) re;
		    ev->window		= event->u.mapRequest.window;
		    ev->parent		= event->u.mapRequest.parent;
		}
		break;
	      case ReparentNotify:
		{
		    register XReparentEvent *ev = (XReparentEvent *) re;
		    ev->event		= event->u.reparent.event;
		    ev->window		= event->u.reparent.window;
		    ev->parent		= event->u.reparent.parent;
		    ev->x		= event->u.reparent.x;
		    ev->y		= event->u.reparent.y;
		    ev->override_redirect	= event->u.reparent.override;
		}
		break;
	      case ConfigureNotify:
		{
		    register XConfigureEvent *ev = (XConfigureEvent *) re;
		    ev->event	= event->u.configureNotify.event;
		    ev->window	= event->u.configureNotify.window;
		    ev->above	= event->u.configureNotify.aboveSibling;
		    ev->x	= event->u.configureNotify.x;
		    ev->y	= event->u.configureNotify.y;
		    ev->width	= event->u.configureNotify.width;
		    ev->height	= event->u.configureNotify.height;
		    ev->border_width  = event->u.configureNotify.borderWidth;
		    ev->override_redirect = event->u.configureNotify.override;
		}
		break;
	      case ConfigureRequest:
		{
		    register XConfigureRequestEvent *ev =
		        (XConfigureRequestEvent *) re;
		    ev->window		= event->u.configureRequest.window;
		    ev->parent		= event->u.configureRequest.parent;
		    ev->above		= event->u.configureRequest.sibling;
		    ev->x		= event->u.configureRequest.x;
		    ev->y		= event->u.configureRequest.y;
		    ev->width		= event->u.configureRequest.width;
		    ev->height		= event->u.configureRequest.height;
		    ev->border_width	= event->u.configureRequest.borderWidth;
		    ev->value_mask	= event->u.configureRequest.valueMask;
		    ev->detail  	= event->u.u.detail;
		}
		break;
	      case GravityNotify:
		{
		    register XGravityEvent *ev = (XGravityEvent *) re;
		    ev->window		= event->u.gravity.window;
		    ev->event		= event->u.gravity.event;
		    ev->x		= event->u.gravity.x;
		    ev->y		= event->u.gravity.y;
		}
		break;
	      case ResizeRequest:
		{
		    register XResizeRequestEvent *ev =
			(XResizeRequestEvent *) re;
		    ev->window		= event->u.resizeRequest.window;
		    ev->width		= event->u.resizeRequest.width;
		    ev->height		= event->u.resizeRequest.height;
		}
		break;
	      case CirculateNotify:
		{
		    register XCirculateEvent *ev = (XCirculateEvent *) re;
		    ev->window		= event->u.circulate.window;
		    ev->event		= event->u.circulate.event;
		    ev->place		= event->u.circulate.place;
		}
		break;
	      case CirculateRequest:
		{
		    register XCirculateRequestEvent *ev =
		        (XCirculateRequestEvent *) re;
		    ev->window		= event->u.circulate.window;
		    ev->parent		= event->u.circulate.event;
		    ev->place		= event->u.circulate.place;
		}
		break;
	      case PropertyNotify:
		{
		    register XPropertyEvent *ev = (XPropertyEvent *) re;
		    ev->window		= event->u.property.window;
		    ev->atom		= event->u.property.atom;
		    ev->time		= event->u.property.time;
		    ev->state		= event->u.property.state;
		}
		break;
	      case SelectionClear:
		{
		    register XSelectionClearEvent *ev =
			 (XSelectionClearEvent *) re;
		    ev->window		= event->u.selectionClear.window;
		    ev->selection	= event->u.selectionClear.atom;
		    ev->time		= event->u.selectionClear.time;
		}
		break;
	      case SelectionRequest:
		{
		    register XSelectionRequestEvent *ev =
		        (XSelectionRequestEvent *) re;
		    ev->owner		= event->u.selectionRequest.owner;
		    ev->requestor	= event->u.selectionRequest.requestor;
		    ev->selection	= event->u.selectionRequest.selection;
		    ev->target		= event->u.selectionRequest.target;
		    ev->property	= event->u.selectionRequest.property;
		    ev->time		= event->u.selectionRequest.time;
		}
		break;
	      case SelectionNotify:
		{
		    register XSelectionEvent *ev = (XSelectionEvent *) re;
		    ev->requestor	= event->u.selectionNotify.requestor;
		    ev->selection	= event->u.selectionNotify.selection;
		    ev->target		= event->u.selectionNotify.target;
		    ev->property	= event->u.selectionNotify.property;
		    ev->time		= event->u.selectionNotify.time;
		}
		break;
	      case ColormapNotify:
		{
		    register XColormapEvent *ev = (XColormapEvent *) re;
		    ev->window		= event->u.colormap.window;
		    ev->colormap	= event->u.colormap.colormap;
		    ev->new		= event->u.colormap.new;
		    ev->state		= event->u.colormap.state;
	        }
		break;
	      case ClientMessage:
		{
		   register int i;
		   register XClientMessageEvent *ev 
		   			= (XClientMessageEvent *) re;
		   ev->window		= event->u.clientMessage.window;
		   ev->format		= event->u.u.detail;
		   switch (ev->format) {
			case 8:	
			   ev->message_type = event->u.clientMessage.u.b.type;
			   for (i = 0; i < 20; i++) 	
			     ev->data.b[i] = event->u.clientMessage.u.b.bytes[i];
			   break;
			case 16:
			   ev->message_type = event->u.clientMessage.u.s.type;
			   ev->data.s[0] = event->u.clientMessage.u.s.shorts0;
			   ev->data.s[1] = event->u.clientMessage.u.s.shorts1;
			   ev->data.s[2] = event->u.clientMessage.u.s.shorts2;
			   ev->data.s[3] = event->u.clientMessage.u.s.shorts3;
			   ev->data.s[4] = event->u.clientMessage.u.s.shorts4;
			   ev->data.s[5] = event->u.clientMessage.u.s.shorts5;
			   ev->data.s[6] = event->u.clientMessage.u.s.shorts6;
			   ev->data.s[7] = event->u.clientMessage.u.s.shorts7;
			   ev->data.s[8] = event->u.clientMessage.u.s.shorts8;
			   ev->data.s[9] = event->u.clientMessage.u.s.shorts9;
			   break;
			case 32:
			   ev->message_type = event->u.clientMessage.u.l.type;
			   ev->data.l[0] = event->u.clientMessage.u.l.longs0;
			   ev->data.l[1] = event->u.clientMessage.u.l.longs1;
			   ev->data.l[2] = event->u.clientMessage.u.l.longs2;
			   ev->data.l[3] = event->u.clientMessage.u.l.longs3;
			   ev->data.l[4] = event->u.clientMessage.u.l.longs4;
			   break;
			default: /* XXX should never occur */
				break;
		    }
	        }
		break;
	      case MappingNotify:
		{
		   register XMappingEvent *ev = (XMappingEvent *)re;
		   ev->first_keycode 	= event->u.mappingNotify.firstKeyCode;
		   ev->request 		= event->u.mappingNotify.request;
		   ev->count 		= event->u.mappingNotify.count;
		}
		break;
	      default:
		return(_XUnknownWireEvent(dpy, re, event));
	}
	return(True);
}


static char *_SysErrorMsg (n)
    int n;
{
    extern char *sys_errlist[];
    extern int sys_nerr;
    char *s = ((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");

    return (s ? s : "no such error");
}

/*
 * _XIOError - Default fatal system error reporting routine.  Called when
 * an X internal system error is encountered.
 */
_XIOError (dpy)
	Display *dpy;
{
	(void) fprintf (stderr, 
	 "XIO:  fatal IO error %d (%s) on X server \"%s\"\r\n",
			errno, _SysErrorMsg (errno), DisplayString (dpy));
	(void) fprintf (stderr, 
	 "      after %lu requests (%lu known processed) with %d events remaining.\r\n",
			NextRequest(dpy) - 1, LastKnownRequestProcessed(dpy),
			QLength(dpy));

	if (errno == EPIPE) {
	    (void) fprintf (stderr,
	 "      The connection was probably broken by a server shutdown or KillClient.\r\n");
	}

	exit (1);
}

/*
 * _XError - Default non-fatal error reporting routine.  Called when an
 * X_Error packet is encountered in the input stream.
 */
int _XError (dpy, rep)
    Display *dpy;
    xError *rep;
{
    XErrorEvent event;
    /* 
     * X_Error packet encountered!  We need to unpack the error before
     * giving it to the user.
     */

    event.display = dpy;
    event.type = X_Error;
    event.serial = _SetLastRequestRead(dpy, (xGenericReply *)rep);
    event.resourceid = rep->resourceID;
    event.error_code = rep->errorCode;
    event.request_code = rep->majorCode;
    event.minor_code = rep->minorCode;
    if (_XErrorFunction != NULL) {
      	return ((*_XErrorFunction)(dpy, &event));
      }
    exit(1);
    /*NOTREACHED*/
}
    
int _XPrintDefaultError (dpy, event, fp)
    Display *dpy;
    XErrorEvent *event;
    FILE *fp;
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "XlibMessage";
    XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);
    XGetErrorDatabaseText(dpy, mtype, "XError", "X Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    XGetErrorDatabaseText(dpy, mtype, "MajorCode", "Request Major code %d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    sprintf(number, "%d", event->request_code);
    XGetErrorDatabaseText(dpy, "XRequest", number, "", 	buffer, BUFSIZ);
    (void) fprintf(fp, " (%s)", buffer);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->minor_code);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->resourceid);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->serial);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, dpy->request);
    fputs("\n", fp);
    if (event->error_code == BadImplementation) return 0;
    return 1;
}

int _XDefaultError(dpy, event)
	Display *dpy;
	XErrorEvent *event;
{
    if (_XPrintDefaultError (dpy, event, stderr) == 0) return 0;
    exit(1);
    /*NOTREACHED*/
}

int (*_XIOErrorFunction)() = _XIOError;
int (*_XErrorFunction)() = _XDefaultError;

/*
 * This routine can be used to (cheaply) get some memory within a single
 * Xlib routine for scratch space.  It is reallocated from the same place
 * each time, unless the library needs a large scratch space.
 */
char *_XAllocScratch (dpy, nbytes)
	register Display *dpy;
	unsigned long nbytes;
{
	if (nbytes > dpy->scratch_length) {
	    if (dpy->scratch_buffer != NULL) Xfree (dpy->scratch_buffer);
	    return( dpy->scratch_length = nbytes, 
	    dpy->scratch_buffer = Xmalloc ((unsigned)nbytes) );
	}
	return (dpy->scratch_buffer);
}

/*
 * Given a visual id, find the visual structure for this id on this display.
 */
Visual *_XVIDtoVisual (dpy, id)
	Display *dpy;
	VisualID id;
{
	register int i, j, k;
	register Screen *sp;
	register Depth *dp;
	register Visual *vp;
	for (i = 0; i < dpy->nscreens; i++) {
		sp = &dpy->screens[i];
		for (j = 0; j < sp->ndepths; j++) {
			dp = &sp->depths[j];
			for (k = 0; k < dp->nvisuals; k++) {
				vp = &dp->visuals[k];
				if (vp->visualid == id) return (vp);
			}
		}
	}
	return (NULL);
}

XFree (data)
	char *data;
{
	Xfree (data);
}

#ifdef DataRoutineIsProcedure
void Data (dpy, data, len)
	Display *dpy;
	char *data;
	long len;
{
	if (dpy->bufptr + (len) <= dpy->bufmax) {
		bcopy(data, dpy->bufptr, (int)len);
		dpy->bufptr += ((len) + 3) & ~3;
	} else {
		_XSend(dpy, data, len);
	}
}
#endif /* DataRoutineIsProcedure */


#ifdef WORD64

/*
 * XXX This is a *really* stupid way of doing this.  It should just use 
 * dpy->bufptr directly, taking into account where in the word it is.
 */

/*
 * Data16 - Place 16 bit data in the buffer.
 *
 * "dpy" is a pointer to a Display.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static doData16(dpy, data, len, packbuffer)
    register Display *dpy;
    short *data;
    unsigned len;
    char *packbuffer;
{
    long *lp,*lpack;
    long i, nwords,bits;
    long mask16 = 0x000000000000ffff;

        lp = (long *)data;
        lpack = (long *)packbuffer;
        *lpack = 0;

/*  nwords is the number of 16 bit values to be packed,
 *  the low order 16 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 1;
        bits = 48;

        for(i=0;i<nwords;i++){
           *lpack ^= (*lp & mask16) << bits;
           bits -= 16 ;
           lp++;
           if(bits < 0){
               lpack++;
               *lpack = 0;
               bits = 48;
           }
        }
        Data(dpy, packbuffer, len);
}

Data16 (dpy, data, len)
    Display *dpy;
    short *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nwords = (PACKBUFFERSIZE >> 1);	/* bytes to CARD16 */

    for (; len > nwords; len -= nwords, data += nwords) {
	doData16 (dpy, data, nwords, packbuffer);
    }
    doData16 (dpy, data, len, packbuffer);
}

/*
 * Data32 - Place 32 bit data in the buffer.
 *
 * "dpy" is a pointer to a Display.
 * "data" is a pointer to the data.
 * "len" is the length in bytes of the data.
 */

static doData32 (dpy, data, len, packbuffer)
    register Display *dpy;
    long *data;
    unsigned len;
    char *packbuffer;
{
    long *lp,*lpack;
    long i,bits,nwords;
    long mask32 = 0x00000000ffffffff;

        lpack = (long *) packbuffer;
        lp = data;

        *lpack = 0;

/*  nwords is the number of 32 bit values to be packed
 *  the low order 32 bits of each word will be packed
 *  into 64 bit words
 */
        nwords = len >> 2;
        bits = 32;

        for(i=0;i<nwords;i++){
           *lpack ^= (*lp & mask32) << bits;
           bits = bits ^32;
           lp++;
           if(bits){
              lpack++;
              *lpack = 0;
           }
        }
        Data(dpy, packbuffer, len);
}

Data32 (dpy, data, len)
    Display *dpy;
    short *data;
    unsigned len;
{
    char packbuffer[PACKBUFFERSIZE];
    unsigned nwords = (PACKBUFFERSIZE >> 2);	/* bytes to CARD32 */

    for (; len > nwords; len -= nwords, data += nwords) {
	doData32 (dpy, data, nwords, packbuffer);
    }
    doData32 (dpy, data, len, packbuffer);
}

#endif /* WORD64 */



/*
 * _XFreeQ - free the queue of events, called by XCloseDisplay when there are
 * no more displays left on the display list
 */

void _XFreeQ ()
{
    register _XQEvent *qelt = _qfree;
  
    while (qelt) {
	register _XQEvent *qnext = qelt->next;
	Xfree (qelt);
	qelt = qnext;
    }
    _qfree = NULL;
    return;
}


