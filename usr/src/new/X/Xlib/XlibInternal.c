#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	XlibInternal.c - Internal support routines for the C subroutine
 *	interface library (Xlib) to the X Window System Protocol V8.0.
 */

#include "XlibInternal.h"
#include <sys/uio.h>

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

#ifndef lint
static char rcsid[] = "$Header: XlibInternal.c,v 10.13 86/04/22 15:30:52 jg Rel $";
#endif

#ifdef titan
#define iovbase iov_base.saddr
#else
#define iovbase iov_base
#endif
Display *_XlibCurrentDisplay = NULL;	/* default display to use in library */
_QEvent *_qfree = NULL;			/* NULL _QEvent. */

static int padlength[4] = {0, 3, 2, 1};
    /* lookup table for adding padding bytes to data that is read from
    	or written to the X socket.  */

/*
 * _XFlush - Flush the X request buffer.  If the buffer is empty, no
 * action is taken.  This routine correctly handles incremental writes.
 */
_XFlush (dpy)
	register Display *dpy;
{
	register int size;
	register int write_stat;
	register char *bufindex;

	size = dpy->bufptr - dpy->buffer;
	bufindex = dpy->bufptr = dpy->buffer;
	/*
	 * While write has not written the entire buffer, keep looping
	 * untill the entire buffer is written.  bufindex will be incremented
	 * and size decremented as buffer is written out.
	 */
	while (size) {
		if ((write_stat = write(dpy->fd, bufindex, size)) == -1) {
			/* Write failed! */
			/* errno set by write system call. */
			_XIOError(dpy);
		}
		size -= write_stat;
		bufindex += write_stat;
	}
	dpy->lastdraw = NULL;
}


/* 
 * _XRead - Read bytes from the socket taking into account incomplete
 * reads.
 */
_XRead (dpy, data, size)
	register Display *dpy;
	register char *data;
	register int size;
{
	register int bytes_read;

	while ((bytes_read = read(dpy->fd, data, size)) != size) {

	    	if (bytes_read > 0) {
		    size -= bytes_read;
		    data += bytes_read;
		    }

		else if (bytes_read == 0) {
		    /* Read failed because of end of file! */
		    errno = EPIPE;
		    _XIOError(dpy);
		    }

		else  /* bytes_read is less than 0; presumably -1 */ {
		    /* If it's a system call interrupt, it's not an error. */
		    if (errno != EINTR)
		    	_XIOError(dpy);
		    }
	    	 }
}

/*
 * _XReadPad - Read bytes from the socket taking into account incomplete
 * reads.  If the number of bytes is not 0 mod 32, read additional pad
 * bytes.
 */
_XReadPad (dpy, data, size)
    	register Display *dpy;	
	register char *data;
	register int size;
{
    	register int bytes_read;
	struct iovec iov[2];
	char pad[3];

	iov[0].iov_len = size;
	iov[0].iovbase = data;
	/* 
	 * The following hack is used to provide 32 bit long-word
	 * aligned padding.  The [1] vector is of length 0, 1, 2, or 3,
	 * whatever is needed.
	 */

	iov[1].iov_len = padlength[size & 3];
	iov[1].iovbase = pad;
	size += iov[1].iov_len;

	while ((bytes_read = readv (dpy->fd, iov, 2)) != size) {

	    if (bytes_read > 0) {
		size -= bytes_read;
	    	if ((iov[0].iov_len -= bytes_read) < 0) {
		    iov[1].iov_len += iov[0].iov_len;
		    iov[1].iovbase -= iov[0].iov_len;
		    iov[0].iov_len = 0;
		    }
	    	else
	    	    iov[0].iovbase += bytes_read;
	    	}

	    else if (bytes_read == 0) {
		/* Read failed because of end of file! */
		errno = EPIPE;
		_XIOError(dpy);
		}
	    
	    else  /* bytes_read is less than 0; presumably -1 */ {
		/* If it's a system call interrupt, it's not an error. */
		if (errno != EINTR)
		    _XIOError(dpy);
		}
	    }

}

/*
 * _XSend - Flush the buffer and send the client data. 32 bit word aligned
 * transmission is used, if size is not 0 mod 4, extra bytes are transmitted.
 */
_XSend (dpy, data, size)
	register Display *dpy;
	char *data;
	register int size;
{
	register int len;
	struct iovec iov[3];
	char pad[3];

	iov[0].iov_len = len = dpy->bufptr - dpy->buffer;
	iov[0].iovbase = dpy->bufptr = dpy->buffer;
	iov[1].iov_len = size;
	iov[1].iovbase = data;
	/* 
	 * The following hack is used to provide 32 bit long-word
	 * aligned padding.  The [2] vector is of length 0, 1, 2, or 3,
	 * whatever is needed.
	 */
	iov[2].iov_len = padlength[size & 3];
	iov[2].iovbase = pad;
	len += (size + 3) & ~3;
	/*
	 * Use while to allow for incremental writes.
	 */
	while ((size = writev(dpy->fd, iov, 3)) != len) {
	    	if (size < 0) _XIOError(dpy);
		len -= size;
		if ((iov[0].iov_len -= size) < 0) {
		    iov[1].iov_len += iov[0].iov_len;
		    iov[1].iovbase -= iov[0].iov_len;
		    iov[0].iov_len = 0;
		    if (iov[1].iov_len < 0) {
	    	    	iov[2].iov_len += iov[1].iov_len;
			iov[2].iovbase -= iov[1].iov_len;
			iov[1].iov_len = 0;
		    }
		}
		else {
			iov[0].iovbase += size;
		}
	}
	dpy->lastdraw = NULL;
}

/*
 * _XReply - Wait for a reply packet and copy its contents into the
 * specified rep.  Mean while we must handle error and event packets that
 * we may encounter.
 */
Status _XReply (dpy, rep)
    register Display *dpy;
    register XRep *rep;
{
    _XFlush(dpy);
    while (1) {
	_XRead(dpy, (char *)rep, sizeof(XRep));
	switch ((int)rep->code) {

	    case X_Reply:
	        /* Reply recieved. */
	        return(1);
		 
    	    case X_Error:
	    	{
	    	/* X_Error packet encountered! */
		int current_request = dpy->request;
		XErrorEvent *error = (XErrorEvent *) rep;

		if (error->serial == current_request)
			/* do not die on "no such font", "can't allocate",
			   "can't grab" failures */
			switch (error->error_code) {
			case BadFont:
				if (error->request_code != X_GetFont)
					break;
			case BadAlloc:
			case BadColor:
			case BadGrab:
				return (0);
			}
		_XError(dpy, error);
		if (error->serial == current_request)
		    return(0);
		}
		break;
	    default:
		_XEnq(dpy, (XEvent *) rep);
		break;
	    }
	}
}   


/*
 * _XEnq - Place event packets on the display's queue.
 */
_XEnq (dpy, event)
	register Display *dpy;
	register XEvent *event;
{
	register _QEvent *qelt;
	extern char *malloc();

	if (
		/* If we are squishing MouseMoved events AND ... */
		dpy->squish && 
		/* the current event is a MouseMoved event AND ... */
		(event->type == MouseMoved) &&
		/* if there is a last event on the display queue AND ... */
		(qelt = dpy->tail) && 
		/* if that last event is also a MouseMoved event AND ... */
		(qelt->event.type == MouseMoved) &&
		/* it has the same event window as the current event ... */
		(event->window == qelt->event.window)
	) {
		/* then replace the last event with the current event! */
		qelt->event = *event;
		return;
	}
	if (qelt = _qfree) {
		/* If _qfree is non-NULL do this, else malloc a new one. */
		_qfree = qelt->next;
	}
	else if ((qelt = (_QEvent *) malloc((unsigned)sizeof(_QEvent))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		_XIOError(dpy);
	}
	qelt->next = NULL;
	qelt->event = *event;
	if (dpy->tail) {
		dpy->tail->next = qelt;
	}
	else {
		dpy->head = qelt;
	}
	dpy->tail = qelt;
	dpy->qlen++;
}


/*
 * Undefine the routine pointers so we can define the following default
 * routines.
 */
#undef _XError
#undef _XIOError


/*
 * _XIOError - Default fatal system error reporting routine.  Called when
 * an X internal system error is encountered.
 */
/*ARGSUSED*/
_XIOError (dpy)
	Display *dpy;
{
	perror("XIO");
	exit(1);
}


/*
 * _XError - Default non-fatal error reporting routine.  Called when an
 * X_Error packet is encountered in the input stream.
 */
_XError (dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    fprintf(stderr, "X Error: %s\n", XErrDescrip (rep->error_code));
    fprintf(stderr, "         Request code: %d\n", rep->request_code);
    fprintf(stderr, "         Request function: %d\n", rep->func);
    fprintf(stderr, "         Request window 0x%x\n", rep->window);
    fprintf(stderr, "         Error Serial #%d\n", rep->serial);
    fprintf(stderr, "         Current serial #%d\n", dpy->request);
    exit(1);
}

int (*_XIOErrorFunction)() = _XIOError;
int (*_XErrorFunction)() = _XError;

#ifdef BIGSHORTS
UnpackShorts(from, to, bytes)
	ushort_p *from;
	short *to;
	unsigned bytes;
{
	unsigned i;
	for (i = 0; i < (bytes/psizeof(short)); i++)
		if (i&1)
			to[i] = from[i>>1].right;
		else
			to[i] = from[i>>1].left;
}

char packbuffer[1000];
PackData(dpy, data, len)
    register Display *dpy;
    short *data;
    unsigned len;
{
	if (dpy->bufptr + len < dpy->bufmax) {
		PackShorts(data, dpy->bufptr, len);
		dpy->bufptr += (len + 3) & ~3;
	} else {
		PackShorts(data, packbuffer, len);
		_XSend(dpy, packbuffer, len);
	}
}

PackShorts(from, to, bytes)
	short *from;
	char *to;
	unsigned bytes;
{
	unsigned i, n, offset;
	ushort_p *uto;

	uto = (ushort_p *)to;
	offset = ((int)to & 2) >> 1; /* lost 2 bits of pointer */
	n = (bytes / 2) + offset;
	for (i = offset; i < n; i++) {
		if (i&1)
			uto[i>>1].right = from[i-offset];
		else
			uto[i>>1].left = from[i-offset];
	}
}
#endif
