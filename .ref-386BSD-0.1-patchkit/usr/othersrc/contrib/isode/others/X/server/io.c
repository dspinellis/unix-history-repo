/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: io.c,v 1.49 88/09/06 15:50:44 jim Exp $ */
/*****************************************************************
 * i/o functions
 *
 *   WriteToClient, ReadRequestFromClient
 *
 *****************************************************************/

#include <stdio.h>
#include "Xos.h"
#include "Xmd.h"
#include <errno.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/uio.h>
#include "X.h"
#include "Xproto.h"
#include "os.h"
#include "osdep.h"
#include "opaque.h"
#include "dixstruct.h"
#include "misc.h"

#ifdef ISOCONN
#include <isode/tsap.h>
#endif /* ISOCONN */

extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long OutputPending[];
extern long OutputBufferSize;
extern ClientPtr ConnectionTranslation[];
extern Bool NewOutputPending;
extern Bool AnyClientsWriteBlocked;
static Bool CriticalOutputPending;
static int timesThisConnection = 0;

extern int errno;

#define request_length(req, cli) ((cli->swapped ? \
	lswaps((req)->length) : (req)->length) << 2)
#define MAX_TIMES_PER         10

#ifdef ISOCONN
/*
 * Convenience Routines
 */
TWriteToClient(sd, buf, len)
int sd, len;
char *buf;
{
    struct TSAPdisconnect tds;
    struct TSAPdisconnect *td = &tds;

    if (TDataRequest(sd, buf, len, td) == NOTOK){	
	if (errno != EWOULDBLOCK)
		fprintf(stderr, "TWriteToClient: %s\n", td->td_reason);
    }
}
/*
 * sd = transport descriptor
 * iov is iovec of iovcnt buffers
 */
TWritevToClient(sd, iov, iovcnt)
int sd, iovcnt;
struct iovec *iov;
{
    int i, ret, tot = 0;
    struct udvec uv[64], *uvp = uv;
    struct TSAPdisconnect tds;
    struct TSAPdisconnect *td = &tds;

/*
 * Grotty hack
 */
    if (iovcnt >= 64) {
	fprintf(stderr, "Oh Spaghettio\n");
	return -1;
    }
    for(i=0; i<iovcnt; i++, uvp++, iov++) {
	uvp->uv_base = iov->iov_base;
	uvp->uv_len = iov->iov_len;
	tot += iov->iov_len;
    }
    uvp->uv_base = NULL;
    uvp->uv_len = 0;
	
    ret = TWriteRequest (sd, uv, td);
    if (ret == NOTOK) {
#ifdef ISODEBUG
	if (errno != EWOULDBLOCK) 
		if (isodexbug)
			fprintf(stderr, "TWritevToCl: %s\n", TErrString(td->td_reason));
#endif /* ISODEBUG */
	return ret;
    } else {
#ifdef ISODEBUG
	if  (isodexbug)
		fprintf(stderr, "TWritevToCl to %d: %d\n", sd, tot);
#endif /* ISODEBUG */
	return tot;
    }
}

TAcceptFromClient(fd, vecp, vec)
int fd;
int vecp;
char **vec;
{
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	struct TSAPstart tsts;
	struct TSAPstart *tst = &tsts;

	if (TInit(vecp, vec, tst,  td) == NOTOK) {
                Error(TErrString(td->td_reason));
                Error("TInit");
		return -1;
	}

	if (TConnResponse(tst->ts_sd, NULLTA, tst->ts_expedited, NULL, 0,
                        &(tst->ts_qos), td) == NOTOK) {
                Error(TErrString(td->td_reason));
                Error("TConnResponse");
		return -1;
	}
	return tst->ts_sd;
}

TDiscFromClient(fd)
int fd;
{
	struct TSAPdisconnect tds;

	if (TDiscRequest(fd, NULLCP, 0, &tds)==NOTOK)
                    fprintf(stderr, "TDisc Failed %s\n",
                        TErrString(tds.td_reason));
}
#endif /* ISOCONN */

/*****************************************************************
 * ReadRequestFromClient
 *    Returns one request from client.  If the client misbehaves,
 *    returns NULL.  The dispatcher closes down all misbehaving clients.  
 *
 *        client:  index into bit array returned from WaitForSomething() 
 *
 *        status: status is set to
 *            > 0 the number of bytes in the request if the read is sucessful 
 *            = 0 if action would block (entire request not ready)
 *            < 0 indicates an error (probably client died)
 *
 *        oldbuf:
 *            To facilitate buffer management (e.g. on multi-processor
 *            systems), the diX layer must tell the OS layer when it is 
 *            done with a request, so the parameter oldbuf is a pointer 
 *            to a request that diX is finished with.  In the 
 *            sample implementation, which is single threaded,
 *            oldbuf is ignored.  We assume that when diX calls
 *            ReadRequestFromClient(), the previous buffer is finished with.
 *
 *    The returned string returned must be contiguous so that it can be
 *    cast in the dispatcher to the correct request type.  Because requests
 *    are variable length, ReadRequestFromClient() must look at the first 4
 *    bytes of a request to determine the length (the request length is
 *    always the 3rd byte in the request).  
 *
 *    Note: in order to make the server scheduler (WaitForSomething())
 *    "fair", the ClientsWithInput mask is used.  This mask tells which
 *    clients have FULL requests left in their buffers.  Clients with
 *    partial requests require a read.  Basically, client buffers
 *    are drained before select() is called again.  But, we can't keep
 *    reading from a client that is sending buckets of data (or has
 *    a partial request) because others clients need to be scheduled.
 *****************************************************************/

ConnectionInput inputBuffers[MAXSOCKS];    /* buffers for clients */

/*ARGSUSED*/
char *
ReadRequestFromClient(who, status, oldbuf)
    ClientPtr who;
    int *status;          /* read at least n from client */
    char *oldbuf;
{
#define YieldControl()				\
        { isItTimeToYield = TRUE;		\
	  timesThisConnection = 0; }
#define YieldControlNoInput()			\
        { YieldControl();			\
	  BITCLEAR(ClientsWithInput, client); }
#define YieldControlAndReturnNull()		\
        { YieldControlNoInput();		\
	  return((char *) NULL ); }

    OsCommPtr oc = (OsCommPtr)who->osPrivate;
    int client = oc->fd;
    int result, gotnow, needed;
    register ConnectionInput *pBuff;
    register xReq *request;

        /* ignore oldbuf, just assume we're done with prev. buffer */

    if (client == -1) 
    {
	ErrorF( "OH NO, %d translates to -1\n", who);
	return((char *)NULL);
    }

    pBuff = &inputBuffers[client];
    pBuff->bufptr += pBuff->lenLastReq;
    pBuff->lenLastReq = 0;

            /* handle buffer empty or full case first */

    if ((pBuff->bufptr - pBuff->buffer) >= pBuff->bufcnt)
    {
#ifdef ISOCONN
	result = SRead(client, pBuff->buffer, pBuff->size, OK);
#else /* ISOCONN */
        result = read(client, pBuff->buffer, pBuff->size);
#endif /* ISOCONN */
	if (result < 0) 
	{
	    if (errno == EWOULDBLOCK)
	        *status = 0;
	    else
	        *status = -1;
	    YieldControlAndReturnNull();
	}
	else if (result == 0)
        {
	    *status = -1;
	    YieldControlAndReturnNull();
	}
	else 
	{
	    pBuff->bufcnt = result; 
	    /* free up some space after huge requests */
	    if ((pBuff->size > BUFWATERMARK) && (result < BUFSIZE))
	    {
		pBuff->size = BUFSIZE;
		pBuff->buffer = (char *)xrealloc(pBuff->buffer, pBuff->size);
	    }
	    pBuff->bufptr = pBuff->buffer;
	}
    }
              /* now look if there is enough in the buffer */

    request = (xReq *)pBuff->bufptr;
    gotnow = pBuff->bufcnt + pBuff->buffer - pBuff->bufptr;

    if (gotnow < sizeof(xReq))
       needed = sizeof(xReq) - gotnow;
    else
    {
        needed = request_length(request, who);
        if (needed > MAXBUFSIZE)
        {
    	    *status = -1;
	    YieldControlAndReturnNull();
        }
	if (needed <= 0)
            needed = sizeof(xReq);
    }
        /* if the needed amount won't fit in what's remaining,
	   move everything to the front of the buffer.  If the
	   entire header isn't available, move what's there too */
    if ((pBuff->bufptr + needed - pBuff->buffer > pBuff->size) ||
		(gotnow < sizeof(xReq)))
    {
        bcopy(pBuff->bufptr, pBuff->buffer, gotnow);
	pBuff->bufcnt = gotnow;
        if (needed > pBuff->size)
        {
	    pBuff->size = needed;
    	    pBuff->buffer = (char *)xrealloc(pBuff->buffer, needed);
        }
        pBuff->bufptr = pBuff->buffer;
    }
               /* don't have a full header */
    if (gotnow < sizeof(xReq))
    {
        while (pBuff->bufcnt + pBuff->buffer - pBuff->bufptr < sizeof(xReq))
	{
#ifdef ISOCONN
	    result = SRead(client, 
			pBuff->buffer + pBuff->bufcnt, 
			pBuff->size - pBuff->bufcnt, 
			OK);
#else /* ISOCONN */
	    result = read(client, pBuff->buffer + pBuff->bufcnt, 
		      pBuff->size - pBuff->bufcnt); 
#endif /* ISOCONN */
	    if (result < 0)
	    {
		if (errno == EWOULDBLOCK)
		    *status = 0;
		else
		    *status = -1;
		YieldControlAndReturnNull();
	    }
	    if (result == 0)
	    {
		*status = -1;
		YieldControlAndReturnNull();
	    }
            pBuff->bufcnt += result;        
	}
        request = (xReq *)pBuff->bufptr;
        gotnow = pBuff->bufcnt + pBuff->buffer - pBuff->bufptr;
        needed = request_length(request, who);
	if (needed <= 0)
            needed = sizeof(xReq);
        if (needed > pBuff->size)
        {
	    pBuff->size = needed;
    	    pBuff->buffer = (char *)xrealloc(pBuff->buffer, needed);
        }
        pBuff->bufptr = pBuff->buffer;
    }	

    if (gotnow < needed )   
    {
	int i, wanted;

	wanted = needed - gotnow;
	i = 0;
	while (i < wanted) 
	{
#ifdef ISOCONN
	    result = SRead(client, 
			    pBuff->buffer + pBuff->bufcnt, 
			    pBuff->size - pBuff->bufcnt,
			    OK); 
#else /* ISOCONN */
	    result = read(client, pBuff->buffer + pBuff->bufcnt, 
			  pBuff->size - pBuff->bufcnt); 
#endif /* ISOCONN */
	    if (result < 0) 
	    {
		if (errno == EWOULDBLOCK)
		    *status = 0;
		else
		    *status = -1;
		YieldControlAndReturnNull();
	    }
	    else if (result == 0)
	    {
		*status = -1;
		YieldControlAndReturnNull();
	    }
	    i += result;
    	    pBuff->bufcnt += result;
	}
    }
    *status = needed;
    pBuff->lenLastReq = needed;

    /*
     *  Check to see if client has at least one whole request in the
     *  buffer.  If there is only a partial request, treat like buffer
     *  is empty so that select() will be called again and other clients
     *  can get into the queue.   
     */

    timesThisConnection++;
    if (pBuff->bufcnt + pBuff->buffer >= pBuff->bufptr + needed + sizeof(xReq)) 
    {
	request = (xReq *)(pBuff->bufptr + needed);
        if ((pBuff->bufcnt + pBuff->buffer) >= 
            ((char *)request + request_length(request, who)))
	    BITSET(ClientsWithInput, client);
        else
	    YieldControlNoInput();
    }
    else
	YieldControlNoInput();
    if (timesThisConnection == MAX_TIMES_PER)
	YieldControl();

    return((char *)pBuff->bufptr);

#undef YieldControlAndReturnNull
#undef YieldControlNoInput
#undef YieldControl
}


    /* lookup table for adding padding bytes to data that is read from
    	or written to the X socket.  */
static int padlength[4] = {0, 3, 2, 1};

 /********************
 * FlushClient()
 *    If the client isn't keeping up with us, then we try to continue
 *    buffering the data and set the apropriate bit in ClientsWritable
 *    (which is used by WaitFor in the select).  If the connection yields
 *    a permanent error, or we can't allocate any more space, we then
 *    close the connection.
 *
 **********************/

static int
FlushClient(who, oc, extraBuf, extraCount)
    ClientPtr who;
    OsCommPtr oc;
    char *extraBuf;
    int extraCount; /* do not modify... returned below */
{
    int connection = oc->fd,
    	total, n, i, notWritten, written,
	iovCnt = 0;
    struct iovec iov[3];
    char padBuffer[3];
#ifdef ISOCONN
    struct TSAPdisconnect tds;
#endif /* ISOCONN */

    total = 0;
    if (oc->count)
    {
	total += iov[iovCnt].iov_len = oc->count;
	iov[iovCnt++].iov_base = (caddr_t)oc->buf;
        /* Notice that padding isn't needed for oc->buf since
           it is alreay padded by WriteToClient */
    }
    if (extraCount)
    {
	total += iov[iovCnt].iov_len = extraCount;
	iov[iovCnt++].iov_base = extraBuf;
	if (extraCount & 3)
	{
	    total += iov[iovCnt].iov_len = padlength[extraCount & 3];
	    iov[iovCnt++].iov_base = padBuffer;
	}
    }

    notWritten = total;
	
#ifdef ISOCONN
    while ((n = SWritev (connection, iov, iovCnt)) != notWritten)
#else /* ISOCONN */
    while ((n = writev (connection, iov, iovCnt)) != notWritten)
#endif /* ISOCONN */
    {
#ifdef hpux
	if (n == -1 && errno == EMSGSIZE)
	    n = swWritev (connection, iov, 2);
#endif
        if (n > 0) 
        {
	    notWritten -= n;
	    for (i = 0; i < iovCnt; i++)
            {
		if (n > iov[i].iov_len)
		{
		    n -= iov[i].iov_len;
		    iov[i].iov_len = 0;
		}
		else
		{
		    iov[i].iov_len -= n;
		    iov[i].iov_base += n;
		    break;
		}
	    }
	    continue;
	}
	else if (errno != EWOULDBLOCK)
        {
#ifdef notdef  
	    if (errno != EBADF)
		ErrorF("Closing connection %d because write failed\n",
			connection);
		/* this close will cause the select in WaitForSomething
		   to return that the connection is dead, so we can actually
		   clean up after the client.  We can't clean up here,
		   because the we're in the middle of doing something
		   and will probably screw up some data strucutres */
#endif
#ifdef ISOCONN
	    SClose(connection);
#else /* ISOCONN */
	    close(connection);
#endif /* ISOCONN */
            MarkClientException(who);
	    return(-1);
	}

	/* If we've arrived here, then the client is stuffed to the gills
	   and not ready to accept more.  Make a note of it and buffer
	   the rest. */
	BITSET(ClientsWriteBlocked, connection);
	AnyClientsWriteBlocked = TRUE;

	written = total - notWritten;
	if (written < oc->count)
	{
	    if (written > 0)
	    {
		oc->count -= written;
		bcopy((char *)oc->buf + written, (char *)oc->buf, oc->count);
		written = 0;
	    }
	}
	else
	{
	    written -= oc->count;
	    oc->count = 0;
	}

	if (notWritten > oc->bufsize)
	{
	    /* allocate at least enough to contain it plus one
	       OutputBufferSize */
	    oc->bufsize = notWritten + OutputBufferSize;
	    oc->buf = (unsigned char *)xrealloc(oc->buf, oc->bufsize);
	    if (oc->buf == NULL)
	    {
	outOfMem:
#ifdef notdef
		ErrorF("Closing connection %d because out of memory\n",
			connection);
		/* this close will cause the select in WaitForSomething
		   to return that the connection is dead, so we can actually
		   clean up after the client.  We can't clean up here,
		   because the we're in the middle of doing something
		   and will probably screw up some data strucutres */
#endif
#ifdef ISOCONN
#ifdef ISODEBUG
		fprintf(stderr, "out of mem: closing connection %d\n", 
			connection);
#endif /* ISODEBUG */
		SClose(connection);
#else /* ISOCONN */
		close(connection);
#endif /* ISOCONN */
		MarkClientException(who);
		oc->count = 0;
		oc->bufsize = 0;
		return(-1);
	    }
	}

	/* If the amount written extended into the padBuffer, then the
	   difference "extraCount - written" may be less than 0 */
	if ((n = extraCount - written) > 0)
	    bcopy (extraBuf + written, (char *)oc->buf + oc->count, n);

	oc->count = notWritten; /* this will include the pad */

	return extraCount; /* return only the amount explicitly requested */
    }

    /* everything was flushed out */
    oc->count = 0;
    if (oc->bufsize > OutputBufferSize)
    {
	oc->bufsize = OutputBufferSize;
	oc->buf = (unsigned char *)xrealloc(oc->buf, OutputBufferSize);
	if (oc->buf == NULL) /* nearly impossible */
	    goto outOfMem;
    }
    return extraCount; /* return only the amount explicitly requested */
}

 /********************
 * FlushAllOutput()
 *    Flush all clients with output.  However, if some client still
 *    has input in the queue (more requests), then don't flush.  This
 *    will prevent the output queue from being flushed every time around
 *    the round robin queue.  Now, some say that it SHOULD be flushed
 *    every time around, but...
 *
 **********************/

void
FlushAllOutput()
{
    register int index, base, mask;
    OsCommPtr oc;
    register ClientPtr client;

    if (! NewOutputPending)
	return;

    /*
     * It may be that some client still has critical output pending,
     * but he is not yet ready to receive it anyway, so we will
     * simply wait for the select to tell us when he's ready to receive.
     */
    CriticalOutputPending = FALSE;
    NewOutputPending = FALSE;

    for (base = 0; base < mskcnt; base++)
    {
	mask = OutputPending[ base ];
	OutputPending[ base ] = 0;
	while (mask)
	{
	    index = ffs(mask) - 1;
	    mask &= ~lowbit(mask);
	    if ((client = ConnectionTranslation[(32 * base) + index ]) == NULL)
		continue;
	    if (client->clientGone)
		continue;
	    oc = (OsCommPtr)client->osPrivate;
	    if (GETBIT(ClientsWithInput, client->index))
	    {
		BITSET(OutputPending, oc->fd); /* set the bit again */
		NewOutputPending = TRUE;
	    }
	    else
		FlushClient(client, oc, (char *)NULL, 0);
	}
    }

}

void
FlushIfCriticalOutputPending()
{
    if (CriticalOutputPending)
	FlushAllOutput();
}

void
SetCriticalOutputPending()
{
    CriticalOutputPending = TRUE;
}

/*****************
 * WriteToClient
 *    Copies buf into ClientPtr.buf if it fits (with padding), else
 *    flushes ClientPtr.buf and buf to client.  As of this writing,
 *    every use of WriteToClient is cast to void, and the result
 *    is ignored.  Potentially, this could be used by requests
 *    that are sending several chunks of data and want to break
 *    out of a loop on error.  Thus, we will leave the type of
 *    this routine as int.
 *****************/

int
WriteToClient (who, count, buf)
    ClientPtr who;
    char *buf;
    int count;
{
    OsCommPtr oc = (OsCommPtr)who->osPrivate;
    int padBytes;

    if (oc->fd == -1) 
    {
	ErrorF( "OH NO, %d translates to -1\n", oc->fd);
	return(-1);
    }

    if (oc->fd == -2) 
    {
#ifdef notdef
	ErrorF( "CONNECTION %d ON ITS WAY OUT\n", oc->fd);
#endif
	return(-1);
    }

    padBytes =  padlength[count & 3];

    if (oc->count + count + padBytes > oc->bufsize)
    {
	BITCLEAR(OutputPending, oc->fd);
	CriticalOutputPending = FALSE;
	NewOutputPending = FALSE;
	return FlushClient(who, oc, buf, count);
    }

    NewOutputPending = TRUE;
    BITSET(OutputPending, oc->fd);
    bcopy(buf, (char *)oc->buf + oc->count, count);
    oc->count += count + padBytes;
    
    return(count);
}

