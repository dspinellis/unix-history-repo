#include <X/mit-copyright.h>

/* $Header: XlibInternal.h,v 10.9 86/04/22 15:22:19 jg Rel $ */
/* Copyright 1984, 1985  Massachusetts Institute of Technology */

/*
 *	XlibInternal.h - Header definition and support file for the internal
 *	support routines (XlibInternal) used by the C subroutine interface
 *	library (Xlib) to the X Window System.
 *
 */

#include <sys/types.h>
#include "Xlib.h"
#include "../X/Xproto.h"
#include <stdio.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h>
#include <errno.h>

extern int errno;			/* Internal system error number. */
extern char *malloc();			/* commonly used in the library. */
extern char *realloc();			/* used with some frequency.	 */

extern Display *_XlibCurrentDisplay;	/* Currently active Display. */

extern (*_XIOErrorFunction)();		/* X system error reporting routine. */
extern (*_XErrorFunction)();		/* X_Error event reporting routine. */

/*
 * Boolean datatype.
 */

typedef enum _bool {FALSE, TRUE} Bool;

/* Pointers to the above routines. */
#define _XIOError (*_XIOErrorFunction)
#define _XError (*_XErrorFunction)

#define BUFSIZE 2048			/* X output buffer size. */


/*
 * X Protocol packetizing macros.
 */


/*
 * GetReq - Get the next avilable X request packet in the buffer and
 * return it. 
 *
 * "cd" is the X protocol code.
 * "id" is the window id of the requesting window.
 */
#define GetReq(cd, id) \
	dpy = _XlibCurrentDisplay;\
	if ((dpy->bufptr + sizeof(XReq)) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (XReq *)dpy->bufptr;\
	req->code = cd;\
	req->windowId = id;\
	dpy->bufptr += sizeof(XReq);\
	dpy->request++;\
	dpy->lastdraw = NULL

/*
 * Data - Place data in the buffer and pad the end to provide
 * 32 bit word alignment.  Transmit if the buffer fills.
 *
 * "dpy" is a pointer to a Display.
 * "data" is a pinter to a data buffer.
 * "len" is the length of the data buffer.
 */
#define Data(dpy, data, len) \
	if (dpy->bufptr + len < dpy->bufmax) {\
		bcopy(data, dpy->bufptr, len);\
		dpy->bufptr += (len + 3) & ~3;\
	} else\
		_XSend(dpy, data, len)

#ifndef BIGSHORTS
#define PackData(dpy, data, len) Data(dpy, (char *) data, len)
#define PackShorts(f, t, n)  bcopy((char *)f, t, n)
#endif



