/* $XConsortium: XQuExt.c,v 11.18 91/01/06 11:47:31 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_REPLIES
#include "Xlibint.h"

#if NeedFunctionPrototypes
Bool XQueryExtension(
    register Display *dpy,
    _Xconst char *name,
    int *major_opcode,  /* RETURN */
    int *first_event,   /* RETURN */
    int *first_error)	/* RETURN */
#else
Bool XQueryExtension(dpy, name, major_opcode, first_event, first_error)
    register Display *dpy;
    char *name;
    int *major_opcode;  /* RETURN */
    int *first_event;   /* RETURN */
    int *first_error;	/* RETURN */
#endif
{       
    xQueryExtensionReply rep;
    register xQueryExtensionReq *req;

    LockDisplay(dpy);
    GetReq(QueryExtension, req);
    req->nbytes = name ? strlen(name) : 0;
    req->length += (req->nbytes+(unsigned)3)>>2;
    _XSend(dpy, name, (long)req->nbytes);
    (void) _XReply (dpy, (xReply *)&rep, 0, xTrue);
    *major_opcode = rep.major_opcode;
    *first_event = rep.first_event;
    *first_error = rep.first_error;
    UnlockDisplay(dpy);
    SyncHandle();
    return (rep.present);
}

