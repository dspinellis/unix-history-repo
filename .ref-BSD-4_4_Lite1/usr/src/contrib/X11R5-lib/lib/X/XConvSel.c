/* $XConsortium: XConvSel.c,v 11.7 91/01/06 11:44:45 rws Exp $ */
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

#include "Xlibint.h"

XConvertSelection(dpy, selection, target, property, requestor, time)
register Display *dpy;
Atom selection, target;
Atom property;
Window requestor;
Time time;
{
    register xConvertSelectionReq *req;

    LockDisplay(dpy);
    GetReq(ConvertSelection, req);
    req->selection = selection;
    req->target = target;
    req->property = property;
    req->requestor = requestor;
    req->time = time;
    UnlockDisplay(dpy);
    SyncHandle();
}
