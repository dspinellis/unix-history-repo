/* $XConsortium: XGetAtomNm.c,v 11.16 91/01/06 11:45:52 rws Exp $ */
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

char *XGetAtomName(dpy, atom)
register Display *dpy;
Atom atom;
{
    xGetAtomNameReply rep;
    xResourceReq *req;
    char *storage;

    LockDisplay(dpy);
    GetResReq(GetAtomName, atom, req);
    if (_XReply(dpy, (xReply *)&rep, 0, xFalse) == 0) {
	UnlockDisplay(dpy);
	SyncHandle();
	return(NULL);
    }
    if (storage = (char *) Xmalloc(rep.nameLength+1)) {
	_XReadPad(dpy, storage, (long)rep.nameLength);
	storage[rep.nameLength] = '\0';
    } else {
	_XEatData(dpy, (unsigned long) (rep.nameLength + 3) & ~3);
	storage = (char *) NULL;
    }
    UnlockDisplay(dpy);
    SyncHandle();
    return(storage);
}
