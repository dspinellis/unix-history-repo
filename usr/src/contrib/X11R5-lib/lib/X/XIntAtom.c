/* $XConsortium: XIntAtom.c,v 11.19 91/08/14 09:52:08 rws Exp $ */
/*

Copyright 1986, 1990 by the Massachusetts Institute of Technology

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

#define TABLESIZE 64

typedef struct _Entry {
    unsigned long sig;
    Atom atom;
} EntryRec, *Entry;

#define EntryName(e) ((char *)(e+1))

typedef struct _XDisplayAtoms {
    Entry table[TABLESIZE];
} AtomTable;

#define HASH(sig) ((sig) & (TABLESIZE-1))
#define REHASHVAL(sig) ((((sig) % (TABLESIZE-3)) + 2) | 1)
#define REHASH(idx,rehash) ((idx + rehash) & (TABLESIZE-1))

static void
_XFreeAtomTable(dpy)
    Display *dpy;
{
    register Entry *table;
    register int i;
    register Entry e;

    if (table = dpy->atoms->table) {
	for (i = TABLESIZE; --i >= 0; ) {
	    if (e = *table++)
		Xfree((char *)e);
	}
	Xfree((char *)dpy->atoms);
    }
}

#if NeedFunctionPrototypes
Atom XInternAtom (
    Display *dpy,
    _Xconst char *name,
    Bool onlyIfExists)
#else
Atom XInternAtom (dpy, name, onlyIfExists)
    Display *dpy;
    char *name;
    Bool onlyIfExists;
#endif
{
    register AtomTable *atoms;
    register char *s1, c, *s2;
    register unsigned long sig;
    register int idx, i;
    Entry e, oe;
    int n, firstidx, rehash;
    xInternAtomReply rep;
    xInternAtomReq *req;

    if (!name)
	name = "";
    LockDisplay(dpy);
    /* look in the cache first */
    if (!(atoms = dpy->atoms)) {
	dpy->atoms = atoms = (AtomTable *)Xcalloc(1, sizeof(AtomTable));
	dpy->free_funcs->atoms = _XFreeAtomTable;
    }
    sig = 0;
    for (s1 = (char *)name; c = *s1++; )
	sig = (sig << 1) + c;
    n = s1 - (char *)name - 1;
    if (atoms) {
	firstidx = idx = HASH(sig);
	while (e = atoms->table[idx]) {
	    if (e->sig == sig) {
	    	for (i = n, s1 = (char *)name, s2 = EntryName(e); --i >= 0; ) {
		    if (*s1++ != *s2++)
		    	goto nomatch;
	    	}
	    	if (!*s2) {
		    rep.atom = e->atom;
		    UnlockDisplay(dpy);
		    return rep.atom;
	    	}
	    }
nomatch:    if (idx == firstidx)
		rehash = REHASHVAL(sig);
	    idx = REHASH(idx, rehash);
	    if (idx == firstidx)
		break;
	}
    }
    /* not found, go to the server */
    GetReq(InternAtom, req);
    req->nbytes = n;
    req->onlyIfExists = onlyIfExists;
    req->length += (n+3)>>2;
    _XSend (dpy, name, n);
    	/* use _XSend instead of Data, since the following _XReply
           will always flush the buffer anyway */
    if(_XReply (dpy, (xReply *)&rep, 0, xTrue) == 0) {
	rep.atom = None;
    } else if (rep.atom && atoms) {
	/* store it in the cache */
	e = (Entry)Xmalloc(sizeof(EntryRec) + n + 1);
	if (e) {
	    e->sig = sig;
	    e->atom = rep.atom;
	    strcpy(EntryName(e), name);
	    if (oe = atoms->table[idx])
		Xfree((char *)oe);
	    atoms->table[idx] = e;
	}
    }
    UnlockDisplay(dpy);
    SyncHandle();
    return (rep.atom);
}
