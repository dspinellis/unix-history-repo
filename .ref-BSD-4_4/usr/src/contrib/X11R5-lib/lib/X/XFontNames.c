/* $XConsortium: XFontNames.c,v 11.26 92/03/03 14:21:22 rws Exp $ */
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
char **XListFonts(
register Display *dpy,
_Xconst char *pattern,  /* null-terminated */
int maxNames,
int *actualCount)	/* RETURN */
#else
char **XListFonts(dpy, pattern, maxNames, actualCount)
register Display *dpy;
char *pattern;  /* null-terminated */
int maxNames;
int *actualCount;	/* RETURN */
#endif
{       
    register long nbytes;
    register unsigned i;
    register int length;
    char **flist;
    char *ch;
    xListFontsReply rep;
    register xListFontsReq *req;
    register long rlen;

    LockDisplay(dpy);
    GetReq(ListFonts, req);
    req->maxNames = maxNames;
    nbytes = req->nbytes = pattern ? strlen (pattern) : 0;
    req->length += (nbytes + 3) >> 2;
    _XSend (dpy, pattern, nbytes);
    /* use _XSend instead of Data, since following _XReply will flush buffer */

    (void) _XReply (dpy, (xReply *)&rep, 0, xFalse);

    if (rep.nFonts) {
	flist = (char **)Xmalloc ((unsigned)rep.nFonts * sizeof(char *));
	rlen = rep.length << 2;
	ch = (char *) Xmalloc((unsigned) (rlen + 1));
	    /* +1 to leave room for last null-terminator */

	if ((! flist) || (! ch)) {
	    if (flist) Xfree((char *) flist);
	    if (ch) Xfree(ch);
	    _XEatData(dpy, (unsigned long) rlen);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (char **) NULL;
	}

	_XReadPad (dpy, ch, rlen);
	/*
	 * unpack into null terminated strings.
	 */
	length = *(unsigned char *)ch;
	*ch = 1; /* make sure it is non-zero for XFreeFontNames */
	for (i = 0; i < rep.nFonts; i++) {
	    flist[i] = ch + 1;  /* skip over length */
	    ch += length + 1;  /* find next length ... */
	    length = *(unsigned char *)ch;
	    *ch = '\0';  /* and replace with null-termination */
	}
    }
    else flist = (char **) NULL;
    *actualCount = rep.nFonts;
    UnlockDisplay(dpy);
    SyncHandle();
    return (flist);
}

XFreeFontNames(list)
char **list;
{       
	if (list) {
		if (!*(list[0]-1)) { /* from ListFontsWithInfo */
			register char **names;
			for (names = list+1; *names; names++)
				Xfree (*names);
		}
		Xfree (list[0]-1);
		Xfree ((char *)list);
	}
}
