/* $XConsortium: PropAlloc.c,v 1.5 91/02/01 16:33:26 gildea Exp $ */
/* Copyright 1989 Massachusetts Institute of Technology */

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
#include "Xutil.h"
#include <stdio.h>


/*
 * Routines for allocating space for structures that are expected to get 
 * longer at some point.
 */

XSizeHints *XAllocSizeHints ()
{
    return ((XSizeHints *) Xcalloc (1, (unsigned) sizeof (XSizeHints)));
}


XStandardColormap *XAllocStandardColormap ()
{
    return ((XStandardColormap *)
	    Xcalloc (1, (unsigned) sizeof (XStandardColormap)));
}


XWMHints *XAllocWMHints ()
{
    return ((XWMHints *) Xcalloc (1, (unsigned) sizeof (XWMHints)));
}


XClassHint *XAllocClassHint ()
{
    register XClassHint *h;

    if (h = (XClassHint *) Xcalloc (1, (unsigned) sizeof (XClassHint))) 
      h->res_name = h->res_class = NULL;

    return h;
}


XIconSize *XAllocIconSize ()
{
    return ((XIconSize *) Xcalloc (1, (unsigned) sizeof (XIconSize)));
}


