/* $XConsortium: TextToStr.c,v 1.3 91/01/08 14:40:22 gildea Exp $ */
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

#include <X11/Xlibint.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>


/*
 * XTextPropertyToStringList - set list and count to contain data stored in
 * null-separated STRING property.
 */

Status XTextPropertyToStringList (tp, list_return, count_return)
    XTextProperty *tp;
    char ***list_return;
    int *count_return;
{
    char **list;			/* return value */
    int nelements;			/* return value */
    register char *cp;			/* temp variable */
    char *start;			/* start of thing to copy */
    int i, j;				/* iterator variables */
    int datalen = (int) tp->nitems;	/* for convenience */

    /*
     * make sure we understand how to do it
     */
    if (tp->encoding != XA_STRING ||  tp->format != 8) return False;

    if (datalen == 0) {
	*list_return = NULL;
	*count_return = 0;
	return True;
    }

    /*
     * walk the list to figure out how many elements there are
     */
    nelements = 1;			/* since null-separated */
    for (cp = (char *) tp->value, i = datalen; i > 0; cp++, i--) {
	if (*cp == '\0') nelements++;
    }

    /*
     * allocate list and duplicate
     */
    list = (char **) Xmalloc (nelements * sizeof (char *));
    if (!list) return False;
	
    start = (char *) Xmalloc ((datalen + 1) * sizeof (char));	/* for <NUL> */
    if (!start) {
	Xfree ((char *) list);
	return False;
    }

    /*
     * copy data
     */
    bcopy ((char *) tp->value, start, tp->nitems);
    start[datalen] = '\0';

    /*
     * walk down list setting value
     */
    for (cp = start, i = datalen + 1, j = 0; i > 0; cp++, i--) {
	if (*cp == '\0') {
	    list[j] = start;
	    start = (cp + 1);
	    j++;
	}
    }

    /*
     * append final null pointer and then return data
     */
    *list_return = list;
    *count_return = nelements;
    return True;
}


void XFreeStringList (list)
    char **list;
{
    if (list) {
	if (list[0]) Xfree (list[0]);
	Xfree ((char *) list);
    }
}

