/* $XConsortium: XwcPrpText.c,v 1.12 91/11/17 15:31:34 rws Exp $ */
/*
 * Copyright 1991 by OMRON Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * OMRON AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OMRON OR MIT BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Hiroshi Kuribayashi    OMRON Corporation
 *
 */

#include <X11/Xlibint.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include "Xlocaleint.h"

#define XA_COMPOUND_TEXT(d) XInternAtom(d, "COMPOUND_TEXT", False)

#if NeedFunctionPrototypes
int
XwcTextPropertyToTextList(
    Display *dpy,
    XTextProperty *tp,
    wchar_t ***list_return,
    int *count_return
)
#else
int
XwcTextPropertyToTextList(dpy, tp, list_return, count_return)
    Display *dpy;
    XTextProperty *tp;
    wchar_t ***list_return;
    int *count_return;
#endif
{
    wchar_t **list;
    unsigned int nelements;
    unsigned char *cp;
    wchar_t *start;
    int i, j;
    int len, scand;
    unsigned int datalen = (int) tp->nitems;
    int ret, error = 0;
#ifndef X_WCHAR
    char *mbuf;
#endif
    XsiLCd lcd = (XsiLCd)_XlcCurrentLC();

    if (tp->format != 8 ||
	(tp->encoding != XA_STRING &&
	 tp->encoding != XA_COMPOUND_TEXT(dpy) &&
	 !(lcd &&
	   tp->encoding == XInternAtom(dpy, lcd->xlc->xlc_db->lc_encoding,
				       False))))
	return XConverterNotFound;

    if (datalen == 0) {
	*list_return = (wchar_t **) Xmalloc(sizeof (wchar_t *));
	**list_return = 0;
	*count_return = 0;
	return Success;
    }

    nelements = 1;
    for (cp = tp->value, i = datalen; i > 0; cp++, i--) {
	if (*cp == '\0') nelements++;
    }

    list = (wchar_t **) Xmalloc (nelements * sizeof (wchar_t *));
    if (!list) return XNoMemory;

    start = (wchar_t *) Xmalloc ((datalen + 1) * sizeof (wchar_t));
    if (!start) {
	Xfree ((char *) list);
	return XNoMemory;
    }

    if (tp->encoding == XA_STRING ||
	tp->encoding == XA_COMPOUND_TEXT(dpy)) {
	cp = tp->value;
	for (i = j = 0; i < nelements; i++, j++) {
	    list[j] = start;
	    if (i == nelements - 1)
		scand = datalen;
	    else
		scand = strlen((char *)cp);
#ifdef X_WCHAR
	    len = datalen + 1;
	    if ((ret = _XConvertCTToWC(0, cp,  scand, start,
				&len, &scand, 0)) < 0) {
		XwcFreeStringList(list);
		return (XConverterNotFound);
	    }
#else
#ifdef macII
	    len = 0; ret = 0;
#else
	    len = scand * 2;
	    mbuf = _XAllocScratch(dpy, len);
	    if ((ret = _XConvertCTToMB(0, cp, scand, mbuf,
				&len, &scand, 0)) < 0) {
		XwcFreeStringList(list);
		return (XConverterNotFound);
	    }
	    len = mbstowcs(start, mbuf, datalen + 1);
	    if (len == datalen) {
		XwcFreeStringList(list);
		return (XConverterNotFound);
	    }
#endif
#endif
	    error += ret;
	    start += len + 1;
	    datalen -= len + 1;
	    cp += scand + 1;
	}
    } else {
	cp = tp->value;
	for (i = j = 0; i < nelements; i++, j++) {
	    list[j] = start;
#ifdef X_WCHAR
	    len = datalen + 1;
	    if (i == nelements - 1)
		scand = datalen;
	    else
		scand = strlen((char *)cp);
	    if ((ret = _XConvertMBToWC(0, cp, scand, start,
				&len, &scand, 0)) < 0) {
		XwcFreeStringList(list);
		return (XConverterNotFound);
	    }
	    error += ret;
#else
#ifdef macII
	    len = 0;
#else
	    len = mbstowcs(start, (char *)cp, datalen);
	    if (len == datalen) {
		XwcFreeStringList(list);
		return (XConverterNotFound);
	    }
#endif
#endif
	    start += len + 1;
	    datalen -= len + 1;
	    cp += scand + 1;
	}
    }

    *list_return = list;
    *count_return = nelements;
    return error;
}

void XwcFreeStringList (list)
    wchar_t **list;
{
    if (list) {
	if (list[0]) Xfree ((char *)(list[0]));
	Xfree ((char *) list);
    }
}
