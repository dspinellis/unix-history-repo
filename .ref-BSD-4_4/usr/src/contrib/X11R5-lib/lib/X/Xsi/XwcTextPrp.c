/* $XConsortium: XwcTextPrp.c,v 1.7 91/11/17 15:31:47 rws Exp $ */
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

static int
_Xwcslen(ws)
wchar_t *ws;
{
    register n;
    n = 0;
    while (*ws++ != WNULL)
	n++;
    return(n);
}

#define XA_COMPOUND_TEXT(d) XInternAtom(d, "COMPOUND_TEXT", False)

#if NeedFunctionPrototypes
int
XwcTextListToTextProperty(
    Display	      *dpy,
    wchar_t	     **list,
    int		       count,
    XICCEncodingStyle  style,
    XTextProperty     *text_prop
)
#else
int
XwcTextListToTextProperty(dpy, list, count, style, text_prop)
    Display	      *dpy;
    wchar_t	     **list;
    int		       count;
    XICCEncodingStyle  style;
    XTextProperty     *text_prop;
#endif
{
    int len, datalen;
    unsigned char *buf, *buf_sv;
    int i, scand;
    register unsigned int nbytes;
    XTextProperty proto;
    int ret, error = 0;
#ifndef X_WCHAR
    char *mbuf;
    int mlen;
#endif

    for (i = 0, nbytes = 0; i < count; i++) {
	nbytes += (unsigned) ((list[i] ? _Xwcslen (list[i]) : 0) + 1);
    }

    proto.format = 8;
    proto.nitems = 0;

    if (nbytes) {
	datalen = len = nbytes * 6 + 6; /* Is it correct? */
	buf_sv = buf = (unsigned char *)Xmalloc((unsigned)len);
	if (!buf) return (XNoMemory);
	proto.value = (unsigned char *) buf;

	if (style == XStringStyle) {
	    proto.encoding = XA_STRING;
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
#ifdef X_WCHAR
		    if ((ret = _XConvertWCToString(*list, _Xwcslen(*list), buf,
					    &len, &scand)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#else
		    mlen = _Xsiwcstombs(dpy, (XLCd)NULL,
					*list, _Xwcslen(*list), True, &mbuf);
		    if ((ret = _XConvertMBToString(mbuf, mlen, buf,
					    &len, &scand)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#endif
		    error += ret;
		    buf += len + 1;
		    datalen -= len + 1;
		    proto.nitems += len + 1;
		} else {
		    *buf++ = '\0';
		    datalen--;
		    proto.nitems++;
		}
	    }
	    proto.nitems--;
	} else if (style == XCompoundTextStyle) {
	    proto.encoding = XA_COMPOUND_TEXT(dpy);
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
#ifdef X_WCHAR
		    if ((ret = _XConvertWCToCT(0, *list, _Xwcslen(*list), buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#else
		    mlen = _Xsiwcstombs(dpy, (XLCd)NULL,
					*list, _Xwcslen(*list), True, &mbuf);
		    if ((ret = _XConvertMBToCT(0, mbuf, mlen, buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#endif
		    error += ret;
		    buf += len + 1;
		    datalen -= len + 1;
		    proto.nitems += len + 1;
		} else {
		    *buf++ = '\0';
		    datalen--;
		    proto.nitems++;
		}
	    }
	    proto.nitems--;
	} else if (style == XTextStyle) { /* MB */
	    XsiLCd lcd = (XsiLCd)_XlcCurrentLC();
	    if (lcd)
		proto.encoding = XInternAtom(dpy,
					     lcd->xlc->xlc_db->lc_encoding,
					     False);
	    else
		proto.encoding = XA_STRING;
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
#ifdef X_WCHAR
		    if ((ret = _XConvertWCToMB(0, *list, _Xwcslen(*list), buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
		    error += ret;
#else
#ifdef macII
		    len = 0;
#else
		    len = wcstombs((char *)buf, *list, len);
		    if (len == datalen) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#endif
#endif
		    buf += len + 1;
		    datalen -= len + 1;
		    proto.nitems += len + 1;
		} else {
		    *buf++ = '\0';
		    datalen--;
		    proto.nitems++;
		}
	    }
	    proto.nitems--;
	} else if (style == XStdICCTextStyle) {
	    int is_xstring = 1;	/* Yes */
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
#ifdef X_WCHAR
		    if ((ret = _XConvertWCToCT(0, *list, _Xwcslen(*list), buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#else
		    mlen = _Xsiwcstombs(dpy, (XLCd)NULL,
					*list, _Xwcslen(*list), True, &mbuf);
		    if ((ret = _XConvertMBToCT(0, mbuf, mlen, buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
#endif
		    error += ret;
		    if (is_xstring)
			for (i = 0; *(buf+i); i++) {
			    if(!_isXString(*(buf+i))) {
				is_xstring = 0;	/* Not XString */
				break;
			    }
			}
		    buf += len + 1;
		    datalen -= len + 1;
		    proto.nitems += len + 1;
		} else {
		    *buf++ = '\0';
		    datalen--;
		    proto.nitems++;
		}
	    }
	    proto.nitems--;
	    if (is_xstring)
		proto.encoding = XA_STRING;
	    else
		proto.encoding = XA_COMPOUND_TEXT(dpy);
	} else {
	    return (XConverterNotFound);
	}
    } else {
	proto.nitems = 0;
	proto.value = 0;
    }
    *text_prop = proto;
    return (error);
}
