/* $XConsortium: XmbTextPrp.c,v 1.6 91/11/17 15:31:30 rws Exp $ */
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
XmbTextListToTextProperty(
    Display	      *dpy,
    char             **list,
    int		       count,
    XICCEncodingStyle  style,
    XTextProperty     *text_prop
)
#else
int
XmbTextListToTextProperty(dpy, list, count, style, text_prop)
    Display	      *dpy;
    char             **list;
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

    for (i = 0, nbytes = 0; i < count; i++) {
	nbytes += (unsigned) ((list[i] ? strlen (list[i]) : 0) + 1);
    }
    proto.format = 8;
    proto.nitems = 0;

    if (nbytes) {
	datalen = len = nbytes * 6 + 6; /* Is it correct/enough ? */
	buf_sv = buf = (unsigned char *)Xmalloc((unsigned)len);
	if (!buf) return (XNoMemory);
	proto.value = (unsigned char *) buf;

	if (style == XStringStyle) {
	    proto.encoding = XA_STRING;
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
		    if ((ret = _XConvertMBToString((unsigned char *)(*list),
					    strlen(*list), buf,
					    &len, &scand)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
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
		    if ((ret = _XConvertMBToCT(0, (unsigned char *)(*list),
					strlen(*list), buf,
					&len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
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
	} else if (style == XTextStyle) { /* MB: need not to convert */
	    XsiLCd lcd = (XsiLCd)_XlcCurrentLC();
	    proto.nitems = nbytes - 1;
	    if (lcd)
		proto.encoding = XInternAtom(dpy,
					     lcd->xlc->xlc_db->lc_encoding,
					     False);
	    else
		proto.encoding = XA_STRING;
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    (void) strcpy((char *)buf, *list);
		    buf += (strlen(*list) + 1);
		} else {
		    *buf++ = '\0';
		}
	    }
	} else if (style == XStdICCTextStyle) {
	    int is_xstring = 1;	/* Yes */
	    for (i = 0; i < count; i++, list++) {
		if (*list) {
		    len = datalen;
		    if ((ret = _XConvertMBToCT(0, (unsigned char *)*list,
					strlen(*list),
					buf, &len, &scand, 0)) < 0) {
			Xfree((char *)buf_sv);
			return (XConverterNotFound);
		    }
		    error += ret;
		    if (is_xstring)
			for (i = 0; *(buf+i); i++) {
			    if (!_isXString(*(buf+i))) {
				is_xstring = 0; /* Not XString */
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
	    /* I don't know such a encoding */
	    return (XConverterNotFound);
	}
    } else {
	proto.nitems = 0;
	proto.value = 0;
    }
    *text_prop = proto;
    return (error);
}
