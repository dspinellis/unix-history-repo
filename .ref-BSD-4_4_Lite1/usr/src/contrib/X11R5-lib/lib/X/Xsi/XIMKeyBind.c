/*
 * $XConsortium: XIMKeyBind.c,v 1.26 92/10/22 14:26:09 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation
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
 *	Author:	Seiji Kuwari	OMRON Corporation
 *				kuwa@omron.co.jp
 *				kuwa%omron.co.jp@uunet.uu.net
 */				

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xi18nint.h"
#include "XIMlibint.h"
#include <X11/Xutil.h>

int
_XipwcLookupString(supic, ev, buffer, nchars, keysym, status)
    XIC supic;
    register XKeyEvent *ev;
    wchar_t *buffer;
    int nchars;
    KeySym *keysym;
    Status *status;
{
    XipIC		ic = (XipIC)supic;
    short		type, string_is_mb;
    int			length;
    unsigned char	*ptr;
    int			ret_len, scanned_bytes;
    int			ret;
    unsigned char	buf[32];
    KeySym		tmp_ks;
#ifndef X_WCHAR
    char		*mbuf;
#endif

    if (ev->keycode == 0) {
	if (ev->state > 0) {
	    _XipGetOverflowICQueue(ic, &type, &length, &tmp_ks,
				   &string_is_mb, (char **)&ptr);
	} else {
	    _XipGetNextICQueue(ic, &type, &length, &tmp_ks,
			       &string_is_mb, (char **)&ptr);
	}
	if (type == XIM_STRING || (type == XIM_KEYSYM && length > 0)) {
#ifdef X_WCHAR
	    ret_len = nchars;
	    if (string_is_mb) {
		ret = _XConvertMBToWC(ic->wc, ptr, length, (wchar *)buffer,
				      &ret_len, &scanned_bytes, (_State *)NULL);
	    } else {
		ret = _XConvertCTToWC(ic->wc, ptr, length, (wchar *)buffer,
				      &ret_len, &scanned_bytes, (_State *)NULL);
	    }
#else
	    ret_len = length * 2;
	    mbuf = _XAllocScratch(ev->display, ret_len);
	    if (string_is_mb) {
		ret = 0;
		strncpy(mbuf, (char *)ptr, length);
		ret_len = length;
	    } else {
		ret = _XConvertCTToMB(ic->mb, ptr, length, mbuf,
				      &ret_len, &scanned_bytes, (_State *)NULL);
	    }
	    if (ret >= 0) {
#ifdef macII
		ret_len = 0;
#else
		mbuf[ret_len] = '\0';
		ret_len = mbstowcs(buffer, mbuf, nchars);
#endif
		if (ret_len == nchars)
		    ret = BadBuffer;
	    }
#endif
	    if (ret == BadBuffer) {
		_XipSaveOverflowICQueue(ic, type, length, tmp_ks,
					string_is_mb, ptr);
		ev->state = 1;
		*status = XBufferOverflow;
		return(0);
	    } else if (ret < 0) {
		*status = XLookupNone;
		return(0);
	    }
	    if (type == XIM_KEYSYM && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
	    return(ret_len);
	} else if (type == XIM_KEYSYM && keysym) {
	    *keysym = tmp_ks;
	    *status = XLookupKeySym;
	    return(0);
	} else {
	    *status = XLookupNone;
	    return(0);
	}
    } else {
	ret_len = XLookupString(ev, (char *)buf, 32, &tmp_ks, NULL);
	if (ret_len > 0) {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
#ifdef X_WCHAR
	    /* XXX BUG Need to save current status */
	    ret = _XConvertMBToWC(ic->wc, (unsigned char *)buf, ret_len,
			    (wchar *)buffer, &nchars, &scanned_bytes,
			    (_State *)NULL);
	    if (ret == BadBuffer) {
		*status = XBufferOverflow;
		return(0);
	    } else if (ret < 0) {
		*status = XLookupNone;
		return(0);
	    }
	    ret_len = nchars;
	    /* XXX BUG Need to restore saved status */
#else
#ifdef macII
	    ret_len = 0;
#else
	    buf[ret_len] = '\0';
	    ret_len = mbstowcs(buffer, (char *)buf, nchars);
#endif
#endif
	} else {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupKeySym;
	    } else {
		*status = XLookupNone;
	    }
	}
	return(ret_len);
    }
}

int
_XipmbLookupString(supic, ev, buffer, nbytes, keysym, status)
    XIC supic;
    register XKeyEvent *ev;
    char *buffer;
    int nbytes;
    KeySym *keysym;
    Status *status;
{
    XipIC		ic = (XipIC)supic;
    short		type, string_is_mb;
    int			length;
    unsigned char	*ptr;
    int			ret_len, scanned_bytes;
    int			ret;
    KeySym		tmp_ks;

    if (ev->keycode == 0) {
	if (ev->state > 0) {
	    _XipGetOverflowICQueue(ic, &type, &length, &tmp_ks,
				   &string_is_mb, (char **)&ptr);
	} else {
	    _XipGetNextICQueue(ic, &type, &length, &tmp_ks,
			       &string_is_mb, (char **)&ptr);
	}
	if (type == XIM_STRING || (type == XIM_KEYSYM && length > 0)) {
	    if (string_is_mb) {
		if (nbytes < length) {
		    _XipSaveOverflowICQueue(ic, type, length, tmp_ks,
					    string_is_mb, ptr);
		    ev->state = 1;
		    *status = XBufferOverflow;
		    return(0);
		}
		(void)strncpy(buffer, (char *)ptr, length);
		if (length < nbytes) buffer[length] = '\0';
		ret_len = length;
	    } else {
		ret_len = nbytes;
		ret = _XConvertCTToMB(ic->mb, ptr, length,
				      (unsigned char *)buffer, &ret_len,
				      &scanned_bytes, (_State *)NULL);
		if (ret == BadBuffer) {
		    _XipSaveOverflowICQueue(ic, type, length, tmp_ks,
					    string_is_mb, ptr);
		    ev->state = 1;
		    *status = XBufferOverflow;
		    return(0);
		} else if (ret < 0) {
		    *status = XLookupNone;
		    return(0);
		}
	    }
	    if (type == XIM_KEYSYM && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
	    return(ret_len);
	} else if (type == XIM_KEYSYM && keysym) {
	    *keysym = tmp_ks;
	    *status = XLookupKeySym;
	    return(0);
	} else {
	    *status = XLookupNone;
	    return(0);
	}
    } else {
	ret_len = XLookupString(ev, buffer, nbytes, &tmp_ks, NULL);
	if (ret_len > 0) {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
	} else {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupKeySym;
	    } else {
		*status = XLookupNone;
	    }
	}
	return(ret_len);
    }
}

int
_XipctLookupString(supic, ev, buffer, nbytes, keysym, status)
    XIC supic;
    register XKeyEvent *ev;
    char *buffer;
    int nbytes;
    KeySym *keysym;
    Status *status;
{
    XipIC		ic = (XipIC)supic;
    short		type, string_is_mb;
    int			length;
    char		*ptr;
    int			ret_len, scanned_bytes;
    KeySym		tmp_ks;
    int			ret;

    if (ev->keycode == 0) {
	if (ev->state > 0) {
	    _XipGetOverflowICQueue(ic, &type, &length, &tmp_ks,
				   &string_is_mb, (char **)&ptr);
	} else {
	    _XipGetNextICQueue(ic, &type, &length, &tmp_ks,
			       &string_is_mb, &ptr);
	}
	if (type == XIM_STRING || (type == XIM_KEYSYM && length > 0)) {
	    if (string_is_mb) {
		ret_len = nbytes;
		ret = _XConvertMBToCT(ic->mb, ptr, length,
				      (unsigned char *)buffer, &ret_len,
				      &scanned_bytes, (_State *)NULL);
		if (ret == BadBuffer) {
		    _XipSaveOverflowICQueue(ic, type, length, tmp_ks,
					    string_is_mb, ptr);
		    ev->state = 1;
		    *status = XBufferOverflow;
		    return(0);
		} else if (ret < 0) {
		    *status = XLookupNone;
		    return(0);
		}
	    } else {
		if (length > nbytes) {
		    _XipSaveOverflowICQueue(ic, type, length, tmp_ks,
					    string_is_mb, ptr);
		    ev->state = 1;
		    *status = XBufferOverflow;
		    return(0);
		}
		(void)strncpy(buffer, ptr, length);
		if (length < nbytes) buffer[length] = '\0';
		ret_len = length;
	    }
	    if (type == XIM_KEYSYM && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
	    return(ret_len);
	} else if (type == XIM_KEYSYM && keysym) {
	    *keysym = tmp_ks;
	    *status = XLookupKeySym;
	    return(0);
	} else {
	    *status = XLookupNone;
	    return(0);
	}
    } else {
	ret_len = XLookupString(ev, buffer, nbytes, &tmp_ks, NULL);
	if (ret_len > 0) {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupBoth;
	    } else {
		*status = XLookupChars;
	    }
	} else {
	    if (tmp_ks != NoSymbol && keysym) {
		*keysym = tmp_ks;
		*status = XLookupKeySym;
	    } else {
		*status = XLookupNone;
	    }
	}
	return(ret_len);
    }
}
