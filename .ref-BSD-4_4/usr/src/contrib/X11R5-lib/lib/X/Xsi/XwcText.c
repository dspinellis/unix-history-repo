/*
 * $XConsortium: XwcText.c,v 1.18 91/06/06 18:47:44 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation, NTT Software Corporation,
 *                      and Nippon Telegraph and Telephone Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON, NTT Software, NTT, and M.I.T.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission. OMRON, NTT Software,
 * NTT, and M.I.T. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * OMRON, NTT SOFTWARE, NTT, AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL OMRON, NTT SOFTWARE, NTT, OR M.I.T. BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *	Authors: Li Yuhong	OMRON Corporation
 *		 Tatsuya Kato	NTT Software Corporation
 *   
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include "Xi18nint.h"
#include <X11/Xfuncs.h>

#ifndef X_WCHAR

extern char *_XAllocScratch();

int
_Xsiwcstombs(dpy, lcd, text, text_len, nullterm, str)
    Display *dpy;
    XLCd lcd;
    wchar_t *text;
    int text_len;
    Bool nullterm;
    char **str;
{
    int wsize;
    int msize = text_len << 1;
    int n;
    char *buf;
    char *old_locale;

    if (!text_len)
	return 0;
    if (lcd) {
	old_locale = setlocale(LC_CTYPE, (char *)NULL);
	/* XXX assume stateless encodings */
	setlocale(LC_CTYPE, lcd->core.name);
    }
    if (nullterm) {
	do {
	    msize += text_len;
	    buf = _XAllocScratch(dpy, msize);
#ifdef macII
	    /* AUX does not implement this yet, so how could we get any? */
	    n = 0;
#else
	    n = wcstombs(buf, text, msize);
#endif
	} while (n == msize);
	*str = buf;
    } else {
	wsize = (text_len + 1) * sizeof(wchar_t);
	do {
	    msize += text_len;
	    buf = _XAllocScratch(dpy, wsize + msize);
	    bcopy((char *)text, buf, wsize - sizeof(wchar_t));
	    ((wchar_t *)buf)[text_len] = 0;
#ifdef macII
	    /* AUX does not implement this yet, so how could we get any? */
	    n = 0;
#else
	    n = wcstombs(buf + wsize, (wchar_t *)buf, msize);
#endif
	} while (n == msize);
	*str = buf + wsize;
    }
    if (lcd)
	setlocale(LC_CTYPE, old_locale);
    return n;
}    

#endif /* X_WCHAR */

/*
 * The bodies of function XwcDrawString and XwcDrawImageString are
 * almost same.
 */
int
_XsiwcDrawString(dpy, d, font_set, gc, x, y, text, text_len)
    register Display   *dpy;
    Drawable            d;
    XFontSet            font_set;
    GC                  gc;
    int                 x, y;
    wchar_t            *text;
    int                 text_len;
{

#ifdef X_WCHAR

#define DecomposeGlyphCharset(xlocale,wcstr,wc_len,cs_str,cs_bytes,scanned,ctid) \
        _XwcDecomposeGlyphCharset(xlocale, wcstr, wc_len, cs_str, cs_bytes, scanned, ctid)

#define DrawString(dpy,d,g,x,y,str,len)                           \
        XDrawString(dpy, d, g, x, y, str, len)

#define DrawString16(dpy,d,g,x,y,str,len)                         \
        XDrawString16(dpy, d, g, x, y, str, len)

#include "TextBody.c"

#undef DrawString
#undef DrawString16

    return 0;

#else

    char *mbstr;
    int mb_len;
    mb_len = _Xsiwcstombs(dpy, font_set->core.lcd, text, text_len, False,
			  &mbstr);
    return (*font_set->methods->mb_draw_string)(dpy, d, font_set, gc,
						x, y, mbstr, mb_len);
#endif
}

void
_XsiwcDrawImageString(dpy, d, font_set, gc, x, y, text, text_len)
    register Display   *dpy;
    Drawable            d;
    XFontSet            font_set;
    GC                  gc;
    int                 x, y;
    wchar_t            *text;
    int                 text_len;
{
#ifdef X_WCHAR

#define DrawString(dpy,d,g,x,y,str,len)                           \
        XDrawImageString(dpy, d, g, x, y, str, len)

#define DrawString16(dpy,d,g,x,y,str,len)                         \
        XDrawImageString16(dpy, d, g, x, y, str, len)

#include "TextBody.c"

#undef DecomposeGlyphCharset
#undef DrawString
#undef DrawString16

#else

    char *mbstr;
    int mb_len;
    mb_len = _Xsiwcstombs(dpy, font_set->core.lcd, text, text_len, False,
			  &mbstr);
    (*font_set->methods->mb_draw_image_string)(dpy, d, font_set, gc,
					       x, y, mbstr, mb_len);
#endif
}
