/*
 * $XConsortium: XlcDefLd.c,v 1.2 92/06/27 21:55:09 rws Exp $
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
 *	Authors: Li Yuhong		OMRON Corporation
 *		 Tatsuya Kato		NTT Software Corporation
 *		 Hiroshi Kuribayashi	OMRON Corporation
 *   
 */

#include "Xlibint.h"
#include "Xi18nint.h"

#if __STDC__ && !defined(NORCONST)
#define RConst const
#else
#define RConst /**/
#endif

static RConst XLCdMethodsRec lcd_methods = {
    _XlcDefaultMapModifiers,
    _XsiCreateFontSet,
    _XipOpenIM
};

XLCd _XlcDefaultLoader(osname)
    char *osname;
{
    char *name;
    XLocale xlc;
    XsiLCd lcd;
#if !defined(X_NOT_STDC_ENV) && !defined(X_LOCALE)
    char siname[256];
    char *_XlcMapOSLocaleName();

    name = _XlcMapOSLocaleName(osname, siname);
#else
    name = osname;
#endif
    xlc = _XlcMakeLocale(name);
    if (!xlc)
	return NULL;
    lcd = (XsiLCd)Xmalloc(sizeof(XsiLCdRec));
    if (!lcd)
	return NULL;
    lcd->methods = (XLCdMethods)&lcd_methods;
    lcd->core.name = (char *)Xmalloc(strlen(osname) + 1);
    if (!lcd->core.name) {
	Xfree((char *)lcd);
	return NULL;
    }
    strcpy(lcd->core.name, osname);
    lcd->core.modifiers = NULL;
    lcd->xlc = xlc;

    return (XLCd)lcd;
}
