/*
 * $XConsortium: XCnvToStr.c,v 1.14 91/05/31 18:57:15 rws Exp $
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
 *	Author:		 Hiroshi Kuribayashi	OMRON Corporation
 *   
 */

/*
 * This files includes 3 functions:
 * 
 *      _XConvertWCToString()
 *      _XConvertMBToString()
 *	DefaultString()
 * 
*/
#include "Xlibint.h"
#include "Xlocaleint.h"
#include <X11/Xos.h>

/*
 * Constant Definitions
*/
#define HT                  0x09        /* horizontal tab */
#define NL                  0x0A        /* new line */

char *
XDefaultString()
{
    return("");
}

/* partial conversion from WC to Xstring */

int
_XConvertWCToString(wc_str, wc_len, string, str_len, scanned_len)
    wchar	       *wc_str;
    int			wc_len;
    unsigned char      *string;
    int		       *str_len;
    int		       *scanned_len;
{
    int			limit;
    wchar		wc;
    unsigned char	group;

    limit = *str_len;
    (*scanned_len) = (*str_len) = 0;

    while (wc_len > 0 && (wc = *wc_str) != WNULL) {
	if ((*str_len) >= limit) {
            return(BadBuffer);
	}
	if (((wc) & SCRIPT1) == LATINSCRIPT ||
	    ((wc) & CONTROLSCRIPT) == CLPADPADPAD &&
		((group = wgetgroup(wc)) == HT ||
		group == NL)) {
	    *string++ = _Xwctoa(wc);
	    (*str_len)++;
	}
        wc_str++, wc_len--, (*scanned_len)++;
    }
    if ((*str_len) < limit)
	*string = 0;
    return(Success);
}

#define Return(result)  {                                               \
	*ct_bytes = ctcnt;                                              \
	*scanned_bytes = mbcnt;                                         \
	if (ctcnt < limit)                                              \
	    *ct_str = 0;        /* additional service */                \
	return (result);                                                \
    }

#define SaveStore(c) {                                                  \
	if (ctcnt >= limit) Return(BadBuffer);                          \
	*ct_str++ = c;                                                  \
	ctcnt++;                                                        \
    }

int
_XConvertMBToString(mb_str, mb_bytes, ct_str, ct_bytes, scanned_bytes)
    unsigned char   *mb_str;
    int              mb_bytes;
    unsigned char   *ct_str;
    int             *ct_bytes;
    int             *scanned_bytes;
{
    XLocale	xlocale;
    _CSID	ctid, csid;
    int         mbcnt, ctcnt;
    int         limit, len;
    unsigned char       c;
    int		code;

    xlocale = _XFallBackConvert();
    _Xmbinit(xlocale);
    _Xctinit(xlocale);

    limit = *ct_bytes;
    mbcnt = ctcnt = 0;

    while (mb_bytes > 0 && (c = *mb_str) != 0) {
	if ((csid = _Xmbcsid(xlocale, mb_str)) == ND)
	    Return(BadEncoding);
	if (_Xmbtype(xlocale) == CDS_STATEFUL &&
	    (len = _Xmbdlen(xlocale, mb_str)) > 0) {
	    /* skip designation sequence */
	    mbcnt += len; mb_str += len, mb_bytes -= len;
	    continue;
	}
	ctid = _Xmbctid(xlocale, csid);
	if (ctid == CODESET0 || ctid == CODESET1) {
	    if (_Xmbctocsc(xlocale, mb_str, &code) < 0) {
		mbcnt++; mb_str++; mb_bytes--;
		Return (BadEncoding);
	    }
	    SaveStore(code);
	    mbcnt++; mb_str++; mb_bytes--;
	    continue;
	} else if (csid == C0) {
	    if (c == HT || c == NL) {
		SaveStore(c);
	    }
	    mbcnt++; mb_str++, mb_bytes--;
	    continue;
	} else {
	    len = _Xmblen(xlocale);
	    mbcnt += len; mb_str += len; mb_bytes -= len;
	}
    }
    Return(Success);
}

