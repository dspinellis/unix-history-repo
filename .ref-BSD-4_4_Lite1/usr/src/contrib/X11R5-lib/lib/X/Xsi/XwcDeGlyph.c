/*
 * $XConsortium: XwcDeGlyph.c,v 1.16 92/04/14 15:55:31 rws Exp $
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
 *	Authors: Li Yuhong		OMRON Corportaion
 *		 Hiroshi Kuribayashi	OMRON Corporation
 *   
 */

/*
 *      Function Name: _XwcDecomposeGlyphCharset
*/
#include <stdio.h>
#include <X11/Xlibint.h>
#include "Xlocaleint.h"
#include <X11/Xos.h>
#include <X11/Xutil.h>

#define MAGIC_NUMBER	(9999)

#define Return(result) {                                                \
        *cs_bytes = cscnt;                                              \
        *scanned_len = wccnt;                                           \
        if (cscnt < limit) *cs_str = '\0'; /*additional service*/       \
        if (error > 0) return(error);                                   \
        return(result);                                                 \
    }

#define StoreByte(code) {                                               \
        int byte = (code);                                              \
        if (byte < (int)stateinfo.code_min || 				\
	    byte > (int)stateinfo.code_max) {				\
            error++;                                                    \
            *cs_str++ = stateinfo.code_min;                             \
        } else                                                          \
            *cs_str++ = byte;                                           \
    }

int
_XwcDecomposeGlyphCharset(xlocale, wc_str, wc_len, cs_str, cs_bytes,
			  scanned_len, ctid)
    XLocale        xlocale;
    wchar	   *wc_str;
    int	            wc_len;
    unsigned char  *cs_str;
    int	           *cs_bytes;
    int	           *scanned_len;
    int		   *ctid;
{
    int    	    cscnt, wccnt;
    wchar           woffset, newwoffset, code, wc;
    int             limit, error;
    ISOStateInfo    stateinfo;
    char	    *esc;
    int i;
    char *defstr = XDefaultString();

    limit = *cs_bytes;
    wccnt = cscnt = error = 0;
    woffset = MAGIC_NUMBER;
    while (wc_len > 0 && (wc = *wc_str) != WNULL) {
	/*
	 * filter control characters.
	 */
        if (_Xiswcntrl(wc)) {
	    if (cscnt >= limit) 
		Return(BadBuffer);
	    *cs_str++ = _Xwctoa(wc);
	    cscnt++;
	    wc_str++, wc_len--, wccnt++;
	    continue;
	}
	if (woffset == MAGIC_NUMBER) {
    	    _XcwGetAll(xlocale, wc, &esc, &woffset, &stateinfo);
	    if (woffset == 0) {
		/* XXX BUG: need to check designate sequence of default string.
		   But current default string is NULL, so OK. :-) */
		for (i = 0; *(defstr + i) != 0; i++) {
		    *cs_str++ = *(defstr + i);
		    cscnt++;
		}
		error++;
		wc_str++, wc_len--, wccnt++;
		continue;
	    }
	    if (ctid != NULL)
		*ctid = ctGetid(xlocale);
	    newwoffset = woffset;
	} else {
            if(_XcwGetWoffsetFromLocale(xlocale, wc, &newwoffset) == ND) { /* MUST not change Status */
		/* XXX BUG: need to check designate sequence of default string.
		   But current default string is NULL, so OK. :-) */
		for (i = 0; *(defstr + i) != 0; i++) {
		    *cs_str++ = *(defstr + i);
		    cscnt++;
		}
		error++;
		wc_str++, wc_len--, wccnt++;
		continue;
	    }
	}
	if (woffset != newwoffset)
	    break;
        code = wc - woffset;
        if ((cscnt + stateinfo.code_bytes) > limit)
            Return(BadBuffer);
	
	/* The space charcter(0x20) is include into the latin-1 charset */
	if (wc == WCHARSPACE && woffset == LATINSCRIPT)
	    *cs_str++ = code;
	else {
	    /*
	     * only 2 or 1 byte(s) supported by X.
	    */
	    if (stateinfo.code_bytes == 2)
		StoreByte((code >> 8) & 0x007F | ctGetGLorGR(xlocale));
	    StoreByte(code & 0x007F | ctGetGLorGR(xlocale));
	}
        wc_str++, wc_len--, wccnt++;
        cscnt += stateinfo.code_bytes;
    }
    Return(Success);    
}
