/*
 * $XConsortium: XCnvWCToMB.c,v 1.24 92/07/29 11:44:21 rws Exp $
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
 *		 Hiroshi Kuribayashi	OMRON Coproration
 *   
 */

/*
 * Functions:
 *  _XConvertMBToWC()
 *  _XConvertWCToMB()
 * 
 */
#include "Xlibint.h"
#include "Xlocaleint.h"
#include <X11/Xos.h>
#include <X11/Xutil.h>

#define ESC 0x1b
#define CSI 0x9b

/*
  _IsValidEscSequence()
  if str is valid escape sequence returns length of it in bytes.
  else if str is not valid escape sequence returns BadEncoding.
  else if str has not enough data returns BadTerminate.

  ISO 2022
    6.3.3 Categories of escape sequence
    6.3.3.1 Two-character escape sequence
    ESC Fs	Fs : 06/00 to 07/15
    ESC Fe	Fe : 04/00 to 05/15
    ESC Fp	Fp : 03/00 to 03/14

    6.3.3.2 Three-character escape sequence
    ESC I F	I  : 02/00 to 02/15
    ESC I Ft	Ft : 04/00 to 07/14
    ESC I Fp	Fp : 03/00 to 03/15

    6.3.3.3 Escape sequence having four or more characters
    ESC I...I F

  ISO 2375 : (I will rewrite this function after reading ISO 2375 :-)

  ISO 6429
    5.5 Control sequence (C1 & CSI)
    CSI P...P I...I F
    CSI : 01/11 05/11 or 09/11
    P...P Paramer Bytes, if present, consist from 03/00 to 03/15
    I...I Intermediate Bytes, if present, consist from 02/00 to 02/15
    F Final Byte, from 04/00 to 07/14

    5.6 Independent control function
    ESC Fs (same as ISO 2022)
*/
static int
_IsValidEscSequence(str, len)
unsigned char *str;
int len;
{
    int i;

    if (len <= 0)
	return (BadTerminate);

    if (*str == ESC) {
	if (len == 1)	/* Only ESC */
	    return (1);
	len--; i = 1; str++;
	if (*str == '[') {
	    /* 7bit CSI */
	    len--; i++; str++;
	} else if (*str >= 0x30 && *str <= 0x7e) {
	    return (2);
	} else if (*str >= 0x20 && *str <= 0x2f) {
	    for (i++, len--, str++; len > 0; str++,i++, len--) {
		if (*str >= 0x30 && *str <= 0x7e)
		    return(i);
		if (*str < 0x20 || *str >= 0x7f)
		    return (BadEncoding); /* Not valid Escape seqence */
	    }
	    return (BadTerminate);
	} else {
	    return (BadEncoding);
	}
    } else if (*str == CSI) {
	len--; i = 1; str++;
    } else {
	return (BadEncoding);	/* Not Escape sequence */
    }

    if (len == 0)
	return (i);	/* Only CSI */

    for (i++; len > 0; str++,i++, len--) {
	if (*str >= 0x40 && *str <= 0x7e)
	    return(i);
	if (*str < 0x20 || *str >= 0x7f)
	    return (BadEncoding); /* Not valid Escape seqence */
    }
    return (BadTerminate);
}


#define Return(result) {                                                \
        *wc_len = wccnt;                                                \
        *scanned_bytes = mbcnt;                                         \
	if (state) *state = xlocale->mb_state;				\
	if (wccnt < limit)						\
	    *wc_str = WNULL;						\
        if (error != 0) return(error);                                  \
        return(result);                                                 \
    }
         
#define SaveStore(wc) {                                                 \
        if (wccnt >= limit) Return(BadBuffer);                          \
        *wc_str++ = wc;                                                 \
        wccnt++;                                                        \
    }
    
            
/*
 * _XConvertMBToWC()
 *   convert string encoded in the current locale to wchar string.
 *   if the codeset of locale is state-dependent, the _Xmbcsid()
 *   will keep the current state, so need not the argument, such as isostate
 *   to remember it. 
 * Return Valur:
 *   Success
 *   BadBuffer
 *   BadTerminate
 *   > 0        number of wrong codepoints, but recovered by function
 * 
*/
int
_XConvertMBToWC(xlocale, mb_str, mb_bytes, wc_str, wc_len, scanned_bytes, state)
    XLocale	     xlocale;
    unsigned char   *mb_str;
    int		     mb_bytes;
    wchar	    *wc_str;
    int		    *wc_len;
    int		    *scanned_bytes;
    _State	    *state;
{
    unsigned char c;
    _CSID	csid_sv, newcsid;
    int         mbcnt, wccnt, deslen, codelen, code;
    int         error, limit;
    wchar       woffset;

    if (!xlocale)
	xlocale = _XFallBackConvert();

    if (state && *state != XDEFAULT_STATE) {
	xlocale->mb_state = *state;
	if (_XmbCheck(xlocale) == False)
	    _Xmbinit(xlocale);
    } else
	_Xmbinit(xlocale);
    _Xctinit(xlocale);

    limit = *wc_len;
    mbcnt = wccnt = error = 0;
    csid_sv = mbGetid(xlocale);
    codelen = _Xmblen(xlocale);
    woffset = _Xmbfswf(xlocale, csid_sv);
    while (mb_bytes > 0 && (c = *mb_str) != 0) {
        if ((newcsid = _Xmbcsid(xlocale, mb_str)) == ND) {
	    int len;
	    if ((len = _IsValidEscSequence(mb_str, mb_bytes)) > 0) {
		for ( ; len > 0; len--) {
		    SaveStore(_Xatowc(*mb_str));
		    mb_str++, mbcnt++, mb_bytes--;
		}
		continue;
	    }
	    Return(len);
	}
        /*
         * filter control characters.
         */
        if (newcsid == C0 || newcsid == C1) {
            SaveStore(_Xatowc(c)); 
            mb_str++, mbcnt++, mb_bytes--;
            continue;
        }
        /*
         * skip designation sequence of state-dependent codeset.
         * Warning: cannot recover the error of BadTerminate in this case.
         */
        if (_Xmbtype(xlocale) == CDS_STATEFUL &&
             (deslen = _Xmbdlen(xlocale, mb_str)) > 0) {
            mb_str += deslen, mbcnt += deslen, mb_bytes -= deslen;
            continue;
        }
        if (csid_sv != newcsid) {
            codelen = _Xmblen(xlocale);
            woffset = _Xmbfswf(xlocale, newcsid);
            csid_sv = newcsid;
        }
        if (codelen > mb_bytes)
            Return(BadTerminate);
        if (_Xmbctocsc(xlocale, mb_str, &code) < 0)
            error++;
        SaveStore((code & 0x7f7f) | woffset);
        mb_str += codelen, mbcnt += codelen, mb_bytes -= codelen;
    }
    Return(Success);
}

#undef  Return
#undef  SaveStore

#define Return(result) {                                                \
        *mb_bytes = mbcnt;                                              \
        *scanned_len = wccnt;                                           \
	if (mbcnt < limit)						\
	    *mb_str = 0;						\
        if (error != 0) return(error);                                  \
        return(result);                                                 \
    }

#define SaveStore(c) {                                                  \
        if (mbcnt >= limit) Return(BadBuffer);                          \
        *mb_str++ = c;                                                  \
        mbcnt++;                                                        \
    }

/*
 * _XConvertWCToMB()
 *   convert wchar string to the string encoded in the current locale.
 * 
 * Return Valur:
 *   Success
 *   BadBuffer
 *   > 0        number of wrong codepoints, but recovered by function
 *
*/
int
_XConvertWCToMB(xlocale, wc_str, wc_len, mb_str, mb_bytes, scanned_len)
    XLocale	        xlocale;
    wchar	       *wc_str;
    int			wc_len;
    unsigned char      *mb_str;
    int		       *mb_bytes;
    int		       *scanned_len;
{
    int     limit, mbcnt, wccnt;
    _CSID   csid_sv, newcsid;
    int     code, codelen, error, i;
    _State  state_sv;
    wchar   wc, woffset, newwoffset;
    extern void _XmbSetCsid();
    char *defstr = XDefaultString();
   
    if (!xlocale)
	xlocale = _XFallBackConvert();

    _Xmbinit(xlocale);
    _Xctinit(xlocale);
    limit = *mb_bytes;
    mbcnt = wccnt = error = 0;
    csid_sv = mbGetid(xlocale);
    codelen = _Xmblen(xlocale);
    woffset = _Xmbfswf(xlocale, csid_sv);
    state_sv = xlocale->mb_state;
    while (wc_len > 0 && ((wc = *wc_str) != WNULL)) {
	_CSID ctid;
        if (_Xiswcntrl(wc)) {
            SaveStore(_Xwctoa(wc));
            wc_str++, wccnt++, wc_len--;
            continue;
        }
	ctid = _XcwGetWoffsetFromLocale(xlocale, *wc_str, &newwoffset);
	if (ctid == ND || (newcsid = _Xmbctidtocsid(xlocale, ctid)) == ND) {
	    /* XXX BUG: if stateful encoding, need to check/add
			designate sequence of default string.
	       But current default string is NULL, so OK. :-) */
	    for (i = 0; *(defstr + i) != 0; i++) {
		SaveStore(*(defstr + i))
	    }
	    error++;
	    wc_str++, wccnt++, wc_len--;
	    continue;
	}
	_XmbSetCsid(xlocale, newcsid);
        if (csid_sv != newcsid) {
	    int mbGLorGR;
	    if (_Xmbtype(xlocale) == CDS_STATEFUL &&
		((mbGLorGR = mbGetGLorGR(xlocale)) == GL &&
		 mbGetGLid(xlocale) != (state_sv & 0xff) ||
		mbGLorGR == GR &&
		 mbGetGRid(xlocale) != (state_sv >> 8 & 0xff))) {
                /*
                 * add designation sequence.
                 */
                char    *dsg;
                int     dsglen;

                dsg = _Xmbdsg(xlocale);
                dsglen = strlen(dsg);
                if (mbcnt + dsglen > limit)
                    Return(BadBuffer);
                strncpy((char *)mb_str, dsg, dsglen);
                mb_str += dsglen, mbcnt += dsglen;
		state_sv = xlocale->mb_state;
            }
            codelen = _Xmblen(xlocale);
            csid_sv = newcsid;
            woffset = newwoffset;
        }
        if (_Xcsctombc(xlocale, (int)(*wc_str - woffset), &code) < 0)
            error++;
        for (i = codelen - 1; i >= 0; i--)
            SaveStore((code >> (i * 8)) & 0x00FF);
        wc_str++, wccnt++, wc_len--;
    }
    if (_Xmbtype(xlocale) == CDS_STATEFUL) {
	/*
	 * add designation sequence.
	 */
	char    dsg[64];
	int     dsglen;

	_XmbGetDefaultEncoding(xlocale, dsg);
	dsglen = strlen(dsg);
	if (mbcnt + dsglen > limit)
	    Return(BadBuffer);
	strncpy((char *)mb_str, dsg, dsglen);
	mb_str += dsglen, mbcnt += dsglen;
	_Xmbinit(xlocale);
    }
    Return(Success);
}
