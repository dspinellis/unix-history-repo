/*
 * $XConsortium: XCnvCTToWC.c,v 1.22 91/11/17 16:15:29 rws Exp $
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
 *		 Hiroshi Kuribayashi	OMRON Corporation
 *   
 */

/*
 * This files includes 2 functions:
 * 
 *      _XConvertCTToWC()
 *      _XConvertWCToCT()
 * 
*/
#include "Xlibint.h"
#include "Xlocaleint.h"
#include <X11/Xos.h>
#include <X11/Xutil.h>

static int _XConvertCTextToWC();
extern int _XParseISOEncoding();
extern Bool _XcwEscSetStatus();
extern void _XwcSetCsid();

/*
 *          Function Name: _XConvertCTToWC
 *
*/

/*
 * Constant Definitions
*/
#define HT                  0x09        /* horizontal tab */
#define NL                  0x0A        /* new line */
#define ESC                 0x1B        /* escape */
#define CSI                 0x9B        /* control sequence introducer. */
#define SPACE               0x20        /* space */
#define GL94MIN             0x21        /* manimun of code of GL 94 charset */  
#define GL94MAX             0x7E        /* maximun of code of GL 94 charset */

/* 
 * Macro Procedure Definitions
*/
/* 
 * Return():
 *   set result values before funciton returns.
 *   If there is more room for output buffer wc_str, provide additional
 *    service, i.e., append WNULL at tail.
*/
#define Return(result) {                                                \
        *wc_len = wcnt;                                                 \
        *scanned_bytes = ctcnt;                                         \
        if (wcnt < limit) *wc_str = WNULL;                              \
	if (state) *state = xlocale->ct_state;				\
        if (error > 0) return(error);                                   \
        return(result);                                                 \
    }

/*
 * SaveStore():
 *   store converted wchar code to buffer, and make sure no overflow.
*/
#define SaveStore(wc) {                                                 \
        if (wcnt >= limit)                                              \
            Return(BadBuffer);						\
        *wc_str++ = wc;                                                 \
        wcnt++;                                                         \
    }

/*
 * CombineCode():
 *   concatenate the byte with code if the byte is valid.
*/
#define CombineCode(code) {                                             \
        if (byte < stateinfo.code_min || byte > stateinfo.code_max) {   \
            error++;                                                    \
            byte = stateinfo.code_min;                                  \
        }                                                               \
        code = (code << 8) | byte;                                      \
        ct_str++, ct_bytes--;                                           \
        if (ct_bytes < 1)                                               \
            Return(BadTerminate);                                       \
        byte = *ct_str;                                                 \
    }

int
_XConvertCTToWC(xlocale, ct_str, ct_bytes, wc_str, wc_len, scanned_bytes, state)
    XLocale	        xlocale;
    unsigned char      *ct_str;
    int			ct_bytes;
    wchar	       *wc_str;
    int		       *wc_len;
    int		       *scanned_bytes;
    _State	       *state;
{
    register unsigned char	byte;
    register unsigned int	code;
    wchar			woffset;
    int				wcnt, ctcnt, limit, len;
    ISOStateInfo		stateinfo;
    char			seq[MAXSEQUENCELENGTH];
    int				error = 0;

    if (!xlocale)
	xlocale = _XFallBackConvert();
    if (state && *state != XDEFAULT_STATE) {
	xlocale->ct_state = *state;
	if (_XcwIdCheck(xlocale) == False)
	    _Xctinit(xlocale);
    } else
	_Xctinit(xlocale);

    limit = *wc_len;
    wcnt = ctcnt = 0;
    while (ct_bytes > 0 && (byte = *ct_str) != 0) {
        switch (byte) {
          case HT:
          case NL:
          case SPACE:
            SaveStore(_Xatowc(byte));
            ct_str++, ct_bytes--, ctcnt++;
            continue;
          case CSI:
            /* not supported yet */
          case ESC:
            /* parse the control escape sequence of CT encoding. */
            switch (_XParseISOEncoding(ct_str, ct_bytes, &len, &stateinfo)) {
              case Success:
		if (*(ct_str+1) == 0x25) {
		    /* Extend segmant */
		    int tmplen = limit - wcnt;
		    int ret;
		    if((ret = _XConvertCTextToWC(xlocale, ct_str, ct_bytes,
					    wc_str, &tmplen, &len)) < 0)
			Return(ret);
		    wc_str += tmplen;
		    wcnt +=tmplen;
		    ct_str += len, ct_bytes -= len, ctcnt += len;
		    continue;
		}
                /*
                 * In mose case the control sequence is new one, so we 
                 * set it to current state directly, enhance little speed
                 * without no comparision. 
                 * 
                */
		(void) strncpy(seq, (char *)ct_str, len);
                seq[len] = '\0';
                if (_XcwEscSetStatus(xlocale, seq) == True) {
                    ct_str += len, ct_bytes -= len;
                    ctcnt += len;
                    continue;
                }
                /*
                 * Actually, it is not registered encoding, can not be
                 * recognized by this convertor. go to next step.
                */
              case BadEncoding:
                /*
                 * wrong escape sequence, the function can not recover
                 * this error, return it.   
                */
                Return(BadEncoding);
              case BadTerminate:
                Return(BadTerminate);
              default:  /* never go to here */
                Return(BadEncoding);
                ;
            }   
          default:
            /* get codepoint of character. */
	    ctSetGLorGR(xlocale, byte&0x80);
	    _XcwIdGetAll(xlocale, &woffset, &stateinfo);
            code = 0; 
            switch (stateinfo.code_bytes) {
              case 4:
                    CombineCode(code);
              case 3:
                    CombineCode(code);
              case 2:
                    CombineCode(code);
              case 1:
		    if (byte < stateinfo.code_min ||
			byte > stateinfo.code_max) {
			error++;
			byte = stateinfo.code_min;
		    }
		    code = (code << 8) | byte;
                    ct_str++, ct_bytes--;
		    code &= 0x7f7f7f7f;	/* MSB off */
                    break;
            }
            SaveStore(woffset | code);
            /*
             * after no any error, then advance scanned_bytes 
             * "ctcnt".
            */
            ctcnt += stateinfo.code_bytes;
            continue;
        }
    } 
    Return(Success);
}

/*
 *          Function Name: _XConvertWCToCT
 *
*/
#undef Return
#undef SaveStore

#define Return(result) {                                                \
        *ct_bytes = ctcnt;                                              \
        *scanned_len = wcnt;                                            \
	if (ctcnt < limit)						\
	    *ct_str = 0;						\
        if (error > 0) return(error);                                   \
        return(result);                                                 \
    }

#define SaveStore(c)   {                                                \
       if (ctcnt >= limit) Return(BadBuffer);                           \
       *ct_str++ = c;                                                   \
       ctcnt++;                                                         \
    }

#define AppendDesignation(state) {                                      \
        int len = strlen(state);                                        \
        if ((ctcnt + len) > limit)                                      \
           Return(BadBuffer);                                           \
        (void) strcpy((char *)ct_str, state);                           \
        ct_str += len;                                                  \
        ctcnt += len;                                                   \
    }
     

int
_XConvertWCToCT(xlocale, wc_str, wc_len, ct_str, ct_bytes, scanned_len)
    XLocale	        xlocale;
    wchar	       *wc_str;
    int			wc_len;
    unsigned char      *ct_str;
    int		       *ct_bytes;
    int		       *scanned_len;
{
    char	       *esc, seq[MAXSEQUENCELENGTH];
    wchar		woffset, wc;
    int			wcnt, ctcnt, crwcnt, crctcnt, limit;
    int			ret, error = 0;
    ISOStateInfo	stateinfo;
    _State		state_sv, state_ext;
    unsigned char      *ct_str_sv = NULL;
    int len_sv;
    char *defstr = XDefaultString();

    if (!xlocale)
	xlocale = _XFallBackConvert();

    _Xctinit(xlocale);
    state_sv = xlocale->ct_state;
    limit = *ct_bytes;
    wcnt = ctcnt = 0;
    while ((wc_len > 0) && ((wc = *wc_str) != WNULL)) {
	_CSID	ctGLorGR;
        /*
         * filter control characters. 
        */
        if (_Xiswcntrl(wc)) {
            SaveStore(_Xwctoa(wc));
            wc_str++, wc_len--, wcnt++;
            continue;
        }
        _XcwGetAll(xlocale, wc, &esc, &woffset, &stateinfo);
	if (woffset == 0) {
	    int i;
	    /* XXX BUG: need to check/add designate sequence of default string.
	       But current default string is NULL, so OK. :-) */
	    for (i = 0; *(defstr + i) != 0; i++) {
		SaveStore(*(defstr + i))
	    }
	    error++;
	    wc_str++, wc_len--, wcnt++;
            continue;
	}
        if ((ctGLorGR = ctGetGLorGR(xlocale)) == GL &&
		ctGetGLid(xlocale) != (state_sv & 0xff) ||
	    ctGLorGR == GR &&
		ctGetGRid(xlocale) != ((state_sv >> 8) & 0xff)) {
	    state_ext = state_sv;
	    state_sv = xlocale->ct_state;
            /*
             * append designation of control sequence.
            */
	    if (*(esc+1) == 0x25) {
		ct_str_sv = ct_str + 4;
		len_sv = strlen(esc) - 6;
	    }
            AppendDesignation(esc);
        }
        /* 
         * remainning buffer length of ct_str.
        */
        crctcnt = limit - ctcnt;
        if ((ret = _XwcDecomposeGlyphCharset(xlocale, wc_str, wc_len, ct_str,
		&crctcnt, &crwcnt, (int *)NULL)) < 0)
            Return(ret);
	if (ct_str_sv) {
	    *ct_str_sv++ = (crctcnt + len_sv) / 128 + 128;
	    *ct_str_sv = (crctcnt + len_sv) % 128 +128;
	    ct_str_sv = NULL;
	    xlocale->ct_state = state_ext;
	    state_sv = state_ext;
	}
        error += ret;
        wc_str += crwcnt, wc_len -= crwcnt, wcnt += crwcnt;
        ct_str += crctcnt, ctcnt += crctcnt;
    }
    if (!_XcwCheckDefaultState(xlocale)) {
        (void) _XcwGetDefaultEncoding(xlocale, seq);
        AppendDesignation(seq);
	_Xctinit(xlocale);
    }
    Return(Success);
}

int
_XctIsExtendSegment(xlocale, ct_str, textlen, bytes)
XLocale xlocale;
unsigned char *ct_str;
int *textlen;
int *bytes;
{
    unsigned char *text;
    int seqlen;
    unsigned char name[128];
    _CSID csid;
    int m, l;

    if(*(ct_str+1) != 0x25 || *(ct_str+2) != 0x2f)
	return (0);     /* Not CT Extend Segment */
    *bytes = *(ct_str+3) -'0';
    if (*bytes < 0 || *bytes > 4)
	return (0);     /* Not CT Extend Segment */
    else if (*bytes == 0)
	/* I'm not sure. Valiable octes pre char cannot convert */
	*bytes = 1;


    text = (unsigned char *)index((char *)ct_str+6, 0x02) + 1;
    m = *(ct_str+4);
    l = *(ct_str+5);
    seqlen = text - ct_str;

    strncpy((char *)name, (char *)ct_str+6, seqlen - 6);
    name[seqlen - 7] =0;
    csid = _XcwNameGetGLorGRId(name, *(text+1) & 0x80);
    ctSetGLorGR(xlocale, *(text+1) & 0x80);
    ctSetid(xlocale, csid);

    *textlen = (m-128)*128+(l-128) + 5 - seqlen + 1;	/* !!!!! */
    return(seqlen);
}

#undef Return
#undef SaveStore
#undef CombineCode

#define Return(result) {                                                \
        *wc_bytes = wccnt;                                              \
        *scanned_bytes = ctcnt;                                         \
	if (wccnt < limit)						\
	    *wc_str = 0;						\
        if (error > 0) return (error);                                  \
        return (result);                                                \
    }

#define SaveStore(wc) {                                                 \
        if (wccnt >= limit) Return(BadBuffer);                          \
        *wc_str++ = wc;                                                 \
        wccnt++;                                                        \
    }

/*
 * CombineCode():
 *   concatenate the byte with code if the byte is valid.
*/
#define CombineCode(code) {                                             \
        code = (code << 8) | byte;                                      \
        ct_str++; ct_bytes--;						\
        if (ct_bytes < 1)                                               \
            Return(BadTerminate);                                       \
        byte = *ct_str;                                                 \
    }

static int
_XConvertCTextToWC(xlocale, ct_str, ct_bytes, wc_str, wc_bytes, scanned_bytes)
    XLocale xlocale;
    unsigned char   *ct_str;
    int              ct_bytes;
    wchar	    *wc_str;
    int             *wc_bytes;
    int             *scanned_bytes;
{
    register unsigned char byte;
    int         wccnt, ctcnt;
    int         code, len, limit, error;
    int textlen;
    int bytes;
    _State state_sv;
    wchar woffset;

    state_sv = xlocale->ct_state;
    limit = *wc_bytes;
    wccnt = ctcnt = error = 0;

    if((len = _XctIsExtendSegment(xlocale, ct_str, &textlen, &bytes)) < 0)
	/* not register encoding by X. */
	Return(BadEncoding);
    ct_str += len; ctcnt += len; ct_bytes -=len;

    if (ct_bytes < textlen)
	Return(BadTerminate); /* Nor enough data. What should do? */

    _XcwIdGetWoffset(xlocale, &woffset);

    while (textlen > 0) {
	/* get codepoint of character. */
	code = 0;
	byte = *ct_str;
	switch (bytes) {
	  case 4: CombineCode(code);
	  case 3: CombineCode(code);
	  case 2: CombineCode(code);
	  case 1:
		code = (code << 8) | byte;
		ct_str++, ct_bytes--;
		code &= 0x7f7f7f7f; /* MSB off */
		break;
	}
	SaveStore(woffset | code);
	textlen -= bytes;
	ctcnt += bytes;
    }
    xlocale->ct_state = state_sv;
    Return(Success);
}
