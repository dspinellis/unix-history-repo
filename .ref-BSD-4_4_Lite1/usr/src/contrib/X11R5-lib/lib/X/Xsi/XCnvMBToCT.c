/*
 * $XConsortium: XCnvMBToCT.c,v 1.21 92/04/14 15:54:22 rws Exp $
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
 * Functions:
 *  _XConvertMBToCT()
 *  _XConvertCTToMB()
 * 
 */
#include "Xlibint.h"
#include "Xlocaleint.h"
#include <X11/Xutil.h>
#include <X11/Xos.h>

static int _XConvertCTextToMB();
extern Bool _XcwEscSetStatus();
extern void _XcwIdGetISOState();
extern int _XParseISOEncoding();
extern void _XmbSetCsid();
extern int _XctIsExtendSegment();

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

#define Return(result)  {                                               \
        *ct_bytes = ctcnt;                                              \
        *scanned_bytes = mbcnt;                                         \
	if (state) *state = xlocale->mb_state;				\
        if (ctcnt < limit)						\
	    *ct_str = 0;	/* additional service */		\
        if (error > 0) return (error);                                  \
        return (result);                                                \
    }

#define SaveStore(c) {                                                  \
        if (ctcnt >= limit) Return(BadBuffer);                          \
        *ct_str++ = c;                                                  \
        ctcnt++;                                                        \
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
_XConvertMBToCT(xlocale, mb_str, mb_bytes, ct_str, ct_bytes, scanned_bytes, state)
    XLocale	     xlocale;
    unsigned char   *mb_str;
    int              mb_bytes;
    unsigned char   *ct_str;
    int             *ct_bytes;
    int             *scanned_bytes;
    _State	    *state;
{
    int         mbcnt, ctcnt, crmbcnt, crctcnt;
    _CSID	csid;
    int		limit, deslen, error, ret;
    char       *esc, seq[MAXSEQUENCELENGTH], c;        
    _State	state_sv, state_ext;
    unsigned char   *ct_str_sv = NULL;
    int len_sv;

    if (!xlocale)
	xlocale = _XFallBackConvert();

    if (state && *state != XDEFAULT_STATE) {
	xlocale->mb_state = *state;
	if (_XmbCheck(xlocale) == False)
	    _Xmbinit(xlocale);
    } else
	_Xmbinit(xlocale);
    _Xctinit(xlocale);

    limit = *ct_bytes;
    mbcnt = ctcnt = 0;
    state_sv = xlocale->ct_state;
    error = 0;
    while (mb_bytes > 0 && (c = *mb_str) != 0) {
	_CSID ctGLorGR;

        if ((csid = _Xmbcsid(xlocale, mb_str)) == ND)
	    Return(BadEncoding);
        /*
         * filter control characters.
         */
        if (csid == C0 || csid == C1) {
            SaveStore(c);
            mb_str++, mb_bytes--, mbcnt++;
            continue;
        }
        /*
         * skip designation sequence of state-dependent codeset.
         * Warning: cannot recover the error of BadTerminate in this case.
         */
        if (_Xmbtype(xlocale) == CDS_STATEFUL &&
             (deslen = _Xmbdlen(xlocale, mb_str)) > 0) {
            mb_str += deslen, mb_bytes -= deslen, mbcnt += deslen;
            continue;
        }
	ctGLorGR = _XmbctGLorGR(xlocale);
	ctSetGLorGR(xlocale, ctGLorGR);
	ctSetid(xlocale, _Xmbctid(xlocale, csid));
	if (ctGLorGR == GL && 
		ctGetGLid(xlocale) != (state_sv & 0xff) ||
	    ctGLorGR == GR &&
		ctGetGRid(xlocale) != ((state_sv >> 8) & 0xff)) {
            /*
             * append designation of control sequence.
            */
	    state_ext = state_sv;
	    state_sv = xlocale->ct_state;
	    _XcwIdGetEncoding(xlocale, &esc);
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
        if ((ret = _XmbDecomposeGlyphCharset(xlocale, mb_str, mb_bytes, ct_str, 
		&crctcnt, &crmbcnt, (int *)NULL)) < 0)
            Return(ret);
	if (ct_str_sv) {
	    *ct_str_sv++ = (crctcnt + len_sv) / 128 + 128;
	    *ct_str_sv = (crctcnt + len_sv) % 128 +128;
	    ct_str_sv = NULL;
	    xlocale->ct_state = state_ext;
	    state_sv = state_ext;
	}
        error += ret;
        mb_str += crmbcnt, mb_bytes -= crmbcnt, mbcnt += crmbcnt;
        ct_str += crctcnt, ctcnt += crctcnt;
    }
    if (!_XcwCheckDefaultState(xlocale)) {
        (void) _XcwGetDefaultEncoding(xlocale, seq);
        AppendDesignation(seq);
	_Xctinit(xlocale);
    }
    Return(Success);
}

#undef Return
#undef SaveStore

#define Return(result) {                                                \
        *mb_bytes = mbcnt;                                              \
        *scanned_bytes = ctcnt;                                         \
	if (state) *state = xlocale->ct_state;				\
	if (mbcnt < limit)						\
	    *mb_str = 0;						\
        if (error > 0) return (error);                                  \
        return (result);                                                \
    }

#define SaveStore(c) {                                                  \
        if (mbcnt >= limit) Return(BadBuffer);                          \
        *mb_str++ = c;                                                  \
        mbcnt++;                                                        \
    }

/*
 * CombineCode():
 *   concatenate the byte with code if the byte is valid.
*/
#define CombineCode(code) {                                             \
	if (tmp == 1)       byte |= 0x80;				\
	else if (tmp == 2)  byte &= 0x7f;				\
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
_XConvertCTToMB(xlocale, ct_str, ct_bytes, mb_str, mb_bytes, scanned_bytes, state)
    XLocale	     xlocale;
    unsigned char   *ct_str;
    int              ct_bytes;
    unsigned char   *mb_str;
    int             *mb_bytes;
    int             *scanned_bytes;
    _State	    *state;
{
    register unsigned char byte;
    int         mbcnt, ctcnt, codelen;
    _CSID	csid_sv, newcsid;
    int         i, code, mbcode, len, limit, error;
    _State	state_sv;
    char 	seq[255];
    ISOStateInfo stateinfo;
    int tmp;
    char *defstr = XDefaultString();

    if (!xlocale)
	xlocale = _XFallBackConvert();

    if (state && *state != XDEFAULT_STATE) {
	xlocale->ct_state = *state;
	if (_XcwIdCheck(xlocale) == False)
	    _Xctinit(xlocale);
    } else
	_Xctinit(xlocale);
    _Xmbinit(xlocale);

    limit = *mb_bytes;
    mbcnt = ctcnt = error = 0;
    csid_sv = mbGetid(xlocale);
    codelen = _Xmblen(xlocale);
    _XcwIdGetISOState(xlocale, &stateinfo);
    state_sv = xlocale->mb_state;
    while (ct_bytes > 0 && (byte = *ct_str) != 0) {
	_CSID	ctid;
        switch (byte) {
          case HT:
          case NL:
            SaveStore(byte);
            ct_str++, ct_bytes--, ctcnt++;
            continue;
          case SPACE:
	    /* force designate ISO8859.1 to GL (Codeset 0) */
	    ctSetGLid(xlocale, CODESET0);
	    goto _Normal_char;
          case CSI:
            /* not supported yet */
          case ESC:
            /* parse the control escape sequence of CT encoding. */
            switch (_XParseISOEncoding(ct_str, ct_bytes, &len, &stateinfo)) {
              case Success:
		if (*(ct_str+1) == 0x25) {
		/* Extend segmant */
		    int tmplen = limit - mbcnt;
		    int ret;
		    if((ret = _XConvertCTextToMB(xlocale, ct_str, ct_bytes,
					mb_str, &tmplen, &len)) < 0)
			Return(ret);
		    error += ret;
		    mb_str += tmplen;
		    mbcnt +=tmplen;
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
                if (_XcwEscSetStatus(xlocale, seq) == False)
		    /* not register encoding by X. */
		    Return (BadEncoding);
                ct_str += len, ct_bytes -= len, ctcnt += len;
                continue;
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
_Normal_char:
	    ctSetGLorGR(xlocale, byte&0x80);
	    ctid = ctGetid(xlocale);
	    
	    tmp = 0;
	    if ((newcsid = _Xmbctidtocsid(xlocale, ctid)) == ND) {
		/* Retry one more: There are some charset which
		   can be designated GL or GR. */
		char *esc;
		_XcwIdGetEncoding(xlocale, &esc);
		strcpy(seq, esc);
		if (seq[1] == '$' && seq[3] >= 'A' && seq[3] <= 'D') {
		    if (seq[2] == '(') {
			tmp = 1; seq[2] = ')'; byte |= 0x80;
		    } else if (seq[2] == ')') {
			tmp = 2; seq[2] = '('; byte &= 0x7f;
		    } else {
			Return(BadEncoding);
		    }
		    _XcwEscSetStatus(xlocale, seq);
		    ctSetGLorGR(xlocale, byte&0x80);
		    ctid = ctGetid(xlocale);
		    newcsid = _Xmbctidtocsid(xlocale, ctid);
		}
		if (newcsid == ND) {
		    /* XXX BUG: if stateful encoding, need to check/add
				designate sequence of default string.
		       But current default string is NULL, so OK. :-) */
		    for (i = 0; *(defstr + i) != 0; i++) {
			SaveStore(*(defstr + i))
		    }
		    ct_str += stateinfo.code_bytes;
		    ct_bytes -= stateinfo.code_bytes;
		    ctcnt += stateinfo.code_bytes;
		    error++;
		    continue;
		}
	    }
	    _XmbSetCsid(xlocale, newcsid);
	    if (newcsid != csid_sv) {
		int	mbGLorGR;
                if (_Xmbtype(xlocale) == CDS_STATEFUL &&
		    ((mbGLorGR = mbGetGLorGR(xlocale)) == GL &&
		      mbGetGLid(xlocale) != (state_sv & 0xff) ||
		     mbGLorGR == GR &&
		      mbGetGRid(xlocale) != (state_sv >> 8 & 0xff))) {
                    /*
                     * append designation of state-dependent codeset.
                     */
                    char *dsg;
                    int   dslen;

                    dsg = _Xmbdsg(xlocale);
                    dslen = strlen(dsg);
                    if (mbcnt + dslen > limit)
			Return(BadBuffer);
                    strncpy((char *)mb_str, dsg, dslen);
                    mb_str += dslen, mbcnt += dslen;
		    state_sv = xlocale->mb_state;
                }
		csid_sv = newcsid;
                codelen = _Xmblen(xlocale);
		_XcwIdGetISOState(xlocale, &stateinfo);
	    }
            /* get codepoint of character. */
            code = 0;
            switch (stateinfo.code_bytes) {
              case 4:
                    CombineCode(code);
              case 3:
                    CombineCode(code);
              case 2:
                    CombineCode(code);
              case 1:
		    if (tmp == 1)	byte |= 0x80;
		    else if (tmp == 2)	byte &= 0x7f;
		    if (byte < stateinfo.code_min ||
			byte > stateinfo.code_max) {
			error++;
			byte = stateinfo.code_min;
		    }
		    code = (code << 8) | byte;

                    ct_str++, ct_bytes--;
                    break;
            }
            if (_Xcsctombc(xlocale, code, &mbcode) < 0)
                error++;
            for (i = codelen - 1; i >= 0; i--)
                SaveStore((mbcode >> (i * 8)) & 0x00FF);
            /*
             * advance the ctcnt without any error.
             */
            ctcnt += stateinfo.code_bytes;
        }
    }
    if (_Xmbtype(xlocale) == CDS_STATEFUL) {
	/*
	 * add designation sequence.
	 */
	int     dsglen;

	_XmbGetDefaultEncoding(xlocale, seq);
	dsglen = strlen(seq);
	if (mbcnt + dsglen > limit)
	    Return(BadBuffer);
	strncpy((char *)mb_str, seq, dsglen);
	mb_str += dsglen, mbcnt += dsglen;
    }
    _Xmbinit(xlocale);
    Return(Success);
}

#undef Return
#undef CombineCode

#define Return(result) {                                                \
        *mb_bytes = mbcnt;                                              \
        *scanned_bytes = ctcnt;                                         \
	if (mbcnt < limit)						\
	    *mb_str = 0;						\
        if (error > 0) return (error);                                  \
        return (result);                                                \
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
_XConvertCTextToMB(xlocale, ct_str, ct_bytes, mb_str, mb_bytes, scanned_bytes)
    XLocale xlocale;
    unsigned char   *ct_str;
    int              ct_bytes;
    unsigned char   *mb_str;
    int             *mb_bytes;
    int             *scanned_bytes;
{
    register unsigned char byte;
    int         mbcnt, ctcnt, codelen;
    int         i, code, mbcode, len, limit, error;
    char seq[128];
    int textlen;
    int bytes;
    _CSID csid, ctid;
    _State state_sv;
    char *defstr = XDefaultString();

    state_sv = xlocale->ct_state;
    limit = *mb_bytes;
    mbcnt = ctcnt = error = 0;

    if((len = _XctIsExtendSegment(xlocale, ct_str, &textlen, &bytes)) <= 0)
	/* not register encoding by X. */
	Return(BadEncoding);
    ct_str += len; ctcnt += len; ct_bytes -=len;

    if (ct_bytes < textlen)
	Return(BadTerminate) /* Nor enough data. What should do? */

    if (_Xmbtype(xlocale) == CDS_STATEFUL) {
	/* append designation of state-dependent codeset. */
	char *dsg;
	int   dslen;

	dsg = _Xmbdsg(xlocale);
	dslen = strlen(dsg);
	if (mbcnt + dslen > limit)
	    Return(BadBuffer);
	strncpy((char *)mb_str, dsg, dslen);
	mb_str += dslen, mbcnt += dslen;
    }

    ctid = ctGetid(xlocale);
    csid = _Xmbctidtocsid(xlocale, ctid);
    if (csid == ND) {
	int j;
	for (j = textlen/bytes; j > 0; j--) {
	    for (i = 0; *(defstr + i) != 0; i++) {
		SaveStore(*(defstr + i));
	    }
	    error++;
	}
	ctcnt += textlen;
	Return(Success);
    }
    _XmbSetCsid(xlocale, csid);
    codelen = _Xmblen(xlocale);

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
		ct_str++; ct_bytes--;
		break;
	}
	textlen -= bytes;
	if (_Xcsctombc(xlocale, code, &mbcode) < 0)
	    error++;
	for (i = codelen - 1; i >= 0; i--)
	    SaveStore((mbcode >> (i * 8)) & 0x00FF);
	ctcnt += bytes;
    }
    if (_Xmbtype(xlocale) == CDS_STATEFUL) {
	/* add designation sequence. */
	int     dsglen;

	_XmbGetDefaultEncoding(xlocale, seq);
	dsglen = strlen(seq);
	if (mbcnt + dsglen > limit)
	    Return(BadBuffer);
	strncpy((char *)mb_str, seq, dsglen);
	mb_str += dsglen, mbcnt += dsglen;
    }
    xlocale->ct_state = state_sv;
    Return(Success);
}
