/*
 * $XConsortium: XlcAccess.c,v 1.21 91/09/18 16:29:59 rws Exp $
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

#include "Xlibint.h"
#include <X11/Xos.h>
#include "Xlocaleint.h"

#define SO                      0x0E
#define SI                      0x0F
#define SS2                     0x8E
#define SS3                     0x8F
#define ESC                     0x1B

#define NotInvokeOrEsc(c)       (c != SO && c != SI &&                      \
                                 c != SS2 && c != SS3 && c != ESC)

int
_Xmbmsbon(xlocale)
    XLocale xlocale;
{
    if (!xlocale) xlocale = _XFallBackConvert();
    return (xlocale->xlc_db->lc_codeset->cds_msbon);
}

_CSID
_Xmbctid(xlocale, csid)
    XLocale	xlocale;
    _CSID	csid;
{
    Fontset    *flist;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    return ((int)csid >= flist->fs_num) ? ND:
	       flist->fs_cset[csid]->cs_id;
}

_CSID
_Xmbctidtocsid(xlocale, ctid)
    XLocale	xlocale;
    _CSID	ctid;
{
    _CSID	csid;
    int		fs_num;
    Fontset	*fontset;
    if (!xlocale) xlocale = _XFallBackConvert();
    fontset = xlocale->xlc_db->lc_fontset;
    fs_num = fontset->fs_num;
    for (csid = 0; (int)csid < fs_num; csid++) {
	if (fontset->fs_cset[csid]->cs_id == ctid)
	return (csid);
    }
    return (ND);
}

_CSID
_Xmbcsid(xlocale, mbstr)
    XLocale	   	xlocale;
    unsigned char      *mbstr;
{
    register Codeset        *codeset;
    register char           *dsg;
    register unsigned char  c;
    _CSID                   numcs;
    _CSID		    i;

    if (!xlocale) xlocale = _XFallBackConvert();

    /*
     * set initial state for state-dependent codeset.
     */
    if (!mbstr) {
	_Xmbinit(xlocale);
        return (mbGetid(xlocale));
    }
    codeset = xlocale->xlc_db->lc_codeset;
    /*
     * state-independent codeset
     */
    if (codeset->cds_type == CDS_STATELESS) {
        return (xlocale->mb_state = codeset->cds_map[*mbstr]);
    }
    /*
     * state-dependent codeset.
     */
    c = *mbstr;                                     /* for macro parameter */
    /* 
     * DEL(0x7F), not 0xFF, is control character.
     */
    if (!iscntrl(c & 0x7f) || (c == 0xFF)) {
	if ((c & 0x80) != mbGetGLorGR(xlocale)) {
	    mbSetGLorGR(xlocale, c & 0x80);
	}
        return (mbGetid(xlocale));
    }
    /*
     * control characters
     */
    if (NotInvokeOrEsc(c))
        return ((c & 0x80) == 0)? C0 : C1;
    /*
     * invoker or escape means a possible designation sequence.
     */
    numcs = codeset->cds_num;
    for (i = CODESET0; i < numcs; i++) {
        dsg = codeset->cds_dsg[i];
        if (!strncmp((char *)mbstr, dsg, strlen(dsg))) {
	    if (_XctisGLdsg(dsg)) {
		mbSetGLorGR(xlocale, GL);
	    } else {
		mbSetGLorGR(xlocale, GR);
	    }
	    mbSetid(xlocale, i);
            return i;
        }
    }
    return ND;
}

void
_XmbSetCsid(xlocale, csid)
    XLocale	 xlocale;
    _CSID	 csid;
{
    Codeset *codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    codeset = xlocale->xlc_db->lc_codeset;
    if (codeset->cds_type == CDS_STATEFUL) {
	if (_XctisGLdsg(codeset->cds_dsg[csid])) {
	    mbSetGLorGR(xlocale, GL);
	} else {
	    mbSetGLorGR(xlocale, GR);
	}
    }
    mbSetid(xlocale, csid);
}

_CSID
_XmbctGLorGR(xlocale)
    XLocale	xlocale;
{
    Fontset    *flist;
    _CSID	csid;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    csid = mbGetid(xlocale);
    return ((int)csid >= flist->fs_num) ? -1:
           flist->fs_cset[csid]->cs_GLorGR;
}

Charset *
_Xmbfscs(xlocale, csid)
    XLocale    xlocale;
    _CSID      csid;
{
    Fontset *flist;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    return ((int)csid >= flist->fs_num) ? (Charset *)0:
           flist->fs_cset[csid];
}

char *
_Xmbfsname(xlocale)
    XLocale	xlocale;
{
    Fontset	*flist;
    _CSID	csid;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    csid = mbGetid(xlocale);
    return ((int)csid >= flist->fs_num) ? (char *)0: 
           flist->fs_cset[csid]->cs_name;
}

int
_Xmblen(xlocale)
    XLocale	xlocale;
{
    _CSID	csid;
    Codeset	*codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    csid = mbGetid(xlocale);
    codeset = xlocale->xlc_db->lc_codeset;
    return (csid >= codeset->cds_num)? 1:
           codeset->cds_mblen[csid];
}

int
_Xmbfslen(xlocale)
    XLocale  	xlocale;
{
    Fontset    *flist;
    _CSID 	csid;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    csid = mbGetid(xlocale);

    return ((int)csid >= flist->fs_num)? 0 :
           flist->fs_cset[csid]->cs_len;
}

int
_Xmbctocsc(xlocale, mbstr, cscode)
    XLocale		xlocale;
    unsigned char      *mbstr;
    int                *cscode;
{
    register Range     *cds_cnvlist;
    int		       *cds_cnvindex;
    register int	i;
    Codeset	       *codeset;
    register unsigned	code;
    register unsigned	c;
    register int	len;
    int			start, stop;
    _CSID		csid;

    codeset = xlocale->xlc_db->lc_codeset;

    if (xlocale->mb_state & 0xff0000)	/* GR */
	csid = (xlocale->mb_state & 0xff00) >> 8;
    else				/* GL */
	csid = xlocale->mb_state & 0xff;
    if (csid >= codeset->cds_num)
	return (BadEncoding);

    cds_cnvindex = codeset->cds_cnvindex;
    cds_cnvlist = codeset->cds_cnvlist;
    start = cds_cnvindex[csid];
    stop = cds_cnvindex[csid + 1];
    len = codeset->cds_mblen[csid];

    if (len == 1) {
	code = *mbstr;
    } else if (len == 2) {
	code = (*mbstr << 8) | *(mbstr+1);
	if (!(*(mbstr+1))) {
	    *cscode = codeset->cds_cnvlist[start].cs_start;
	    return (BadEncoding);
	}
    } else  {
	code = 0;
	while ((len > 0) && ((c = *mbstr++))) {
	    code = (code << 8) | c; 
	    len--;
	}
	/* recover codepoint if expect more bytes from mbstr. */
	if (len > 0) {
	    *cscode = codeset->cds_cnvlist[start].cs_start;
	    return (BadEncoding);
	}
    }

    for (i = start; i < stop; i++) {
	if (code < cds_cnvlist[i].mb_start)
	    break;
    }
    /*
     * recover wrong code always with the first codepoint of the
     * charset id.
     */
    if (i == start || code > cds_cnvlist[i - 1].mb_end) {
	*cscode = cds_cnvlist[start].cs_start;
	return (BadEncoding);
    } else {
	*cscode = (cds_cnvlist[i - 1].cs_start + 
		code - cds_cnvlist[i - 1].mb_start);
	return (Success);
    }
}

int
_Xcsctombc(xlocale, cscode, code)
    XLocale		xlocale;
    int			cscode;
    int	       	       *code;
{
    register int        start, stop, i;
    register Codeset   *codeset;
    _CSID		csid;

    codeset = xlocale->xlc_db->lc_codeset;
    csid = mbGetid(xlocale);

    if (csid >= codeset->cds_num)
	return (BadEncoding);

    cscode |= codeset->cs_offset[csid];

    start = codeset->cds_cnvindex[csid];
    stop = codeset->cds_cnvindex[csid + 1];
    for (i = start; i < stop; i++) {
        if (cscode >= codeset->cds_cnvlist[i].cs_start &&
	    cscode <= codeset->cds_cnvlist[i].cs_end)
            break;
    }
    if (i == stop)
	return BadEncoding;
    *code = codeset->cds_cnvlist[i].mb_start +
	    cscode - codeset->cds_cnvlist[i].cs_start;
    return Success;
}


/*
 * get the designation sequence of state-dependent codeset.
 * any error should be controlled by caller.
 */
char *
_Xmbdsg(xlocale)
    XLocale	xlocale;
{
    _CSID	csid;
    Codeset    *codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    codeset = xlocale->xlc_db->lc_codeset;
    csid = mbGetid(xlocale);
    return (csid >= codeset->cds_num)? (char *)0:
           codeset->cds_dsg[csid];
}

/*
 * get the designation sequence of the font charset which 
 * is registered by X in CT encoding.
 * any error should be controlled by caller.
 */
char *
_Xmbfsdsg(xlocale)
    XLocale  	xlocale;
{
    Fontset    *flist;
    _CSID	csid;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    csid = mbGetid(xlocale);
    return ((int)csid >= flist->fs_num)? (char *)0:
           flist->fs_cset[csid]->cs_des;
}

/*
 * return the woffset of the font charset.
 */
wchar
_Xmbfswf(xlocale, csid)
    XLocale    xlocale;
    _CSID	csid;
{
    Fontset    *flist;
    if (!xlocale) xlocale = _XFallBackConvert();
    flist = xlocale->xlc_db->lc_fontset;
    return ((int)csid >= flist->fs_num) ? (~0L):
           flist->fs_cset[csid]->cs_woff;
}

/*
 * if the mbstr points to the designation sequence, return the length
 * of the designation, otherwise return 0;
 */
int 
_Xmbdlen(xlocale, mbstr)
    XLocale		xlocale;
    unsigned char      *mbstr;
{
    register int	len;
    _CSID		csid;
    Codeset 	       *codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    codeset = xlocale->xlc_db->lc_codeset;
    csid = mbGetid(xlocale);

    if (csid >= codeset->cds_num || 
	codeset->cds_type != CDS_STATEFUL)
        return 0;
    len = strlen(codeset->cds_dsg[csid]);
    if (!strncmp((char *)mbstr, codeset->cds_dsg[csid], len))
        return (len);
    else
        return (0);
}

Bool
_XmbCheck(xlocale)
    XLocale	xlocale;
{
    Codeset    *codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    codeset = xlocale->xlc_db->lc_codeset;
    if (mbGetGLid(xlocale) >= codeset->cds_num ||
	mbGetGRid(xlocale) >= codeset->cds_num)
	return (False);
    return (True);
}

void
_XmbGetDefaultEncoding(xlocale, dsg)
    XLocale	xlocale;
    char       *dsg;
{
    _CSID	id;
    _CSID	gl, gr;
    _State	mb_init;
    Codeset    *codeset;
    if (!xlocale) xlocale = _XFallBackConvert();
    codeset = xlocale->xlc_db->lc_codeset;

    *dsg = 0;
    if (codeset->cds_type != CDS_STATEFUL)
	return;
    mb_init = _XmbDefaultState(xlocale);
    gl = mb_init & 0xff;
    gr = (mb_init & 0xff00) >> 8;

    id = mbGetGLid(xlocale);
    if (id != gl && id < codeset->cds_num)
	strcpy(dsg, codeset->cds_dsg[gl]);
    id = mbGetGRid(xlocale);
    if (gl != gr && id != gr && 
	id < codeset->cds_num)
	strcat(dsg, codeset->cds_dsg[gr]);
}
