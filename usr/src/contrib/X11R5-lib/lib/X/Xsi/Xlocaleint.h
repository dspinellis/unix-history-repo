/*
 * $XConsortium: Xlocaleint.h,v 1.35 92/04/14 15:55:37 rws Exp $
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
 *		 Hiroashi Kuribayashi	OMRON Corporation
 *   
 */

/*
 *      Xlocaleint.h -- Header definition.
 * 
 */
#ifndef _XLOCALEINT_H_
#define _XLOCALEINT_H_

#define _WCHARXLIB_
#include "wcharint.h"
#undef _WCHARXLIB_

#include <X11/Xlocale.h>
#include "Xlcint.h"

#define MAXCHARSETS         16
#define MAXSEQUENCELENGTH   255

#define XDEFAULT_STATE 0xffffff  /* to specify default _State for XConvertXXToXX */
/*
 * Charset id of codeset. Maximun is 10 charsets of one codeset.
 * Must Define:  CODESETi = i, other functions will use this convension.
 * 
 */
typedef	unsigned int _State;
typedef	unsigned char _CSID;

#define CODESET0     0   /* Charset 0 */
#define CODESET1     1   /* Charset 1 */
#define CODESET2     2   /* Charset 2 */
#define CODESET3     3   /* Charset 3 */
#define CODESET4     4   /* Charset 4 */
#define CODESET5     5   /* Charset 5 */
#define CODESET6     6   /* Charset 6 */
#define CODESET7     7   /* Charset 7 */
#define CODESET8     8   /* Charset 8 */
#define CODESET9     9   /* Charset 9 */
#define CODESETA     10   /* Charset A */
#define CODESETB     11   /* Charset B */
#define CODESETC     12   /* Charset C */
#define CODESETD     13   /* Charset D */
#define CODESETE     14   /* Charset E */
#define CODESETF     15   /* Charset F */
#define C0      16  /* Control Set 0 */
#define C1      17  /* Control Set 1 */
#define ND      127 /* Not Defined */

/* ISO encoding */
#define	GL	0
#define	GR	0x80
#define	C94	0
#define	C96	1

/*
 * Type of codeset
 */
#define CDS_STATELESS   1   /* state-independent codeset */
#define CDS_STATEFUL    2   /* state-dependent codeset */
#define CDS_SELFDEFINED 10  /* self defined codeset, not used yet.*/


/*
 * Data Structure for Convert Map List
*/
typedef struct {
    unsigned int  mb_start;    /* starting code point of multibyte*/
    unsigned int  mb_end;      /* ending code point of multibyte  */
    unsigned int  cs_start;    /* starting code point of charset */
    unsigned int  cs_end;      /* ending code point of charset  */
} Range;

/*
 * Data Structure for Codeset
 */
typedef struct {                    /* Stateless Codeset */
    int     cds_type;               /* stateless type */
    _CSID   cds_num;                /* number of charsets */
    _State  mb_init;		    /* inital designate */
    int     cds_msbon;              /* 8bit encoding or not */
    int     cds_mb_cur_max;	    /* max number of bytes of this codeset */
				    /* = max(cds_mblen) + max(cds_dsg) */
    int     cds_mblen[MAXCHARSETS]; /* mb length of each charsets */
    unsigned short cs_offset[MAXCHARSETS]; /* GL or GR */
    Range  *cds_cnvlist;            /* table-driven conversion list */
    int     cds_cnvindex[MAXCHARSETS];/* conversion list index */
    int   (*cds_cnvproc)();         /* procedure-driven conversion, not used */
    union {
        _CSID  map[256];         /* charset mapping table,stateless CCS*/
        char *dsg[MAXCHARSETS]; /* designation of charaset,stateful CCS*/
   } cds;
#define cds_map  cds.map
#define cds_dsg  cds.dsg
} Codeset; 

/*
 * The number of charasets is different from the number of fonts in
 * some codeset, e.g., the current UJIS, no extended font for CODESET3.
 */
typedef struct {
    char           *cs_name;    /* charset name: CharSetReg-CharSetEncoding */
    wchar           cs_woff;    /* wchar offset of DP 10646 */
    int             cs_len;     /* length in bytes */
    char           *cs_des;     /* designate sequence of COMPOUND TEXT */
    _CSID	    cs_GLorGR;
    _CSID	    cs_id;	/* index of ISOStateTable(_XInitCTEncoding) */
}               Charset;

typedef struct {
    int       fs_num;           /* number of fonts of locale */
    Charset **fs_cset;          /* charset encoding */
} Fontset;

typedef struct _XLocaleDB {
    char	      *lc_name;		/* locale name for LC_ALL */
    char	      *lc_encoding;	/* encoding name */
    Codeset	      *lc_codeset;	/* category of codeset */
    Fontset	      *lc_fontset;	/* category of fontset */
    struct _XLocaleDB *next;		/* chain pointer */
} XLocaleDB;

/* the definition for initial state of locale is:

    initial state of state-independent codeset:
     byte 2         byte 1         byte 0
   +-------------+--------------+--------------+
   | GL/GR flag  |  CODESET0    |  CODESET0    |
   +-------------+--------------+--------------+

    initial state of state-dependent codeset:
      byte 2         byte 1         byte 0
    +-------------+--------------+--------------+
    | GL/GR flag  | GR CODESETj  | GL CODESETi  |
    +-------------+--------------+--------------+

     where:
	GL CODESETi is the first GL encoding of codeset, usually is
	CODESET0.
	GR CODESETj is the first GR encoding of codeset.
 */

typedef struct {
    XLocaleDB	       *xlc_db;
    char	       *lc_lang;		/* locale name */
    _State		ct_state;
    _State		mb_state;
} XLocaleRec, *XLocale;

typedef struct {
    XLCdMethods		methods;		/* methods of this LC */
    XLCdCoreRec		core;			/* core data of this LC */
    XLocale		xlc;
} XsiLCdRec, *XsiLCd;

/* 
 * Data Structure for Charset.
 * This charset is designated by escape sequence that is defined in ISO 2022.
 */
typedef struct {
    int             code_bytes;     /* number of bytes of 94/96 charset*/
    unsigned char   code_min,       /* minimun of all bytes of GL/GR charset */ 
                    code_max;       /* maximun of all bytes of GL/GR charset */
} ISOStateInfo;

/*
 * Must define:
 *   Success  = 0
 *   BadName < 0
 * Some functions will use this convension, e.g., _XwcDecomposeGlyphCharset().
 */
#define BufferOverFlow  (-1)
#define BadBuffer       (-1)
#define BadTerminate    (-2)
#define BadEncoding     (-3)

#define _XctDefaultState()	(_State)(CODESET0 | CODESET1 << 8 | GL << 16)
#define _XmbDefaultState(xlocale) ((xlocale)->xlc_db->lc_codeset->mb_init)

#define _Xmbtype(xlocale)	((xlocale)->xlc_db->lc_codeset->cds_type)

#define _Xmbcsnum(xlocale)	((xlocale)->xlc_db->lc_codeset->cds_num)
#define _Xmbfsnum(xlocale)	((xlocale)->xlc_db->lc_fontset->fs_num)


#define ctGetGLid(xlocale)	(_CSID)((xlocale)->ct_state & 0xff)
#define ctGetGRid(xlocale)	(_CSID)((xlocale)->ct_state >> 8 & 0xff)
#define ctGetGLorGR(xlocale)	(_CSID)((xlocale)->ct_state >> 16 & 0xff)
#define ctGetid(xlocale)	(_CSID)((ctGetGLorGR(xlocale) == GL) ? \
				 ctGetGLid(xlocale) : ctGetGRid(xlocale))
#define ctSetGLid(xlocale, id)	((xlocale)->ct_state = (xlocale)->ct_state & 0xffff00 | (id))
#define ctSetGRid(xlocale, id)	((xlocale)->ct_state = \
				 (xlocale)->ct_state & 0xff00ff | (_State)(id) << 8)
#define ctSetGLorGR(xlocale, id) ((xlocale)->ct_state = \
				 (xlocale)->ct_state & 0x00ffff | (_State)(id) << 16)
#define ctSetid(xlocale, id)	((xlocale)->ct_state = \
				 (ctGetGLorGR(xlocale) == GL) ? \
				 ctSetGLid(xlocale, id) : ctSetGRid(xlocale, id))

#define mbGetGLid(xlocale)	(_CSID)((xlocale)->mb_state & 0xff)
#define mbGetGRid(xlocale)	(_CSID)((xlocale)->mb_state >> 8 & 0xff)
#define mbGetGLorGR(xlocale)	(_CSID)((xlocale)->mb_state >> 16 & 0xff)
#define mbGetid(xlocale)	(_CSID)((mbGetGLorGR(xlocale) == GL) ? \
				 mbGetGLid(xlocale) : mbGetGRid(xlocale))
#define mbSetGLid(xlocale, id)	((xlocale)->mb_state = (xlocale)->mb_state & 0xffff00 | (id))
#define mbSetGRid(xlocale, id)	((xlocale)->mb_state = \
				 (xlocale)->mb_state & 0xff00ff | ((_State)(id) << 8))
#define mbSetGLorGR(xlocale, id) ((xlocale)->mb_state = \
				 (xlocale)->mb_state & 0x00ffff | (_State)(id) << 16)
#define mbSetid(xlocale, id)	((xlocale)->mb_state = \
				 (mbGetGLorGR(xlocale) == GL) ? \
				 mbSetGLid(xlocale, id) : mbSetGRid(xlocale, id))

#define _Xctinit(xlocale)	((xlocale)->ct_state = _XctDefaultState())
#define _Xmbinit(xlocale)	((xlocale)->mb_state = _XmbDefaultState(xlocale))

#define _XlcFreeLocale(xlc) Xfree((char *)(xlc))

#ifdef notdef /* I am not yet convinced we need these inside Xlib */

#ifndef X_NOT_STDC_ENV
#include <limits.h>
#endif
#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#endif
#ifndef MB_CUR_MAX
#define MB_CUR_MAX (_XFallBackConvert()->xlc_db->lc_codeset->cds_mb_cur_max)
#endif

#endif

#define _isXString(c) \
   (((c) >= 0x20 && (c) <= 0x7e) || (c) >= 0xa0 || (c) == 0x09 || (c) == 0x0a)

/* XlcAccess.c */
extern int              _Xmbmsbon();
extern _CSID		_Xmbcsid();
extern int              _Xmblen();
extern char *           _Xmbdsg();
extern int              _Xmbdlen();
extern Charset *        _Xmbfscs();
extern char *           _Xmbfsname();
extern int              _Xmbfslen();
extern char *           _Xmbfsdsg();
extern wchar            _Xmbfswf();
extern int              _Xmbctocsc();
extern int              _Xcsctombc();
extern _CSID		_XmbctGLorGR();
extern _CSID		_Xmbctid();
extern _CSID		_Xmbctidtocsid();
extern void		_XmbGetDefaultEncoding();

/* XInitCT.c */
extern void             _XInitCTEncoding();

extern Bool             _XctisGLdsg();
extern Bool             _XcwNameGetAll();
extern _CSID		_XcwNameGetGLorGRId();
extern void             _XcwGetAll();
extern void		_XcwGetDefaultEncoding();
extern _CSID		_XcwGetWoffsetFromLocale();
extern Bool             _XcwCheckDefaultState();
extern Bool		_XmbCheck();
extern Bool		_XcwIdCheck();
extern Bool		_XcwIdGetAll();
extern Bool		_XcwIdGetEncoding();
extern int		_XcwGetLength();

/* XlcLoad.c */
extern XLocale		_XlcDupLocale();
extern XLocale		_XlcMakeLocale();
extern XLocale		_XFallBackConvert();

extern int		_XmbDecomposeGlyphCharset();
extern int		_XwcDecomposeGlyphCharset();

#endif /* _XLOCALEINT_H_ */
