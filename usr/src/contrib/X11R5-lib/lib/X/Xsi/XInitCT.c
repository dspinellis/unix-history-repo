/*
 * $XConsortium: XInitCT.c,v 1.18 92/04/14 15:55:19 rws Exp $
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
 * This module provides functions operating on the CT(ISO 2022) encoding to
 * support such conversion functions.
 * All approved standard CT encodings are stored here, i.e., allocated
 * to client's memory.  I don't know whether it is suitable or not, because
 * X server does not provide any propertis for the CT encodings registered
 * by X consortium.
 *
 * by Li Yuhong, Nov. 7, 1990
 * 
*/
#include "Xlibint.h"
#include <X11/Xos.h>
#include <X11/Xresource.h>
#include "Xlocaleint.h"

/*
 * Font CharSet Name Registered by X Consortium. 
 * The order is same in page 5 of document, "Compound Text Encoding"
*/
#define NISO8859_1      "iso8859-1"
#define NISO8859_2      "iso8859-2"
#define NISO8859_3      "iso8859-3"
#define NISO8859_4      "iso8859-4"
#define NISO8859_5      "iso8859-5"
#define NISO8859_6      "iso8859-6"
#define NISO8859_7      "iso8859-7"
#define NISO8859_8      "iso8859-8"
#define NISO8859_9      "iso8859-9"
#define NJISX0201_0     "jisx0201.1976-0"
#define NGB2312_0       "gb2312.1980-0"
#define NGB2312_1       "gb2312.1980-1"
#define NJISX0208_0     "jisx0208.1983-0"
#define NJISX0208_1     "jisx0208.1983-1"
#define NKSC5601_0      "ksc5601.1987-0"
#define NKSC5601_1      "ksc5601.1987-1"

#define NISO646_0       "iso646-0"

#ifndef OLDJISX0208ONLY
#define NNJISX0208_0    "jisx0208.1990-0"
#define NNJISX0208_1    "jisx0208.1990-1"
#define NNJISX0212_0    "jisx0212.1990-0"
#define NNJISX0212_1    "jisx0212.1990-1"
#endif

/*
 * Approved Standard Encodings
*/
#define EISO8859_1L     "\033(B"        /* GL 94 character set */
#define EJISX0201_L     "\033(J"        /* GR 94 character set */
#define EJISX0201_R     "\033)I"        /* GR 94 character set */
#define EISO8859_1R     "\033-A"        /* GR 96 character set */
#define EISO8859_2R     "\033-B"
#define EISO8859_3R     "\033-C"
#define EISO8859_4R     "\033-D"
#define EISO8859_7R     "\033-F"
#define EISO8859_6R     "\033-G"
#define EISO8859_8R     "\033-H"
#define EISO8859_5R     "\033-L"
#define EISO8859_9R     "\033-M"
#define EGB2312_0       "\033$(A"       /* GL 94N character set */
#define EGB2312_1       "\033$)A"       /* GR 94N character set */
#define EJISX0208_0     "\033$(B"
#define EJISX0208_1     "\033$)B"
#define EKSC5601_0      "\033$(C"
#define EKSC5601_1      "\033$)C"
#ifndef OLDJISX0208ONLY
#define EJISX0212_0     "\033$(D"
#define EJISX0212_1     "\033$)D"
#endif

#define EISO646_0       EISO8859_1L
/*
 * Add one more for Taiwan.
 * It would be better to invoke final characters for Taiwan into 03/01
 * and 03/02 in accordance with ISO 2022 because it has not registered yet.
 * However strict CT definition contains the following statements:
 *
 *    [Final characters for private encodings (in the range 03/00 to 03/15),
 *    are not permitted here in Compound Text.]  
 *
 * Ooh,Ooh!
 * So the final characters for Taiwan is defined to 05/01 and 05/02.
 *
 * By Li Yuhong, Nov. 20, 1990
*/
#ifdef	nodef
#define NTW2312_1       "tw2312-1.1980-0"
#define NTW2312_2       "tw2312-2.1980-0"
#define ETW2312_1       "\033$(Y"       /* Taiwan Font1, not registered yet.*/
#define ETW2312_2       "\033$(Z"       /* Taiwan Font2, not registered yet.*/
#define WTW2312_1       PrivatePlaneLastI11
#define WTW2312_2       PrivatePlaneLastI10
#endif

/*
 * Offset in 4-byte wchar encoding, referred to 
	ISO/IEC DIS 10646 
	Infomation technology - Universal Coded Character Set (UCS)
*/
#define WISO8859_1      0x20202000
#define WISO8859_1R     0x20202080
/* We use Right half of JISX0201 (Katakana) in
   Last rows of Private Use Zone */
#define WJISX0201_L     PrivateZoneLast-0x100
#define WJISX0201_R     PrivateZoneLast
#define WISO8859_2R     0x20202180	/* Row33:Latin Alphabet No.2 */
#define WISO8859_3R     0x20202280      /*? not found in DIS 10646 */
#define WISO8859_4R     0x20202380      /*? not found in DIS 10646 */
#define WISO8859_9R     0x20202480      /*? not found in DIS 10646 */
#define WISO8859_5R     0x20202880	/* Row40:Cyrillic*/
#define WISO8859_7R     0x20202A80	/* Row42:Greek(little bit diffrent)*/
#define WISO8859_6R     0x20202C80	/* Row44:Araabic(little bit diffrent)*/
#define WISO8859_8R     0x20202D80	/* Row45:Hebrew(little bit diffrent)*/
#define WGB2312		HANZISCRIPT
#define WJISX0208	KANJISCRIPT
#define WKSC5601	HANJASCRIPT
#ifndef OLDJISX0208ONLY
#define WJISX0212	0x20408000
#endif

#define WISO646_0       WISO8859_1

/* 
 * table for registered states, i.e., CT encodings.
*/
#define MAXISOSTATE     127             /* for 127 countries, enough? */

typedef struct {
    char     *nstate_name;              /* standard name registered by X */
    char     *nstate_encoding;          /* control sequence of CT encoding */
    XrmQuark state_name;                /* same as nstate_name, but quark */
    XrmQuark state_encoding;            /* same as nstate_encoding, but quark*/
    wchar    state_woffset;             /* offset in 4-byte wchar encoding */
    int      state_length;              /* number of bytes of encoding */
    _CSID     GLorGR;
    _CSID     C94orC96;
} ISORegisterState;

static ISORegisterState *ISOStateTable;
static _CSID ISOStateTableNum = 0;
#ifndef lint
static int lock;
#endif

/*
 *   Function Name: _XInitCTEncoding()
 *    Arguments IN: none.
 *             OUT: none.
 *          Return: none.
 *     Description: The function builds the internal table for CT encoding
 *                  registered by X Consortium.  It will convert string
 *                  names to quark names, and hire the resource memory
 *                  to store strings.
 *                  
*/
void
_XInitCTEncoding()
{
    register ISORegisterState *ptr;

    LockMutex(&lock);

    if (ISOStateTableNum > 0) {
	UnlockMutex(&lock);
	return;    /* already initialized */
    }
    ptr = ISOStateTable = (ISORegisterState *)Xmalloc(MAXISOSTATE * 
                        sizeof(ISORegisterState));
    if (ptr == NULL) {
	UnlockMutex(&lock);
	return;
    }

#define RegisterStandard(name, encoding, woffset)			\
        ptr->state_name = XrmStringToQuark(name);                       \
        ptr->state_encoding = XrmStringToQuark(encoding);               \
        ptr->state_woffset = woffset;                                   \
        /* hire the resource memory to store the string. */             \
        ptr->nstate_name = XrmQuarkToString(ptr->state_name);           \
        ptr->nstate_encoding = XrmQuarkToString(ptr->state_encoding);   \
        /* only 2 or 1 bytes are supported in X. */                     \
        ptr->state_length = (ptr->nstate_encoding[1] == '$')? 2 : 1;    \
        ptr->GLorGR = ((_XctisGLdsg(ptr->nstate_encoding)) ? GL: GR);	\
	ptr->C94orC96 = (ptr->nstate_encoding[1] == '-') ? C96 : C94;	\
        ptr++; ISOStateTableNum++;                                       


    /* 
     * sort the table with woffset key, but not done yet.
     * So do not change the following order, it decides the previlidge 
     * as that first match is first got. 
    */
    /* Must Registerate ISO8859-1 GL first */
    RegisterStandard(NISO8859_1, EISO8859_1L, WISO8859_1);
    /* Must Registerate ISO8859-1 GR second */
    RegisterStandard(NISO8859_1, EISO8859_1R, WISO8859_1R);

    RegisterStandard(NJISX0208_0,EJISX0208_0, WJISX0208);
    RegisterStandard(NJISX0201_0,EJISX0201_R, WJISX0201_R);
    RegisterStandard(NJISX0201_0,EJISX0201_L, WJISX0201_L);

    RegisterStandard(NGB2312_0,  EGB2312_0,   WGB2312);
    RegisterStandard(NKSC5601_0, EKSC5601_0,  WKSC5601);

    RegisterStandard(NISO646_0,  EISO646_0,   WISO646_0);

    RegisterStandard(NISO8859_2, EISO8859_2R, WISO8859_2R);
    RegisterStandard(NISO8859_3, EISO8859_3R, WISO8859_3R);
    RegisterStandard(NISO8859_4, EISO8859_4R, WISO8859_4R);
    RegisterStandard(NISO8859_9, EISO8859_9R, WISO8859_9R);
    RegisterStandard(NISO8859_5, EISO8859_5R, WISO8859_5R);
    RegisterStandard(NISO8859_7, EISO8859_7R, WISO8859_7R);
    RegisterStandard(NISO8859_6, EISO8859_6R, WISO8859_6R);
    RegisterStandard(NISO8859_8, EISO8859_8R, WISO8859_8R);
    RegisterStandard(NGB2312_1,  EGB2312_1,   WGB2312);
    RegisterStandard(NJISX0208_1,EJISX0208_1, WJISX0208);
    RegisterStandard(NKSC5601_1, EKSC5601_1,  WKSC5601);
/*
    RegisterStandard(NTW2312_1,  ETW2312_1,   WTW2312_1);
    RegisterStandard(NTW2312_2,  ETW2312_2,   WTW2312_2);
*/
#ifndef OLDJISX0208ONLY
    RegisterStandard(NNJISX0208_0,EJISX0208_0, WJISX0208);
    RegisterStandard(NNJISX0208_1,EJISX0208_1, WJISX0208);
    RegisterStandard(NNJISX0212_0,EJISX0212_0, WJISX0212);
    RegisterStandard(NNJISX0212_1,EJISX0212_1, WJISX0212);
#endif
    UnlockMutex(&lock);
}

/*
 *   Function Name: _XRegisterCharSet()
 *    Arguments IN: state_name - name of CharSet.
 *                  state_encoding - CT encoding, control sequence,
 *                                   provided by caller. null-terminated.
 *                  woffset - offset in wchar encoding.
 *             OUT: none
 *          Return: True - successful.
 *                  False -- can not registered state name.
 *     Description: The function registers a CharSet name into internal
 *                  table.
 *                  The control sequence and woffset should not conflict with
 *                  already registered state. 
*/

Bool
_XRegisterCharSet(state_name, state_encoding, woffset, GLorGR, bytes)
    char *state_name;
    char *state_encoding;
    wchar woffset;
    int GLorGR;
    int bytes;
{
    register ISORegisterState *ptr;

    if (ISOStateTableNum >= MAXISOSTATE)
        /* over maxinum, unacceptable fact!!! */
        return(False);
    /*
     * Check Correctness. coding in later if this function is necessary.
     */
    ptr = ISOStateTable + ISOStateTableNum;
    ptr->state_name = XrmStringToQuark(state_name);
    ptr->state_encoding = XrmStringToQuark(state_encoding);
    ptr->state_woffset = woffset;
    ptr->nstate_name = XrmQuarkToString(ptr->state_name);
    ptr->nstate_encoding = XrmQuarkToString(ptr->state_encoding);
    ptr->state_length = bytes;
    ptr->GLorGR = GLorGR;
    ptr->C94orC96 = (ptr->nstate_encoding[1] == '-') ? C96 : C94;
    ptr++; ISOStateTableNum++;
    return True;
}

Bool
_XctisGLdsg(dsg)
    char *dsg;
{
    if (*(dsg+1) == '(' || *(dsg+2) == '(')
	return (True);
    else if (*(dsg+1) == ')' || *(dsg+2) == ')' || *(dsg+1) == '-')
	return (False);
    else if (*(dsg+1) == '$' &&
    		(*(dsg+2) == '@' || *(dsg+2) == 'A' || *(dsg+2) == 'B'))
	return (True);
    return (False);
}


/*
 * The prefix of the following function names "_Xcw" means the functions
 * operating on the CT and wchar.
*/
/*
 * _XcwNameGetALL()
 *   get all items of RegisterTable with the index state_name.
*/
_CSID
_XcwNameGetGLorGRId(state_name, GLorGR)
    char       *state_name;
    _CSID	GLorGR;
{
    register XrmQuark		 name;
    register ISORegisterState 	*ptr;
    register _CSID		 id;

    name = XrmStringToQuark(state_name);    
    for (id = 0, ptr = ISOStateTable; id < ISOStateTableNum; id++, ptr++)
        if (name == ptr->state_name) {
	    if (ptr->GLorGR != GLorGR)
		continue;
            return(id);
        }
    return(ND);
}

Bool
_XcwNameGetAll(state_name, state_encoding, woffset, GLorGR)
    char       *state_name;
    char      **state_encoding;
    wchar      *woffset;
    _CSID	GLorGR;
{
    _CSID	id;
    if((id = _XcwNameGetGLorGRId(state_name, GLorGR)) == ND)
	return(False);
    *woffset = ISOStateTable[id].state_woffset;
    *state_encoding = ISOStateTable[id].nstate_encoding;
    return(True);
}

#ifdef not_use
Bool
_XcwNameGetId(xlocale, state_name)
    XLocale	 xlocale;
    char	*state_name;
{
    _CSID	 GLorGR;
    _CSID	 id;
    GLorGR = ctGetGLorGR(xlocale);
    if((id = _XcwNameGetGLorGRId(state_name, GLorGR)) == ND)
	return(False);
    ctSetid(xlocale, id);
    return(True);
}

int
_XcwNameGetLength(state_name, GLorGR)
    char       *state_name;
    _CSID	GLorGR;
{
    _CSID	id;
    if((id = _XcwNameGetGLorGRId(state_name, GLorGR)) == ND)
	return (0);
    return ISOStateTable[id].state_length;
        
}

Bool
_XcwNameGetEncoding(state_name, state_encoding, GLorGR)
    char       *state_name;
    char      **state_encoding;
    _CSID	GLorGR;
{
    _CSID	id;
    if((id = _XcwNameGetGLorGRId(state_name, GLorGR)) == ND)
	return(False);
    *state_encoding = ISOStateTable[id].nstate_encoding;
    return(True);
}

Bool
_XcwNameGetWoffset(state_name, woffset, GLorGR)
    char       *state_name;
    wchar      *woffset;
    _CSID	GLorGR;
{
    _CSID	id;
    if((id = _XcwNameGetGLorGRId(state_name, GLorGR)) == ND)
	return(False);
    *woffset = ISOStateTable[id].state_woffset;
    return(True);
}
#endif

/*
 * _XcwEscGetAll()
 *   get all items of ISOStateTable with the index state_encoding.
*/
static _CSID
_XcwEscGetId(state_encoding)
    char	*state_encoding;
{
    register XrmQuark           encoding;
    register ISORegisterState   *ptr;
    register _CSID		i;
    char			esc[128];

    if(state_encoding[1] == 0x25) {
	strcpy(esc, state_encoding);
	esc[4] = '0'; esc[5] = '0';	/* mask length */
	state_encoding = esc;
    }
    encoding = XrmStringToQuark(state_encoding);
    for (i = 0, ptr = ISOStateTable; i < ISOStateTableNum; i++, ptr++)
        if (encoding == ptr->state_encoding) {
            return(i);
        }
    return(ND);
}

static _CSID
_XcwEscGetIdFromLocale(xlocale, state_encoding)
    XLocale	 xlocale;
    char	*state_encoding;
{
    Fontset			*fontset;
    _CSID                       id;
    char			esc[128];

    if(!xlocale && !xlocale->xlc_db && !xlocale->xlc_db->lc_fontset) {
	return (_XcwEscGetId(state_encoding));
    }
    fontset = xlocale->xlc_db->lc_fontset;

    if(state_encoding[1] == 0x25) {
	strcpy(esc, state_encoding);
	esc[4] = '0'; esc[5] = '0';	/* mask length */
	state_encoding = esc;
    }
    for (id = 0; id < fontset->fs_num; id++) {
        if (!strcmp(state_encoding, fontset->fs_cset[id]->cs_des))
	    return(fontset->fs_cset[id]->cs_id);
        }
    return (_XcwEscGetId(state_encoding));
}

Bool
_XcwEscSetStatus(xlocale, state_encoding)
    XLocale	 xlocale;
    char	*state_encoding;
{
    _CSID	id;
    if((id = _XcwEscGetIdFromLocale(xlocale, state_encoding)) == ND)
	return(False);
    ctSetGLorGR(xlocale, ISOStateTable[id].GLorGR);
    ctSetid(xlocale, id);
    return(True);
}	

#ifdef not_use
Bool
_XcwEscGetAll(xlocale, state_encoding, woffset)
    XLocale	 xlocale;
    char	*state_encoding;
    wchar	*woffset;
{
    _CSID	id;
    if((id = _XcwEscGetIdFromLocale(xlocale, state_encoding)) == ND)
	return(False);
    ctSetGLorGR(xlocale, ISOStateTable[id].GLorGR);
    ctSetid(xlocale, id);
    *woffset = ISOStateTable[id].state_woffset;
    return(True);
}

int
_XcwEscGetLength(state_encoding)
    char	*state_encoding;
{
    _CSID	id;
    if((id = _XcwEscGetId(state_encoding)) == ND)
	return 0;

    return ISOStateTable[id].state_length;
}

Bool
_XcwEscGetName(state_encoding, state_name)
    char	*state_encoding;
    char	**state_name;
{
    _CSID	id;
    if((id = _XcwEscGetId(state_encoding)) == ND)
	return(False);
    *state_name = ISOStateTable[id].nstate_name;
    return(True);
}	

Bool
_XcwEscGetWoffset(state_encoding, woffset)
    char	*state_encoding;
    wchar	*woffset;
{
    _CSID	id;
    if((id = _XcwEscGetId(state_encoding)) == ND)
	return(False);
    *woffset = ISOStateTable[id].state_woffset;
    return(True);
}

/*
 * _XcwEscGetEncoding()
 *   get same encoding sequence as caller passed, but the returned encoding
 *   sequence stored in the internal table.
*/
Bool
_XcwEscGetEncoding(passed_encoding, state_encoding)
    char	*passed_encoding;
    char       **state_encoding;
{
    _CSID	id;
    if((id = _XcwEscGetId(passed_encoding)) == ND)
	return(False);

    *state_encoding = ISOStateTable[id].nstate_encoding;
    return(False);
}
#endif

/*
 * _XcwGetAll()
 *  calculate the minimun of difference between wc and woffset, then
 *  return all items of ISOStateTable.
 *
*/

_CSID
_XcwGetWoffset(wc, woffset)
    wchar		wc;
    wchar	       *woffset;
{
    register ISORegisterState 	*ptr = ISOStateTable;
    register wchar 		disp_min, new;
    _CSID			ind, i;
    _CSID 			n = ISOStateTableNum;

#define PosSub(s1, s2) (((s1) >= (s2)) ? ((s1) - (s2)) : ~0L)

    *woffset = ptr->state_woffset;
    wc &= ~0x7f;
    ind = CODESET0;
    disp_min = PosSub(wc, ptr->state_woffset);
    if (disp_min == (wchar) ~0L) {
	*woffset = 0;
	return(ND);
    }
    if (disp_min < 0x80)
	return(CODESET0);
    for (i = 1, ptr++; i < n; i++, ptr++) {
	if (ptr->state_length == 2)
	    new = PosSub(wc & ~0x7f00, ptr->state_woffset);
	else
	    new = PosSub(wc, ptr->state_woffset);
        if ((new >= 0) && (new < disp_min)) {
            *woffset = ptr->state_woffset;
	    if (new < 0x80)
		return(i);
            ind = i;
            disp_min = new;
        }
    }
    return(ind);
}

_CSID
_XcwGetWoffsetFromLocale(xlocale, wc, woffset)
    XLocale	        xlocale;
    wchar		wc;
    wchar	       *woffset;
{
    register wchar              disp_min;
    _CSID                       id;
    Fontset			*fontset;

    if(!xlocale && !xlocale->xlc_db && !xlocale->xlc_db->lc_fontset) {
	*woffset = 0;
	return (_XcwGetWoffset(wc, woffset));
    }

    fontset = xlocale->xlc_db->lc_fontset;
    wc &= ~0x7f;
    for (id = 0; id < fontset->fs_num; id++) {
	if (fontset->fs_cset[id]->cs_len == 2)
	    disp_min = PosSub(wc & ~0x7f00, fontset->fs_cset[id]->cs_woff);
	else
	    disp_min = PosSub(wc, fontset->fs_cset[id]->cs_woff);
	if (disp_min < 0x80) {
	    *woffset = fontset->fs_cset[id]->cs_woff;
	    return(fontset->fs_cset[id]->cs_id);
	}
    }
    return (_XcwGetWoffset(wc, woffset));
#undef PosSub
}

static void
SetISOinfo(isoinfo, id)
    ISOStateInfo    *isoinfo;
    _CSID	     id;
{
    ISORegisterState *ptr;

    ptr = ISOStateTable + id;
    isoinfo->code_bytes = ptr->state_length;

    if (ptr->C94orC96 == C96) {
	isoinfo->code_min = 0xa0;
	isoinfo->code_max = 0xff;
    } else if (ptr->GLorGR == GL) {
	if (id == CODESET0 || !strcmp(ptr->nstate_encoding, "\033(J"))
	/* ISO8859.1 GL  or JISX0201 GL: for SPACE */
	    isoinfo->code_min = 0x20;
	else
	    isoinfo->code_min = 0x21;
	isoinfo->code_max = 0x7e;
    } else {
	isoinfo->code_min = 0xa1;
	isoinfo->code_max = 0xfe;
    }
}

void
_XcwGetAll(xlocale, wc, state_encoding, woffset, isoinfo)
    XLocale	        xlocale;
    wchar		wc;
    char	      **state_encoding;
    wchar	       *woffset;
    ISOStateInfo       *isoinfo;
{
    _CSID		id;

    if ((id = _XcwGetWoffsetFromLocale(xlocale, wc, woffset)) == ND)
	return;

    ctSetGLorGR(xlocale, ISOStateTable[id].GLorGR);
    ctSetid(xlocale, id);
    *state_encoding = ISOStateTable[id].nstate_encoding;

    SetISOinfo(isoinfo, id);
}

int
_XcwGetLength(wc)
    wchar	wc;
{
    wchar	woffset;
    _CSID	id;

    id = _XcwGetWoffset(wc, &woffset);
    if(id == ND)
	return(-1);

    return (ISOStateTable[id].state_length);
}

#ifdef not_use
void
_XcwGetName(xlocale, wc)
    XLocale    xlocale;
    wchar	wc;
{
    wchar	woffset;
    _CSID	id;

    if ((id = _XcwGetWoffsetFromLocale(xlocale, wc, woffset)) == ND)
	return;
    
    if (ISOStateTable[id].GLorGR == GL)
	ctSetGLid(xlocale, id);
    else
	ctSetGRid(xlocale, id);
}

void
_XcwGetEncoding(wc, state_encoding)
    wchar	wc;
    char      **state_encoding;
{
    wchar	woffset;
    _CSID	id;

    id = _XcwGetWoffset(wc, &woffset);
    if(id == ND)
	return;

    *state_encoding = ISOStateTable[id].nstate_encoding;
}
#endif


/* 
 * _XcwCheckDefaultState():
 *   check whether the isostate is default encoding, both GL and GR of
 *   Latin-1.
 *   It will be called by conversion function CT->WC.
*/
Bool
_XcwCheckDefaultState(xlocale)
    XLocale  xlocale;
{
    if ((xlocale->ct_state & 0xffff) == ((CODESET1 << 8) | CODESET0))
        return(True);
    else
        return(False);
}

/*
 *   get items of RegisterTable with the index ct_state.
*/
Bool
_XcwIdCheck(xlocale)
    XLocale  xlocale;
{
    if ((ctGetGLid(xlocale) >= ISOStateTableNum) ||
	(ctGetGRid(xlocale) >= ISOStateTableNum))
        return(False);
    return(True);
}

Bool
_XcwIdGetAll(xlocale, woffset, isoinfo)
    XLocale		xlocale;
    wchar	       *woffset;
    ISOStateInfo       *isoinfo;
{
    _CSID		id;

    id = ctGetid(xlocale);

    if (id >= ISOStateTableNum)
	return(False);
    *woffset = ISOStateTable[id].state_woffset;

    SetISOinfo(isoinfo, id);
    return(True);
}

int
_XcwIdGetLength(xlocale)
    XLocale	xlocale;
{
    _CSID	id;

    id = ctGetid(xlocale);

    if (id >= ISOStateTableNum)
	return(False);
    return ISOStateTable[id].state_length;
}

Bool
_XcwIdGetEncoding(xlocale, state_encoding)
    XLocale	xlocale;
    char      **state_encoding;
{
    _CSID	id;

    id = ctGetid(xlocale);

    if (id >= ISOStateTableNum)
	return(False);
    *state_encoding = ISOStateTable[id].nstate_encoding;
    return (True);
}

void
_XcwGetDefaultEncoding(xlocale, state_encoding)
    XLocale	 xlocale;
    char	*state_encoding;
{
    _CSID	 id;

    *state_encoding = 0;
    id = ctGetGLid(xlocale);
    if (id < ISOStateTableNum && id != 0)	/* GL is not ISO8859.1 */
	strcpy(state_encoding, ISOStateTable[0].nstate_encoding);
    id = ctGetGRid(xlocale);
    if (id < ISOStateTableNum && id != 1)	/* GR is not ISO8859.1 */
	strcat(state_encoding, ISOStateTable[1].nstate_encoding);
}

Bool
_XcwIdGetWoffset(xlocale, woffset)
    XLocale	 xlocale;
    wchar	*woffset;
{
    _CSID	 id;

    id = ctGetid(xlocale);

    if (id >= ISOStateTableNum)
	return(False);
    *woffset = ISOStateTable[id].state_woffset;
    return (True);
}

#ifdef not_use
char *
_XcwIdGetName(xlocale)
    XLocale	xlocale;
{
    _CSID	id;

    id = ctGetid(xlocale);

    if (id >= ISOStateTableNum)
	return(NULL);
    return (ISOStateTable[id].nstate_name);
}
#endif

void
_XcwIdGetISOState(xlocale, isoinfo)
    XLocale		 xlocale;
    ISOStateInfo	*isoinfo;
{
    SetISOinfo(isoinfo, ctGetid(xlocale));
}
