/*
 * $XConsortium: XParseCT.c,v 1.8 91/06/26 10:55:02 rws Exp $
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
 *	Author: Li Yuhong	OMRON Corporation
 *   
 */

#include <X11/Xlib.h>           /* for data type Status */		
#include "Xlocaleint.h"

#define ESC         '\033'      /* escape */
#define CSI         '\233'      /* control sequence introducer */
#define GMCHAR      0x24        /* character stands for 94N character set */
#define GPMASK      0xFA        /* mask for char. only 02/08,02/09,02/13 */
#define GPFLAG      0x28
#define INTCH1      0x21        /* code of 1st intermediate character:  */
#define INTCH2      0x23        /* from 02/01 to 02/03                  */ 
#define INMASK      0xF0        /* mask for char. from 02/00 to 02/15 */
#define INFLAG      0x20
#define FNCHAR      0x40        /* minimun code of final character */
#define FNDEL       0x7F        /* DEL character */
#define GL94MIN     0x21        /* minimun code of GL 94 character set */
#define GL94MAX     0x7E        /* maximun code of GL 94 character set */

#define Skip(ct_bytes, cnt) {                                         \
        if (ct_bytes-- <= 0) {                                        \
            *scanned_bytes = cnt;                                     \
            return(BadTerminate);                                     \
        } else                                                        \
            cnt++;                                                    \
    }

/*      Function Name: _XParseISOEncoding
 *      Description: parse escape control sequence of compound text.
 *      Arguments In: ct_str -  passed CT string. 
 *                     ct_bytes -  length of CT string, counted in bytes.
 *                Out: scanned_bytes - number of bytes of control sequence.
 *                     isoinfo - character information: bytes of character
 *                                set, range of code.
 *      Returns: Success - OK result.
 *               BadTerminate - ct_str was terminated uncompletely.
 *               BadEncoding - wrong code in ct_str.
 *
*/

Status
_XParseISOEncoding(ct_str, ct_bytes, scanned_bytes, isoinfo)
    register unsigned char *ct_str;
    int ct_bytes;
    int *scanned_bytes;
    ISOStateInfo *isoinfo;
{
    register unsigned char code;
    int multi, cnt = 0;

	/*
	 * must put action of skip before action of getting character to 
	 * detect if more characters are expected.
	*/
    Skip(ct_bytes, cnt);
    if (*ct_str++ != ESC)		/* not supported CSI yet. */
        return(BadEncoding);
    /*
     * pick code 02/24: 94N(multiple) character set
    */
    if (multi = (*ct_str == GMCHAR)) {
        Skip(ct_bytes, cnt);
        ct_str++;
    }
    Skip(ct_bytes, cnt);
    /*
     * pick code:
     *   02/08 -- GL 94 character set
     *   02/09 -- GR 94 character set
     *   02/13 -- GR 96 character set
    */
    if (((code = *ct_str++) & GPMASK) != GPFLAG)
    {
	if (code == 0x25) /* non-standard char set, use extended segment */
	{
	    Skip(ct_bytes, cnt);
	    if (*ct_str++ != 0x2f)
		return(BadEncoding);
	    Skip(ct_bytes, cnt);
	    if ((*ct_str & 0xf0) != 0x30)
		return(BadEncoding);
	    ct_str++;
	    do {
		Skip(ct_bytes, cnt);
	    } while (*ct_str++ != 0x02);
	    *scanned_bytes = cnt;
	    return(Success);
	}
        return(BadEncoding);
    }
    if ((code = (code & ~GPMASK)) == 0x04)       /* exclude 02/12 */
        return(BadEncoding);
    /*
     * now the value of "code" stands for only:
     *   0x00 for 02/08 GL 94
     *   0x01 for 02/09 GR 94
     *   0x05 for 02/13 GR 96
     * get min and max code of character set.
    */
    isoinfo->code_min = (GL94MIN - ((int)(code&0x04) >> 2)) | ((code&0x01) << 7);
    isoinfo->code_max = (GL94MAX + ((int)(code&0x04) >> 2)) | ((code&0x01) << 7);

    /*
     * parse intermediate characters.
     * first intermediate character: from 02/01 to 02/03
    */
    Skip(ct_bytes, cnt);
    code = *ct_str++;
    if (code >= INTCH1 && code <= INTCH2) {
        /*
         * others of intermediate characters: from 02/00 to 02/15
        */
        do {
            Skip(ct_bytes, cnt);
            code = *ct_str++;
        } while ((code & INMASK) == INFLAG);
    }
    /*
     * get final character: from 04/00 to 07/14
    */
    if (code < FNCHAR || code >= FNDEL)
        return(BadEncoding);
    if (multi) {
        /*
         * code column of final character means:
         *   04 or 05 -- 2-byte character set
         *   06       -- 3-byte character set
         *   07       -- 4-byte character set
        */
        code = ((int)(code&0x30) >> 4);
        isoinfo->code_bytes = (code == 0)?2: (code + 1);
    } else {
        isoinfo->code_bytes = 1;
	if (isoinfo->code_min == 0x21 || (code == 'B' || code == 'J'))
	/* SPACE includes in ISO8859.1 GL or JISX0201 GL */
	    isoinfo->code_min--;
    }
    *scanned_bytes = cnt;
    return(Success);
}
