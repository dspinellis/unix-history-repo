/*
 * $XConsortium: TextExtBd.c,v 1.14 91/10/08 14:18:01 rws Exp $
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
 *	Authors: Li Yuhong	OMRON Corporation
 *		 Tatsuya Kato	NTT Software Corporation
 *   
 */

/*************************************************************************
 *                                                                       *
 *     TextExtentsBody.c -- body of functions:                           *
 *                            XmbTextExtents()                           *
 *                            XwcTextExtents()                           *
 *                                                                       *
 *************************************************************************/
/*--------------------------- BEGIN -------------------------------------*/
#ifdef DecomposeGlyphCharset

    XLocale	    xlocale = ((XsiFontSet)font_set)->xlc;
    XFontStruct    *fnt;
    char            gstr[BUFSIZ];
    int		    ctid;
    int             glen;
    int             scanned, ret;
    int             dir, ascent, descent, max_ascent, max_descent;
    XCharStruct     overall, all;
    Bool            first;

#define MAX(a, b)           ((a > b)? a : b)
#define MIN(a, b)           ((a < b)? a : b)

#ifdef XML
    if (!xlocale)
	xlocale = _XFallBackConvert();
#endif
    max_ascent = max_descent = 0;
    overall.ascent= 0;
    overall.descent = 0;
    overall.width = 0;
    overall.lbearing = 0;
    overall.rbearing = 0;
    first = True;
    _Xmbinit(xlocale);
    _Xctinit(xlocale);
    while (text_len > 0) {
        /* buffer size */
        glen = BUFSIZ;
        scanned = 0;
        ret = DecomposeGlyphCharset(xlocale, text, text_len,
                (unsigned char *)gstr, &glen, &scanned, &ctid);
        /*
         * if ret is BadEncoding, uncovered wrong codepoint, must stop!
         */
        if (ret == BadEncoding || scanned == 0)
            break;
        /* 
         * if missing font, no drawing or measuring
         */
        if ((fnt = _XsiQueryFontSetFromId(font_set, ctid)) != NULL) {
            /*
             * only 1 or 2 byte-encoding font supported by X.
             */
            if (fnt->min_byte1 == 0 && fnt->max_byte1 == 0) {
                XTextExtents(fnt, gstr, glen,
                    &dir, &ascent, &descent, &all);
            } else {
                XTextExtents16(fnt, (XChar2b *)gstr, (int)(glen/2),
                    &dir, &ascent, &descent, &all);
            }
            if (first) {
                max_ascent = ascent;
                max_descent = descent;
                overall = all;
                first = False;
            } else {
                max_ascent = MAX(max_ascent, ascent);
                max_descent = MAX(max_descent, descent);
                overall.ascent = MAX(overall.ascent, all.ascent);
                overall.descent = MAX(overall.descent, all.descent);
                all.lbearing += overall.width;
                overall.lbearing = MIN(overall.lbearing, all.lbearing);
                all.rbearing += overall.width;
                overall.rbearing = MAX(overall.rbearing, all.rbearing);
                overall.width += all.width;
            }
        }
        if (ret == BadTerminate)
            /* The passed string "text" is terminated unexpectly, stop!*/
            break;
        text += scanned;
        text_len -= scanned;
    }
    if (overall_ink_extents) {
	overall_ink_extents->x = overall.lbearing;
	overall_ink_extents->y = - overall.ascent;
	overall_ink_extents->width = overall.rbearing - overall.lbearing;
	overall_ink_extents->height = overall.ascent + overall.descent;
    }

    if (overall_logical_extents) {
	overall_logical_extents->x = 0;
	overall_logical_extents->y = - max_ascent;
	overall_logical_extents->width = overall.width;
	overall_logical_extents->height = max_ascent + max_descent;
    }

    return overall.width;

#endif
/*--------------------------- END ---------------------------------------*/
