/*
 * $XConsortium: TextPerBd.c,v 1.16 92/07/29 11:39:57 rws Exp $
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
 *   
 */

/*************************************************************************
 *                                                                       *
 *     TextPerExtBody.c -- body of functions:                            *
 *                           XmbTextPerCharExtents()                     *
 *                           XwcTextPerCharExtents()                     *
 *                                                                       *
 *************************************************************************/
/*--------------------------- BEGIN -------------------------------------*/
#ifdef DecomposeGlyphCharset

    XLocale	    xlocale = ((XsiFontSet)font_set)->xlc;
    XFontStruct    *fnt;
    unsigned char  gstr[BUFSIZ];
    int		    ctid;
    int             glen;
    int             scanned, ret;
    int             count, i;
    XCharStruct     overall;
    int             max_ascent, max_descent;
    XCharStruct     *onechar;

#define MAX(a, b)           ((a > b)? a : b)
#define MIN(a, b)           ((a < b)? a : b)

#ifdef XML
    if (!xlocale)
	xlocale = _XFallBackConvert();
#endif
    overall.ascent = 0;
    overall.descent = 0;
    overall.width = 0;
    overall.lbearing = 0;
    overall.rbearing = 0;
    max_ascent = 0;
    max_descent = 0;
    count = 0;
    _Xmbinit(xlocale);
    _Xctinit(xlocale);
    while (text_len > 0 && count < buffer_size) {
        /* buffer size */
        glen = BUFSIZ;
        scanned = 0;
        ret = DecomposeGlyphCharset(xlocale, text, text_len,
				    gstr, &glen, &scanned, &ctid);
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
            if (!count) {
                max_ascent = fnt->ascent;
                max_descent = fnt->descent;
            } else {
                max_ascent = MAX(max_ascent, fnt->ascent);
                max_descent = MAX(max_descent, fnt->descent);
            }
            if (fnt->min_byte1 != 0 || fnt->max_byte1 != 0)
                glen &= ~1;
            for (i = 0; i < glen && count < buffer_size; i++, count++) {
                int ind, d, byte1, byte2;
                /*
                 * get index of glyph of font.
                 */
                if (fnt->min_byte1 == 0 && fnt->max_byte1 == 0) {
                    byte2 = gstr[i];
                    if (byte2 > fnt->max_char_or_byte2)
                        ind = 0;
                    else 
                        ind = byte2 - fnt->min_char_or_byte2;
                } else {
                    d = fnt->max_char_or_byte2 - fnt->min_char_or_byte2 + 1;
                    byte1 = gstr[i++];
                    byte2 = gstr[i];
                    if (byte1 > fnt->max_byte1 ||
                        byte2 > fnt->max_char_or_byte2)
                        ind = 0;
                    else
                        ind  = (byte1 - fnt->min_byte1) * d
			     + (byte2 - fnt->min_char_or_byte2);
                }
                if (fnt->per_char != NULL)
                    onechar = fnt->per_char + ind;
                else
                    onechar = &(fnt->max_bounds);
                /*
                 * set ink & logical extent.
                 */
                ink_extents_buffer[count].x = overall.width + onechar->lbearing;
                ink_extents_buffer[count].y = - onechar->ascent;
                ink_extents_buffer[count].width = 
                                onechar->rbearing - onechar->lbearing;
                ink_extents_buffer[count].height = 
                                onechar->ascent + onechar->descent;

                logical_extents_buffer[count].x = overall.width;
                logical_extents_buffer[count].y = - fnt->ascent;
                logical_extents_buffer[count].width = onechar->width;
                logical_extents_buffer[count].height =
                                fnt->ascent + fnt->descent;
                /*
                 * get min and max of ink/logical extent.
                 */
                if (!count) {
                    overall = *onechar;
                } else {
                    overall.ascent = MAX(overall.ascent, onechar->ascent);
                    overall.descent = MAX(overall.descent, onechar->descent);
                    overall.lbearing = MIN(overall.lbearing,
                                           overall.width + onechar->lbearing);
                    overall.rbearing = MAX(overall.rbearing,
                                           overall.width + onechar->rbearing);
                    overall.width += onechar->width;
                }
            }
        }
        if (ret == BadTerminate)
            /* The passed string "text" is terminated unexpectly, stop!*/
            break;
        text += scanned;
        text_len -= scanned;
    }
    if (max_ink_extents) {
	max_ink_extents->x = overall.lbearing;
	max_ink_extents->y = - overall.ascent;
	max_ink_extents->width = overall.rbearing - overall.lbearing;
	max_ink_extents->height = overall.ascent + overall.descent;
    }

    if (max_logical_extents) {
	max_logical_extents->x = 0;
	max_logical_extents->y = - max_ascent;
	max_logical_extents->width = overall.width;
	max_logical_extents->height = max_ascent + max_descent;
    }

    *num_chars = count;
 
    return (count <= buffer_size);

#endif
/*--------------------------- END ---------------------------------------*/
