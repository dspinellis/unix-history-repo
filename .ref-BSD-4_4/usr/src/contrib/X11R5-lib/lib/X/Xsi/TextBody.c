/*
 * $XConsortium: TextBody.c,v 1.9 91/05/02 16:33:55 rws Exp $
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
 *     TextBody.c -- body of functions:                                  *
 *                     XmbDrawString()                                   *
 *                     XmbDrawImageString()                              *
 *                     XwcDrawString()                                   *
 *                     XwcDrawImageString()                              *
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

#ifdef XML
    if (!xlocale)
	xlocale = _XFallBackConvert();
#endif
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
        if ((fnt =_XsiQueryFontSetFromId(font_set, ctid)) != NULL) {
            /*
             * only 1 or 2 byte-encoding font supported by X.
             */  
            XSetFont(dpy, gc, fnt->fid);
            if (fnt->min_byte1 == 0 && fnt->max_byte1 == 0) {
                DrawString(dpy, d, gc, x, y, gstr, glen);
                x += XTextWidth(fnt, gstr, glen);
            } else {
                DrawString16(dpy, d, gc, x, y, (XChar2b *)gstr, (int)(glen/2));
                x += XTextWidth16(fnt, (XChar2b *)gstr, (int)(glen/2));
            }
        }
        if (ret == BadTerminate)
            /* The passed string "text" is terminated unexpectly, stop!*/
            break;
        text += scanned;
        text_len -= scanned;
    }

#endif
/*--------------------------- END ---------------------------------------*/
