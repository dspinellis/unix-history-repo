#ifndef lint
static char *rcsid_text_c = "$Header: text.c,v 10.1 86/11/29 13:52:56 jg Rel $";
#endif	lint
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

/*
 *	ToDo:
 *		xymask's
 */

#include "Xapollo.h"
#ifndef stdin
#include <stdio.h>
#endif   

extern boolean borrow_flag;
extern int old_op;

PrintText(string, strlen, font, fore, back, charpad, spacepad, dstx, dsty,
	  clipstart, clipcount, func, zmask)
    unsigned char *string;
    FONT       *font;
    int         strlen, fore, back, charpad, spacepad, dstx, dsty;
    CLIP       *clipstart;
    int         clipcount, zmask;
    int         func;
{
    int         op, i;
    extern char FBMap[];
    register    w = 0;
    int         bsize = 0;
    int         lheight;
    int         sbot, sright;
    int         savestrlen = strlen;
    unsigned char *savestring = string;
    gpr_$window_t cwindow;
    boolean active;
    status_$t status;
    FontPriv	* fp;
    static int old_fore = -1,
               old_back = -1;

    fp = (FontPriv *)font->data;

/* Do apollo-format fonts */
    if (fp->ap_font) {
      int lback, lfore;

      if (!borrow_flag)
        active = gpr_$acquire_display( status );
      if ((charpad != 0) || (spacepad != 0))
        fprintf(stderr, "charpad=%d, spacepad=%d\n", charpad,spacepad);
/* if window is inverted (and just using 0 & 1 pixels, use inverted font */
      gpr_$set_text_font( (short)fp->ap_font_id, status);
      if ((back == 1) && (fore == 0))
        gpr_$set_text_font( (short)fp->ap_font_id_inv, status);
      fore = (fore == 0) ? 1 : fore;
      back = (back == 1) ? 0 : back;

      if (old_fore != fore) {
        old_fore = fore;
        gpr_$set_text_value((gpr_$pixel_value_t)fore, status);
        }
      if (old_back != back) {
        old_back = back;
        gpr_$set_text_background_value((gpr_$pixel_value_t)back, status);
        }
      set_zmask(zmask);
      set_op( func );
      CheckCursor(dstx, dsty, strlen * fp->maxwidth, font->height);
      dsty = dsty + font->height - 1;
      do {
       	GetNextClip(clipstart, cwindow);
        gpr_$set_clip_window( cwindow, status);   
        gpr_$move( (short)dstx, (short)dsty, status);
        gpr_$text( *string, (short)strlen, status);
        } while (--clipcount);
      if (!borrow_flag)
        gpr_$release_display( status);
      }        
    else {
/* wasn't in apollo format, use internal format--this should be avoided! */
    if (fore & 1)
	func += 0x20;
    if (back & 1)
	func += 0x10;
    func = FBMap[func];

    lheight = font->height;

    sbot = dsty + lheight;
    sright = dstx + w;

    dsty -= font->base;
    do {
        gpr_$window_t window;
        gpr_$position_t dest;
        int w;

        gpr_$set_raster_op((short)0, (short)func, status);
        old_op = func;
    	GetNextClip(clipstart, cwindow);
    	CheckCursor(cwindow.x_coord, cwindow.y_coord,
                cwindow.x_size, cwindow.y_size);
        gpr_$set_clip_window( cwindow, status);
        window.y_size = lheight;
        dest.y_coord = dsty;
        dest.x_coord = dstx;

        while( strlen-- )
	    {
          window.x_coord = fp->leftarray[*string];
          w = fp->widths[*string];
          window.x_size = w;
          gpr_$bit_blt(fp->strike->data, window, (short)0, dest, (short)0, status); 
          ++string;
    	  dest.x_coord += w; 
        }
        string = savestring;
        strlen = savestrlen;
        } while (--clipcount > 0);
    }
	RestoreCursor();
}

PrintTextMask(string, strlen, font, srcpix, charpad, spacepad, dstx, dsty,
	      clips, clipcount, func, zmask)
    unsigned char *string;
    FONT       *font;
    int         strlen, srcpix, charpad, spacepad, dstx, dsty;
    CLIP       *clips;
    int         clipcount, zmask;
    register int func;
{
    int         cleft, ctop, cwidth, cheight;
    int         fore, back;
    extern char SSMap[];        

/* ( assume depth == 1) */
    fore = 1;
    back = 0;
	if ((srcpix & 1) == 0) {
        fore = 0;
        back = 1;
    }
/* Punt for now on masking */
    PrintText(string, strlen, font, fore, back, charpad, spacepad, dstx, dsty,
	      clips, clipcount, func, zmask);

}
