#ifndef lint
static char *rcsid_put_c = "$Header: put.c,v 10.1 86/11/29 13:52:41 jg Rel $";
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

/* put.c	Perform a raster operation with a source bitmap
 *
 *	PixmapPut	Puts a pixmap up on the screen
 *	PixmapBitsPut	Puts a pixmap up on the screen
 *	BitmapBitsPut	Puts a pixmap up on the screen
 *
 */

/*
 *	ToDo:
 *		masks & XY format Pixmaps
 */

#include "Xapollo.h"

#define GPIXARY_SIZE 2048
status_$t status;
long gpixary[GPIXARY_SIZE];

char *Xalloc();

PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
	   op, zmask)
     register PIXMAP *src;
     int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
     register int op;
     CLIP *clips;
{
  gpr_$position_t dest;
  gpr_$window_t sou, cwindow;
  int  cleft, ctop, cwidth, cheight;
  int i;

  set_zmask(zmask);
  set_op( op );
  switch (PTYPE(src)) {
  case BitmapPixmap:
    {
      BITMAP     *bm = (BITMAP *) src->data;
      do {

    	GetNextClip(clips, cwindow);
	if (OverLap(cwindow, dstx, dsty, width, height))
	  { 
	    CheckCursor(dstx, dsty, width, height);
	    gpr_$set_clip_window( cwindow, status);
	    sou.x_coord = srcx;
	    sou.y_coord = srcy;
	    sou.x_size = width;
	    sou.y_size = height;
	    dest.x_coord = dstx;
	    dest.y_coord = dsty;
	    gpr_$pixel_blt((gpr_$bitmap_desc_t)bm->data, sou, dest, status);
	  } 
      } while (--clipcount > 0);
    }
    break;
  case ConstantPixmap:
    do {
      
      GetNextClip(clips, cwindow);
      if (OverLap(cwindow, dstx, dsty, width, height))
	{
	  CheckCursor(dstx, dsty, width, height);
	  /* Not yet implemented */
	}
    } while (--clipcount > 0);
    break;
  case ZColorPixmap:
    gpr_$set_plane_mask((gpr_$mask_t)(zmask & Screen.plane_mask), status);
    do {

      GetNextClip(clips, cwindow);      
      /* Not yet implemented */

    } while (--clipcount > 0);
    break;
  case XYColorPixmap:
    /* Not yet implemented */
    break;
  }
  RestoreCursor();
}


/*ARGSUSED*/
PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	unsigned char *data;
	int width, height, format, dstx, dsty, clipcount, zmask;
	BITMAP *xymask;
	CLIP *clips;
	int func;
{
  if (Screen.depth == 1)
      BitsPut(width, height, data, 1, 0, xymask, dstx, dsty,
	      clips, clipcount, func, zmask, 1);
  else if (Screen.depth <= 8)
      switch (format) {
      case ZFormat: {
	  gpr_$window_t dstwin;
	  long *pixary, *p, *mask_ary;
	  int i, j, size;
      
	  dstwin.x_coord = dstx;
	  dstwin.x_size = width;
	  dstwin.y_coord = dsty;
	  dstwin.y_size = height;
	  if ((size = width*height) > GPIXARY_SIZE)
	      pixary = (long *)malloc(size * 4);
	  else
	      pixary = gpixary;
	  if (xymask) {
	      mask_ary = malloc(size*4);
	      gpr_$set_bitmap( xymask->data, status);
	      gpr_$read_pixels( *mask_ary, dstwin, status);
	      gpr_$set_bitmap(Screen.bm, status);
	      gpr_$read_pixels( *pixary, dstwin, status);
	    }
	  p = pixary;
	  for (i=0;i<width;i++) 
	      for (j=0;j<height;j++)
		if (xymask && !(*mask_ary++))
		  continue;
		*p++ = *data++;
	  gpr_$write_pixels(*pixary, dstwin, status);
	  check_status(status, "PixmapBitsput");
	  if (size > GPIXARY_SIZE)
	    free(pixary);
	  break;
	}
		    break;
      case XYFormat:
	  /* Not yet supported */
	  break;
      }

}

BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, fore, back, dstx, dsty, clipcount, zmask;
	register BITMAP *xymask;
	CLIP *clips;
	register int func;
{
	InvertPixelOrder((short *) data, BitmapSize(width, height) >> 1);
	BitsPut(width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask, 1);
}

static
BitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask, srcdepth)
     char *data;
     int width, height, fore, back, dstx, dsty, clipcount, zmask;
     register BITMAP *xymask;
     CLIP *clips;
     register int func;
     int srcdepth;
{
    extern char FBMap[];
    extern int old_op;
    gpr_$window_t srcwin, cwindow;
    gpr_$position_t dstorg;
    gpr_$plane_t dplane = 0;
    BITMAP *bm;
    int op;

    if ((Screen.depth == 1) && !(zmask & 1))
    	return;          
    if (fore & 1)
	func += 0x20;
    if (back & 1)
	func += 0x10;

    srcwin.x_coord = 0;
    srcwin.y_coord = 0;
    srcwin.x_size = width;
    srcwin.y_size = height;
    dstorg.x_coord = dstx;
    dstorg.y_coord = dsty;
    bm = make_bitmap( data, width, height, NULL);
    func = FBMap[func];
    CheckCursor(dstx, dsty, dstx+width, dsty+height);
    if (xymask == NULL) {
      gpr_$set_raster_op((gpr_$plane_t)0, (gpr_$raster_op_t)func, status);
      do {
	GetNextClip(clips, cwindow);
	gpr_$set_clip_window( cwindow, status);   
	gpr_$bit_blt(bm->data, srcwin, (gpr_$plane_t)0,
		     dstorg, (gpr_$plane_t)dplane, status);
	check_status(status, "BitsPut: ");
      } while (--clipcount > 0);
      old_op = func;
    }
    else {  
/* this is slightly bogus and doesn't work correctly, but without it icons are
 * invisible
 */
	int botop = (back == 0 ? 7 : 4);
	int topop = (fore == 0 ? 7 : 4);

        do {
	  GetNextClip(clips, cwindow);
	  CheckCursor(cwindow.x_coord, cwindow.y_coord,
		      cwindow.x_size, cwindow.y_size);
	  gpr_$set_clip_window( cwindow, status);   
	  gpr_$set_raster_op((gpr_$plane_t)0, botop, status);
	  gpr_$bit_blt(xymask->data, srcwin, (gpr_$plane_t)0, dstorg,
		       dplane, status);
	  gpr_$set_raster_op((gpr_$plane_t)0, (short)3, status);
	  gpr_$bit_blt(bm->data, srcwin, (gpr_$plane_t)0, dstorg,
		       (gpr_$plane_t)dplane, status);
	} while (--clipcount);
	old_op = topop;
      }
    FreeBitmap(bm);
    RestoreCursor();
    return;
}

