#ifndef lint
static char *rcsid_tile_c = "$Header: tile.c,v 10.1 86/11/29 13:53:11 jg Rel $";
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

/* tile.c	Perform a raster operation involving a pattern
 *
 *	TileFill	Patterns a portion of the screen
 *	DrawFilled	Draw a filled generalized line/polygon/combination
 *
 */

/*
 *	ToDo:
 *		Implement tile fill with xymap
 */

#include "Xapollo.h"

extern boolean borrow_flag;
extern gpr_$bitmap_desc_t TileBM;
extern int old_op;

status_$t status;

static
tilefill(tile, xymask, dstx, dsty, width, height, op, clips, clipcount, xoff, yoff)
    PIXMAP *tile;
    BITMAP *xymask;
    int dstx, dsty, width, height;
    unsigned op;
    CLIP *clips;
    int	clipcount;
    int xoff, yoff;
{
    gpr_$position_t dstorg;
    gpr_$window_t cwindow;
    gpr_$window_t srcwin;
    int twidth = tile->width,
        theight = tile->height;  
    int zmask, i, j;

    if (xymask == NULL) {
        cache_tile(tile, &theight, &twidth);
        zmask = (1 << Screen.depth) - 1;
	set_zmask(zmask);
	set_op( 3 );
        gpr_$set_fill_value((gpr_$pixel_value_t)1, status);
        gpr_$set_fill_background_value((gpr_$pixel_value_t)0, status);
        gpr_$set_fill_pattern( TileBM, (short)1, status);
        srcwin.x_coord = dstx;
        srcwin.x_size = width;
        srcwin.y_coord = dsty;
        srcwin.y_size = height;
        do {

          GetNextClip(clips, cwindow);
/*        CheckCursor(cwindow.x_coord, cwindow.y_coord,
                      cwindow.x_size, cwindow.y_size);
 */
	  CheckCursor(dstx, dsty, width, height);
          gpr_$set_clip_window( cwindow, status);
        /* optimize only absolute case */
	  if ((xoff == 0) && (yoff == 0))
	      gpr_$rectangle( srcwin, status);
	  else {
	      gpr_$position_t dstorg;

	      srcwin.x_coord = 0;
	      srcwin.y_coord = 0;
	      srcwin.x_size = twidth;
	      srcwin.y_size = theight;
	      dstorg.x_coord = dstx;
	      for (i=0; i< width; i += twidth) {
		  dstorg.y_coord = dsty;
		  if ((dstorg.x_coord <=
		      (cwindow.x_coord + cwindow.x_size)) &&
		      ((dstorg.x_coord + twidth) >=
		       cwindow.x_coord))
		    for (j=0; j<height; j += theight) {
		      if ((dstorg.y_coord <=
			   (cwindow.y_coord + cwindow.y_size)) &&
			  ((dstorg.y_coord + theight) >= 
			   cwindow.y_coord))
			gpr_$pixel_blt(TileBM, srcwin, dstorg, status);
		      dstorg.y_coord += theight;
		    }
		  dstorg.x_coord += twidth;
		}
	    }
	} while (--clipcount > 0);
	gpr_$set_fill_pattern( gpr_$nil_bitmap_desc, (short)1, status);
	return;
      }
    else {
	/* xymask not implemented yet */
      }
    RestoreCursor();                    
    if (!borrow_flag) gpr_$release_display(status);
}

static
constfill(tile, xymask, dstx, dsty, width, height, op, clips, clipcount)
    PIXMAP *tile;
    BITMAP *xymask;
    int dstx, dsty, width, height;
    unsigned op;
    CLIP *clips;
    int	clipcount;                                                    
{
    gpr_$window_t cwindow,
                  dstwin;
    int i;
    int data = (int)tile->data;

    /*  set drawing color based on invert flag ^ tiling value */
    data ^= PINVERT(tile);
    gpr_$set_plane_mask((gpr_$mask_t) Screen.plane_mask, status);
    gpr_$set_fill_value((gpr_$pixel_value_t)data, status);
    set_op( op );
    dstwin.x_coord = dstx;
    dstwin.x_size = width;
    dstwin.y_coord = dsty;
    dstwin.y_size = height;
    if (xymask == NULL) {
	do {
        GetNextClip(clips, cwindow);
        CheckCursor(dstx, dsty, width, height);
/*	CheckCursor(cwindow.x_coord, cwindow.y_coord,
                    cwindow.x_size, cwindow.y_size);
 */
        gpr_$set_clip_window( cwindow, status);
        gpr_$rectangle( dstwin, status );
    	} while (--clipcount > 0);
    }
    else {
        /* xymask not implemented yet */
    }
    RestoreCursor();
}


/*ARGSUSED*/
TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
	  clips, clipcount, func, zmask)
	PIXMAP *tile;
	BITMAP *xymask;
	int xoff, yoff, dstx, dsty, width, height, zmask;
	register int func;
	CLIP *clips;
{

    if ((Screen.depth == 1) && !(zmask & 1))
    	return;

    switch (PTYPE(tile)) {
    case BitmapPixmap:
      tilefill(tile, xymask, dstx, dsty, width, height, func, clips, clipcount, xoff, yoff);
      break;
    case ConstantPixmap:
      constfill(tile, xymask, dstx, dsty, width, height, func, clips, clipcount);
      break;
    case XYColorPixmap:
      /* Not yet implemented */
      break;
    case ZColorPixmap:
      /* Not yet implemented */
      fprintf(stderr, "TileFill: ZColorPixmap\n");
      break;
    }
}

/*ARGSUSED*/
DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;
	register PIXMAP *tile;
	int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
	register int func;
	CLIP *clips;
{
    gpr_$window_t cwindow;
    status_$t status;
    short *x, *y, *oldx, *oldy, *savx, *savy;
/*    int data = (int)tile->data; */
	Vertex *v = verts;
    int vc = vertcount;

    x = (short *)malloc(vertcount * 2);
    y = (short *)malloc(vertcount * 2);
    savx = x;
    savy = y;
   	do {
        if (v->flags & VertexRelative) {
	      *x = v->x + *oldx;
   	      *y = v->y + *oldy;
          }
   	    else {
          *x = v->x + xbase;
          *y = v->y + ybase;
          }
    /* XXX - ignore VertexCurved for now */
    /* XXX - ignore VertexDrawLastPoint for now */
        oldx = x;
        oldy = y;
        x++;
        y++;
        v++;
   	} while (--vc > 0);

/*    switch (PTYPE(tile)) {
    case BitmapPixmap:
        fprintf(stderr,"DrawFilled: BitmapPixmap\n");
        return;
    case XYColorPixmap:
        fprintf(stderr,"DrawFilled: XYColorPixmap\n");
        return;
    case ZColorPixmap:
        fprintf(stderr,"DrawFilled: ZColorPixmap\n");
        return;
    case ConstantPixmap:
        fprintf(stderr,"DrawFilled: ConstantPixmap\n");
    	break;            
    }
    /*  set drawing color based on invert flag ^ tiling value */
    /*data ^= PINVERT(tile); */
    gpr_$set_fill_value((gpr_$pixel_value_t)srcpix, status);
    set_zmask( zmask );
    do {
        x = savx; y = savy;
        GetNextClip(clips, cwindow);
        CheckCursor(cwindow.x_coord, cwindow.y_coord,
                    cwindow.x_size, cwindow.y_size);
        gpr_$set_clip_window( cwindow, status);   
        gpr_$start_pgon((short)*x, (short)*y, status);
        check_status( status, "DrawFilled");
        gpr_$pgon_polyline( *x, *y, (short)vertcount, status);
        check_status( status, "DrawFilled");
        gpr_$close_fill_pgon( status );
        check_status( status, "DrawFilled");
        } while (--clipcount > 0);
    RestoreCursor();   
    free(x);
    free(y);
}

cache_tile(tile, twidth, theight)
    PIXMAP *tile;
    int *twidth, *theight;
{
    gpr_$bitmap_desc_t bm;
    long ptr;
    int line_width;
    status_$t status;
    boolean flag;
    gpr_$window_t srcwin;
    gpr_$position_t dstorg;

    flag = gpr_$acquire_display(status);
    gpr_$set_bitmap(TileBM, status);
    gpr_$set_raster_op( (short)0, (short)3, status);
    old_op = -1;
    srcwin.x_coord = 0;
    srcwin.x_size = *twidth;
    srcwin.y_coord = 0;
    srcwin.y_size = *theight;
    dstorg.y_coord = 0;
    dstorg.x_coord = 0;
    gpr_$pixel_blt(tile->data->data, srcwin, dstorg, status);
    dstorg.x_coord = *twidth;
    gpr_$pixel_blt(tile->data->data, srcwin, dstorg, status);

    *twidth = *twidth * 2;
    srcwin.x_coord = 0;
    srcwin.x_size = *twidth;
    srcwin.y_coord = 0;
    srcwin.y_size = *theight;
    dstorg.y_coord = *theight;
    dstorg.x_coord = 0;
    gpr_$pixel_blt(TileBM, srcwin, dstorg, status);
    gpr_$set_bitmap(Screen.bm, status);
    *theight = *theight * 2;
}
