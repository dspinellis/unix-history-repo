#ifndef lint
static char *rcsid_prim_c = "$Header: prim.c,v 10.1 86/11/29 13:52:30 jg Rel $";
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

#include "Xapollo.h"
#include "/sys/ins/smdu.ins.c"

extern boolean borrow_flag;
extern gpr_$plane_t plane;
extern int old_op;
status_$t status;

BITMAP *
make_bitmap( area, width, height, invert )
     short * area;	/* the bits of the image or NULL */
     int width;
     int height;
     boolean invert;	/* true->the image bits need to inverted */
{
  
    BITMAP * bm; 
    gpr_$bitmap_desc_t bitmap;
    static gpr_$attribute_desc_t attr;
    gpr_$offset_t size;

    short * pointer;	/* pointer to Apollo bitmap */
    short bwid;		/* Apollo bitmap line width */
    int rowwid;		/* image bits row width */
    int i;

    if( (bm = (BITMAP *)Xalloc(sizeof(BITMAP))) == NULL )
        return( NULL );
    
    bm->refcnt = 1;
    bm->kind = (int)apollo_bitmap;

    size.x_size = bm->width = width;
    size.y_size = bm->height = height;
    
    gpr_$allocate_attribute_block( attr, status );
    if (check_status(status, "make_bitmap:"))
        return( NULL );             
    gpr_$allocate_bitmap_nc( size, (gpr_$plane_t)(Screen.depth-1), attr, bitmap, status );
    if (check_status(status, "make_bitmap:"))
        return( NULL );             

    bm->data = (caddr_t)bitmap;
    if (!(area == NULL)) {
        gpr_$inq_bitmap_pointer( bitmap, pointer, bwid,  status );
        rowwid = ((width+15)>>4);
        for( i=0; i<height; i++ ) {
            bcopy( area, pointer, rowwid*2 );
            if (invert)
        	    InvertPixelOrder((short *) pointer, rowwid);
            pointer += bwid;
            area += rowwid;
            }
        }
    return( bm );
    }

FONT *
make_fontmap( fd )
FONT *fd;
    {

    int width;
    int height;
    BITMAP * bm; 
    gpr_$bitmap_desc_t bitmap;
    static gpr_$attribute_desc_t attr = -1;
    gpr_$window_t window;
    gpr_$position_t dest;
    static int got_attr = 0;
    gpr_$offset_t size;
    FontPriv *fpriv;

    short col, row;
    short left, oldleft, wid, oldwid;
    int start, stop;

    int i;

    fpriv = (FontPriv *)fd->data;
    bm = fpriv->strike;
    width = fd->last * fd->avg_width;
    height = (width / 224) + 1;
    height = height * fd->height;
/* should check for height > 224 */
    size.x_size = 224;
    size.y_size = height;
    
    if( !got_attr )                
        {
        gpr_$allocate_attribute_block( attr, status );
	if (check_status( status, "make_fontmap:"))
            return( NULL );             
        got_attr = 1;
        }
    gpr_$allocate_bitmap( size, (gpr_$plane_t)0, attr, bitmap, status );
    if (check_status( status, "make_fontmap:"))
        return( NULL );             

    row = start = 0;
    gpr_$set_bitmap( bitmap, status);
    for (i=0; i<=fd->last; i++) {
        left = fpriv->leftarray[i];
	wid = fpriv->widths[i];
	if ((left + wid) > (start + 224)) {
	    stop = oldleft + oldwid;
	    window.x_coord = start;
	    window.y_coord = 0;
	    window.x_size = stop - start;
	    window.y_size = fd->height;
	    dest.x_coord = 0;
	    dest.y_coord = row * fd->height;
	    gpr_$bit_blt(bm->data, window, (short)0, dest, (short)0, status);
	    start = stop;
	    row++;
	    col = 0;
	  }
	oldleft = left;
	oldwid = wid;
	fpriv->leftarray[i] = col + (row << 10);
	col += fpriv->widths[i];
      }
    if (start != (oldleft + oldwid)) {
        window.x_coord = start;
	window.x_size = (left + wid) - start;
	window.y_size = fd->height;
	dest.y_coord = row * fd->height;
	gpr_$bit_blt(bm->data, window, (short)0, dest, (short)0, status);
      }
    bm->width = 224;
    bm->height = height;
    bm->refcnt = 1;
    bm->kind = (int)apollo_bitmap;

    bm->data = (caddr_t)bitmap;
    gpr_$set_bitmap( Screen.bm, status);
    return( fd );

    }

