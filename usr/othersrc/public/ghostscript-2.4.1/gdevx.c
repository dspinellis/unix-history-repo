/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevx.c */
/* X Windows driver for Ghostscript library */
/* The X include files include <sys/types.h>, which, on some machines */
/* at least, define uint, ushort, and ulong, which std.h also defines. */
/* std.h has taken care of this. */
#include "gx.h"			/* for gx_bitmap; includes std.h */
#include "memory_.h"
#include "x_.h"
#include "gxdevice.h"
#include "gdevx.h"

/* Flags for patching around bugs in the X library */
private int use_XPutImage = 1;
private int use_XSetTile = 1;

/* Define the maximum size of the temporary pixmap for copy_mono */
/* that we are willing to leave lying around in the server */
/* between uses.  (Assume 32-bit ints here!) */
private int max_temp_pixmap = 20000;

/* Forward references */
private int set_tile(P2(gx_device *, gx_bitmap *));
private void free_cp(P1(gx_device *));
/* Screen updating machinery */
#define update_init(dev)\
  ((gx_device_X *)(dev))->up_area = 0,\
  ((gx_device_X *)(dev))->up_count = 0
#define update_flush(dev)\
  if ( ((gx_device_X *)(dev))->up_area != 0 ) update_do_flush(dev)
private void update_do_flush(P1(gx_device *));
private void update_add(P5(gx_device *, int, int, int, int));
private void send_event(P2(gx_device *, Atom));

/* Procedures */

extern int gdev_x_open(P1(gx_device_X *));
private dev_proc_open_device(x_open);
private dev_proc_get_initial_matrix(x_get_initial_matrix);
private dev_proc_sync_output(x_sync);
private dev_proc_output_page(x_output_page);
private dev_proc_close_device(x_close);
private dev_proc_map_rgb_color(x_map_rgb_color);
private dev_proc_map_color_rgb(x_map_color_rgb);
private dev_proc_fill_rectangle(x_fill_rectangle);
private dev_proc_tile_rectangle(x_tile_rectangle);
private dev_proc_copy_mono(x_copy_mono);
private dev_proc_copy_color(x_copy_color);
private dev_proc_draw_line(x_draw_line);

/* The device descriptor */
private gx_device_procs x_procs = {
	x_open,
	x_get_initial_matrix,
	x_sync,
	x_output_page,
	x_close,
	x_map_rgb_color,
	x_map_color_rgb,
	x_fill_rectangle,
	x_tile_rectangle,
	x_copy_mono,
	x_copy_color,
	x_draw_line,
	gx_default_get_bits,
	gx_default_get_props,
	gx_default_put_props
};

/* The instance is public. */
gx_device_X gs_x11_device = {
	sizeof(gx_device_X),
	&x_procs,
	"x11",
	(int)(FAKE_RES*DEFAULT_WIDTH_INCHES), (int)(FAKE_RES*DEFAULT_HEIGHT_INCHES),	/* x and y extent (nominal) */
	FAKE_RES, FAKE_RES,	/* x and y density (nominal) */
	no_margins,
	dci_black_and_white,
	0,			/* connection not initialized */
	{ /* image */
	  0, 0,			/* width, height */
	  0, XYBitmap, NULL,	/* xoffset, format, data */
	  LSBFirst, 8,    	/* byte-order, bitmap-unit */
	  MSBFirst, 8, 1,	/* bitmap-bit-order, bitmap-pad, depth */
	  0, 1,			/* bytes_per_line, bits_per_pixel */
	  0, 0, 0,		/* red_mask, green_mask, blue_mask */
	  NULL,			/* *obdata */
	   { NULL,			/* *(*create_image)() */
	     NULL,			/* (*destroy_image)() */
	     NULL,			/* (*get_pixel)() */
	     NULL,			/* (*put_pixel)() */
	     NULL,			/* *(*sub_image)() */
	     NULL			/* (*add_pixel)() */
	   },
	},
	NULL, NULL,		/* dpy, scr */
				/* (connection not initialized) */
	NULL,			/* vinfo */
	(Colormap)None,		/* cmap */
	(Window)None,		/* win */
	NULL,			/* gc */
	(Pixmap)0,		/* bpixmap */
	0,			/* ghostview */
	(Window)None,		/* mwin */
#if HaveStdCMap
	NULL,			/* std_cmap */
#endif
	identity_matrix_body,	/* initial matrix (filled in) */
	(Atom)0, (Atom)0, (Atom)0,	/* Atoms: NEXT, PAGE, DONE */
	 { 0, 0, 0, 0 }, 0, 0,	/* update, up_area, up_count */
	(Pixmap)0,		/* dest */
	0L, ~0L,		/* colors_or, colors_and */
	 { /* cp */
	   (Pixmap)0,		/* pixmap */
	   NULL,		/* gc */
	   -1, -1		/* raster, height */
	 },
	 { /* ht */
	   (Pixmap)None,		/* pixmap */
	   (Pixmap)None,		/* no_pixmap */
	   gx_no_bitmap_id,		/* id */
	   0, 0, 0,			/* width, height, raster */
	   0, 0				/* fore_c, back_c */
	 },
	GXcopy,			/* function */
	FillSolid,		/* fill_style */
	0,			/* pixel_fix */
	{ 0, 0, 0, 0, 0, 0, 0, 0 }, /* colors[8] */
	0, 0			/* back_color, fore_color */

};

/* Macro for casting gx_device argument */
#define xdev ((gx_device_X *)dev)

/* Macros to validate and coerce arguments */
#define check_rect_extent()\
	if ( x + w > xdev->width ) w = xdev->width - x;\
	if ( y + h > xdev->height ) h = xdev->height - y;\
	if ( w <= 0 || h <= 0 ) return 0
#define check_rect()\
	if ( x < 0 ) w += x, x = 0;\
	if ( y < 0 ) h += y, y = 0;\
	check_rect_extent()

/* If XPutImage doesn't work, do it ourselves. */
private void alt_put_image();
#define put_image(dpy,win,gc,im,sx,sy,x,y,w,h)\
  if ( use_XPutImage) XPutImage(dpy,win,gc,im,sx,sy,x,y,w,h);\
  else alt_put_image(dev,dpy,win,gc,im,sx,sy,x,y,w,h)


/* Open the device.  Most of the code is in gdevxini.c. */
private int
x_open(gx_device *dev)
{	int code = gdev_x_open(xdev);
	if ( code < 0 ) return code;
	update_init(dev);
	return 0;
}

/* Close the device.  NOT SURE WHAT TO DO HERE YET. */
private int
x_close(gx_device *dev)
{	if ( xdev->ghostview )
	  { send_event(dev, xdev->done);
	  }
	XCloseDisplay(xdev->dpy);
	return 0;
}

/* Map a color.  The "device colors" are just r,g,b packed together. */
private gx_color_index
x_map_rgb_color(register gx_device *dev,
		gx_color_value r, gx_color_value g, gx_color_value b)
{
#if HaveStdCMap
	if ( xdev->std_cmap )
	  { XStandardColormap *cmap = xdev->std_cmap;
	    x_pixel color;
	    if ( r == 0 && g == 0 && b == 0 )
	      return pixel_black;
	    if ( r == gx_max_color_value && g == gx_max_color_value &&
		 b == gx_max_color_value )
	      return pixel_white;
#define cv_denom (gx_max_color_value + 1)
	    color =
	      (gx_device_has_color(xdev) ?
	       (r * (cmap->red_max + 1) / cv_denom * cmap->red_mult) +
	       (g * (cmap->green_max + 1) / cv_denom * cmap->green_mult) +
	       (b * (cmap->blue_max + 1) / cv_denom * cmap->blue_mult) :
	       (r * (xdev->color_info.max_gray + 1) / cv_denom *
		cmap->red_mult)) +
	      cmap->base_pixel;
#undef cv_denom
	    return pixel_to_color_index(color);
	  }
	else
#endif
#define cv_half (gx_max_color_value / 2)
	  { return
	      pixel_to_color_index(xdev->colors[(r > cv_half ? 4 : 0) +
						(g > cv_half ? 2 : 0) +
						(b > cv_half ? 1 : 0)]);
	  }
#undef cv_half
}


/* Map a "device color" back to r-g-b. */
private int
x_map_color_rgb(register gx_device *dev, gx_color_index color,
		gx_color_value prgb[3])
{	x_pixel pixel = color_index_to_pixel(color);
	if ( pixel == pixel_black )
	 { prgb[0] = prgb[1] = prgb[2] = 0;
	 }
	else if ( pixel == pixel_white )
	 { prgb[0] = prgb[1] = prgb[2] = gx_max_color_value;
	 }
#if HaveStdCMap
	else if ( xdev->std_cmap )
	  { XStandardColormap *cmap = xdev->std_cmap;
	    if ( gx_device_has_color(xdev) )
	      { prgb[0] =
		  ((pixel - cmap->base_pixel) / cmap->red_mult) %
		    (cmap->red_max + 1) * gx_max_color_value /
		      cmap->red_max;
	        prgb[1] =
		  ((pixel - cmap->base_pixel) / cmap->green_mult) %
		    (cmap->green_max + 1) * gx_max_color_value /
		      cmap->green_max;
	        prgb[2] =
		  ((pixel - cmap->base_pixel) / cmap->blue_mult) %
		    (cmap->blue_max + 1) * gx_max_color_value /
		      cmap->blue_max;
	      }
	    else
	      { prgb[0] = prgb[1] = prgb[2] =
		  (pixel - cmap->base_pixel) / cmap->red_mult *
		    gx_max_color_value / xdev->color_info.max_gray;
	      }
	  }
#endif
	else
	 { int i;
	   for ( i = 1; i < 7; i++ )
	    { if ( pixel == xdev->colors[i] )
	       { prgb[0] = (i & 4 ? gx_max_color_value : 0);
		 prgb[1] = (i & 2 ? gx_max_color_value : 0);
		 prgb[2] = (i & 1 ? gx_max_color_value : 0);
		 break;
	       }
	    }
	 }
	return 0;
}

/* Get initial matrix for X device */
private void
x_get_initial_matrix(register gx_device *dev, register gs_matrix *pmat)
{	pmat->xx = xdev->initial_matrix.xx;
	pmat->xy = xdev->initial_matrix.xy;
	pmat->yx = xdev->initial_matrix.yx;
	pmat->yy = xdev->initial_matrix.yy;
	pmat->tx = xdev->initial_matrix.tx;
	pmat->ty = xdev->initial_matrix.ty;
}

/* Synchronize the display with the commands already given */
private int
x_sync(register gx_device *dev)
{	update_flush(dev);
	XSync(xdev->dpy, 0);
	return 0;
}

/* Send event to ghostview process */
private void
send_event(gx_device *dev, Atom msg)
{	XEvent event;
	event.xclient.type = ClientMessage;
	event.xclient.display = xdev->dpy;
	event.xclient.window = xdev->win;
	event.xclient.message_type = msg;
	event.xclient.format = 32;
	event.xclient.data.l[0] = xdev->mwin;
	event.xclient.data.l[1] = xdev->dest;
	XSendEvent(xdev->dpy, xdev->win, False, 0, &event);
}

/* Output "page" */
private int
x_output_page(gx_device *dev, int num_copies, int flush)
{	x_sync(dev);

	/* Send ghostview a "page" client event */
	/* Wait for a "next" client event */
	if ( xdev->ghostview )
	  { XEvent event;
	    send_event(dev, xdev->page);
	    XNextEvent(xdev->dpy, &event);
	    while (event.type != ClientMessage ||
		   event.xclient.message_type != xdev->next)
	      { XNextEvent(xdev->dpy, &event);
	      }
	  }
	return 0;
}

/* Fill a rectangle with a color. */
private int
x_fill_rectangle(register gx_device *dev,
  int x, int y, int w, int h, gx_color_index color)
{	check_rect();
	set_fill_style(FillSolid);
	set_fore_color(color_index_to_pixel(color));
	set_function(GXcopy);
	XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);
	/* If we are filling the entire screen, reset */
	/* colors_or and colors_and.  It's wasteful to do this */
	/* on every operation, but there's no separate driver routine */
	/* for erasepage (yet). */
	if ( x == 0 && y == 0 && w == xdev->width && h == xdev->height )
	 { xdev->colors_or = xdev->colors_and = color_index_to_pixel(color);
	 }
	if ( xdev->bpixmap != (Pixmap)0 )
	 { update_add(dev, x, y, w, h);
	 }
#ifdef DEBUG
if ( gs_debug['F'] )
	dprintf5("[F] fill (%d,%d):(%d,%d) %ld\n",
	         x, y, w, h, (long)color);
#endif
	return 0;
}

/* Tile a rectangle. */
private int
x_tile_rectangle(register gx_device *dev, gx_bitmap *tile,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one,
  int px, int py)
{	x_pixel
	  p_zero = color_index_to_pixel(zero),
	  p_one = color_index_to_pixel(one);
	check_rect();

	/* Check for a colored tile.  We should implement this */
	/* properly someday, since X can handle it. */

	if ( one == gx_no_color_index && zero == gx_no_color_index )
		return -1;

	/* For the moment, give up if the phase is non-zero. */
	if ( px | py )
		return -1;

	/* 
	 * Remember, an X tile is already filled with particular
	 * pixel values (i.e., colors).  Therefore if we are changing
	 * fore/background color, we must invalidate the tile (using
	 * the same technique as in set_tile).  This problem only
	 * bites when using grayscale -- you may want to change
	 * fg/bg but use the same halftone screen.
	 */
	if ( (p_zero != xdev->ht.back_c) || (p_one != xdev->ht.fore_c) )
	  xdev->ht.id = ~tile->id; /* force reload */

	set_back_color(p_zero);
	set_fore_color(p_one);
	if ( !set_tile(dev, tile) )
	 { /* Bad news.  Fall back to the default algorithm. */
	   return gx_default_tile_rectangle(dev, tile, x, y, w, h, zero, one, px, py);
	 }
	else
	  { /* Use the tile to fill the rectangle */
	    set_fill_style(FillTiled);
	    set_function(GXcopy);
	    XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);
	    if ( xdev->bpixmap != (Pixmap)0 )
	     { update_add(dev, x, y, w, h);
	     }
	  }
#ifdef DEBUG
if ( gs_debug['F'] )
	dprintf6("[F] tile (%d,%d):(%d,%d) %ld,%ld\n",
	         x, y, w, h, (long)zero, (long)one);
#endif
	return 0;
}

/* Set up with a specified tile. */
/* Return false if we can't do it for some reason. */
private int
set_tile(register gx_device *dev, register gx_bitmap *tile)
{
#ifdef DEBUG
if ( gs_debug['T'] )
	return 0;
#endif
	if ( tile->id == xdev->ht.id && tile->id != gx_no_bitmap_id )
	  return use_XSetTile;
	/* Set up the tile Pixmap */
	if ( tile->size.x != xdev->ht.width ||
	     tile->size.y != xdev->ht.height ||
	     xdev->ht.pixmap == (Pixmap)0
	   )
	  { if ( xdev->ht.pixmap != (Pixmap)0 )
	      XFreePixmap(xdev->dpy, xdev->ht.pixmap);
	    xdev->ht.pixmap = XCreatePixmap(xdev->dpy, xdev->win,
					    tile->size.x, tile->size.y,
					    xdev->vinfo->depth);
	    if ( xdev->ht.pixmap == (Pixmap)0 )
	      return 0;
	    xdev->ht.width = tile->size.x, xdev->ht.height = tile->size.y;
	    xdev->ht.raster = tile->raster;
	  }
	xdev->ht.fore_c = xdev->fore_color;
	xdev->ht.back_c = xdev->back_color;
	/* Copy the tile into the Pixmap */
	xdev->image.data = (char *)tile->data;
	xdev->image.width = tile->size.x;
	xdev->image.height = tile->size.y;
	xdev->image.bytes_per_line = tile->raster;
	xdev->image.format = XYBitmap;
	set_fill_style(FillSolid);
#ifdef DEBUG
if ( gs_debug['H'] )
	 { int i;
	   dprintf4("[H] 0x%x: width=%d height=%d raster=%d\n",
		    tile->data, tile->size.x, tile->size.y, tile->raster);
	   for ( i = 0; i < tile->raster * tile->size.y; i++ )
	     dprintf1(" %02x", tile->data[i]);
	   dputc('\n');
	 }
#endif
	XSetTile(xdev->dpy, xdev->gc, xdev->ht.no_pixmap); /* *** X bug *** */
	set_function(GXcopy);
	put_image(xdev->dpy, xdev->ht.pixmap, xdev->gc, &xdev->image,
		  0, 0, 0, 0, tile->size.x, tile->size.y);
	XSetTile(xdev->dpy, xdev->gc, xdev->ht.pixmap);
	xdev->ht.id = tile->id;
	return use_XSetTile;
}

/* Copy a monochrome bitmap. */
private int
x_copy_mono(register gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
/*
 * X doesn't directly support the simple operation of writing a color
 * through a mask specified by an image.  The plot is the following: 
 *  If neither color is gx_no_color_index ("transparent"),
 *	use XPutImage with the "copy" function as usual.
 *  If the color either bitwise-includes or is bitwise-included-in
 *      every color written to date
 *      (a special optimization for writing black/white on color displays),
 *	use XPutImage with an appropriate Boolean function.
 *  Otherwise, do the following complicated stuff:
 *	Create pixmap of depth 1 if necessary.
 *	If foreground color is "transparent" then
 *	  invert the raster data.
 *	Use XPutImage to copy the raster image to the newly
 *	  created Pixmap.
 *	Install the Pixmap as the clip_mask in the X GC and
 *	  tweak the clip origin.
 *	Do an XFillRectangle, fill style=solid, specifying a
 *	  rectangle the same size as the original raster data.
 *	De-install the clip_mask.
 */
{	int function = GXcopy;
	x_pixel
	  p_zero = color_index_to_pixel(zero),
	  p_one = color_index_to_pixel(one);
	x_pixel
	  bc = p_zero,
	  fc = p_one;
	
	/* We need a different version of check_rect, because */
	/* we have to adjust the source coordinates too. */
	if ( x < 0 ) w += x, sourcex -= x, x = 0;
	if ( y < 0 ) h += y, base -= y * raster, y = 0;
	check_rect_extent();

	xdev->image.width = raster << 3;
	xdev->image.height = h;
	xdev->image.data = (char *)base;
	xdev->image.bytes_per_line = raster;
	set_fill_style(FillSolid);

	/* Check for null, easy 1-color, hard 1-color, and 2-color cases. */
	if ( zero != gx_no_color_index )
	  { if ( one != gx_no_color_index )
	      { /* 2-color case. */
		/* Simply replace existing bits with what's in the image. */
	      }
	    else if ( !(~xdev->colors_and & bc) )
	      function = GXand,
	      fc = ~(x_pixel)0;
	    else if ( !(~bc & xdev->colors_or) )
	      function = GXor,
	      fc = 0;
	    else
	      goto hard;
	  }
	else
	  { if ( one == gx_no_color_index ) /* no-op */
	      return 0;
	    else if ( !(~xdev->colors_and & fc) )
	      function = GXand,
	      bc = ~(x_pixel)0;
	    else if ( !(~fc & xdev->colors_or) )
	      function = GXor,
	      bc = 0;
	    else
	      goto hard;
	  }
	xdev->image.format = XYBitmap;
	set_function(function);
	if ( bc != xdev->back_color )
	  XSetBackground(xdev->dpy, xdev->gc, (xdev->back_color = bc));
	if ( fc != xdev->fore_color )
	  XSetForeground(xdev->dpy, xdev->gc, (xdev->fore_color = fc));
	if ( zero != gx_no_color_index )
	  note_color(p_zero);
	if ( one != gx_no_color_index )
	  note_color(p_one);
	put_image(xdev->dpy, xdev->dest, xdev->gc, &xdev->image,
		  sourcex, 0, x, y, w, h);

	goto out;

hard:	/* Handle the hard 1-color case. */
	if ( raster > xdev->cp.raster || h > xdev->cp.height )
	  { /* Must allocate a new pixmap and GC. */
	    /* Release the old ones first. */
	    free_cp(dev);

	    /* Create the clipping pixmap, depth must be 1. */
	    xdev->cp.pixmap =
	      XCreatePixmap(xdev->dpy, xdev->win, raster << 3, h, 1);
	    if ( xdev->cp.pixmap == (Pixmap)0 )
	      {	lprintf("x_copy_mono: can't allocate pixmap\n");
		exit(1);
	      }
	    xdev->cp.gc = XCreateGC(xdev->dpy, xdev->cp.pixmap, 0, 0);
	    if ( xdev->cp.gc == (GC)0 )
	      {	lprintf("x_copy_mono: can't allocate GC\n");
		exit(1);
	      }
	    xdev->cp.raster = raster;
	    xdev->cp.height = h;
	  }

	/* Initialize static mask image params */
	xdev->image.format = ZPixmap;

	/* Select polarity based on fg/bg transparency. */
	if ( one == gx_no_color_index )	/* invert */
	  { XSetBackground(xdev->dpy, xdev->cp.gc, (x_pixel)1);
	    XSetForeground(xdev->dpy, xdev->cp.gc, (x_pixel)0);
	    set_fore_color(p_zero);
	  }
	else
	  { XSetBackground(xdev->dpy, xdev->cp.gc, (x_pixel)0);
	    XSetForeground(xdev->dpy, xdev->cp.gc, (x_pixel)1);
	    set_fore_color(p_one);
	  }
	put_image(xdev->dpy, xdev->cp.pixmap, xdev->cp.gc,
		  &xdev->image, sourcex, 0, 0, 0, w, h);

	/* Install as clipmask. */
	XSetClipMask(xdev->dpy, xdev->gc, xdev->cp.pixmap);
	XSetClipOrigin(xdev->dpy, xdev->gc, x, y);

	/*
	 * Draw a solid rectangle through the raster clip mask.
	 * Note fill style is guaranteed to be solid from above.
	 */
	XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);

	/* Tidy up.  Free the pixmap if it's big. */
	XSetClipMask(xdev->dpy, xdev->gc, None);
	if ( raster * h > max_temp_pixmap )
	  free_cp(dev);

out:	if ( xdev->bpixmap != (Pixmap)0 )
	  { /* We wrote to the pixmap, so update the display now. */
	    update_add(dev, x, y, w, h);
	  }

	return 0;
}

/* Internal routine to free the GC and pixmap used for copying. */
private void
free_cp(register gx_device *dev)
{	if ( xdev->cp.gc != NULL )
	   {	XFreeGC(xdev->dpy, xdev->cp.gc);
		xdev->cp.gc = NULL;
	   }
	if ( xdev->cp.pixmap != (Pixmap)0 )
	   {	XFreePixmap(xdev->dpy, xdev->cp.pixmap);
		xdev->cp.pixmap = (Pixmap)0;
	   }
	xdev->cp.raster = -1;	/* mark as unallocated */
}

/* Copy a "color" bitmap.  Since "color" is the same as monochrome, */
/* this just reduces to copying a monochrome bitmap. */
/****** THIS ROUTINE IS COMPLETELY WRONG, SINCE WE DO SUPPORT COLOR. ******/
/* Fortunately, no one uses it at the moment. */
private int
x_copy_color(register gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	return x_copy_mono(dev, base, sourcex, raster, id, x, y, w, h,
			   pixel_to_color_index(pixel_black),
			   pixel_to_color_index(pixel_white));
}

/* Draw a line */
private int
x_draw_line(register gx_device *dev,
  int x0, int y0, int x1, int y1, gx_color_index color)
{	set_fore_color(color_index_to_pixel(color));
	set_fill_style(FillSolid);
	set_function(GXcopy);
	XDrawLine(xdev->dpy, xdev->dest, xdev->gc, x0, y0, x1, y1);
	if ( xdev->bpixmap != (Pixmap)0 )
	 { int x = x0, y = y0, w = x1 - x0, h = y1 - y0;
	   if ( w < 0 ) x = x1, w = - w;
	   if ( h < 0 ) y = y1, h = - h;
	   w++; h++;
	   check_rect();
	   update_add(dev, x, y, w, h);
	 }
	return 0;
}

/* ------ Screen update procedures ------ */

/* Flush updates to the screen if needed. */
private void
update_do_flush(register gx_device *dev)
{	int xo = xdev->update.xo, yo = xdev->update.yo;
	set_function(GXcopy);
	XCopyArea(xdev->dpy, xdev->bpixmap, xdev->win, xdev->gc,
		  xo, yo, xdev->update.xe - xo, xdev->update.ye - yo,
		  xo, yo);
	update_init(dev);
}

/* Add a region to be updated. */
/* This is only called if xdev->bpixmap != 0. */
private void
update_add(register gx_device *dev, int xo, int yo, int w, int h)
{	int xe = xo + w, ye = yo + h;
	long new_area = (long)w * h;
	++xdev->up_count;
	if ( xdev->up_area != 0 )
	  { /* See whether adding this rectangle */
	    /* would result in too much being copied unnecessarily. */
	    long old_area = xdev->up_area;
	    long new_up_area;
	    rect u;
	    u.xo = min(xo, xdev->update.xo);
	    u.yo = min(yo, xdev->update.yo);
	    u.xe = max(xe, xdev->update.xe);
	    u.ye = max(ye, xdev->update.ye);
	    new_up_area = (long)(u.xe - u.xo) * (u.ye - u.yo);
	    if ( new_up_area > 100 &&
		old_area + new_area < new_up_area * 2 / 3 ||
		xdev->up_count >= 200
		)
	      update_do_flush(dev);
	    else
	      { xdev->update = u;
		xdev->up_area = new_up_area;
		return;
	      }
	  }
	xdev->update.xo = xo;
	xdev->update.yo = yo;
	xdev->update.xe = xe;
	xdev->update.ye = ye;
	xdev->up_area = new_area;
}

/* ------ Internal procedures ------ */

/* Substitute for XPutImage using XFillRectangle. */
/* This is a total hack to get around an apparent bug */
/* in some X servers.  It only works with the specific */
/* parameters (bit/byte order, padding) used above. */
private void
alt_put_image(gx_device *dev, Display *dpy, Drawable win, GC gc,
  XImage *pi, int sx, int sy, int dx, int dy, unsigned w, unsigned h)
{	int raster = pi->bytes_per_line;
	byte *data = (byte *)pi->data + sy * raster + (sx >> 3);
	int init_mask = 0x80 >> (sx & 7);
	int invert;
	int yi;
#define nrects 40
	XRectangle rects[nrects];
	XRectangle *rp = rects;
	if ( xdev->fore_color != gx_no_color_index )
	  { if ( xdev->back_color != gx_no_color_index )
	      { XSetForeground(dpy, gc, xdev->back_color);
		XFillRectangle(dpy, win, gc, dx, dy, w, h);
	      }
	    XSetForeground(dpy, gc, xdev->fore_color);
	    invert = 0;
	  }
	else if ( xdev->back_color != gx_no_color_index )
	  { XSetForeground(dpy, gc, xdev->back_color);
	    invert = 0xff;
	  }
	else
	  return;
	for ( yi = 0; yi < h; yi++, data += raster )
	  { register int mask = init_mask;
	    register byte *dp = data;
	    register int xi = 0;
	    while ( xi < w )
	      { if ( (*dp ^ invert) & mask )
		  { int xleft = xi;
		    if ( rp == &rects[nrects] )
		      { XFillRectangles(dpy, win, gc, rects, nrects);
			rp = rects;
		      }
		    /* Scan over a run of 1-bits */
		    rp->x = dx + xi, rp->y = dy + yi;
		    do
		      { if ( !(mask >>= 1) ) mask = 0x80, dp++;
			xi++;
		      }
		    while ( xi < w && (*dp & mask) );
		    rp->width = xi - xleft, rp->height = 1;
		    rp++;
		  }
		else
		  { if ( !(mask >>= 1) ) mask = 0x80, dp++;
		    xi++;
		  }
	      }
	  }
	XFillRectangles(dpy, win, gc, rects, rp - rects);
}
