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

/* gdevx.h */
/* Header file with X device structure */
/* Requires gxdevice.h and x_.h */

/* Define the type of an X pixel. */
typedef unsigned long x_pixel;

/*
 * We have two alternatives in mapping between X pixel values and
 * Ghostscript gx_color_index values.  If we make them the same,
 * Ghostscript will get confused if there is an X pixel value
 * corresponding to gx_no_color_index.  If we make them different,
 * we have to keep some kind of map, with all the associated bookkeeping.
 *
 * We opt for the first solution.  When we open the device,
 * we check whether black or white maps to gx_no_color_index.
 * (Since gx_no_color_index is all 1's, we assume no other color
 * could have this pixel value.)  If this is the case, we xor all
 * pixel values with a value chosen so that no pixel value will map
 * to gx_no_color_index.
 */
#define pixel_to_color_index(px) ((px) ^ xdev->pixel_fix)
#define color_index_to_pixel(ci) ((ci) ^ xdev->pixel_fix)

/* Define a rectangle structure for update bookkeeping */
typedef struct rect_s {
  int xo, yo, xe, ye;
} rect;

/* Define the X Windows device */
typedef struct gx_device_X_s {
	gx_device_common;

	/* An XImage object for writing bitmap images to the screen */
	XImage image;

	/* Global X state */
	Display *dpy;
	Screen *scr;
	XVisualInfo *vinfo;
	Colormap cmap;
	Window win;
	GC gc;

	/* A backing pixmap so X will handle exposure automatically */
	Pixmap bpixmap;			/* 0 if use_backing is false, */
					/* or if it can't be allocated */
	int ghostview;		/* flag to tell if ghostview is in control */
	Window mwin;		/* window to receive ghostview messages */
/* Don't include standard colormap stuff for X11R3 and earlier */
#if HaveStdCMap
	XStandardColormap *std_cmap;	/* standard color map if available */
#endif
	gs_matrix initial_matrix;	/* the initial transformation */
	Atom next, page, done;	/* Atoms used to talk to ghostview */
	rect update;		/* region needing updating */
	long up_area;		/* total area of update */
				/* (always 0 if no backing pixmap) */
	int up_count;		/* # of updates since flush */
	Pixmap dest;		/* bpixmap if non-0, else win */
	x_pixel colors_or;	/* 'or' of all device colors used so far */
	x_pixel colors_and;	/* 'and' ditto */

	/* An intermediate pixmap for the stencil case of copy_mono */
	struct {
	  Pixmap pixmap;
	  GC gc;
	  int raster, height;
	} cp;

	/* Structure for dealing with the halftone tile. */
	/* Later this might become a multi-element cache. */
	struct {
	  Pixmap pixmap;
	  Pixmap no_pixmap;	/* kludge to get around X bug */
	  gx_bitmap_id id;
	  int width, height, raster;
	  x_pixel fore_c, back_c;
	} ht;

	/* Cache the function and fill style from the GC */
	int function;
	int fill_style;

#define set_fill_style(style)\
  if ( xdev->fill_style != style )\
    XSetFillStyle(xdev->dpy, xdev->gc, (xdev->fill_style = style))
#define set_function(func)\
  if ( xdev->function != func )\
    XSetFunction(xdev->dpy, xdev->gc, (xdev->function = func))

	/* Map color indices to X pixel values */
	unsigned long pixel_fix;
	x_pixel colors[8];	/* primary colors */
#define pixel_black xdev->colors[0]
#define pixel_white xdev->colors[7]
	x_pixel back_color, fore_color;

#define note_color(pixel)\
  xdev->colors_or |= pixel,\
  xdev->colors_and &= pixel
#define set_back_color(pixel)\
  if ( xdev->back_color != pixel )\
   { xdev->back_color = pixel;\
     note_color(pixel);\
     XSetBackground(xdev->dpy, xdev->gc, pixel);\
   }
#define set_fore_color(pixel)\
  if ( xdev->fore_color != pixel )\
   { xdev->fore_color = pixel;\
     note_color(pixel);\
     XSetForeground(xdev->dpy, xdev->gc, pixel);\
   }

} gx_device_X;

/* Default window size */
#define DEFAULT_WIDTH_INCHES 8.5
#define DEFAULT_HEIGHT_INCHES 11

/* Define a fake value for X and Y resolution, */
/* so an uninitialized value can be detected easily. */
#define FAKE_RES 2	/* easily detected fake value */
