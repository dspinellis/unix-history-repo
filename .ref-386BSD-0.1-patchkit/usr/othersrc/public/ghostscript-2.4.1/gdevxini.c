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

/* gdevxini.c */
/* X Windows driver initialization for Ghostscript library */
#include "gx.h"			/* for gx_bitmap; includes std.h */
#include "memory_.h"
#include "x_.h"
#include "gxdevice.h"
#include "gdevx.h"

extern char *getenv(P1(const char *));
extern double atof(P1(const char *));

/* Define whether to use a backing pixmap to handle expose events. */
/* Note that this is a variable rather than a #define. */
/* Note also that it is consulted each time we open an X device. */
private int use_backing = 1;

/* Define default window parameters. */
/* Some of these can be set in the makefile. */

#ifndef PROGRAM_NAME
#  define PROGRAM_NAME "Ghostscript"
#endif

#define ARG_BORDER_WIDTH "borderWidth"
#define DEFAULT_BORDER_WIDTH 1

#define ARG_BORDER_COLOR "borderColor"
#define DEFAULT_BORDER_COLOR  pixel_black

#define ARG_GEOMETRY "geometry"

#define DEFAULT_X_POSITION 0
#define DEFAULT_Y_POSITION 0

#define ARG_X_RESOLUTION "xResolution"
#define ARG_Y_RESOLUTION "yResolution"

/* Define constants for orientation from ghostview */
/* Number represents clockwise rotation of the paper in degrees */
typedef enum {
  Portrait = 0,		/* Normal portrait orientation */
  Landscape = 90,	/* Normal landscape orientation */
  Upsidedown = 180,	/* Don't think this will be used much */
  Seascape = 270	/* Landscape rotated the wrong way */
} orientation;

/* Open the X device */
int
gdev_x_open(register gx_device_X *xdev)
{       XSizeHints sizehints;
	int border_width;
	char *border_width_str, *border_color_str;
	unsigned long border_color;
	char *geometry;
	char *window_id;
	XColor screen_color, exact_color;
	XSetWindowAttributes xswa;
	XEvent event;
	XVisualInfo xvinfo;
	int nitems;
#if HaveStdCMap
        XStandardColormap *scmap, *sp;
        Atom prop;
#endif
#ifdef DEBUG
if ( gs_debug['X'] )
	{ extern int _Xdebug;
	  _Xdebug = 1;
	}
#endif
	if ( !(xdev->dpy = XOpenDisplay((char *)NULL)) )
	  { char *dispname = getenv("DISPLAY");
	    eprintf1("gs: Cannot open X display `%s'.\n",
		     (dispname == NULL ? "(null)" : dispname));
	    exit(1);
	  }
	if ( (window_id = getenv("GHOSTVIEW")) )
	  { if ( !(xdev->ghostview = sscanf(window_id, "%d %d",
					    &(xdev->win), &(xdev->dest))) )
	      { eprintf("gs: Cannot get Window from ghostview.\n");
	        exit(1);
	      }
	  }
	if ( xdev->ghostview )
	  { XWindowAttributes attrib;
	    Atom type;
	    int format;
	    unsigned long nitems, bytes_after;
	    char *buf;
	    Atom ghostview_atom = XInternAtom(xdev->dpy, "GHOSTVIEW", False);
	    if ( XGetWindowAttributes(xdev->dpy, xdev->win, &attrib) )
	      { xdev->scr = attrib.screen;
	        xvinfo.visual = attrib.visual;
		xdev->cmap = attrib.colormap;
	        xdev->width = attrib.width;
		xdev->height = attrib.height;
	      }
	    /* Delete property if explicit dest is given */
	    if ( XGetWindowProperty(xdev->dpy, xdev->win, ghostview_atom, 0, 
				    256, (xdev->dest != 0), XA_STRING,
				    &type, &format, &nitems, &bytes_after,
				    (unsigned char **)&buf) == 0 )
	      { int llx, lly, urx, ury;
		int left_margin = 0, bottom_margin = 0;
		int right_margin = 0, top_margin = 0;
		/* We declare page_orientation as an int so that we can */
		/* use an int * to reference it for sscanf; compilers */
		/* might be tempted to use less space to hold it if */
		/* it was declared as an orientation. */
		int /*orientation*/ page_orientation;
		float xppp, yppp;	/* pixels per point */
		nitems = sscanf(buf,
				"%d %d %d %d %d %d %f %f %d %d %d %d %d %d",
		                &(xdev->bpixmap), &page_orientation,
		                &llx, &lly, &urx, &ury,
		                &(xdev->x_pixels_per_inch),
				&(xdev->y_pixels_per_inch),
				&left_margin, &bottom_margin,
				&right_margin, &top_margin,
				&(xdev->width), &(xdev->height));
		if ( (!xdev->dest && !(nitems == 8 || nitems == 12)) ||
		     (xdev->dest && nitems != 14) )
		  { eprintf("gs: Cannot get ghostview property.\n");
		    exit(1);
		  }
		if ( xdev->dest && xdev->bpixmap )
		  { eprintf("gs: Both destination and backing pixmap specified.\n");
		    exit(1);
		  }
		xppp = xdev->x_pixels_per_inch / 72.0;
		yppp = xdev->y_pixels_per_inch / 72.0;
		switch (page_orientation)
		  {
		  case Portrait:
		    xdev->initial_matrix.xx = xppp;
		    xdev->initial_matrix.xy = 0.0;
		    xdev->initial_matrix.yx = 0.0;
		    xdev->initial_matrix.yy = -yppp;
		    xdev->initial_matrix.tx = -llx * xppp;
		    xdev->initial_matrix.ty = ury * yppp;
		    break;
		  case Landscape:
		    xdev->initial_matrix.xx = 0.0;
		    xdev->initial_matrix.xy = yppp;
		    xdev->initial_matrix.yx = xppp;
		    xdev->initial_matrix.yy = 0.0;
		    xdev->initial_matrix.tx = -lly * xppp;
		    xdev->initial_matrix.ty = -llx * yppp;
		    break;
		  case Upsidedown:
		    xdev->initial_matrix.xx = -xppp;
		    xdev->initial_matrix.xy = 0.0;
		    xdev->initial_matrix.yx = 0.0;
		    xdev->initial_matrix.yy = yppp;
		    xdev->initial_matrix.tx = urx * xppp;
		    xdev->initial_matrix.ty = -lly * yppp;
		    break;
		  case Seascape:
		    xdev->initial_matrix.xx = 0.0;
		    xdev->initial_matrix.xy = -yppp;
		    xdev->initial_matrix.yx = -xppp;
		    xdev->initial_matrix.yy = 0.0;
		    xdev->initial_matrix.tx = ury * xppp;
		    xdev->initial_matrix.ty = urx * yppp;
		    break;
		}

		/* The following sets the imageable area according to the */
		/* bounding box and margins sent by ghostview.            */
		xdev->l_margin = (llx - left_margin) / 72.0;
		xdev->b_margin = (lly - bottom_margin) / 72.0;
		xdev->r_margin = xdev->width / xdev->x_pixels_per_inch -
				 (urx + right_margin) / 72.0;
		xdev->t_margin = xdev->height / xdev->y_pixels_per_inch -
				 (ury + top_margin) / 72.0;
	      }
	    else
	      { eprintf("gs: Cannot get ghostview property.\n");
		exit(1);
	      }
	  }
	else
	  { Screen *scr = DefaultScreenOfDisplay(xdev->dpy);
	    xdev->scr = scr;
	    xvinfo.visual = DefaultVisualOfScreen(scr);
	    xdev->cmap = DefaultColormapOfScreen(scr);
	  }

	xvinfo.visualid = XVisualIDFromVisual(xvinfo.visual);
	xdev->vinfo = XGetVisualInfo(xdev->dpy, VisualIDMask, &xvinfo, &nitems);
	if ( xdev->vinfo == NULL )
	  { eprintf("gs: Cannot get XVisualInfo.\n");
	    exit(1);
	  }
	xdev->color_info.num_components =
	  ((xdev->vinfo->class != StaticGray) &&
	   (xdev->vinfo->class != GrayScale) ? 3 : 1);

#if HaveStdCMap
	if ( gx_device_has_color(xdev) )
	  { if ( xvinfo.visual == DefaultVisualOfScreen(xdev->scr) )
	      prop = XA_RGB_DEFAULT_MAP;
	    else
	      prop = XA_RGB_BEST_MAP;
	  }
	else
	  prop = XA_RGB_GRAY_MAP;

	if ( XGetRGBColormaps(xdev->dpy, RootWindowOfScreen(xdev->scr),
			      &scmap, &nitems, prop) )
	  { int i;
	    for (i = 0, sp = scmap; i < nitems; i++, sp++)
	      { if ( (xdev->ghostview && (xdev->cmap == sp->colormap)) ||
		     (!xdev->ghostview && (xdev->vinfo->visualid ==
					    sp->visualid)) )
		  { xdev->std_cmap = sp;
		    break;
		  }
	      }
	  }

	if ( xdev->std_cmap )
	  { xdev->cmap = xdev->std_cmap->colormap;
	    /* Acquire white and black pixel values. */
	    if ( xdev->cmap == DefaultColormapOfScreen(xdev->scr) )
	     { pixel_black = BlackPixelOfScreen(xdev->scr);
	       pixel_white = WhitePixelOfScreen(xdev->scr);
	     }
	    else
	     {
#define pixv(v)\
  color_index_to_pixel((*xdev->procs->map_rgb_color)((gx_device *)xdev,\
						      v, v, v))
	       pixel_black = pixv(0);
	       pixel_white = pixv(gx_max_color_value);
#undef pixv
	     }
	    if ( gx_device_has_color(xdev) )
	      { /* Set the color_info in the device structure. */
		xdev->color_info.max_gray =
		  xdev->color_info.max_rgb =
		    min(xdev->std_cmap->red_max,
			min(xdev->std_cmap->green_max,
			    xdev->std_cmap->blue_max));
		xdev->color_info.depth = 8;	/* arbitrary */
		xdev->color_info.dither_gray =
		  xdev->color_info.dither_rgb =
		    xdev->color_info.max_rgb + 1;
	      }
	    else
	      { xdev->color_info.max_gray = xdev->std_cmap->red_max +
		  xdev->std_cmap->green_max + xdev->std_cmap->blue_max;
		xdev->color_info.depth = 8;	/* arbitrary */
		xdev->color_info.dither_gray = xdev->color_info.max_gray + 1;
	      }
	  }
	else
#endif
	  { if ( xdev->cmap == DefaultColormapOfScreen(xdev->scr) )
	      { pixel_black = BlackPixelOfScreen(xdev->scr);
		pixel_white = WhitePixelOfScreen(xdev->scr);
	      }
	    else
	      { XColor xc;
		xc.red = xc.green = xc.blue = 0;
	        XAllocColor(xdev->dpy, xdev->cmap, &xc);
		pixel_black = xc.pixel;
		xc.red = xc.green = xc.blue = ~(ushort)0;
	        XAllocColor(xdev->dpy, xdev->cmap, &xc);
		pixel_white = xc.pixel;
	      }

	    /* Figure out monochrome vs. color */
	    if ( gx_device_has_color(xdev) )
	      /* Just do primary colors for now */
	      { XColor xc;
		int i;
		for ( i = 1; i < 7; i++ )
		  { xc.red = (i & 4 ? ~(ushort)0 : 0);
		    xc.green = (i & 2 ? ~(ushort)0 : 0);
		    xc.blue = (i & 1 ? ~(ushort)0 : 0);
		    XAllocColor(xdev->dpy, xdev->cmap, &xc);
		    xdev->colors[i] = xc.pixel;
		  }
		xdev->color_info.max_rgb = 1;
		xdev->color_info.dither_rgb = 2;
		xdev->color_info.depth = 8;
	      }
	    else
	      { int i;
	        for ( i = 1; i < 7; i++ )
		  xdev->colors[i] = pixel_white;
	      }
	  }

	/* Check for a pixel value equal to gx_no_color_index. */
	if (
#if HaveStdCMap
	    !xdev->std_cmap &&
#endif
	    (pixel_black == gx_no_color_index ||
	     pixel_white == gx_no_color_index)
	    )
	  { /* Pick a non-zero value guaranteed not to map any primary */
	    /* color to gx_no_color_index. */
	    xdev->pixel_fix = 0x100000ff ^
	      (xdev->colors[1] & 2) ^ (xdev->colors[2] & 4) ^
	      (xdev->colors[3] & 8) ^ (xdev->colors[4] & 16) ^
	      (xdev->colors[5] & 32) ^ (xdev->colors[6] & 64);
	  }
	else
	  { xdev->pixel_fix = 0;
	  }

	if ( !xdev->ghostview )
	  { /*
	     * Figure out the resolution of our screen; 25.4 is the
	     * number of millimeters in an inch.  The only reason for
	     * allowing the user to specify the resolution is that
	     * X servers commonly lie about it (and about the screen size).
	     * We assume that the server is more likely to lie about
	     * the resolution than about the pixel size of the screen.
	     * Don't do any of this if the resolution was set from the
	     * command line (detected by resolution != FAKE_RES).
	     */

	    if ( xdev->x_pixels_per_inch == FAKE_RES )
	      { char *x_res_str = XGetDefault(xdev->dpy, PROGRAM_NAME,
					      ARG_X_RESOLUTION);
		char *y_res_str = XGetDefault(xdev->dpy, PROGRAM_NAME,
					      ARG_Y_RESOLUTION);
		float x_res, y_res;
		if ( x_res_str != NULL && y_res_str != NULL )
		  { x_res = atof(x_res_str);
		    y_res = atof(y_res_str);
		  }
		else
		  { int screen_width = WidthOfScreen(xdev->scr);
		    int screen_height = HeightOfScreen(xdev->scr);
		    x_res = 25.4 * screen_width / WidthMMOfScreen(xdev->scr);
		    y_res = 25.4 * screen_height / HeightMMOfScreen(xdev->scr);
		    if ( x_res * DEFAULT_WIDTH_INCHES > screen_width ||
			y_res * DEFAULT_HEIGHT_INCHES > screen_height
			)
		      { /* Force a full page to fit on the screen */
			/* by adjusting the server's claimed resolution. */
			x_res = (screen_width - 32) / (float)DEFAULT_WIDTH_INCHES;
			y_res = (screen_height - 32) / (float)DEFAULT_HEIGHT_INCHES;
			x_res = y_res = min(x_res, y_res);
		      }
		  }
		xdev->x_pixels_per_inch = x_res;
		xdev->y_pixels_per_inch = y_res;
	      }

	    /* Get defaults from the database. */
	    border_width_str = XGetDefault(xdev->dpy, PROGRAM_NAME,
					   ARG_BORDER_WIDTH);

	    border_width = (border_width_str == NULL ? DEFAULT_BORDER_WIDTH :
			    atoi(border_width_str));

	    border_color_str = XGetDefault(xdev->dpy, PROGRAM_NAME,
					   ARG_BORDER_COLOR);

	    border_color = (border_color_str == NULL ||
			     !XAllocNamedColor(xdev->dpy, xdev->cmap, 
					       border_color_str, 
					       &screen_color, &exact_color) ?
			    DEFAULT_BORDER_COLOR :
			    screen_color.pixel);

	    sizehints.x = DEFAULT_X_POSITION;
	    sizehints.y = DEFAULT_Y_POSITION;
	    sizehints.width =
	              (int)(xdev->x_pixels_per_inch * DEFAULT_WIDTH_INCHES);
	    sizehints.height =
	              (int)(xdev->y_pixels_per_inch * DEFAULT_HEIGHT_INCHES);
	    sizehints.flags = 0;

	    geometry = XGetDefault(xdev->dpy, PROGRAM_NAME, ARG_GEOMETRY);

	    if (geometry != NULL)
	       {	/*
		     * Note that border_width must be set first.  We can't use
		     * scr, because that is a Screen*, and XGeometry wants
		     * the screen number.
		     */
		    char gstr[40];
		    int bitmask;
		    sprintf(gstr, "%dx%d+%d+%d", sizehints.width,
			    sizehints.height, sizehints.x, sizehints.y);
		    bitmask = XGeometry(xdev->dpy, DefaultScreen(xdev->dpy),
					geometry, gstr, border_width,
					1, 1, /* ``Font'' width and height. */
					0, 0, /* Interior padding. */
					&sizehints.x, &sizehints.y,
					&sizehints.width, &sizehints.height);

		    if (bitmask & (XValue | YValue))
			    sizehints.flags |= USPosition;

		    if (bitmask & (WidthValue | HeightValue))
			    sizehints.flags |= USSize;
	       }

	    if ( xdev->width == (int)(FAKE_RES*DEFAULT_WIDTH_INCHES) )
	      xdev->width = sizehints.width,
	      xdev->height = sizehints.height;
	    else			/* set from command line */
	      sizehints.width = xdev->width,
	      sizehints.height = xdev->height;

	    gx_default_get_initial_matrix((gx_device *)xdev,
					  &(xdev->initial_matrix));

	    xswa.event_mask = ExposureMask;
	    xswa.background_pixel = pixel_black;
	    xswa.border_pixel = border_color;
	    xswa.colormap = xdev->cmap;
	    xdev->win = XCreateWindow(xdev->dpy, RootWindowOfScreen(xdev->scr),
				      sizehints.x, sizehints.y, /* upper left */
				      sizehints.width, sizehints.height,
				      border_width,
				      xdev->vinfo->depth,
				      InputOutput, /* class */
				      xdev->vinfo->visual, /* visual */
				      CWEventMask | CWBackPixel |
				      CWBorderPixel | CWColormap,
				      &xswa);
	    XChangeProperty(xdev->dpy, xdev->win, XA_WM_NAME, XA_STRING, 8,
			    PropModeReplace, (const unsigned char *)PROGRAM_NAME,
			    strlen(PROGRAM_NAME));
	    XSetNormalHints(xdev->dpy, xdev->win, &sizehints);
	   
	    if ( use_backing )
	      xdev->bpixmap =
		    XCreatePixmap(xdev->dpy, xdev->win,
				  xdev->width, xdev->height,
				  xdev->vinfo->depth);
	    else
	      xdev->bpixmap = (Pixmap)0;
	}

	xdev->ht.pixmap = (Pixmap)0;
	xdev->ht.id = gx_no_bitmap_id;;
	xdev->fill_style = FillSolid;
	xdev->function = GXcopy;

	/* Set up a graphics context */
	xdev->gc = XCreateGC(xdev->dpy, xdev->win, 0, (XGCValues *)NULL);
	XSetFunction(xdev->dpy, xdev->gc, GXcopy);
	XSetLineAttributes(xdev->dpy, xdev->gc, 0,
			   LineSolid, CapButt, JoinMiter);

	/* Clear the destination pixmap to avoid initializing with garbage. */
	if ( xdev->dest != (Pixmap)0 )
	  { XSetForeground(xdev->dpy, xdev->gc, pixel_white);
	    XFillRectangle(xdev->dpy, xdev->dest, xdev->gc,
			   0, 0, xdev->width, xdev->height);
	  }
	else
	  { xdev->dest = (xdev->bpixmap != (Pixmap)0 ?
			  xdev->bpixmap : (Pixmap)xdev->win);
	  }

	/* Clear the background pixmap to avoid initializing with garbage. */
	if ( xdev->bpixmap != (Pixmap)0 )
	  { if ( !xdev->ghostview )
	      XSetWindowBackgroundPixmap(xdev->dpy, xdev->win, xdev->bpixmap);
	    XSetForeground(xdev->dpy, xdev->gc, pixel_white);
	    XFillRectangle(xdev->dpy, xdev->bpixmap, xdev->gc,
			   0, 0, xdev->width, xdev->height);
	  }

	/* Initialize foreground and background colors */
	xdev->back_color = pixel_white;
	XSetBackground(xdev->dpy, xdev->gc, pixel_white);
	xdev->fore_color = pixel_white;
	XSetForeground(xdev->dpy, xdev->gc, pixel_white);
	xdev->colors_or = xdev->colors_and = pixel_white;

	if ( !xdev->ghostview )
	  { /* Make the window appear. */
	    XMapWindow(xdev->dpy, xdev->win);

	    /* Before anything else, do a flush and wait for */
	    /* an exposure event. */
	    XFlush(xdev->dpy);
	    XNextEvent(xdev->dpy, &event);
	  }
	else
	  { /* Create an unmapped window, that the window manager will ignore.
	     * This invisble window will be used to receive "next page"
	     * events from ghostview */
	    XSetWindowAttributes attributes;
	    attributes.override_redirect = True;
	    xdev->mwin = XCreateWindow(xdev->dpy, RootWindowOfScreen(xdev->scr),
				       0, 0, 1, 1, 0, CopyFromParent,
				       CopyFromParent, CopyFromParent,
				       CWOverrideRedirect, &attributes);
	    xdev->next = XInternAtom(xdev->dpy, "NEXT", False);
	    xdev->page = XInternAtom(xdev->dpy, "PAGE", False);
	    xdev->done = XInternAtom(xdev->dpy, "DONE", False);
	  }

	xdev->ht.no_pixmap = XCreatePixmap(xdev->dpy, xdev->win, 1, 1,
					   xdev->vinfo->depth);

	XSync(xdev->dpy, 0);
	return 0;
}
