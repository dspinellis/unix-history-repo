/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

/* gdevbgi.c */
/* Ghostscript driver for Borland Graphics Interface (BGI) */
#include <string.h>
#include <stdlib.h>
#include <conio.h>
#include <graphics.h>
#include "gx.h"
#include "gxdevice.h"

#ifndef BGI_LIB				/* may be set in makefile */
#  define BGI_LIB ""
#endif

/*
 * BGI supports these video cards:
 *   Hercules, CGA, MCGA, EGA, VGA, AT&T 400, IBM 8514, PC3270.
 * Highest resolution mode is used with all these video cards.
 * EGA and VGA display 16 colors, the rest are black-and-white only.
 * In addition, the environment variable BGIUSER may be used
 * to define a user-supplied Super VGA driver: see the use.doc file
 * for details.
 */
#define SUPER_VGA 999			/* bogus # for user-defined driver */

/* See gxdevice.h for the definitions of the procedures. */

dev_proc_open_device(bgi_open);
dev_proc_close_device(bgi_close);
dev_proc_map_rgb_color(bgi_map_rgb_color);
dev_proc_map_color_rgb(bgi_map_color_rgb);
dev_proc_fill_rectangle(bgi_fill_rectangle);
dev_proc_tile_rectangle(bgi_tile_rectangle);
dev_proc_copy_mono(bgi_copy_mono);
dev_proc_copy_color(bgi_copy_color);
dev_proc_draw_line(bgi_draw_line);

/* The device descriptor */
typedef struct gx_device_bgi_s gx_device_bgi;
struct gx_device_bgi_s {
	gx_device_common;
	int display_mode;
	struct text_info text_mode;
};
#define bgi_dev ((gx_device_bgi *)dev)
static gx_device_procs bgi_procs = {
	bgi_open,
	gx_default_get_initial_matrix,
	gx_default_sync_output,
	gx_default_output_page,
	bgi_close,
	bgi_map_rgb_color,
	bgi_map_color_rgb,
	bgi_fill_rectangle,
	bgi_tile_rectangle,
	bgi_copy_mono,
	bgi_copy_color,
	bgi_draw_line,
	gx_default_get_bits,
	gx_default_get_props,
	gx_default_put_props
};
gx_device_bgi gs_bgi_device = {
	sizeof(gx_device_bgi),
	&bgi_procs,
	"bgi",
	0, 0,		/* width and height are set in bgi_open */
	0, 0,		/* density is set in bgi_open */
	no_margins,
	dci_black_and_white,
	0		/* not open yet */
};

/* Detection procedure for user-defined driver. */
private int huge
detectVGA()
{	return gs_bgi_device.display_mode;
}

/* Open the BGI driver for graphics mode */
int
bgi_open(gx_device *dev)
{	int driver, mode;
	char *bgi_user = getenv("BGIUSER");
	char *bgi_path = getenv("BGIPATH");

	gettextinfo(&bgi_dev->text_mode);

	if ( bgi_path == NULL )
		bgi_path = BGI_LIB;
	if ( bgi_user != NULL )
	   {	/* A user-supplied driver is specified as "mode.dname", */
		/* where mode is a hex number and dname is the name */
		/* of the driver file. */
		char dname[40];
		if ( strlen(bgi_user) > sizeof(dname) ||
		     sscanf(bgi_user, "%x.%s", &mode, dname) != 2
		   )
		   {	eprintf("BGIUSER not in form nn.dname.\n");
			exit(1);
		   }
		gs_bgi_device.display_mode = mode;	/* sigh.... */
		installuserdriver(dname, detectVGA);
		driver = DETECT;
		initgraph(&driver, &mode, bgi_path);
		driver = SUPER_VGA;
	   }
	else				/* not user-defined driver */
	   {	/* We include the CGA and Hercules drivers */
		/* in the Ghostscript executable, so end-users don't */
		/* have to have the BGI files. */
		if ( registerbgidriver(CGA_driver) < 0 )
		   {	eprintf("BGI: Can't register CGA driver!\n");
			exit(1);
		   }
		if ( registerbgidriver(Herc_driver) < 0 )
		   {	eprintf("BGI: Can't register Hercules driver!\n");
			exit(1);
		   }

		detectgraph(&driver, &mode);
		if ( driver < 0 )
		   {	eprintf("BGI: No graphics hardware detected!\n");
			exit(1);
		   }

		if ( driver == EGA64 )
		   {	/* Select 16 color video mode if video card is EGA with 64 Kb of memory */
			mode = EGA64LO;
		   }

		/* Initialize graphics mode. */

		/* Following patch for AT&T 6300 is courtesy of */
		/* Allan Wax, Xerox Corp. */
		if ( driver == CGA )
		   {	/* The actual hardware might be an AT&T 6300. */
			/* Try initializing it that way. */
			int save_mode = mode;
			driver = ATT400, mode = ATT400HI;
			initgraph(&driver, &mode, bgi_path);
			if ( graphresult() != grOk )
			   {	/* Nope, it was a real CGA. */
				closegraph();
				driver = CGA, mode = save_mode;
				initgraph(&driver, &mode, bgi_path);
			   }
		   }
		else
			initgraph(&driver, &mode, bgi_path);
	   }

	   {	int code = graphresult();
		if ( code != grOk )
		   {	eprintf1("Error initializing BGI driver: %s\n",
				 grapherrormsg(code));
			exit(1);
		   }
	   }

	/* Set parameters that were unknown before opening device */

	/* Size and nominal density of screen. */
	/* The following algorithm maps an appropriate fraction of */
	/* the display screen to an 8.5" x 11" coordinate space. */
	/* This may or may not be what is desired! */
	dev->width = getmaxx() + 1;
	dev->height = getmaxy() + 1;
	dev->y_pixels_per_inch = dev->height / 11.0;
	   {	/* Get the aspect ratio from the driver. */
		int arx, ary;
		getaspectratio(&arx, &ary);
		dev->x_pixels_per_inch =
			dev->y_pixels_per_inch * ((float)ary / arx);
	   }

	/* Find out if the device supports color */
	/* (default initialization is monochrome). */
	/* We only recognize 16-color devices right now. */
	if ( getmaxcolor() > 1 )
	   {	static gx_device_color_info bgi_16color = dci_color(4, 2, 3);
		dev->color_info = bgi_16color;
	   }
	return 0;
}

/* Close the BGI driver */
int
bgi_close(gx_device *dev)
{	closegraph();
	textmode(bgi_dev->text_mode.currmode);
	return 0;
}

/* Map a r-g-b color to the 16 colors available with an EGA/VGA video card. */
gx_color_index
bgi_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	return (gx_color_index)
		((r > gx_max_color_value / 4 ? 4 : 0) +
		 (g > gx_max_color_value / 4 ? 2 : 0) +
		 (b > gx_max_color_value / 4 ? 1 : 0) +
		 (r > gx_max_color_value / 4 * 3 ||
		  g > gx_max_color_value / 4 * 3 ? 8 : 0));
}

/* Map a color code to r-g-b.  Surprisingly enough, this is algorithmic. */
int
bgi_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{
#define icolor (int)color
	gx_color_value one =
		(icolor & 8 ? gx_max_color_value : gx_max_color_value / 3);
	prgb[0] = (icolor & 4 ? one : 0);
	prgb[1] = (icolor & 2 ? one : 0);
	prgb[2] = (icolor & 1 ? one : 0);
	return 0;
#undef icolor
}

/* Copy a monochrome bitmap.  The colors are given explicitly. */
/* Color = gx_no_color_index means transparent (no effect on the image). */
int
bgi_copy_mono(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h,
  gx_color_index zero, gx_color_index one)
{	byte *ptr_line = base + (sourcex >> 3);
	int left_bit = 0x80 >> (sourcex & 7);
	int dest_y = y, end_x = x + w;
	int invert = 0;
	int color;

	if ( zero == gx_no_color_index )
	   {	if ( one == gx_no_color_index ) return 0;
		color = (int)one;
	   }
	else
	   {	if ( one == gx_no_color_index )
		   {	color = (int)zero;
			invert = -1;
		   }
		else
		   {	/* Pre-clear the rectangle to zero */
			setfillstyle(SOLID_FILL, (int)zero);
			bar(x, y, x + w - 1, y + h - 1);
			color = (int)one;
		   }
	   }

	while ( h-- )              /* for each line */
	   {	byte *ptr_source = ptr_line;
		register int dest_x = x;
		register int bit = left_bit;
		while ( dest_x < end_x )     /* for each bit in the line */
		   {	if ( (*ptr_source ^ invert) & bit )
				putpixel(dest_x, dest_y, color);
			dest_x++;
			if ( (bit >>= 1) == 0 )
				bit = 0x80, ptr_source++;
		   }
		dest_y++;
		ptr_line += raster;
	   }
	return 0;
}


/* Copy a color pixel map.  This is just like a bitmap, except that */
/* each pixel takes 4 bits instead of 1 when device driver has color. */
int
bgi_copy_color(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	if ( gx_device_has_color(dev) )
	   {	/* color device, four bits per pixel */
		byte *line = base + (sourcex >> 1);
		int dest_y = y, end_x = x + w;

		if ( w <= 0 ) return 0;
		while ( h-- )              /* for each line */
		   {	byte *source = line;
			register int dest_x = x;
			if ( sourcex & 1 )    /* odd nibble first */
			   {	int color =  *source++ & 0xf;
				putpixel(dest_x, dest_y, color);
				dest_x++;
			   }
			/* Now do full bytes */
			while ( dest_x < end_x )
			   {	int color = *source >> 4;
				putpixel(dest_x, dest_y, color);
				dest_x++;
				if ( dest_x < end_x )
				   {	color =  *source++ & 0xf;
					putpixel(dest_x, dest_y, color);
					dest_x++;
				   }
			   }
			dest_y++;
			line += raster;
		   }
	   }
	else /* monochrome device: one bit per pixel */
	   {	/* bitmap is the same as bgi_copy_mono: one bit per pixel */
		bgi_copy_mono(dev, base, sourcex, raster, id, x, y, w, h,
			(gx_color_index)BLACK, (gx_color_index)WHITE);
	   }
	return 0;
}


/* Fill a rectangle. */
int
bgi_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	setfillstyle(SOLID_FILL, (int)color);
	bar(x, y, x + w - 1, y + h - 1);
	return 0;
}


/* Tile a rectangle.  If neither color is transparent, */
/* pre-clear the rectangle to color0 and just tile with color1. */
/* This is faster because of how bgi_copy_mono is implemented. */
/* Note that this also does the right thing for colored tiles. */
int
bgi_tile_rectangle(gx_device *dev, gx_bitmap *tile,
  int x, int y, int w, int h, gx_color_index czero, gx_color_index cone,
  int px, int py)
{	if ( czero != gx_no_color_index && cone != gx_no_color_index )
	   {	bgi_fill_rectangle(dev, x, y, w, h, czero);
		czero = gx_no_color_index;
	   }
	return gx_default_tile_rectangle(dev, tile, x, y, w, h, czero, cone, px, py);
}


/* Draw a line */
int
bgi_draw_line(gx_device *dev, int x0, int y0, int x1, int y1,
  gx_color_index color)
{	setcolor((int)color);
	setlinestyle(SOLID_LINE, 0, NORM_WIDTH);  /* solid, one pixel wide */
	line(x0, y0, x1, y1);
	return 0;
}
