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

/* gxdevice.h */
/* Device description structure for Ghostscript library */
#include "gsmatrix.h"
#include "gxbitmap.h"

/* See drivers.doc for documentation of the driver interface. */

/* Define the type for device color indices. */
typedef unsigned long gx_color_index;
/* Define the 'transparent' color index. */
#define gx_no_color_value (-1)		/* no cast -> can be used in #if */
#define gx_no_color_index ((gx_color_index)gx_no_color_value)

/* Define the type for gray or RGB values at the driver interface. */
typedef unsigned short gx_color_value;
/* We might use less than the full range someday. */
/* ...bits must lie between 8 and 16. */
#define gx_color_value_bits (sizeof(gx_color_value) * 8)
#define gx_max_color_value ((gx_color_value)((1L << gx_color_value_bits) - 1))
#define gx_color_value_to_byte(cv)\
  ((cv) >> (gx_color_value_bits - 8))
#define gx_color_value_from_byte(cb)\
  (((cb) << (gx_color_value_bits - 8)) + ((cb) >> (16 - gx_color_value_bits)))

/* Define the structure for device color capabilities. */
typedef struct gx_device_color_info_s {
	int num_components;		/* 1 = gray only, 3 = RGB, */
					/* 4 = CMYK (not supported) */
	int depth;			/* # of bits per pixel */
	gx_color_value max_gray;	/* # of distinct gray levels -1 */
	gx_color_value max_rgb;		/* # of distinct color levels -1 */
					/* (only relevant if num_comp. > 1) */
	gx_color_value dither_gray;	/* size of gray ramp for dithering */
	gx_color_value dither_rgb;	/* size of color cube ditto */
					/* (only relevant if num_comp. > 1) */
} gx_device_color_info;
#define dci_black_and_white { 1, 1, 1, 0, 2, 0 }
#define dci_color(depth,maxv,dither) { 3, depth, maxv, maxv, dither, dither }
#define gx_device_has_color(dev) ((dev)->color_info.num_components > 1)

/* Structure for device procedures */
typedef struct gx_device_procs_s gx_device_procs;

/* Structure for generic device description */
#define gx_device_common\
	int params_size;		/* size of this structure */\
	gx_device_procs *procs;\
	const char *dname;		/* the device name */\
	int width;			/* width in pixels */\
	int height;			/* height in pixels */\
	float x_pixels_per_inch;	/* x density */\
	float y_pixels_per_inch;	/* y density */\
	float l_margin, b_margin, r_margin, t_margin;	/* margins around */\
					/* imageable area, in inches */\
	gx_device_color_info color_info;	/* color information */\
	int is_open			/* true if device has been opened */
#define no_margins 0, 0, 0, 0
/* A generic device */
struct gx_device_s {
	gx_device_common;
};
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif

/* Define an opaque type for property lists. */
#ifndef gs_prop_item_DEFINED
#  define gs_prop_item_DEFINED
typedef struct gs_prop_item_s gs_prop_item;
#endif

/* Definition of device procedures */
struct gx_device_procs_s {

#define dev_proc_open_device(proc)\
  int proc(P1(gx_device *dev))
	dev_proc_open_device((*open_device));

#define dev_proc_get_initial_matrix(proc)\
  void proc(P2(gx_device *dev, gs_matrix *pmat))
	dev_proc_get_initial_matrix((*get_initial_matrix));

#define dev_proc_sync_output(proc)\
  int proc(P1(gx_device *dev))
	dev_proc_sync_output((*sync_output));

#define dev_proc_output_page(proc)\
  int proc(P3(gx_device *dev, int num_copies, int flush))
	dev_proc_output_page((*output_page));

#define dev_proc_close_device(proc)\
  int proc(P1(gx_device *dev))
	dev_proc_close_device((*close_device));

#define dev_proc_map_rgb_color(proc)\
  gx_color_index proc(P4(gx_device *dev,\
    gx_color_value red, gx_color_value green, gx_color_value blue))
	dev_proc_map_rgb_color((*map_rgb_color));

#define dev_proc_map_color_rgb(proc)\
  int proc(P3(gx_device *dev,\
    gx_color_index color, gx_color_value rgb[3]))
	dev_proc_map_color_rgb((*map_color_rgb));

#define dev_proc_fill_rectangle(proc)\
  int proc(P6(gx_device *dev,\
    int x, int y, int width, int height, gx_color_index color))
	dev_proc_fill_rectangle((*fill_rectangle));

#define dev_proc_tile_rectangle(proc)\
  int proc(P10(gx_device *dev,\
    gx_bitmap *tile, int x, int y, int width, int height,\
    gx_color_index color0, gx_color_index color1,\
    int phase_x, int phase_y))
	dev_proc_tile_rectangle((*tile_rectangle));

#define dev_proc_copy_mono(proc)\
  int proc(P11(gx_device *dev,\
    unsigned char *data, int data_x, int raster, gx_bitmap_id id,\
    int x, int y, int width, int height,\
    gx_color_index color0, gx_color_index color1))
	dev_proc_copy_mono((*copy_mono));

#define dev_proc_copy_color(proc)\
  int proc(P9(gx_device *dev,\
    unsigned char *data, int data_x, int raster, gx_bitmap_id id,\
    int x, int y, int width, int height))
	dev_proc_copy_color((*copy_color));

#define dev_proc_draw_line(proc)\
  int proc(P6(gx_device *dev,\
    int x0, int y0, int x1, int y1, gx_color_index color))
	dev_proc_draw_line((*draw_line));

#define dev_proc_get_bits(proc)\
  int proc(P5(gx_device *dev,\
    int y, unsigned char *data, unsigned int size, int pad_to_word))
	dev_proc_get_bits((*get_bits));

#define dev_proc_get_props(proc)\
  int proc(P2(gx_device *dev, gs_prop_item *plist))
	dev_proc_get_props((*get_props));
    
#define dev_proc_put_props(proc)\
  int proc(P3(gx_device *dev, gs_prop_item *plist, int count))
	dev_proc_put_props((*put_props));
    
};

/* Calculate the number of bytes in a scan line, */
/* with byte or word padding. */
extern unsigned int gx_device_bytes_per_scan_line(P2(gx_device *dev, int pad_to_word));

/* Default implementations of optional procedures */
dev_proc_open_device(gx_default_open_device);
dev_proc_get_initial_matrix(gx_default_get_initial_matrix);
dev_proc_sync_output(gx_default_sync_output);
dev_proc_output_page(gx_default_output_page);
dev_proc_close_device(gx_default_close_device);
dev_proc_map_rgb_color(gx_default_map_rgb_color);
dev_proc_map_color_rgb(gx_default_map_color_rgb);
dev_proc_tile_rectangle(gx_default_tile_rectangle);
dev_proc_copy_color(gx_default_copy_color);
dev_proc_draw_line(gx_default_draw_line);
dev_proc_get_bits(gx_default_get_bits);
dev_proc_get_props(gx_default_get_props);
dev_proc_put_props(gx_default_put_props);
