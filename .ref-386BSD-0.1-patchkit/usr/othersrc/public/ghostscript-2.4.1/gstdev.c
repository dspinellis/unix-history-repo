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

/* gstdev.c */
/* Device tracing for Ghostscript library */
#include "gx.h"
#include "gxfixed.h"			/* for gxmatrix.h */
#include "gxmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"

#ifdef DEBUG

/* ------ Tracing 'device' ------*/

/* To avoid unpleasant interactions with copydevice, */
/* the tracing 'device' uses an external linked list to keep track of */
/* the real procedures that were replaced in the procedure vector. */

typedef struct trace_record_s trace_record;
struct trace_record_s {
	trace_record *next;
	gx_device_procs *tprocs;
	gx_device_procs procs;
	int index;
};

private gx_device *trace_cache_device = NULL;
private gx_device_procs *trace_cache_procs;
private trace_record *trace_list = NULL;
private int trace_next_index = 0;

#define rprocs\
	(dev == trace_cache_device ? trace_cache_procs :\
	 trace_find_procs(dev))

/* Procedure structure */
private dev_proc_open_device(trace_open_device);
private dev_proc_get_initial_matrix(trace_get_initial_matrix);
private dev_proc_sync_output(trace_sync_output);
private dev_proc_output_page(trace_output_page);
private dev_proc_close_device(trace_close_device);
private dev_proc_map_rgb_color(trace_map_rgb_color);
private dev_proc_map_color_rgb(trace_map_color_rgb);
private dev_proc_fill_rectangle(trace_fill_rectangle);
private dev_proc_tile_rectangle(trace_tile_rectangle);
private dev_proc_copy_mono(trace_copy_mono);
private dev_proc_copy_color(trace_copy_color);
private dev_proc_draw_line(trace_draw_line);
private dev_proc_get_bits(trace_get_bits);
private dev_proc_get_props(trace_get_props);
private dev_proc_put_props(trace_put_props);

private gx_device_procs trace_procs = {
	trace_open_device,
	trace_get_initial_matrix,
	trace_sync_output,
	trace_output_page,
	trace_close_device,
	trace_map_rgb_color,
	trace_map_color_rgb,
	trace_fill_rectangle,
	trace_tile_rectangle,
	trace_copy_mono,
	trace_copy_color,
	trace_draw_line,
	trace_get_bits,
	trace_get_props,
	trace_put_props
};

/* Find the real procedures for a traced device */
private gx_device_procs *
trace_find_procs(gx_device *tdev)
{	gx_device_procs *tprocs = tdev->procs;
	register trace_record *tp = trace_list;
	while ( tp != NULL )
	   {	if ( tp->tprocs == tprocs )
		   {	trace_cache_device = tdev;
			return (trace_cache_procs = &tp->procs);
		   }
		tp = tp->next;
	   }
	lprintf("Traced procedures not found!\n");
	gs_exit(1);
}

/* Trace a device. */
gx_device *
gs_trace_device(gx_device *rdev)
{	trace_record *tp;
	if ( rdev->procs->open_device == trace_procs.open_device )
		return rdev;		/* already traced */
	tp = (trace_record *)gs_malloc(1, sizeof(trace_record), 
				       "gs_trace_device");
	if ( tp == 0 ) return 0;
	tp->next = trace_list;
	tp->tprocs = rdev->procs;
	tp->procs = *rdev->procs;
	tp->index = ++trace_next_index;
	trace_list = tp;
	*rdev->procs = trace_procs;
	return rdev;
}

/* Utilities */
private int
trace_print_code(int result)
{	if ( result == 0 )
		dprintf(";\n");
	else
		dprintf1(";\t/* = %d */\n", result);
	return result;
}
private void
trace_print_tile(gx_bitmap *tile)
{	int i;
	dprintf1("\t{ static byte data = { 0x%x", tile->data[0]);
	for ( i = 1; i < tile->raster * tile->size.y; i++ )
		dprintf1(", 0x%x", tile->data[i]);
	dprintf4(" };\n\t  static gx_bitmap tile = { &data, %d, %d, %d, 0x%lx };\n",
		 tile->raster, tile->size.x, tile->size.y, tile->id);
}

/* Procedures */
private int
trace_open_device(gx_device *dev)
{	int result = (*rprocs->open_device)(dev);
if ( gs_debug['V'] )
	dprintf2("[V]\topen_device(0x%lx /*%s*/)", (ulong)dev, dev->dname),
	  trace_print_code(result);
	return result;
}
private void
trace_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{	(*rprocs->get_initial_matrix)(dev, pmat);
if ( gs_debug['V'] )
	dprintf6("[V]\tget_initial_matrix(dev) = (%6g, %6g, %6g, %6g, %6g, %6g);\n",
		 pmat->xx, pmat->xy, pmat->yx, pmat->yy, pmat->tx, pmat->ty);
}
private int
trace_sync_output(gx_device *dev)
{	int result = (*rprocs->sync_output)(dev);
if ( gs_debug['V'] )
	dprintf("[V]\tsync_output(dev)"),
	  trace_print_code(result);
	return result;
}
private int
trace_output_page(gx_device *dev, int num_copies, int flush)
{	int result = (*rprocs->output_page)(dev, num_copies, flush);
if ( gs_debug['V'] )
	dprintf2("[V]\toutput_page(dev, %d, %d)", num_copies, flush),
	  trace_print_code(result);
	return result;
}
private int
trace_close_device(gx_device *dev)
{	int result = (*rprocs->close_device)(dev);
if ( gs_debug['V'] )
	dprintf2("[V]\tclose_device(0x%lx /*%s*/)", (ulong)dev, dev->dname),
	  trace_print_code(result);
	return result;
}
private gx_color_index
trace_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	gx_color_index result = (*rprocs->map_rgb_color)(dev, r, g, b);
if ( gs_debug['V'] )
	dprintf4("[V]\tmap_rgb_color(dev, %u, %u, %u) /*= %ld */;\n",
		 r, g, b, (long)result);
	return result;
}
private int
trace_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	int result = (*rprocs->map_color_rgb)(dev, color, prgb);
if ( gs_debug['V'] )
	dprintf4("[V]\t{ gx_color_value rgb[3]; map_color_rgb(dev, %ld, rgb /* %u, %u, %u */); }",
		 (long)color, prgb[0], prgb[1], prgb[2]),
	  trace_print_code(result);
	return result;
}
private int
trace_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	int result = (*rprocs->fill_rectangle)(dev, x, y, w, h, color);
if ( gs_debug['V'] )
	dprintf5("[V]\tfill_rectangle(dev, %d, %d, %d, %d, %ld)",
		 x, y, w, h, (long)color),
	  trace_print_code(result);
	return result;
}
private int
trace_tile_rectangle(gx_device *dev, gx_bitmap *tile,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one,
  int px, int py)
{	int result = (*rprocs->tile_rectangle)(dev, tile,
		x, y, w, h, zero, one, px, py);
if ( gs_debug['V'] )
   {	trace_print_tile(tile);
	dprintf8("[V]\t  tile_rectangle(dev, &tile, %d, %d, %d, %d, %ld, %ld, %d, %d);\n\t}",
		 x, y, w, h, (long)zero, (long)one, px, py);
	trace_print_code(result);
   }
	return result;
}
private int
trace_copy_mono(gx_device *dev, byte *data,
  int dx, int raster, gx_bitmap_id id, int x, int y, int w, int h,
  gx_color_index zero, gx_color_index one)
{	int result = (*rprocs->copy_mono)(dev, data,
		dx, raster, id, x, y, w, h, zero, one);
if ( gs_debug['V'] )
	dprintf9("[V]\tcopy_mono(dev, data, %d, %d, 0x%lx, %d, %d, %d, %d, %ld, %ld)",
		 dx, raster, id, x, y, w, h, (long)zero, (long)one),
	  trace_print_code(result);
	return result;
}
private int
trace_copy_color(gx_device *dev, byte *data,
  int dx, int raster, gx_bitmap_id id, int x, int y, int w, int h)
{	int result = (*rprocs->copy_color)(dev, data,
		dx, raster, id, x, y, w, h);
if ( gs_debug['V'] )
	dprintf7("[V]\tcopy_color(dev, data, %d, %d, 0x%lx, %d, %d, %d, %d)",
		 dx, raster, id, x, y, w, h),
	  trace_print_code(result);
	return result;
}
private int
trace_draw_line(gx_device *dev, int x0, int y0, int x1, int y1,
  gx_color_index color)
{	int result = (*rprocs->draw_line)(dev, x0, y0, x1, y1, color);
if ( gs_debug['V'] )
	dprintf5("[V]\tdraw_line(dev, %d, %d, %d, %d, %ld)",
		 x0, y0, x1, y1, (long)color),
	  trace_print_code(result);
	return result;
}
private int
trace_get_bits(gx_device *dev, int y, byte *data, uint size, int pad)
{	int result = (*rprocs->get_bits)(dev, y, data, size, pad);
if ( gs_debug['V'] )
	dprintf3("[V]\tget_bits(dev, %d, data, %d, %d)",
		y, size, pad),
	  trace_print_code(result);
	return result;
}
private int
trace_get_props(gx_device *dev, gs_prop_item *plist)
{	int result = (*rprocs->get_props)(dev, plist);
if ( gs_debug['V'] )
	dprintf("[V]\tget_props(dev, plist)\n"),
	  trace_print_code(result);
	return result;
}
private int
trace_put_props(gx_device *dev, gs_prop_item *plist, int count)
{	int result = (*rprocs->put_props)(dev, plist, count);
if ( gs_debug['V'] )
	dprintf1("[V]\tput_props(dev, plist, %d)", count),
	  trace_print_code(result);
	return result;
}

#endif
