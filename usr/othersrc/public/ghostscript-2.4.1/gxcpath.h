/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* gxcpath.h */
/* Interface to clipping devices */
/* requires gzpath.h */

/* A cached cursor, relative to a known gx_clip_list. */
typedef struct rect_cursor_s {
	gx_clip_rect *rptr;
} rect_cursor;

/* Device for accumulating a rectangle list. */
typedef struct gx_device_accum_s {
	gx_device_common;
	gs_memory_procs memory_procs;	/* set by client */
	gs_int_rect bbox;
	gx_clip_list list;
	gx_clip_rect *last;
} gx_device_accum;
extern gx_device_accum gs_accum_device;

/* Device for clipping with a rectangle list. */
typedef struct gx_clip_device_s {
	gx_device_common;
	gx_device *target;		/* set by client */
	gx_clip_list list;		/* set by client */
	rect_cursor current;
} gx_device_clip;
extern gx_device_clip gs_clip_device;
