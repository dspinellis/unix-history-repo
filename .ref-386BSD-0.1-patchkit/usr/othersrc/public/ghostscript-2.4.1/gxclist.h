/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gxclist.h */
/* Command list definitions for Ghostscript. */
/* Requires gxdevice.h and gxdevmem.h */

/*
 * A command list is essentially a compressed list of driver calls.
 * Command lists are used to record an image that must be rendered in bands
 * for a high-resolution printer.  In the future, they may be used
 * for other purposes as well, such as providing a more compact representation
 * of cached characters than a full bitmap (at high resolution), or
 * representing fully rendered user paths (a Level 2 feature).
 */

/* A command list device outputs commands to a stream, */
/* then reads them back to render in bands. */
typedef struct gx_device_clist_s gx_device_clist;
typedef struct gx_clist_state_s gx_clist_state;
typedef struct {
	int slot_index;
} tile_hash;
typedef struct {
	gx_bitmap_id id;
	/* byte band_mask[]; */
	/* byte bits[]; */
} tile_slot;
struct gx_device_clist_s {
	gx_device_common;		/* (see gxdevice.h) */
	/* Following must be set before writing or reading. */
	gx_device *target;		/* device for which commands */
					/* are being buffered */
	byte *data;			/* buffer area */
	uint data_size;			/* size of buffer */
	FILE *cfile;			/* command list file */
	FILE *bfile;			/* command list block file */
	/* Following are used only when writing. */
	gx_clist_state *states;		/* current state of each band */
	byte *cbuf;			/* start of command buffer */
	byte *cnext;			/* next slot in command buffer */
	byte *cend;			/* end of command buffer */
	gx_clist_state *ccls;		/* clist_state of last command */
	/* Following is the tile cache, used only when writing. */
	gx_bitmap tile;			/* current tile cache parameters */
					/* (data pointer & id not used) */
	tile_hash *tile_hash_table;	/* hash table for tile cache: */
					/* -1 = unused, >= 0 = slot index */
	uint tile_max_size;		/* max size of a single tile */
	uint tile_hash_mask;		/* size of tile hash table -1 */
	uint tile_band_mask_size;	/* size of band mask preceding */
					/* each tile in the cache */
	uint tile_count;		/* # of occupied entries in cache */
	uint tile_max_count;		/* capacity of the cache */
	/* Following are used for both writing and reading. */
	byte *tile_data;		/* data for cached tiles */
	uint tile_data_size;		/* total size of tile cache data */
	uint tile_slot_size;		/* size of each slot in tile cache */
#define tile_slot_ptr(cldev,index)\
  (tile_slot *)((cldev)->tile_data + (index) * (cldev)->tile_slot_size)
#define ts_mask(pts) (byte *)((pts) + 1)
#define ts_bits(cldev,pts) (ts_mask(pts) + (cldev)->tile_band_mask_size)
	/* Following are set when writing, read when reading. */
	int band_height;		/* height of each band */
	int nbands;			/* # of bands */
	long bfile_end_pos;		/* ftell at end of bfile */
	/* Following are used only when reading. */
	int ymin, ymax;			/* current band */
	gx_device_memory mdev;
};
extern gx_device_clist
	gs_clist_device;
