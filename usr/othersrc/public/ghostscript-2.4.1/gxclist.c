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

/* gxclist.c */
/* Command list 'device' for Ghostscript. */
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxdevmem.h"			/* must precede gxclist.h */
#include "gxclist.h"

/* Patch a couple of things possibly missing from stdio.h. */
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif
#ifndef SEEK_END
#  define SEEK_END 2
#endif

#define cdev ((gx_device_clist *)dev)

/* Forward declarations of procedures */
private dev_proc_open_device(clist_open);
private dev_proc_get_initial_matrix(clist_get_initial_matrix);
private dev_proc_output_page(clist_output_page);
private dev_proc_map_rgb_color(clist_map_rgb_color);
private dev_proc_map_color_rgb(clist_map_color_rgb);
private dev_proc_fill_rectangle(clist_fill_rectangle);
private dev_proc_tile_rectangle(clist_tile_rectangle);
private dev_proc_copy_mono(clist_copy_mono);
private dev_proc_copy_color(clist_copy_color);
private dev_proc_get_bits(clist_get_bits);
private dev_proc_get_props(clist_get_props);
private dev_proc_put_props(clist_put_props);

/* The device descriptor */
private gx_device_procs clist_procs =
{	clist_open,
	clist_get_initial_matrix,
	gx_default_sync_output,
	clist_output_page,
	gx_default_close_device,
	clist_map_rgb_color,
	clist_map_color_rgb,
	clist_fill_rectangle,
	clist_tile_rectangle,
	clist_copy_mono,
	clist_copy_color,
	gx_default_draw_line,
	clist_get_bits,
	clist_get_props,
	clist_put_props
};
gx_device_clist gs_clist_device =
{	sizeof(gx_device_clist),
	&clist_procs,
	"command list",
	0, 0, 1, 1, no_margins, dci_black_and_white, 0,	/* generic */
	NULL, NULL, 0
};

/* ------ Define the command set and syntax ------ */

/* A command always consists of an operation followed by operands. */
/* The operands are implicit in the procedural code. */
typedef enum {
	cmd_op_misc = 0x00,		/* (see below) */
	  cmd_opv_end_run = 0x00,	/* (nothing) */
	  cmd_opv_set_tile_size = 0x01,	/* width, height */
	  cmd_opv_set_tile_phase = 0x02, /* x, y */
	cmd_op_set_color0 = 0x10,	/* color+2 in op byte, [color] */
	cmd_op_set_color1 = 0x20,	/* color+2 in op byte, [color] */
	cmd_op_set_tile_index = 0x30,	/* index */
	cmd_op_fill_rect = 0x40,	/* rect */
	cmd_op_fill_rect_short = 0x50,	/* dh in op byte,dx,dw | rect_short */
	cmd_op_fill_rect_tiny = 0x60,	/* dw in op byte, rect_tiny */
	cmd_op_tile_rect = 0x70,	/* rect */
	cmd_op_tile_rect_short = 0x80,	/* dh in op byte,dx,dw | rect_short */
	cmd_op_tile_rect_tiny = 0x90,	/* dw in op byte, rect_tiny */
	cmd_op_copy_mono = 0xa0,	/* rect, data_x, raster | */
					/* d_x+1 in op byte, x, y, w, h */
	cmd_op_copy_color = 0xb0,	/* rect, data_x, raster */
	cmd_op_set_tile_bits = 0xc0,	/* index, <bits> */
	cmd_op_delta_tile_bits = 0xd0,	/* n-1 in op byte, n x <offset, bits> */
	cmd_op_end
} gx_cmd_op;
/* Define the size of the largest command, */
/* not counting any bitmap. */
#define w sizeof(short)			/* size of coordinate */
#define d 3				/* size of tile delta */
private uint cmd_largest_size =
  max(1 + 6 * sizeof(short) /* copy_mono */, 2 + 16 * d /* delta_tile 15 */);
#undef d
#undef w
#ifdef DEBUG
private char *cmd_op_names[16] = {
  "misc", "set_color_0", "set_color_1", "set_tile",
  "fill_rect", "fill_rect_short", "fill_rect_tiny", "tile_rect",
  "tile_rect_short", "tile_rect_tiny", "copy_mono", "copy_color",
  "set_tile_bits", "delta_tile_bits", "?e0?", "?f0?"
};
private char *cmd_misc_op_names[16] = {
  "end_run", "set_tile_size", "set_tile_phase", "?03?",
  "?04?", "?05?", "?06?", "?07?",
  "?08?", "?09?", "?0a?", "?0b?",
  "?0c?", "?0d?", "?0e?", "?0f?"
};
private ulong cmd_op_counts[256];
private ulong cmd_tile_count, cmd_copy_count, cmd_delta_tile_count;
private ulong cmd_tile_reset, cmd_tile_found, cmd_tile_added;
private int
count_op(int op)
{	++cmd_op_counts[op];
if ( gs_debug['L'] )
	dprintf2(", %s %d\n", cmd_op_names[op >> 4], op & 0xf),
	fflush(dstderr);
	return op;
}
#  define count_add(v, n) (v += (n))
#else
#  define count_op(store_op) store_op
#  define count_add(v, n) 0
#endif
#define count_add1(v) count_add(v, 1)

typedef struct {
	short x, y, width, height;
} gx_cmd_rect;
typedef struct {
	byte dx, dwidth, dy, dheight;	/* dy and dheight are optional */
} gx_cmd_rect_short;
#define cmd_min_short (-128)
#define cmd_max_short 127
typedef struct {
	unsigned dx : 4;
	unsigned dy : 4;
} gx_cmd_rect_tiny;
#define cmd_min_tiny (-8)
#define cmd_max_tiny 7

/* Define the prefix on each command run in the writing buffer. */
typedef struct cmd_prefix_s cmd_prefix;
struct cmd_prefix_s {
	cmd_prefix *next;
	uint size;
};

/* Define the entries in the block file. */
typedef struct cmd_block_s {
	int band;
	long pos;			/* starting position in cfile */
} cmd_block;

/* Remember the current state of one band when writing or reading. */
struct gx_clist_state_s {
	gx_color_index color0, color1;	/* most recent colors */
	tile_slot *tile;		/* most recent tile */
	gs_int_point tile_phase;	/* most recent tile phase */
	gx_cmd_rect rect;		/* most recent rectangle */
	/* Following are only used when writing */
	cmd_prefix *head, *tail;	/* list of commands for band */
};
private tile_slot no_tile = { ~0L };

/* The initial values for a band state */
private gx_clist_state cls_initial =
   {	gx_no_color_index, gx_no_color_index, &no_tile,
	 { 0, 0 }, { 0, 0, 0, 0 },
	0, 0
   };

/* Define the size of the command buffer used for reading. */
/* This is needed to split up very large copy_ operations. */
#define cbuf_size 500

/* Initialize the device state */
private void clist_init_tiles(P1(gx_device_clist *));
private int
clist_open(gx_device *dev)
{	/*
	 * The buffer area (data, data_size) holds a tile cache and a
	 * set of block range bit masks when both writing and reading.
	 * The rest of the space is used for
	 * the command buffer and band state bookkeeping when writing,
	 * and for the rendering buffer (image device) when reading.
	 * For the moment, we divide the space up arbitrarily.
	 */
	byte *data = cdev->data;
	uint size = cdev->data_size;
#define alloc_data(n) data += (n), size -= (n)
	gx_device *target = cdev->target;
	int raster, nbands, band;
	gx_clist_state *states;
	uint state_size;
	cdev->ymin = cdev->ymax = -1;	/* render_init not done yet */
	cdev->tile_data = data;
	cdev->tile_data_size = (size / 5) & -4;	/* arbitrary! */
	alloc_data(cdev->tile_data_size);
	raster = (((target->width * target->color_info.depth + 31) >> 5) << 2) + sizeof(byte *);
	cdev->band_height = size / (uint)raster;
	nbands = target->height / cdev->band_height + 1;
	cdev->nbands = nbands;
#ifdef DEBUG
if ( gs_debug['l'] | gs_debug['L'] )
	dprintf4("[l]width=%d, raster=%d, band_height=%d, nbands=%d\n",
	         target->width, raster, cdev->band_height, cdev->nbands);
#endif
	state_size = nbands * sizeof(gx_clist_state);
	if ( state_size > size / 2 )
	  return -1;		/* not enough room */
	cdev->mdev.base = data;
	cdev->states = states = (gx_clist_state *)data;
	alloc_data(state_size);
	cdev->cbuf = data;
	cdev->cnext = data;
	cdev->cend = data + size;
	cdev->ccls = 0;
	for ( band = 0; band < nbands; band++, states++ )
	  *states = cls_initial;
#undef alloc_data
	cdev->tile_band_mask_size = (nbands + 31) / 32 * 4;
	cdev->tile_max_size = cdev->tile_data_size -
		(sizeof(tile_hash) * 2 + sizeof(tile_slot) +
		 cdev->tile_band_mask_size);
	clist_init_tiles(cdev);
	return 0;
}

/* (Re)initialize the tile cache. */
private void
clist_init_tiles(register gx_device_clist *cldev)
{	gx_clist_state *pcls;
	int i, hc;
	cldev->tile_slot_size =
	  sizeof(tile_slot) + cldev->tile_band_mask_size +
	  cldev->tile.raster * cldev->tile.size.y;
	cldev->tile_max_count = cldev->tile_data_size /
	  (sizeof(tile_hash) * 3 /*(worst case)*/ + cldev->tile_slot_size);
	hc = (cldev->tile_max_count - 1) * 2;
	while ( (hc + 1) & hc ) hc |= hc >> 1;	/* make mask */
	if ( hc >= cldev->tile_max_count * 3 ) hc >>= 1;
	if ( hc > 255 )		/* slot index in set_tile is only 1 byte */
	   {	hc = 255;
		if ( cldev->tile_max_count > 200 )
			cldev->tile_max_count = 200;
	   }
	cldev->tile_hash_mask = hc;
	hc++;				/* make actual size */
#ifdef DEBUG
if ( gs_debug['l'] | gs_debug['L'] )
	dprintf5("[l]tile.size=%dx%d, slot_size=%d, max_count=%d, hc=%d\n",
		 cldev->tile.size.x, cldev->tile.size.y,
		 cldev->tile_slot_size, cldev->tile_max_count, hc);
#endif
	cldev->tile_hash_table =
		(tile_hash *)(cldev->tile_data + cldev->tile_data_size) - hc;
	cldev->tile_count = 0;
	memset(cldev->tile_data, 0, cldev->tile_data_size);
	memset(cldev->tile_hash_table, -1, hc * sizeof(tile_hash));
	for ( i = 0, pcls = cldev->states; i < cldev->nbands; i++, pcls++ )
		pcls->tile = &no_tile;
	count_add1(cmd_tile_reset);
}

/* Forward the non-displaying operations to the target device. */
private void
clist_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{	(*cdev->target->procs->get_initial_matrix)(dev, pmat);
}
private gx_color_index
clist_map_rgb_color(gx_device *dev, gx_color_value red, gx_color_value green,
  gx_color_value blue)
{	return (*cdev->target->procs->map_rgb_color)(dev, red, green, blue);
}
private int
clist_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value rgb[3])
{	return (*cdev->target->procs->map_color_rgb)(dev, color, rgb);
}
private int
clist_get_props(gx_device *dev, gs_prop_item *plist)
{	gx_device *tdev = cdev->target;
	return (*tdev->procs->get_props)(tdev, plist);
}
private int
clist_put_props(gx_device *dev, gs_prop_item *plist, int count)
{	gx_device *tdev = cdev->target;
	return (*tdev->procs->put_props)(tdev, plist, count);
}

/* Print a bitmap for tracing */
#ifdef DEBUG
private void
cmd_print_bits(byte *data, int height, int raster)
{	int i, j;
	for ( i = 0; i < height; i++ )
	   {	byte *row = data + i * raster;
		dprintf("[L]");
		for ( j = 0; j < raster; j++ )
		  dprintf1(" %02x", row[j]);
		dputc('\n');
	   }
}
#else
#  define cmd_print_bits(data, height, raster)
#endif

/* ------ Writing ------ */

/* Utilities */

#define cmd_set_rect(rect)\
  ((rect).x = x, (rect).y = y,\
   (rect).width = width, (rect).height = height)

#define clist_write(f, str, len)\
  fwrite(str, 1, len, f)

/* Write out the buffered commands, and reset the buffer. */
private void
cmd_write_buffer(gx_device_clist *cldev)
{	FILE *cfile = cldev->cfile;
	FILE *bfile = cldev->bfile;
	int nbands = cldev->nbands;
	gx_clist_state *pcls;
	int band;
	for ( band = 0, pcls = cldev->states; band < nbands; band++, pcls++ )
	   {	cmd_prefix *cp = pcls->head;
		if ( cp != 0 )
		   {	cmd_block cb;
			cb.band = band;
			cb.pos = ftell(cfile);
#ifdef DEBUG
if ( gs_debug['l'] | gs_debug['L'] )
			dprintf2("[l]writing for band %d at %ld\n",
				 band, cb.pos);
#endif
			clist_write(bfile, (byte *)&cb, sizeof(cb));
			for ( ; cp != 0; cp = cp->next )
			  clist_write(cfile, (byte *)(cp + 1), cp->size);
			pcls->head = pcls->tail = 0;
			fputc(cmd_opv_end_run, cfile);
		   }
	   }
	cldev->cnext = cldev->cbuf;
	cldev->ccls = 0;
}

/* Add a command to the appropriate band list, */
/* and allocate space for its data. */
/* Return the pointer to the data area. */
private byte *
cmd_put_op(gx_device_clist *cldev, gx_clist_state *pcls, uint size)
{	byte *dp = cldev->cnext;
#ifdef DEBUG
if ( gs_debug['L'] )
	dprintf3("[L]band %d: size=%u, left=%d",
	         (int)(pcls - cldev->states), size, (int)(cldev->cend - dp));
#endif
	if ( size + (sizeof(cmd_prefix) + 4) > cldev->cend - dp )
	  { cmd_write_buffer(cldev);
	    return cmd_put_op(cldev, pcls, size);
	  }
	if ( cldev->ccls == pcls )
	  { /* We're adding another command for the same band. */
	    /* Tack it onto the end of the previous one. */
	    pcls->tail->size += size;
	  }
	else
	  { cmd_prefix *cp = (cmd_prefix *)(dp + (((byte *)0 - dp) & 3));
	    dp = (byte *)(cp + 1);
	    if ( pcls->tail != 0 ) pcls->tail->next = cp;
	    else pcls->head = cp;
	    pcls->tail = cp;
	    cldev->ccls = pcls;
	    cp->next = 0;
	    cp->size = size;
	  }
	cldev->cnext = dp + size;
	return dp;
}

/* We store all short quantities little-endian. */
/* This is OK, because we read them back little-endian explicitly. */
#define cmd_putw(w, dp)\
  (*dp = (w) & 0xff, dp[1] = (w) >> 8, dp += 2)

/* Write a short bitmap.  1 <= bwidth <= 3. */
private void
cmd_put_short_bits(register byte *dp, register byte *data,
  int raster, register int bwidth, register int height)
{	while ( --height >= 0 )
	   {	switch ( bwidth )
		   {
		case 3: dp[2] = data[2];
		case 2: dp[1] = data[1];
		case 1: dp[0] = data[0];
		   }
		dp += bwidth, data += raster;
	   }
}

private int
cmd_write_rect_cmd(gx_device *dev, gx_clist_state *pcls,
  int op, int x, int y, int width, int height)
{	int dx = x - pcls->rect.x;
	int dy = y - pcls->rect.y;
	int dwidth = width - pcls->rect.width;
	int dheight = height - pcls->rect.height;
#define check_ranges_1()\
  ((unsigned)(dx - rmin) <= (rmax - rmin) &&\
   (unsigned)(dy - rmin) <= (rmax - rmin) &&\
   (unsigned)(dwidth - rmin) <= (rmax - rmin))
#define check_ranges()\
  (check_ranges_1() &&\
   (unsigned)(dheight - rmin) <= (rmax - rmin))
#define rmin cmd_min_tiny
#define rmax cmd_max_tiny
	cmd_set_rect(pcls->rect);
	if ( dheight == 0 && check_ranges_1() )
	   {	byte *dp = cmd_put_op(cdev, pcls, 2);
		count_op(*dp = op + 0x20 + dwidth - rmin);
		dp[1] = (dx << 4) + dy - (rmin * 0x11);
	   }
#undef rmin
#undef rmax
#define rmin cmd_min_short
#define rmax cmd_max_short
	else if ( check_ranges() )
	   {	int dh = dheight - cmd_min_tiny;
		byte *dp;
		if ( (unsigned)dh <= cmd_max_tiny - cmd_min_tiny && dh != 0 &&
		     dy == 0
		   )
		   {	op += dh;
			dp = cmd_put_op(cdev, pcls, 3);
		   }
		else
		   {	dp = cmd_put_op(cdev, pcls, 5);
			dp[3] = dy - rmin;
			dp[4] = dheight - rmin;
		   }
		count_op(*dp = op + 0x10);
		dp[1] = dx - rmin;
		dp[2] = dwidth - rmin;
	   }
	else
	   {	byte *dp = cmd_put_op(cdev, pcls, 1 + sizeof(pcls->rect));
		count_op(*dp = op);
		memcpy(dp + 1, &pcls->rect, sizeof(pcls->rect));
	   }
	return 0;
}

private void
cmd_put_color(gx_device *dev, gx_clist_state *pcls,
  int op, gx_color_index color)
{	if ( (long)color >= -1 && (long)color <= 13 )
		count_op(*cmd_put_op(cdev, pcls, 1) = op + (int)color + 2);
	else
	   {	byte *dp = cmd_put_op(cdev, pcls, 1 + sizeof(color));
		count_op(*dp = op);
		memcpy(dp + 1, &color, sizeof(color));
	   }
}
private void
cmd_set_colors(gx_device *dev, gx_clist_state *pcls,
  gx_color_index color0, gx_color_index color1)
{	if ( color0 != pcls->color0 )
	   {	cmd_put_color(dev, pcls, cmd_op_set_color0, color0);
		pcls->color0 = color0;
	   }
	if ( color1 != pcls->color1 )
	   {	cmd_put_color(dev, pcls, cmd_op_set_color1, color1);
		pcls->color1 = color1;
	   }
}

/* Driver interface */

/* Macros for dividing up a single call into bands */
#define BEGIN_RECT\
   {	int yend = y + height;\
	int band_height = cdev->band_height;\
	do\
	   {	int band = y / band_height;\
		gx_clist_state *pcls = cdev->states + band;\
		height = band_height - y % band_height;\
		if ( yend - y < height ) height = yend - y;\
		   {
#define END_RECT\
		   }\
		y += height;\
	   }\
	while ( y < yend );\
   }

private int
clist_fill_rectangle(gx_device *dev, int x, int y, int width, int height,
  gx_color_index color)
{	BEGIN_RECT
	if ( color != pcls->color1 )
		cmd_set_colors(dev, pcls, pcls->color0, color);
	cmd_write_rect_cmd(dev, pcls, cmd_op_fill_rect, x, y, width, height);
	END_RECT
	return 0;
}

/* Compare unequal tiles.  Return -1 if unrelated, */
/* or 2<=N<=50 for the size of the delta encoding. */
private int
tile_diff(byte *old_data, byte *new_data, uint tsize, byte _ss *delta)
{	register ushort *old2, *new2;
	register ushort diff;
	int count;
	register int i;
	byte _ss *pd;
	if ( tsize > 128 ) return -1;
	old2 = (ushort *)old_data;
	new2 = (ushort *)new_data;
	count = 0;
	pd = delta + 2;			/* skip slot index */
	for ( i = 0; i < tsize; i += 2, old2++, new2++ )
	  if ( (diff = *new2 ^ *old2) != 0 )
#if arch_is_big_endian
#  define i_hi 0
#  define b_0(w) ((w) >> 8)
#  define b_1(w) ((byte)(w))
#else
#  define i_hi 1
#  define b_0(w) ((byte)(w))
#  define b_1(w) ((w) >> 8)
#endif
	   {	if ( count == 16 ) return -1;
		if ( diff & 0xff00 )
		   {	if ( diff & 0xff )
				*pd++ = 0x80 + i,
				*pd++ = b_0(diff),
				*pd++ = b_1(diff);
			else
				*pd++ = i + i_hi, *pd++ = diff >> 8;
		   }
		else			/* know diff != 0 */
			*pd++ = i + (1 - i_hi), *pd++ = (byte)diff;
		count++;
	   }
#undef b_0
#undef b_1
#undef i_hi
	delta[0] = (byte)cmd_op_delta_tile_bits + count - 1;
	return pd - delta;
}

/* Handle changing tiles for clist_tile_rectangle. */
/* We put this in a separate routine, even though it is called only once, */
/* to avoid cluttering up the main-line case of tile_rectangle. */
private int
clist_change_tile(gx_device_clist *cldev, gx_clist_state *pcls,
  gx_bitmap *tile)
{	uint tile_size = tile->raster * tile->size.y;
	tile_slot *old_tile, *new_tile;
	int slot_index;
	/* Look up the tile in the cache. */
top:	   {	gx_bitmap_id id = tile->id;
		uint probe = (uint)(id >> 16) + (uint)(id);
		old_tile = pcls->tile;
		for ( ; ; probe += 25 /* semi-random odd # */ )
		   {	tile_hash *hptr = cldev->tile_hash_table +
			  (probe & cldev->tile_hash_mask);
			if ( (slot_index = hptr->slot_index) < 0 ) /* empty entry */
			   {	/* Must change tiles.  Check whether the */
				/* tile size has changed. */
				if ( tile->size.x != cldev->tile.size.x ||
				     tile->size.y != cldev->tile.size.y
				   )
				   {	if ( tile->raster !=
					     ((tile->size.x + 31) >> 5) << 2 ||
					     tile_size > cldev->tile_max_size
					   )
						return -1;
					cldev->tile = *tile;	/* reset size, raster */
					clist_init_tiles(cldev);
					goto top;
				   }
				if ( cldev->tile_count == cldev->tile_max_count )
				   {	/* Punt. */
					clist_init_tiles(cldev);
					goto top;
				   }
				hptr->slot_index = slot_index =
				  cldev->tile_count++;
				new_tile = tile_slot_ptr(cldev, slot_index);
				new_tile->id = id;
				memcpy(ts_bits(cldev, new_tile), tile->data, tile_size);
				count_add1(cmd_tile_added);
#ifdef DEBUG
if ( gs_debug['L'] )
				dprintf3("[L]adding tile %d, hash=%d, id=%lx\n",
					 slot_index,
					 (int)(hptr - cldev->tile_hash_table),
					 id);
#endif
				break;
			   }
			new_tile = tile_slot_ptr(cldev, slot_index);
			if ( new_tile->id == id )
			   {	count_add1(cmd_tile_found);
#ifdef DEBUG
if ( gs_debug['L'] )
				dprintf1("[L]found tile %d\n", slot_index);
#endif
				break;
			   }
		   }
	   }
	/* Check whether this band knows about this tile yet. */
	   {	int band_index = pcls - cldev->states;
		byte pmask = 1 << (band_index & 7);
		byte *ppresent = ts_mask(new_tile) + (band_index >> 3);
		if ( *ppresent & pmask )
		   {	/* Tile is known, just put out the index. */
			byte *dp = cmd_put_op(cldev, pcls, 2);
			count_op(*dp = cmd_op_set_tile_index);
			dp[1] = slot_index;
		   }
		else
		   {	/* Tile is not known, put out the bits.  Use a */
			/* delta encoding or a short encoding if possible. */
			byte *new_data = ts_bits(cldev, new_tile);
			byte *dp;
			byte delta[2+16*3];
			int diff;
			*ppresent |= pmask;
			if ( old_tile != &no_tile &&
			     (diff = tile_diff(ts_bits(cldev, old_tile), new_data, tile_size, delta)) >= 0
			   )
			   {	/* Use delta representation */
				dp = cmd_put_op(cldev, pcls, diff);
				count_op(delta[0]);
				delta[1] = slot_index;
				memcpy(dp, delta, diff);
				count_add(cmd_delta_tile_count, diff - 2);
			   }
			else
			   {	if ( old_tile == &no_tile )
				   {	byte *dp = cmd_put_op(cldev, pcls,
						1 + sizeof(cldev->tile.size));
					count_op(*dp = (byte)cmd_opv_set_tile_size);
					memcpy(dp + 1, &cldev->tile.size,
					       sizeof(cldev->tile.size));
				   }
				if ( tile->size.x <= 16 )
				   {	dp = cmd_put_op(cldev, pcls, 2 + (tile_size >> 1));
					cmd_put_short_bits(dp + 2, new_data, tile->raster, 2, tile->size.y);
					count_add(cmd_tile_count, tile_size >> 1);
				   }
				else
				   {	dp = cmd_put_op(cldev, pcls, 2 + tile_size);
					memcpy(dp + 2, new_data, tile_size);
					count_add(cmd_tile_count, tile_size);
				   }
				count_op(*dp = (byte)cmd_op_set_tile_bits);
				dp[1] = slot_index;
			   }
		   }
	   }
	pcls->tile = new_tile;
	return 0;
}
private int
clist_tile_rectangle(gx_device *dev, gx_bitmap *tile, int x, int y,
  int width, int height, gx_color_index color0, gx_color_index color1,
  int px, int py)
{	BEGIN_RECT
	if ( tile->id != pcls->tile->id )
	   {	if ( clist_change_tile(cdev, pcls, tile) < 0 )
			return gx_default_tile_rectangle(dev, tile, x, y, width, height, color0, color1, px, py);
	   }
	if ( color0 != pcls->color0 || color1 != pcls->color1 )
		cmd_set_colors(dev, pcls, color0, color1);
	if ( px != pcls->tile_phase.x || py != pcls->tile_phase.y )
	   {	byte *dp = cmd_put_op(cdev, pcls, 1 + sizeof(pcls->tile_phase));
		count_op(*dp = (byte)cmd_opv_set_tile_phase);
		pcls->tile_phase.x = px;
		pcls->tile_phase.y = py;
		memcpy(dp + 1, &pcls->tile_phase, sizeof(pcls->tile_phase));
	   }
	cmd_write_rect_cmd(dev, pcls, cmd_op_tile_rect, x, y, width, height);
	END_RECT
	return 0;
}

private int
clist_copy_mono(gx_device *dev,
    byte *data, int data_x, int raster, gx_bitmap_id id,
    int x, int y, int width, int height,
    gx_color_index color0, gx_color_index color1)
{	int y0 = y;
	BEGIN_RECT
	gx_cmd_rect rect;
	uint dsize;
	int bwidth;
	byte *row = data + (y - y0) * raster;
	byte *dp;
	if ( color0 != pcls->color0 || color1 != pcls->color1 )
		cmd_set_colors(dev, pcls, color0, color1);
	cmd_set_rect(rect);
	if ( width >= 2 && (bwidth = (width + (data_x & 7) + 7) >> 3) <= 3 &&
	    height <= min(255, (cbuf_size - (1 + 2 * 2 + 2)) / bwidth)
	   )
	   {	dsize = height * bwidth;
		dp = cmd_put_op(cdev, pcls, 1 + 2 * 2 + 2 + dsize);
		count_op(*dp++ = (byte)cmd_op_copy_mono + (data_x & 7) + 1);
		cmd_putw(x, dp);
		cmd_putw(y, dp);
		*dp++ = width;
		*dp++ = height;
		row += data_x >> 3;
		cmd_put_short_bits(dp, row, raster, bwidth, height);
	   }
	else
	   {	dsize = height * raster;
		if ( dsize > cbuf_size )
		   {	/* We have to split it into pieces. */
			if ( height > 1 )
			   {	int h2 = height >> 1;
				clist_copy_mono(dev, data, data_x, raster,
					gx_no_bitmap_id, x, y, width, h2,
					color0, color1);
				return clist_copy_mono(dev, data + h2 * raster,
					data_x, raster, gx_no_bitmap_id,
					x, y + h2, width, height - h2,
					color0, color1);
			   }
			/* Split a single (very long) row. */
			   {	int w2 = width >> 1;
				clist_copy_mono(dev, data, data_x, raster,
					gx_no_bitmap_id, x, y, w2, 1,
					color0, color1);
				return clist_copy_mono(dev, data, data_x + w2,
					raster, gx_no_bitmap_id, x + w2, y,
					width - w2, 1, color0, color1);
			   }
		   }
		dp = cmd_put_op(cdev, pcls, 1 + sizeof(rect) + 4 + dsize);
		count_op(*dp++ = (byte)cmd_op_copy_mono);
		memcpy(dp, (byte *)&rect, sizeof(rect));
		dp += sizeof(rect);
		cmd_putw(data_x, dp);
		cmd_putw(raster, dp);
		memcpy(dp, row, dsize);
	   }
	pcls->rect = rect;
	count_add(cmd_copy_count, dsize);
	END_RECT
	return 0;
}

private int
clist_copy_color(gx_device *dev,
    byte *data, int data_x, int raster, gx_bitmap_id id,
    int x, int y, int width, int height)
{	int y0 = y;
	BEGIN_RECT
	gx_cmd_rect rect;
	uint dsize = height * raster;
	byte *dp;
	if ( dsize > cbuf_size )
	   {	/* We have to split it into pieces. */
		if ( height > 1 )
		   {	int h2 = height >> 1;
			clist_copy_color(dev, data, data_x, raster,
				gx_no_bitmap_id, x, y, width, h2);
			return clist_copy_color(dev, data + h2 * raster, gx_no_bitmap_id,
				data_x, raster, x, y + h2, width, height - h2);
		   }
		/* Split a single (very long) row. */
		   {	int w2 = width >> 1;
			clist_copy_color(dev, data, data_x, raster,
				gx_no_bitmap_id, x, y, w2, 1);
			return clist_copy_color(dev, data, data_x + w2,
				raster, gx_no_bitmap_id, x + w2, y,
				width - w2, 1);
		   }

	   }
	cmd_set_rect(rect);
	dp = cmd_put_op(cdev, pcls, 1 + sizeof(rect) + 4 + dsize);
	count_op(*dp++ = (byte)cmd_op_copy_color);
	memcpy(dp, (byte *)&rect, sizeof(rect));
	pcls->rect = rect;
	dp += sizeof(rect);
	cmd_putw(data_x, dp);
	cmd_putw(raster, dp);
	memcpy(dp, data + (y - y0) * raster, dsize);
	END_RECT
	return 0;
}

/* ------ Reading/rendering ------ */

/* Clean up after rendering a page. */
private int
clist_output_page(gx_device *dev, int num_copies, int flush)
{	if ( flush )
	   {	rewind(cdev->cfile);
		rewind(cdev->bfile);
		cdev->bfile_end_pos = 0;
	   }
	else
	   {	fseek(cdev->cfile, 0L, SEEK_END);
		fseek(cdev->bfile, 0L, SEEK_END);
	   }
	return clist_open(dev);		/* reinitialize */
}

private int clist_render_init(P1(gx_device_clist *));
private int clist_render(P3(gx_device_clist *, gx_device *, int));

/* Copy scan lines to the client.  This is where rendering gets done. */
private int
clist_get_bits(gx_device *dev, int start_y,
  byte *str, uint size, int pad_to_word)
{	int y = start_y;
	byte *dest = str;
	gx_device_memory *mdev = &cdev->mdev;
	uint bytes_per_line;
	uint count, left;
	/* Initialize for rendering if we haven't done so yet. */
	if ( cdev->ymin < 0 )
		clist_render_init(cdev);
	bytes_per_line = gx_device_bytes_per_scan_line((gx_device *)mdev,
						       pad_to_word);
	count = min(size / bytes_per_line,
		    cdev->target->height - start_y);
	/* Render bands and copy them incrementally. */
	for ( left = count; left; )
	   {	int n;
		if ( !(y >= cdev->ymin && y < cdev->ymax) )
		   {	int band = y / mdev->height;
			int code;
			rewind(cdev->bfile);
			(*mdev->procs->open_device)((gx_device *)mdev);	/* reinitialize */
			code = clist_render(cdev, (gx_device *)mdev, band);
			if ( code < 0 ) return code;
			cdev->ymin = band * mdev->height;
			cdev->ymax = cdev->ymin + mdev->height;
		   }
		n = min(cdev->ymax - y, left);
		(*mdev->procs->get_bits)((gx_device *)mdev,
					 y - cdev->ymin, dest,
					 bytes_per_line * n, pad_to_word);
		y += n, dest += bytes_per_line * n, left -= n;
	   }
	return count;
}

#undef cdev

/* Initialize for reading. */
private int
clist_render_init(gx_device_clist *cdev)
{	gx_device *target = cdev->target;
	byte *base = cdev->mdev.base;	/* save */
	int depth = target->color_info.depth;
	uint raster = ((target->width * depth + 31) >> 5) << 2;
	gx_device_memory *mdev = gdev_mem_device_for_bits(depth);
	if ( mdev == 0 )
		return_error(gs_error_rangecheck);
	cmd_write_buffer(cdev);		/* flush buffer */
	/* Write the terminating entry in the block file. */
	/* Note that because of copypage, there may be many such entries. */
	   {	cmd_block cb;
		cb.band = -1;
		cb.pos = ftell(cdev->cfile);
		clist_write(cdev->bfile, (byte *)&cb, sizeof(cb));
		cdev->bfile_end_pos = ftell(cdev->bfile);
	   }
	cdev->mdev = *mdev;
	cdev->mdev.base = base;		/* restore */
	(*target->procs->get_initial_matrix)(target, &cdev->mdev.initial_matrix);
	cdev->mdev.width = target->width;
	cdev->mdev.height = cdev->band_height;
	cdev->mdev.raster = raster;
	cdev->ymin = cdev->ymax = 0;
#ifdef DEBUG
if ( gs_debug['l'] | gs_debug['L'] )
   {	int ci, cj;
	dprintf3("[l]counts: tile = %ld, copy = %ld, delta = %ld\n",
	         cmd_tile_count, cmd_copy_count, cmd_delta_tile_count);
	dprintf3("           reset = %ld, found = %ld, added = %ld\n",
	         cmd_tile_reset, cmd_tile_found, cmd_tile_added);
	for ( ci = 0; ci < 0x100; ci += 0x10 )
	   {	dprintf1("[l]  %s =", cmd_op_names[ci >> 4]);
		for ( cj = ci; cj < ci + 0x10; cj++ )
			dprintf1(" %ld", cmd_op_counts[cj]);
		dputs("\n");
	   }
   }
#endif
	return 0;
}

/* Render one band to a specified target device. */
#define assign_getw(var, p)\
  (var = *p + ((uint)p[1] << 8), p += 2)
typedef byte _ss *cb_ptr;
private void clist_read(P3(FILE *, byte *, uint));
private cb_ptr clist_read_short_bits(P6(FILE *, byte *, int, int, cb_ptr, cb_ptr));
private int
clist_render(gx_device_clist *cdev, gx_device *tdev, int band)
{	byte cbuf[cbuf_size];
	byte bits[4 * 255];		/* for short copy_mono bits */
	register cb_ptr cbp;
	cb_ptr cb_limit;
	cb_ptr cb_end;
	FILE *file = cdev->cfile;
	FILE *bfile = cdev->bfile;
	int y0 = band * cdev->band_height;
	gx_clist_state state;
	gx_bitmap state_tile;
	uint tile_bits_size;		/* size of bits of each tile */
	gs_int_point tile_phase;
	cmd_block b_this;
	long pos;
	uint left;
#define cmd_read_var(ptr, cbp)\
  memcpy(ptr, cbp, sizeof(*ptr)),\
  cbp += sizeof(*ptr)
#define cmd_read(ptr, vsize, cbp)\
  if ( cb_end - cbp >= vsize )\
    memcpy(ptr, cbp, vsize), cbp += vsize;\
  else\
   { uint cleft = cb_end - cbp;\
     memcpy(ptr, cbp, cleft); vsize -= cleft;\
     clist_read(file, ptr + cleft, vsize);\
     cbp = cb_end;\
   }
#define cmd_read_short_bits(ptr, bw, ht, cbp)\
  cbp = clist_read_short_bits(file, ptr, bw, ht, cbp, cb_end)
	state = cls_initial;
	state_tile.id = 0;
	tile_phase.x = tile_phase.y = 0;
trd:	clist_read(bfile, (byte *)&b_this, sizeof(b_this));
top:	/* Find the next run of commands for this band. */
	if ( b_this.band < 0 && ftell(bfile) == cdev->bfile_end_pos )
		return 0;	/* end of bfile */
	if ( b_this.band != band ) goto trd;
	pos = b_this.pos;
	clist_read(bfile, (byte *)&b_this, sizeof(b_this));
	fseek(file, pos, SEEK_SET);
	left = (uint)(b_this.pos - pos);
	cb_limit = cbuf + (cbuf_size - cmd_largest_size);
	cb_end = cbuf + cbuf_size;
	cbp = cb_end;
	for ( ; ; )
	   {	int op;
		uint bytes;
		int data_x, raster;
		int code;
		cb_ptr source;
		gx_color_index _ss *pcolor;
		/* Make sure the buffer contains a full command. */
		if ( cbp > cb_limit )
		   {	uint nread;
			memcpy(cbuf, cbp, cb_end - cbp);
			cbp = cbuf + (cb_end - cbp);
			nread = cb_end - cbp;
			if ( nread > left ) nread = left;
			clist_read(file, cbp, nread);
			cb_end = cbp + nread;
			cbp = cbuf;
			left -= nread;
			if ( cb_limit > cb_end ) cb_limit = cb_end;
		   }
		op = *cbp++;
#ifdef DEBUG
if ( gs_debug['L'] )
		dprintf2("[L]%s %d:\n", cmd_op_names[op >> 4], op & 0xf);
#endif
		switch ( op >> 4 )
		   {
		case cmd_op_misc >> 4:
			switch ( op )
			   {
			case cmd_opv_end_run:
				goto top;
			case cmd_opv_set_tile_size:
				cmd_read_var(&state_tile.size, cbp);
				state_tile.raster = ((state_tile.size.x + 31) >> 5) << 2;
				/* We can't actually know the rep_size, */
				/* so we play it safe. */
				state_tile.rep_width = state_tile.size.x;
				state_tile.rep_height = state_tile.size.y;
				cdev->tile_slot_size = tile_bits_size =
					state_tile.raster * state_tile.size.y;
				break;
			case cmd_opv_set_tile_phase:
				cmd_read_var(&state.tile_phase, cbp);
				break;
			default:
				goto bad_op;
			   }
			tile_phase.x = state.tile_phase.x % state_tile.size.x;
			tile_phase.y = (state.tile_phase.y + y0) % state_tile.size.y;
			continue;
		case cmd_op_set_color0 >> 4:
			pcolor = &state.color0;
			goto set_color;
		case cmd_op_set_color1 >> 4:
			pcolor = &state.color1;
set_color:		if ( op & 0xf )
				*pcolor = (gx_color_index)(long)((op & 0xf) - 2);
			else
				cmd_read_var(pcolor, cbp);
			continue;
		case cmd_op_set_tile_index >> 4:
			state_tile.data = (byte *)tile_slot_ptr(cdev, *cbp);
			cbp++;
			continue;
		case cmd_op_copy_mono >> 4:
			if ( op & 0xf )
			   {	assign_getw(state.rect.x, cbp);
				assign_getw(state.rect.y, cbp);
				state.rect.width = *cbp++;
				state.rect.height = *cbp++;
				break;
			   }
			/* falls through */
		case cmd_op_fill_rect >> 4:
		case cmd_op_tile_rect >> 4:
		case cmd_op_copy_color >> 4:
			cmd_read_var(&state.rect, cbp);
			break;
		case cmd_op_fill_rect_short >> 4:
		case cmd_op_tile_rect_short >> 4:
			state.rect.x += *cbp + cmd_min_short;
			state.rect.width += cbp[1] + cmd_min_short;
			if ( op & 0xf )
			   {	state.rect.height += (op & 0xf) + cmd_min_tiny;
				cbp += 2;
			   }
			else
			   {	state.rect.y += cbp[2] + cmd_min_short;
				state.rect.height += cbp[3] + cmd_min_short;
				cbp += 4;
			   }
			break;
		case cmd_op_fill_rect_tiny >> 4:
		case cmd_op_tile_rect_tiny >> 4:
		   {	int txy = *cbp++;
			state.rect.x += (txy >> 4) + cmd_min_tiny;
			state.rect.y += (txy & 0xf) + cmd_min_tiny;
			state.rect.width += (op & 0xf) + cmd_min_tiny;
		   }	break;
		case cmd_op_set_tile_bits >> 4:
			state_tile.data = (byte *)tile_slot_ptr(cdev, *cbp);
			cbp++;
			if ( state_tile.size.x <= 16 )
			   {	cmd_read_short_bits(state_tile.data, 2, state_tile.size.y, cbp);
			   }
			else
			   {	bytes = tile_bits_size;
				cmd_read(state_tile.data, bytes, cbp);
			   }
#ifdef DEBUG
if ( gs_debug['L'] )
			cmd_print_bits(state_tile.data, state_tile.size.y,
				       state_tile.raster);
#endif
			continue;
		case cmd_op_delta_tile_bits >> 4:
		   {	byte *new_data = (byte *)tile_slot_ptr(cdev, *cbp);
			cbp++;
			memcpy(new_data, state_tile.data, tile_bits_size);
			state_tile.data = new_data;
			do
			   {	uint offset = *cbp;
				if ( offset < 0x80 )
					new_data[offset] ^= cbp[1],
					cbp += 2;
				else
					offset -= 0x80,
					new_data[offset] ^= cbp[1],
					new_data[offset + 1] ^= cbp[2],
					cbp += 3;
			   }
			while ( op-- & 0xf );
		   }	continue;
		default:
bad_op:			printf/*lprintf5*/("Bad op %02x band %d file pos %ld buf pos %d/%d\n",
				 op, band, ftell(file), (int)(cbp - cbuf), (int)(cb_end - cbuf));
			   {	cb_ptr pp;
				for ( pp = cbuf; pp < cb_end; pp += 10 )
				  printf/*lprintf10*/(" %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n",
					   pp[0], pp[1], pp[2], pp[3], pp[4],
					   pp[5], pp[6], pp[7], pp[8], pp[9]);
			   }
					
			gs_exit(1);
			return -1;
		   }
#ifdef DEBUG
if ( gs_debug['L'] )
		dprintf4("[L]  x=%d y=%d w=%d h=%d\n",
			 state.rect.x, state.rect.y, state.rect.width,
			 state.rect.height);
#endif
		switch ( op >> 4 )
		   {
		case cmd_op_fill_rect >> 4:
		case cmd_op_fill_rect_short >> 4:
		case cmd_op_fill_rect_tiny >> 4:
			code = (*tdev->procs->fill_rectangle)
			  (tdev, state.rect.x, state.rect.y - y0,
			   state.rect.width, state.rect.height, state.color1);
			break;
		case cmd_op_tile_rect >> 4:
		case cmd_op_tile_rect_short >> 4:
		case cmd_op_tile_rect_tiny >> 4:
			code = (*tdev->procs->tile_rectangle)
			  (tdev, &state_tile,
			   state.rect.x, state.rect.y - y0,
			   state.rect.width, state.rect.height,
			   state.color0, state.color1,
			   tile_phase.x, tile_phase.y);
			break;
		case cmd_op_copy_mono >> 4:
			if ( op & 0xf )
			   {	data_x = (op & 0xf) - 1;
				raster = 4;
				cmd_read_short_bits(bits, (data_x + state.rect.width + 7) >> 3, state.rect.height, cbp);
				source = bits;
				goto copy;
			   }
			/* falls through */
		case cmd_op_copy_color >> 4:
			assign_getw(data_x, cbp);
			assign_getw(raster, cbp);
			bytes = state.rect.height * raster;
			/* copy_mono and copy_color have ensured that */
			/* the bits will fit in a single buffer. */
			cmd_read(cbuf, bytes, cbp);
			source = cbuf;
copy:
#ifdef DEBUG
if ( gs_debug['L'] )
   {			dprintf2("[L]  data_x=%d raster=%d\n",
				 data_x, raster);
			cmd_print_bits(source, state.rect.height, raster);
   }
#endif
			code = (op >> 4 == (byte)cmd_op_copy_mono >> 4 ?
			  (*tdev->procs->copy_mono)
			    (tdev, source, data_x, raster, gx_no_bitmap_id,
			     state.rect.x, state.rect.y - y0,
			     state.rect.width, state.rect.height,
			     state.color0, state.color1) :
			  (*tdev->procs->copy_color)
			    (tdev, source, data_x, raster, gx_no_bitmap_id,
			     state.rect.x, state.rect.y - y0,
			     state.rect.width, state.rect.height));
			break;
		   }
		if ( code < 0 ) return_error(code);
	   }
}
/* The typical implementations of fread and fseek */
/* are extremely inefficient for small counts, */
/* so we use loops instead. */
private void
clist_read(FILE *f, byte *str, uint len)
{	switch ( len )
	   {
	default: fread(str, 1, len, f); break;
	case 8: *str++ = (byte)getc(f);
	case 7: *str++ = (byte)getc(f);
	case 6: *str++ = (byte)getc(f);
	case 5: *str++ = (byte)getc(f);
	case 4: *str++ = (byte)getc(f);
	case 3: *str++ = (byte)getc(f);
	case 2: *str++ = (byte)getc(f);
	case 1: *str = (byte)getc(f);
	   }
}
/* Read a short bitmap */
private cb_ptr
clist_read_short_bits(FILE *file, byte *data, register int bwidth, int height,
  cb_ptr cbp, cb_ptr cb_end)
{	uint bytes = bwidth * height;
	byte *pdata = data + bytes;
	byte *udata = data + (height << 2);
	cmd_read(data, bytes, cbp);
	while ( --height > 0 )		/* first row is in place already */
	   {	udata -= 4, pdata -= bwidth;
		switch ( bwidth )
		   {
		case 3: udata[2] = pdata[2];
		case 2: udata[1] = pdata[1];
		case 1: udata[0] = pdata[0];
		   }
	   }
	return cbp;
}
