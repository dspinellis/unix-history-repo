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

/* gdevmem.h */
/* Private definitions for memory devices. */

/*
   The representation for a "memory" device is simply a
   contiguous bitmap stored in something like the PostScript
   representation, i.e., each scan line (in left-to-right order), padded
   to a byte or word boundary, followed immediately by the next one.

   The representation of strings in the Ghostscript interpreter limits
   the size of a string to 64K-1 bytes, which means we can't simply use
   a string for the contents of a memory device.
   We get around this problem by making the client read out the
   contents of a memory device bitmap in pieces.

   On PCs with segmented architectures, there is no way to
   obtain a contiguous block of storage larger than 64K bytes,
   which isn't big enough for a full-screen bitmap, even in monochrome.
   If this ever becomes a problem, we will use a banded representation.

   Even though the scan lines are stored contiguously, we store a table
   of their base addresses, because indexing into it is faster than
   the multiplication that would otherwise be needed.
*/

/* ------ Generic macros ------ */

/* Macro for declaring the essential device procedures. */
#define declare_mem_map_procs(map_rgb_color, map_color_rgb)\
  private dev_proc_map_rgb_color(map_rgb_color);\
  private dev_proc_map_color_rgb(map_color_rgb)
#define declare_mem_procs(copy_mono, copy_color, fill_rectangle)\
  private dev_proc_copy_mono(copy_mono);\
  private dev_proc_copy_color(copy_color);\
  private dev_proc_fill_rectangle(fill_rectangle)

/* Macro for generating the procedure record in the device descriptor */
extern dev_proc_open_device(mem_open);
extern dev_proc_get_initial_matrix(mem_get_initial_matrix);
extern dev_proc_get_bits(mem_get_bits);
#define mem_procs(map_rgb_color, map_color_rgb, copy_mono, copy_color, fill_rectangle)\
{	mem_open,\
	mem_get_initial_matrix,\
	gx_default_sync_output,\
	gx_default_output_page,\
	gx_default_close_device,\
	map_rgb_color,			/* differs */\
	map_color_rgb,			/* differs */\
	fill_rectangle,			/* differs */\
	gx_default_tile_rectangle,\
	copy_mono,			/* differs */\
	copy_color,			/* differs */\
	gx_default_draw_line,\
	mem_get_bits,\
	gx_default_get_props,\
	gx_default_put_props\
}

/*
 * Macro for generating the device descriptor.
 * Various compilers have problems with the obvious definition
 * for max_value, namely:
 *	(depth >= 8 ? 255 : (1 << depth) - 1)
 * I tried changing (1 << depth) to (1 << (depth & 15)) to forestall bogus
 * error messages about invalid shift counts, but the H-P compiler chokes
 * on this.  Since the only values of depth we ever plan to support are
 * powers of 2 (and 24), we just go ahead and enumerate them.
 */
#define max_value(depth)\
  (depth >= 8 ? 255 : depth == 4 ? 15 : depth == 2 ? 3 : 1)
#define mem_device(name, depth, procs)\
{	sizeof(gx_device_memory),\
	&procs,			/* differs */\
	name,			/* differs */\
	0, 0,			/* x and y extent (filled in) */\
	72, 72,			/* density (makes initclip come out right) */\
	no_margins,		/* margins */\
	   {	(depth >= 4 ? 3 : 1),	/* num_components */\
		depth,\
		max_value(depth),	/* max_gray */\
		max_value(depth),	/* max_rgb */\
		max_value(depth) + 1,	/* dither_gray */\
		max_value(depth) + 1,	/* dither_color */\
	   },\
	0,			/* not open yet */\
	identity_matrix_body,	/* initial matrix (filled in) */\
	0,			/* raster (filled in) */\
	(byte *)0,		/* base (filled in) */\
	(byte **)0,		/* line_ptrs (filled in by mem_open) */\
	0,			/* initial bytes_le (filled in by mem_open) */\
	0,			/* invert (filled in for mono) */\
	0, (byte *)0		/* palette (filled in for color) */\
}

/* Macro for casting gx_device argument */
#define mdev ((gx_device_memory *)dev)

/* Macro for rectangle arguments (x,y,w,h) */
#define check_rect()\
	if ( w <= 0 || h <= 0 ) return 0;\
	if ( (x | y) < 0 || x > mdev->width - w || y > mdev->height - h )\
		return -1

/*
 * Macros for processing bitmaps in the largest possible chunks.
 * Bits within a byte are always stored big-endian;
 * bytes are normally stored in the natural order for the platform,
 * but the client can force them into big-endian order (which is required
 * for the source of copy_mono) by calling gdev_mem_ensure_byte_order.
 * (mem_get_bits swaps bytes if necessary.)
 * Note that we use type uint for register variables holding a chunk:
 * for this reason, the chunk size cannot be larger than uint.
 */
/******
 ****** Note: we assume that ints are 2 or 4 bytes in size!
 ****** (This is required by the clog2_bytes macro.)
 ******/
/* Generic macros for chunk accessing. */
#define cbytes(ct) ((int)sizeof(ct))	/* sizeof may be unsigned */
#  define chunk_bytes cbytes(chunk)
#define clog2_bytes(ct) ((int)sizeof(ct)>>1)	/* works for 1,2,4! */
#  define chunk_log2_bytes clog2_bytes(chunk)
#define cbits(ct) ((int)sizeof(ct)*8)	/* sizeof may be unsigned */
#  define chunk_bits cbits(chunk)
#define clog2_bits(ct) (clog2_bytes(ct)+3)
#  define chunk_log2_bits clog2_bits(chunk)
#define cbit_mask(ct) (cbits(ct)-1)
#  define chunk_bit_mask cbit_mask(chunk)
/*
 * The obvious definition for cmask is:
 *	#define cmask(ct) ((ct)~(ct)0)
 * but this doesn't work on the VAX/VMS compiler, which fails to truncate
 * the value to 16 bits when ct is ushort.
 * Instead, we have to generate the mask with no extra 1-bits.
 * We can't do this in the obvious way:
 *	#define cmask(ct) ((1 << (sizeof(ct) * 8)) - 1)
 * because some compilers won't allow a shift of 32 bits.  Instead,
 * we have to do something really awkward:
 */
#define cmask(ct) ((ct) (((((ct)1 << (sizeof(ct)*8-2)) - 1) << 2) + 3))
#  define chunk_all_bits cmask(chunk)
/*
 * The obvious definition for chi_bits is:
 *	#define chi_bits(ct,n) (cmask(ct)-(cmask(ct)>>(n)))
 * but this doesn't work on the DEC/MIPS compilers.
 * Instead, we have to restrict chi_bits to only working for values of n
 * between 0 and cbits(ct)-1, and use
 */
#define chi_bits(ct,n) (ct)(~(ct)1 << (cbits(ct)-1 - (n)))
#  define chunk_hi_bits(n) chi_bits(chunk,n)

/* Define whether this is a machine where chunks are long, */
/* but the machine can't shift a long by its full width. */
#define arch_cant_shift_full_chunk\
  (arch_is_big_endian && !arch_ints_are_short && !arch_can_shift_full_long)

/* Macros for scan line access. */
/* x_to_byte is different for each number of bits per pixel. */
/* Note that these macros depend on the definition of chunk: */
/* each procedure that uses the scanning macros should #define */
/* (not typedef) chunk as either uint or byte. */
#define scan_line_base(dev,y) (dev->line_ptrs[y])
#define declare_scan_ptr(ptr)   declare_scan_ptr_as(ptr, chunk *)
#define declare_scan_ptr_as(ptr,ptype)\
	register ptype ptr; uint draster
#define inc_chunk_ptr(ptr,delta)\
	ptr = (chunk *)((byte *)ptr + (delta))
#define setup_rect(ptr)   setup_rect_as(ptr, chunk *)
#define setup_rect_as(ptr,ptype)\
	draster = mdev->raster;\
	ptr = (ptype)(scan_line_base(mdev, y) +\
		(x_to_byte(x) & -chunk_bytes))
