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

/* gdevmem1.c */
/* Generic and monobit "memory" (stored bitmap) device */
/* for Ghostscript library. */
#include "memory_.h"
#include "gs.h"
#include "gxdevice.h"
#include "gxdevmem.h"			/* semi-public definitions */
#include "gdevmem.h"			/* private definitions */

/* Define the chunk size for monobit operations. */
#if arch_is_big_endian
#  define mono_chunk uint
#else
#  define mono_chunk ushort
#endif

/* ------ Generic code ------ */

/* Return the appropriate memory device for a given */
/* number of bits per pixel (0 if none suitable). */
gx_device_memory *
gdev_mem_device_for_bits(int bits_per_pixel)
{	switch ( bits_per_pixel )
	   {
	case 1: return &mem_mono_device;
	case 2: return &mem_mapped2_color_device;
	case 4: return &mem_mapped4_color_device;
	case 8: return &mem_mapped8_color_device;
	case 16: return &mem_true16_color_device;
	case 24: return &mem_true24_color_device;
	case 32: return &mem_true32_color_device;
	default: return 0;
	   }
}

/* Compute the size of the bitmap storage, */
/* including the space for the scan line pointer table. */
/* Note that scan lines are padded to a multiple of 4 bytes. */
ulong
gdev_mem_bitmap_size(gx_device_memory *dev)
{	unsigned raster =
		((dev->width * dev->color_info.depth + 31) >> 5) << 2;
	mdev->raster = raster;
	return (ulong)dev->height * (raster + sizeof(byte *));
}

/* 'Open' the memory device and create the scan line table. */
int
mem_open(gx_device *dev)
{	byte *scan_line = mdev->base;
	uint raster = mdev->raster;
	byte **pptr = (byte **)(scan_line + (uint)dev->height * raster);
	byte **pend = pptr + dev->height;
	mdev->line_ptrs = pptr;
	while ( pptr < pend )
	   {	*pptr++ = scan_line;
		scan_line += raster;
	   }
	mdev->bytes_le =
#if arch_is_big_endian
		0
#else
		/* NOTE: mem_mono_get_bits relies on the fact that */
		/* sizeof(mono_chunk) == 2! */
		(mdev->color_info.depth < 8 ? sizeof(mono_chunk) : 0);
#endif
		;
	return 0;
}

/* Return the initial transformation matrix */
void
mem_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{	*pmat = mdev->initial_matrix;
}

/* Test whether a device is a memory device */
int
gs_device_is_memory(const gx_device *dev)
{	/* We can't just compare the procs, or even an individual proc, */
	/* because we might be tracing.  Compare the device name, */
	/* and hope for the best. */
	const char *name = dev->dname;
	int i;
	for ( i = 0; i < 6; i++ )
	  if ( name[i] != "image("[i] ) return 0;
	return 1;
}

/* Ensure that the data bytes are in big-endian order. */
/* This is never called on big-endian platforms; */
/* on little-endian platforms, the chunk size is ushort, */
/* regardless of the size of an int. */
void
gdev_mem_ensure_byte_order(gx_device_memory *dev)
{
#if !arch_is_big_endian
	if ( !dev->bytes_le ) return;	/* already in order */
	memswab(dev->base, dev->base, dev->raster * dev->height);
	dev->bytes_le = 0;
#endif
}

/* Copy one or more scan lines to a client. */
#undef chunk
#define chunk byte
int
mem_get_bits(gx_device *dev, int y, byte *str, uint size, int pad_to_word)
{	uint bytes_per_line =
		gx_device_bytes_per_scan_line(dev, pad_to_word);
	byte *src = scan_line_base(mdev, y);
	byte *dest = str;
	uint count = min(size / bytes_per_line, dev->height - y);
	int swap = mdev->bytes_le;
	if ( !mdev->raster )		/* compute it now */
		(void)gdev_mem_bitmap_size(mdev);
	if ( bytes_per_line == mdev->raster )
	   {	if ( swap && pad_to_word >= 0 )
			memswab(src, dest, bytes_per_line * count);
		else
			memcpy(dest, src, bytes_per_line * count);
	   }
	else				/* know pad_to_word == 0 */
	   {	uint c;
		for ( c = count; c-- != 0; )
		   {	if ( swap )
			   {	/* We have to take extra care if */
				/* bytes_per_line is odd. */
				if ( bytes_per_line & 1 )
				   {	memswab(src, dest, bytes_per_line - 1);
					dest[bytes_per_line - 1] =
					  src[bytes_per_line];
				   }
				else
					memswab(src, dest, bytes_per_line);
			   }
			else
				memcpy(dest, src, bytes_per_line);
			src += mdev->raster;
			dest += bytes_per_line;
		   }
	   }
	return (swap && pad_to_word < 0 ? swap : 0);
}

/* ------ Monochrome ------ */

/* Procedures */
private dev_proc_copy_mono(mem_mono_copy_mono);
private dev_proc_fill_rectangle(mem_mono_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_mono_procs =
  mem_procs(gx_default_map_rgb_color, gx_default_map_color_rgb,
    mem_mono_copy_mono, gx_default_copy_color, mem_mono_fill_rectangle);

/* The instance is public. */
gx_device_memory mem_mono_device =
  mem_device("image(mono)", 1, mem_mono_procs);

/* Convert x coordinate to byte offset in scan line. */
#define x_to_byte(x) ((x) >> 3)

/* Fill a rectangle with a color. */
#undef chunk
#define chunk mono_chunk
private int
mem_mono_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	uint bit;
	chunk right_mask;
	byte fill;
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
#define write_loop(stat)\
 { int line_count = h;\
   chunk *ptr = dest;\
   do { stat; inc_chunk_ptr(ptr, draster); }\
   while ( --line_count );\
 }
#define write_partial(msk)\
   if ( fill ) write_loop(*ptr |= msk)\
   else write_loop(*ptr &= ~msk)
	switch ( color )
	   {
	case 0: fill = mdev->invert; break;
	case 1: fill = ~mdev->invert; break;
	case gx_no_color_index: return 0;		/* transparent */
	default: return -1;		/* invalid */
	   }
	bit = x & chunk_bit_mask;
	if ( bit + w <= chunk_bits )
	   {	/* Only one word. */
		right_mask =
		  (w == chunk_bits ? chunk_all_bits : chunk_hi_bits(w))
		    >> bit;
	   }
	else
	   {	int byte_count;
		if ( bit )
		   {	/* We have to split the following statement */
			/* into two because of a bug in the DEC */
			/* VAX/VMS C compiler. */
			chunk mask = chunk_all_bits;
			mask >>= bit;
			write_partial(mask);
			dest++;
			w += bit - chunk_bits;
		   }
		right_mask = chunk_hi_bits(w & chunk_bit_mask);
		if ( (byte_count = (w >> 3) & -chunk_bytes) != 0 )
		   {	write_loop(memset(ptr, fill, byte_count));
			inc_chunk_ptr(dest, byte_count);
		   }
	   }
	if ( right_mask )
		write_partial(right_mask);
	return 0;
}

/* Copy a monochrome bitmap. */

/* Fetch a chunk from the source. */
/* Note that the source data are always stored big-endian. */
/* Note also that the macros always cast cptr, */
/* so it doesn't matter what the type of cptr is. */
#undef chunk
#if arch_is_big_endian
#  define chunk uint
#  define cfetch(cptr) (*(chunk *)(cptr))
#else
#  define chunk ushort
#  define cfetch(cptr) (((chunk)*(byte *)(cptr) << 8) + ((byte *)(cptr))[1])
#endif
/* Fetch a chunk that straddles a chunk boundary. */
/***
#if arch_is_big_endian
***/
#  define cfetch2(cptr, cskew, skew)\
    ((cfetch(cptr) << cskew) + (cfetch((chunk *)(cptr) + 1) >> skew))
/***
#else
#  define cfetch2(cptr, cskew, skew)\
    (cskew <= 8 ?\
     (cfetch(cptr) << cskew) + (((byte *)(cptr))[2] >> (skew - 8)) :\
     (((byte *)(cptr))[1] << cskew) + (cfetch((chunk *)(cptr) + 1) >> skew))
#endif
***/

/* copy_function and copy_shift get added together for dispatch */
typedef enum {
	copy_or = 0, copy_store, copy_and, copy_funny
} copy_function;
typedef enum {
	copy_right = 0, copy_left = 4
} copy_shift;
typedef struct {
	short invert;
	ushort op;			/* copy_function */
} copy_mode;
/* Map from <c0,c1,invert> to copy_mode. */
#define cm(i,op) { i, (ushort)op }
private copy_mode copy_modes[9*2] = {
	cm(-1, copy_funny),		/* NN */
	cm(-1, copy_and),		/* N0 */
	cm(0, copy_or),			/* N1 */
	cm(0, copy_and),		/* 0N */
	cm(0, copy_funny),		/* 00 */
	cm(0, copy_store),		/* 01 */
	cm(-1, copy_or),		/* 1N */
	cm(-1, copy_store),		/* 10 */
	cm(0, copy_funny),		/* 11 */
	cm(-1, copy_funny),		/* NNi */
	cm(0, copy_or),			/* N1i */
	cm(-1, copy_and),		/* N0i */
	cm(-1, copy_or),		/* 1Ni */
	cm(0, copy_funny),		/* 11i */
	cm(-1, copy_store),		/* 10i */
	cm(0, copy_and),		/* 0Ni */
	cm(0, copy_store),		/* 01i */
	cm(0, copy_funny)		/* 00i */
};
private int
mem_mono_copy_mono(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	register byte *bptr;		/* actually chunk * */
	int dbit, wleft;
	uint mask;
	copy_mode mode;
#define function (copy_function)(mode.op)
	declare_scan_ptr_as(dbptr, byte *);
#define optr ((chunk *)dbptr)
	register int skew;
	register uint invert;
	check_rect();
#if gx_no_color_value != -1		/* hokey! */
	if ( zero == gx_no_color_index ) zero = -1;
	if ( one == gx_no_color_index ) one = -1;
#endif
#define izero (int)zero
#define ione (int)one
	mode =
	  copy_modes[(mdev->invert & 9) + izero + izero + izero + ione + 4];
#undef izero
#undef ione
	invert = (uint)(int)mode.invert;	/* load register */
	setup_rect_as(dbptr, byte *);
	bptr = base + ((sourcex & ~chunk_bit_mask) >> 3);
	dbit = x & chunk_bit_mask;
	skew = dbit - (sourcex & chunk_bit_mask);
	/* We have to split the following statement */
	/* into two because of a bug in the DEC */
	/* VAX/VMS C compiler. */
	mask = chunk_all_bits;
	mask >>= dbit;
/* Macros for writing partial chunks. */
/* The destination pointer is always named optr, */
/* and must be declared as chunk *. */
/* cinvert may be temporarily redefined. */
#define cinvert(bits) ((bits) ^ invert)
#define write_or_masked(bits, mask, off)\
  optr[off] |= (cinvert(bits) & mask)
#define write_store_masked(bits, mask, off)\
  optr[off] = ((optr[off] & ~mask) | (cinvert(bits) & mask))
#define write_and_masked(bits, mask, off)\
  optr[off] &= (cinvert(bits) | ~mask)
/* Macros for writing full chunks. */
#define write_or(bits)  *optr |= cinvert(bits)
#define write_store(bits) *optr = cinvert(bits)
#define write_and(bits) *optr &= cinvert(bits)
/* Macro for incrementing to next chunk. */
#define next_x_chunk\
  bptr += chunk_bytes; dbptr += chunk_bytes
/* Common macro for the end of each scan line. */
#define end_y_loop(sdelta, ddelta)\
  if ( --h == 0 ) break;\
  bptr += sdelta; dbptr += ddelta
	if ( (wleft = w + dbit - chunk_bits) <= 0 )
	   {	/* The entire operation fits in one (destination) chunk. */
		/* Some machines can't handle w == chunk_bits! */
#if arch_cant_shift_full_chunk
		if ( w != chunk_bits )
#endif
		  mask -= mask >> w;
#define write_single(wr_op, src)\
  for ( ; ; )\
   { wr_op(src, mask, 0);\
     end_y_loop(sraster, draster);\
   }
#define write1_loop(src)\
  switch ( function ) {\
    case copy_or: write_single(write_or_masked, src); break;\
    case copy_store: write_single(write_store_masked, src); break;\
    case copy_and: write_single(write_and_masked, src); break;\
    default: goto funny;\
  }
		if ( skew >= 0 )	/* single -> single, right/no shift */
		   {	write1_loop(cfetch(bptr) >> skew);
		   }
		else if ( wleft <= skew )	/* single -> single, left shift */
		   {	skew = -skew;
			write1_loop(cfetch(bptr) << skew);
		   }
		else			/* double -> single */
		   {	int cskew = -skew;
			skew += chunk_bits;
			write1_loop(cfetch2(bptr, cskew, skew));
		   }
#undef write1_loop
#undef write_single
	   }
	else if ( wleft <= skew )
	   {	/* 1 source chunk -> 2 destination chunks. */
		/* This is an important special case for */
		/* both characters and halftone tiles. */
		register uint bits;
		uint rmask = chunk_hi_bits(wleft);
		int cskew = chunk_bits - skew;
#define write_1to2(wr_op)\
  for ( ; ; )\
   { bits = cfetch(bptr) ^ invert;\
     wr_op(bits >> skew, mask, 0);\
     wr_op(bits << cskew, rmask, 1);\
     end_y_loop(sraster, draster);\
   }
#undef cinvert
#define cinvert(bits) (bits)		/* pre-inverted here */
		switch ( function )
		   {
		case copy_or: write_1to2(write_or_masked); break;
		case copy_store: write_1to2(write_store_masked); break;
		case copy_and: write_1to2(write_and_masked); break;
		default: goto funny;
		   }
#undef cinvert
#define cinvert(bits) ((bits) ^ invert)
#undef write_1to2
	   }
	else
	   {	/* More than one source chunk and more than one */
		/* destination chunk are involved. */
		uint rmask = chunk_hi_bits(wleft & chunk_bit_mask);
		int words = (wleft & ~chunk_bit_mask) >> 3;
		uint sskip = sraster - words;
		uint dskip = draster - words;
		register uint bits;
		if ( skew == 0 )	/* optimize the aligned case */
		   {
#define write_aligned(wr_op, wr_op_masked)\
  for ( ; ; )\
   { int count = wleft;\
     /* Do first partial chunk. */\
     wr_op_masked(cfetch(bptr), mask, 0);\
     /* Do full chunks. */\
     while ( (count -= chunk_bits) >= 0 )\
      { next_x_chunk; wr_op(cfetch(bptr)); }\
     /* Do last chunk */\
     if ( count > -chunk_bits )\
      { wr_op_masked(cfetch(bptr + chunk_bytes), rmask, 1); }\
     end_y_loop(sskip, dskip);\
   }
			switch ( function )
			  {
			  case copy_or:
			    write_aligned(write_or, write_or_masked);
			    break;
			  case copy_store:
			    write_aligned(write_store, write_store_masked);
			    break;
			  case copy_and:
			    write_aligned(write_and, write_and_masked);
			    break;
			  default:
			    goto funny;
			  }
#undef write_aligned
		   }
		else			/* not aligned */
		   {	int ccase =
			  (skew >= 0 ? copy_right :
			   ((bptr += chunk_bytes), copy_left)) + function;
			int cskew = -skew & chunk_bit_mask;
			skew &= chunk_bit_mask;
			for ( ; ; )
			   {	int count = wleft;
#define prefetch_right\
  bits = cfetch(bptr) >> skew
#define prefetch_left\
  bits = cfetch2(bptr - chunk_bytes, cskew, skew)
#define write_unaligned(wr_op, wr_op_masked)\
  wr_op_masked(bits, mask, 0);\
  /* Do full chunks. */\
  while ( count >= chunk_bits )\
    { bits = cfetch2(bptr, cskew, skew);\
      next_x_chunk; wr_op(bits); count -= chunk_bits;\
    }\
  /* Do last chunk */\
  if ( count > 0 )\
    { bits = cfetch(bptr) << cskew;\
      if ( count > skew ) bits += cfetch(bptr + chunk_bytes) >> skew;\
      wr_op_masked(bits, rmask, 1);\
    }
				switch ( ccase )
				  {
				  case copy_or + copy_left:
				    prefetch_left; goto uor;
				  case copy_or + copy_right:
				    prefetch_right;
uor:				    write_unaligned(write_or, write_or_masked);
				    break;
				  case copy_store + copy_left:
				    prefetch_left; goto ustore;
				  case copy_store + copy_right:
				    prefetch_right;
ustore:				    write_unaligned(write_store, write_store_masked);
				    break;
				  case copy_and + copy_left:
				    prefetch_left; goto uand;
				  case copy_and + copy_right:
				    prefetch_right;
uand:				    write_unaligned(write_and, write_and_masked);
				    break;
				  default:
				    goto funny;
				  }
				end_y_loop(sskip, dskip);
#undef write_unaligned
#undef prefetch_left
#undef prefetch_right
			   }
		   }
	   }
#undef end_y_loop
#undef next_x_chunk
	return 0;
	/* Handle the funny cases that aren't supposed to happen. */
funny:	return (invert ? -1 : mem_mono_fill_rectangle(dev, x, y, w, h, zero));
#undef optr
}
