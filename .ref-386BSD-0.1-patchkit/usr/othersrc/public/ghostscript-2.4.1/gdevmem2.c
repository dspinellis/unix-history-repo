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

/* gdevmem2.c */
/* 8-and-more-bit-per-pixel "memory" (stored bitmap) devices */
/* for Ghostscript library. */
#include "memory_.h"
#include "gs.h"
#include "gxdevice.h"
#include "gxdevmem.h"			/* semi-public definitions */
#include "gdevmem.h"			/* private definitions */

/* ------ Generic procedures ------ */

/* Copy a rectangle of bytes from a source to a destination. */
#undef chunk
#define chunk byte
private int
copy_byte_rect(gx_device_memory *dev, const byte *source, int sraster,
  int offset, int y, int byte_count, int h)
{	uint draster = dev->raster;
	byte *dest = scan_line_base(dev, y) + offset;
	while ( h-- > 0 )
	   {	memcpy(dest, source, byte_count);
		source += sraster;
		dest += draster;
	   }
	return 0;
}

/* Map a r-g-b color to a color index. */
/* This requires searching the palette. */
gx_color_index
mem_mapped_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	byte br = gx_color_value_to_byte(r);
	byte bg = gx_color_value_to_byte(g);
	byte bb = gx_color_value_to_byte(b);
	register byte *pptr = mdev->palette;
	int cnt = mdev->palette_size;
	byte *which;
	int best = 256*3;
	while ( cnt-- > 0 )
	   {	register int diff = *pptr - br;
		if ( diff < 0 ) diff = -diff;
		if ( diff < best )	/* quick rejection */
		   {	int dg = pptr[1] - bg;
			if ( dg < 0 ) dg = -dg;
			if ( (diff += dg) < best )	/* quick rejection */
			   {	int db = pptr[2] - bb;
				if ( db < 0 ) db = -db;
				if ( (diff += db) < best )
					which = pptr, best = diff;
			   }
		   }
		pptr += 3;
	   }
	return (gx_color_index)((which - mdev->palette) / 3);
}

/* Map a color index to a r-g-b color. */
int
mem_mapped_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	byte *pptr = mdev->palette + (int)color * 3;
	prgb[0] = gx_color_value_from_byte(pptr[0]);
	prgb[1] = gx_color_value_from_byte(pptr[1]);
	prgb[2] = gx_color_value_from_byte(pptr[2]);
	return 0;
}

/* ------ Mapped 8-bit color ------ */

/* Procedures */
declare_mem_procs(mem_mapped8_copy_mono, mem_mapped8_copy_color, mem_mapped8_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_mapped8_procs =
  mem_procs(mem_mapped_map_rgb_color, mem_mapped_map_color_rgb,
    mem_mapped8_copy_mono, mem_mapped8_copy_color, mem_mapped8_fill_rectangle);

/* The instance is public. */
gx_device_memory mem_mapped8_color_device =
  mem_device("image(8)", 8, mem_mapped8_procs);

/* Convert x coordinate to byte offset in scan line. */
#undef x_to_byte
#define x_to_byte(x) (x)

/* Fill a rectangle with a color. */
private int
mem_mapped8_fill_rectangle(gx_device *dev,
  int x, int y, int w, int h, gx_color_index color)
{	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	while ( h-- > 0 )
	   {	memset(dest, (byte)color, w);
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a monochrome bitmap. */
private int
mem_mapped8_copy_mono(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	byte *line;
	int first_bit;
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	line = base + (sourcex >> 3);
	first_bit = 0x80 >> (sourcex & 7);
	while ( h-- > 0 )
	   {	register byte *pptr = dest;
		byte *sptr = line;
		register int sbyte = *sptr;
		register uint bit = first_bit;
		int count = w;
#define is_color(c) ((int)(c) != (int)gx_no_color_index)
#define next_bit()\
  if ( (bit >>= 1) == 0 ) bit = 0x80, sbyte = *++sptr;\
  pptr++
		if ( is_color(one) )
		   {	if ( is_color(zero) )
			   {	/* Optimize halftone coloring */
				do
				   {	*pptr = (sbyte & bit ? (byte)one :
						 (byte)zero);
					next_bit();
				   }
				while ( --count > 0 );
			   }
			else
			   {	/* Optimize stenciling */
				do
				   {	if ( sbyte & bit )
					  *pptr = (byte)one;
					next_bit();
				   }
				while ( --count > 0 );
			   }
		   }
		else if ( is_color(zero) )
		   {	do
			   {	if ( !(sbyte & bit) )
				  *pptr = (byte)zero;
				next_bit();
			   }
			while ( --count > 0 );
		   }
#undef next_bit
#undef is_color
		line += sraster;
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a color bitmap. */
private int
mem_mapped8_copy_color(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	check_rect();
	return copy_byte_rect(mdev, base + x_to_byte(sourcex), sraster,
		x_to_byte(x), y, x_to_byte(w), h);
}

/* ------ 16-bit true color ------ */
/* The 16 bits are divided 5 for red, 6 for green, and 5 for blue. */

/* Procedures */
declare_mem_map_procs(mem_true16_map_rgb_color, mem_true16_map_color_rgb);
declare_mem_procs(mem_true16_copy_mono, mem_true16_copy_color, mem_true16_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_true16_procs =
  mem_procs(mem_true16_map_rgb_color, mem_true16_map_color_rgb,
    mem_true16_copy_mono, mem_true16_copy_color, mem_true16_fill_rectangle);

/* The instance is public. */
gx_device_memory mem_true16_color_device =
  mem_device("image(16)", 16, mem_true16_procs);

/* Map a r-g-b color to a color index. */
private gx_color_index
mem_true16_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	return ((r >> (gx_color_value_bits - 5)) << 11) +
		((g >> (gx_color_value_bits - 6)) << 5) +
		(b >> (gx_color_value_bits - 5));
}

/* Map a color index to a r-g-b color. */
private int
mem_true16_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	ushort value;
	value = color >> 11;
	prgb[0] = ((value << 11) + (value << 6) + (value << 1) + (value >> 4)) >> (16 - gx_color_value_bits);
	value = (color >> 6) & 0x7f;
	prgb[1] = ((value << 10) + (value << 4) + (value >> 2)) >> (16 - gx_color_value_bits);
	value = color & 0x3f;
	prgb[2] = ((value << 11) + (value << 6) + (value << 1) + (value >> 4)) >> (16 - gx_color_value_bits);
	return 0;
}

/* Convert x coordinate to byte offset in scan line. */
#undef x_to_byte
#define x_to_byte(x) ((x) << 1)

/* Fill a rectangle with a color. */
private int
mem_true16_fill_rectangle(gx_device *dev,
  int x, int y, int w, int h, gx_color_index color)
{	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	while ( h-- > 0 )
	   {	ushort *pptr = (ushort *)dest;
		int cnt = w;
		do { *pptr++ = (ushort)color; } while ( --cnt > 0 );
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a monochrome bitmap. */
private int
mem_true16_copy_mono(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	byte *line;
	int first_bit;
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	line = base + (sourcex >> 3);
	first_bit = 0x80 >> (sourcex & 7);
	while ( h-- > 0 )
	   {	register ushort *pptr = (ushort *)dest;
		byte *sptr = line;
		register int sbyte = *sptr++;
		register int bit = first_bit;
		int count = w;
		do
		   {	if ( sbyte & bit )
			   {	if ( one != gx_no_color_index )
				  *pptr = (ushort)one;
			   }
			else
			   {	if ( zero != gx_no_color_index )
				  *pptr = (ushort)zero;
			   }
			if ( (bit >>= 1) == 0 )
				bit = 0x80, sbyte = *sptr++;
			pptr++;
		   }
		while ( --count > 0 );
		line += sraster;
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a color bitmap. */
private int
mem_true16_copy_color(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	check_rect();
	return copy_byte_rect(mdev, base + x_to_byte(sourcex), sraster,
		x_to_byte(x), y, x_to_byte(w), h);
}

/* ------ 24- or 32-bit true color ------ */

/* Procedures */
declare_mem_map_procs(mem_true_map_rgb_color, mem_true_map_color_rgb);

/* The device descriptor. */
#define mem_true_procs(copy_mono, copy_color, fill_rectangle)\
  mem_procs(mem_true_map_rgb_color, mem_true_map_color_rgb,\
    copy_mono, copy_color, fill_rectangle)

/* We want the bytes of a color always to be in the order -,r,g,b, */
/* but we want to manipulate colors as longs.  This requires careful */
/* handling to be byte-order independent. */
#define color_byte(cx,i) (((byte *)&(cx))[i])

/* Map a r-g-b color to a color index. */
private gx_color_index
mem_true_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	return gx_color_value_to_byte(b) +
	       ((uint)gx_color_value_to_byte(g) << 8) +
	       ((ulong)gx_color_value_to_byte(r) << 16);
}

/* Map a color index to a r-g-b color. */
private int
mem_true_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	prgb[0] = gx_color_value_from_byte(color >> 16);
	prgb[1] = gx_color_value_from_byte((color >> 8) & 0xff);
	prgb[2] = gx_color_value_from_byte(color & 0xff);
	return 0;
}

/* ------ 24-bit color ------ */
/* 24-bit takes less space than 32-bit, but is slower. */

/* Procedures */
declare_mem_procs(mem_true24_copy_mono, mem_true24_copy_color, mem_true24_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_true24_procs =
  mem_true_procs(mem_true24_copy_mono, mem_true24_copy_color,
    mem_true24_fill_rectangle);
gx_device_memory mem_true24_color_device =
  mem_device("image(24)", 24, mem_true24_procs);

/* Convert x coordinate to byte offset in scan line. */
#undef x_to_byte
#define x_to_byte(x) ((x) * 3)

/* Unpack a color into its bytes. */
#define declare_unpack_color(r, g, b, color)\
	byte r = (byte)(color >> 16);\
	byte g = (byte)((uint)color >> 8);\
	byte b = (byte)color
#define put3(ptr, r, g, b)\
	ptr[0] = r, ptr[1] = g, ptr[2] = b

/* Fill a rectangle with a color. */
private int
mem_true24_fill_rectangle(gx_device *dev,
  int x, int y, int w, int h, gx_color_index color)
{	declare_unpack_color(r, g, b, color);
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	while ( h-- > 0 )
	   {	register int cnt = w;
		register byte *pptr = dest;
		do { put3(pptr, r, g, b); pptr += 3; } while ( --cnt > 0 );
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a monochrome bitmap. */
private int
mem_true24_copy_mono(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	byte *line;
	int first_bit;
	declare_unpack_color(r0, g0, b0, zero);
	declare_unpack_color(r1, g1, b1, one);
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	line = base + (sourcex >> 3);
	first_bit = 0x80 >> (sourcex & 7);
	while ( h-- > 0 )
	   {	register byte *pptr = dest;
		byte *sptr = line;
		register int sbyte = *sptr++;
		register int bit = first_bit;
		int count = w;
		do
		   {	if ( sbyte & bit )
			   {	if ( one != gx_no_color_index )
				  put3(pptr, r1, g1, b1);
			   }
			else
			   {	if ( zero != gx_no_color_index )
				  put3(pptr, r0, g0, b0);
			   }
			pptr += 3;
			if ( (bit >>= 1) == 0 )
				bit = 0x80, sbyte = *sptr++;
		   }
		while ( --count > 0 );
		line += sraster;
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a color bitmap. */
private int
mem_true24_copy_color(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	check_rect();
	return copy_byte_rect(mdev, base + x_to_byte(sourcex), sraster,
		x_to_byte(x), y, x_to_byte(w), h);
}

/* ------ 32-bit color ------ */

/* Procedures */
declare_mem_procs(mem_true32_copy_mono, mem_true32_copy_color, mem_true32_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_true32_procs =
  mem_true_procs(mem_true32_copy_mono, mem_true32_copy_color,
    mem_true32_fill_rectangle);
gx_device_memory mem_true32_color_device =
  mem_device("image(32)", 32, mem_true32_procs);

/* Convert x coordinate to byte offset in scan line. */
#undef x_to_byte
#define x_to_byte(x) ((x) << 2)

/* Swap the bytes of a color if needed. */
#if arch_is_big_endian
#  define arrange_bytes(color) (color)
#else
#  define arrange_bytes(color)\
    (((color) >> 24) + (((color) >> 16) & 0xff00) +\
     (((color) & 0xff00) << 8) + ((color) << 24))
#endif

/* Fill a rectangle with a color. */
private int
mem_true32_fill_rectangle(gx_device *dev,
  int x, int y, int w, int h, gx_color_index color)
{	gx_color_index a_color = arrange_bytes(color);
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	while ( h-- > 0 )
	   {	gx_color_index *pptr = (gx_color_index *)dest;
		int cnt = w;
		do { *pptr++ = a_color; } while ( --cnt > 0 );
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a monochrome bitmap. */
private int
mem_true32_copy_mono(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	gx_color_index a_zero = arrange_bytes(zero);
	gx_color_index a_one = arrange_bytes(one);
	byte *line;
	int first_bit;
	declare_scan_ptr(dest);
	check_rect();
	setup_rect(dest);
	line = base + (sourcex >> 3);
	first_bit = 0x80 >> (sourcex & 7);
	while ( h-- > 0 )
	   {	register gx_color_index *pptr = (gx_color_index *)dest;
		byte *sptr = line;
		register int sbyte = *sptr++;
		register int bit = first_bit;
		int count = w;
		do
		   {	if ( sbyte & bit )
			   {	if ( one != gx_no_color_index )
				  *pptr = a_one;
			   }
			else
			   {	if ( zero != gx_no_color_index )
				  *pptr = a_zero;
			   }
			if ( (bit >>= 1) == 0 )
				bit = 0x80, sbyte = *sptr++;
			pptr++;
		   }
		while ( --count > 0 );
		line += sraster;
		inc_chunk_ptr(dest, draster);
	   }
	return 0;
}

/* Copy a color bitmap. */
private int
mem_true32_copy_color(gx_device *dev,
  byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	check_rect();
	return copy_byte_rect(mdev, base + x_to_byte(sourcex), sraster,
		x_to_byte(x), y, x_to_byte(w), h);
}
