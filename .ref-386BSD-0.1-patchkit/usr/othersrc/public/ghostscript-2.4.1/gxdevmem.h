/* Copyright (C) 1989, 1991 Aladdin Enterprises.  All rights reserved.
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

/* gxdevmem.h */
/* "Memory" device structure for Ghostscript library */
/* Requires gxdevice.h */

/*
 * A 'memory' device is essentially a stored bitmap.
 * There are several different kinds: 1-bit black and white,
 * 2-, 4-, and 8-bit mapped color, and 16-, 24-, and 32-bit true color.
 * (16-bit uses 5/6/5 bits per color.  24- and 32-bit are equivalent:
 * 24-bit takes less space, but is slower.)  All use the same structure,
 * since it's so awkward to get the effect of subclasses in C.
 *
 * On little-endian machines, the bytes can be stored in either order.
 * Little-endian order is the default, since this allows efficient
 * updating; big-endian is required if the bits will be used as the
 * source for a rendering operation (e.g., in the character cache).
 * We provide an operation to normalize the byte order, and we trust the
 * client not to do any rendering operations if the byte order is
 * reversed.
 */
typedef struct gx_device_memory_s gx_device_memory;
struct gx_device_memory_s {
	gx_device_common;		/* (see gxdevice.h) */
	gs_matrix initial_matrix;	/* the initial transformation */
	uint raster;			/* bytes per scan line, */
					/* filled in by '...bitmap_size' */
	byte *base;
	byte **line_ptrs;		/* scan line pointers */
	int bytes_le;			/* chunk size (2 bytes) if */
					/* bytes are stored in */
					/* little-endian order, else 0 */
	/* Following is only needed for monochrome */
	int invert;			/* 0 if 1=white, -1 if 1=black */
	/* Following are only needed for mapped color */
	int palette_size;		/* # of entries */
	byte *palette;			/* RGB triples */
};
extern gx_device_memory
	mem_mono_device,
	mem_mapped2_color_device,
	mem_mapped4_color_device,
	mem_mapped8_color_device,
	mem_true16_color_device,
	mem_true24_color_device,
	mem_true32_color_device;

/* Memory devices may have special setup requirements. */
/* In particular, it may not be obvious how much space to allocate */
/* for the bitmap.  Here is the routine that computes this */
/* from the width and height in the device structure. */
extern ulong gdev_mem_bitmap_size(P1(gx_device_memory *));

/* Determine the appropriate memory device for a given */
/* number of bits per pixel (0 if none suitable). */
extern gx_device_memory *gdev_mem_device_for_bits(P1(int));

/* Test whether a device is a memory device. */
extern int gs_device_is_memory(P1(const gx_device *));

/* Ensure that the data bytes are in big-endian order. */
/* This is only needed when the bitmap will be used as the source */
/* for a copy_mono operation, and is only used for the character cache */
/* and similar RAM-resident devices. */
extern void gdev_mem_ensure_byte_order(P1(gx_device_memory *));

/*
 * A memory device is guaranteed to allocate the bitmap consecutively,
 * i.e., in the form that can serve as input to copy_mono or copy_color
 * operations (provided that the bytes are in big-endian order, of course).
 */
