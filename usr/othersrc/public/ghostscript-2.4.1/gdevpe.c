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

/* gdevpe.c  Private Eye display driver for Ghostscript
   Hacked by Fran Taylor, Reflection Technology Inc. */

#include "gx.h"
#include "gxdevice.h"

char *getenv(char *name);

typedef struct gx_device_pe_s {
	gx_device_common;
	byte *fbaddr;
	unsigned regs;
} gx_device_pe;
#define pedev ((gx_device_pe *)dev)

typedef struct {
	ushort reg, val;
} regval;

#define XSIZE 720
#define YSIZE 280
#define BPL 90
#define XPPI 160.0
#define YPPI 96.0
#define DEFAULT_ADDRESS ((byte *) 0xb8000000)
#define DEFAULT_REGISTERS 0x3d0

dev_proc_open_device(pe_open);
dev_proc_close_device(pe_close);
dev_proc_fill_rectangle(pe_fill_rectangle);
dev_proc_copy_mono(pe_copy_mono);

static gx_device_procs pe_procs =
{	pe_open,
	gx_default_get_initial_matrix,
	gx_default_sync_output,
	gx_default_output_page,
	pe_close,
	gx_default_map_rgb_color,
	gx_default_map_color_rgb,
	pe_fill_rectangle,
	gx_default_tile_rectangle,
	pe_copy_mono,
	gx_default_copy_color,
	gx_default_draw_line,
	gx_default_get_bits,
	gx_default_get_props,
	gx_default_put_props
};

gx_device_pe gs_pe_device = 
{	sizeof(gx_device_pe),
	&pe_procs,
	"Private Eye",
	XSIZE, YSIZE,
	XPPI, YPPI,
	no_margins,
	dci_black_and_white,
	0,
	DEFAULT_ADDRESS, DEFAULT_REGISTERS
};

static regval peinit[] = {{0x04, 0x1e}, {0x05, 0x00},
			  {0x04, 0x0c}, {0x05, 0x21},
			  {0x04, 0x0d}, {0x05, 0x98},
			  {0x08, 0x00}, {0x08, 0x1e},
			  {0x04, 0x1e}, {0x05, 0x01}};

static regval pedone[] = {{0x04, 0x1e}, {0x05, 0x10},
			  {0x04, 0x0a}, {0x05, 0x00},
			  {0x04, 0x0b}, {0x05, 0x07},
			  {0x04, 0x0c}, {0x05, 0x00},
			  {0x04, 0x0d}, {0x05, 0x00},
			  {0x04, 0x0e}, {0x05, 0x00},
			  {0x04, 0x0f}, {0x05, 0x00},
			  {0x08, 0x00}, {0x08, 0x29}};

int pe_open(gx_device *dev)
{
	char *str;
	int i;

	if ((str = getenv("PEFBADDR")) != 0)
	{
		if (!sscanf(str, "%lx", &(pedev->fbaddr)))
		{
			eprintf("Private Eye: PEFBADDR environment string format error\n");
			exit(1);
		}
	}

	if ((str = getenv("PEREGS")) != 0)
	{
		if (!sscanf(str, "%x", &(pedev->regs)))
		{
			eprintf("Private Eye: PEREGS environment string format error\n");
			exit(1);
		}
	}

	for (i = 0; i < 10; i++)
		outportb(pedev->regs + peinit[i].reg, peinit[i].val);

	return 0;
}

int pe_close(gx_device *dev)
{
	int i;

	/* restore the screen */
	for (i = 0; i < 16; i++)
		outportb(pedev->regs + pedone[i].reg, pedone[i].val);

	/* clear the frame buffer */
	memset(pedev->fbaddr, 0, 4000);

	return 0;
}

int pe_fill_rectangle(gx_device *dev, int x1, int y1, int w, int h,
                      gx_color_index color)
{
	int x2, y2, xlen;
	byte led, red, d;
	byte *ptr;

	/* cull */

	if ((w <= 0) || (h <= 0) || (x1 > XSIZE) || (y1 > YSIZE))
		return 0;

	x2 = x1 + w - 1;
	y2 = y1 + h - 1;

	/* cull some more */

	if ((x2 < 0) || (y2 < 0))
		return 0;

	/* clip */

	if (x1 < 0) x1 = 0;
	if (x2 > XSIZE-1) x2 = XSIZE-1;
	if (y1 < 0) y1 = 0;
	if (y2 > YSIZE-1) y2 = YSIZE-1;

	w = x2 - x1 + 1;
	h = y2 - y1 + 1;
	xlen = (x2 >> 3) - (x1 >> 3) - 1;
	led = 0xff >> (x1 & 7);
	red = 0xff << (7 - (x2 & 7));

	ptr = pedev->fbaddr + (y1 * BPL) + (x1 >> 3);

	if (color)
	{
		/* here to set pixels */
		
		if (xlen == -1)
		{
			/* special for rectangles that fit in a byte */
			
			d = led & red;
			for(; h >= 0; h--, ptr += BPL)
				*ptr |= d;
			return 0;
		}
		
		/* normal fill */
		
		for(; h >= 0; h--, ptr += BPL)
		{	register int x = xlen;
			register byte *p = ptr;
			*p++ |= led;
			while ( x-- ) *p++ = 0xff;
			*p |= red;
		}
	}

	/* here to clear pixels */

	led = ~led;
	red = ~red;

	if (xlen == -1)
	{
		/* special for rectangles that fit in a byte */
		
		d = led | red;
		for(; h >= 0; h--, ptr += BPL)
			*ptr &= d;
		return 0;
	}

	/* normal fill */
		
	for(; h >= 0; h--, ptr += BPL)
	{	register int x = xlen;
		register byte *p = ptr;
		*p++ &= led;
		while ( x-- ) *p++ = 0x00;
		*p &= red;
	}
	return 0;
}

int pe_copy_mono(gx_device *dev,
		 byte *base, int sourcex, int raster, gx_bitmap_id id,
                 int x, int y, int w, int h, 
		 gx_color_index zero, gx_color_index one)
{
	byte *line;
	int sleft, dleft;
	int mask, rmask;
	int invert, zmask, omask;
	byte *dest;
	int offset;

#define izero (int)zero
#define ione (int)one

if ( ione == izero )		/* vacuous case */
		return pe_fill_rectangle(dev, x, y, w, h, zero);

	/* clip */

	if ((x > XSIZE) || (y > YSIZE) || ((x + w) < 0) || ((y + h) < 0))
		return 0;

	offset = x >> 3;
	dest = pedev->fbaddr + (y * BPL) + offset;
	line = base + (sourcex >> 3);
	sleft = 8 - (sourcex & 7);
	dleft = 8 - (x & 7);
	mask = 0xff >> (8 - dleft);
	if ( w < dleft )
		mask -= mask >> w;
	else
		rmask = 0xff00 >> ((w - dleft) & 7);

	/* Macros for writing partial bytes. */
	/* bits has already been inverted by xor'ing with invert. */

#define write_byte_masked(ptr, bits, mask)\
  *ptr = ((bits | ~mask | zmask) & *ptr | (bits & mask & omask))

#define write_byte(ptr, bits)\
  *ptr = ((bits | zmask) & *ptr | (bits & omask))

/*	if ( dev->invert )
	{
		if ( izero != (int)gx_no_color_index ) zero ^= 1;
		if ( ione != (int)gx_no_color_index ) one ^= 1;
	} */
	invert = (izero == 1 || ione == 0 ? -1 : 0);
	zmask = (izero == 0 || ione == 0 ? 0 : -1);
	omask = (izero == 1 || ione == 1 ? -1 : 0);

#undef izero
#undef ione

	if (sleft == dleft)		/* optimize the aligned case */
	{
		w -= dleft;
		while ( --h >= 0 )
		{
			register byte *bptr = line;
			int count = w;
			register byte *optr = dest;
			register int bits = *bptr ^ invert;	/* first partial byte */
			
			write_byte_masked(optr, bits, mask);
			
			/* Do full bytes. */
			
			while ((count -= 8) >= 0)
			{
				bits = *++bptr ^ invert;
				++optr;
				write_byte(optr, bits);
			}
			
			/* Do last byte */
			
			if (count > -8)
			{
				bits = *++bptr ^ invert;
				++optr;
				write_byte_masked(optr, bits, rmask);
			}
			dest += BPL;
			line += raster;
		}
	}
	else
	{
		int skew = (sleft - dleft) & 7;
		int cskew = 8 - skew;
		
		while (--h >= 0)
		{
			byte *bptr = line;
			int count = w;
			byte *optr = dest;
			register int bits;
			
			/* Do the first partial byte */
			
			if (sleft >= dleft)
			{
				bits = *bptr >> skew;
			}	
			else /* ( sleft < dleft ) */
			{
				bits = *bptr++ << cskew;
				if (count > sleft)
					bits += *bptr >> skew;
			}
			bits ^= invert;
			write_byte_masked(optr, bits, mask);
			count -= dleft;
			optr++;
			
			/* Do full bytes. */
			
			while ( count >= 8 )
			{
				bits = *bptr++ << cskew;
				bits += *bptr >> skew;
				bits ^= invert;
				write_byte(optr, bits);
				count -= 8;
				optr++;
			}
			
			/* Do last byte */
			
			if (count > 0)
			{
				bits = *bptr++ << cskew;
 				if (count > skew)
					bits += *bptr >> skew;
				bits ^= invert;
				write_byte_masked(optr, bits, rmask);
			}
			dest += BPL;
			line += raster;
		}
	}
	return 0;
}
