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

/* gdevpcfb.c */
/* IBM PC EGA and VGA display drivers for Ghostscript */
#include "dos_.h"
typedef union REGS registers;
/* This is fundamentally an EGA driver with some parameters */
/* that allow it to drive larger displays. */
#include "memory_.h"
#include "gs.h"
#include "gxdevice.h"

#ifndef USE_ASM
#  define USE_ASM 0			/* don't use assembly language */
#endif

/* Define the short (integer) version of "transparent" color. */
/* ****** Depends on gx_no_color_index being all 1's. ******/
#define no_color ((int)gx_no_color_index)

/* For testing, the EGA may be defined as a monochrome, 8-color, or */
/* 16-color device. */
#define ega_bits_of_color 2		/* 0, 1, or 2 */

/* Range of r-g-b values */
#define rgb_max ega_bits_of_color

/* Procedures */

	/* See gxdevice.h for the definitions of the procedures. */

dev_proc_open_device(ega_open);
dev_proc_close_device(ega_close);
dev_proc_map_rgb_color(ega_map_rgb_color);
dev_proc_map_color_rgb(ega_map_color_rgb);
dev_proc_fill_rectangle(ega_fill_rectangle);
dev_proc_tile_rectangle(ega_tile_rectangle);
int ega_write_dot(P4(gx_device *, int, int, gx_color_index));
dev_proc_copy_mono(ega_copy_mono);
dev_proc_copy_color(ega_copy_color);
dev_proc_get_bits(ega_get_bits);

/* Type for frame buffer pointers. */
/* Note that 'far' gets defined as null on 32-bit systems. */
typedef byte far *fb_ptr;

/* The device descriptor */
typedef struct gx_device_ega_s gx_device_ega;
struct gx_device_ega_s {
	gx_device_common;
	int raster;			/* frame buffer bytes per line */
	int fb_seg_mult;		/* multiplier for segment part */
					/* of frame buffer pointer */
	int fb_byte_mult;		/* multiplier for word part ditto */
#define mk_fb_ptr(x, y)\
  (fb_dev->fb_byte_mult == 0 ?\
   (fb_ptr)MK_PTR(regen + (y) * (fb_dev->fb_seg_mult), (x) >> 3) :\
   (fb_ptr)MK_PTR(regen + ((y) >> 4) * (fb_dev->fb_seg_mult),\
		 (((y) & 15) * fb_dev->fb_byte_mult) + ((x) >> 3)))
/* Define the largest height that can be processed */
/* within a single 64K segment.  If fb_dev->height > max_rop_height, */
/* we may have to break up operations into pieces. */
	unsigned max_rop_height;
	int video_mode;
};

/* Macro for casting gx_device argument */
#define fb_dev ((gx_device_ega *)dev)

/* Procedure record */
private gx_device_procs ega_procs = {
	ega_open,
	gx_default_get_initial_matrix,
	gx_default_sync_output,
	gx_default_output_page,
	ega_close,
	ega_map_rgb_color,
	ega_map_color_rgb,
	ega_fill_rectangle,
	ega_tile_rectangle,
	ega_copy_mono,
	ega_copy_color,
	gx_default_draw_line,
	ega_get_bits,
	gx_default_get_props,
	gx_default_put_props
};

/* Macro for creating instances */
/* The initial parameters map an appropriate fraction of */
/* the screen to an 8.5" x 11" coordinate space. */
/* This may or may not be what is desired! */
#define ega_device(dev_name, fb_raster, screen_height, aspect_ratio, video_mode)\
   {	sizeof(gx_device_ega),\
	&ega_procs,\
	dev_name,\
	fb_raster * 8, screen_height,\
	  (screen_height * (aspect_ratio)) / 11.0,	/* x density */\
	  screen_height / 11.0,		/* y density */\
	no_margins,\
	   {	(rgb_max ? 3 : 1),	/* num_components */\
		4,			/* depth */\
		(rgb_max ? rgb_max : 1),	/* gray_max */\
		rgb_max,\
		3,			/* dither_gray */\
		(rgb_max ? rgb_max + 1 : 0)	/* dither_rgb */\
	   },\
	0,			/* not opened yet */\
	fb_raster,\
	(fb_raster & 15 ? fb_raster : fb_raster >> 4),\
	(fb_raster & 15 ? fb_raster : 0),\
	((unsigned)(0xffff - fb_raster) / fb_raster),\
	video_mode\
   }

/* All the known instances */
		/* EGA */
gx_device_ega gs_ega_device =
	ega_device("ega", 80, 350, 48.0/35.0, 0x10);
		/* VGA */
gx_device_ega gs_vga_device =
	ega_device("vga", 80, 480, 1.0, 0x12);
		/* Trident SuperVGA, 800x600, 16-color mode */
gx_device_ega gs_tvga16_device =
	ega_device("tvga16", 100, 600, 1.0, 0x5b);
		/* Tseng Labs SuperVGA, 800x600, 16-color mode */
gx_device_ega gs_tseng16_device =
	ega_device("tseng16", 100, 600, 1.0, 0x29);
		/* EIZO MDB-10 */
gx_device_ega gs_mdb10_device =
	ega_device("mdb10", 128, 768, 1.0, 0x37);

/* Define the color spectrum */
#define black 0
#define blue 1
#define green 2
#define cyan 3
#define red 4
#define magenta 5
#define brown 6
#define white 7
#define dgray 8				/* dark gray is not very usable */
#define lblue 9
#define lgreen 10
#define lcyan 11
#define lred 12
#define lmagenta 13
#define yellow 14
#define bwhite 15

/* Forward declarations */
private int ega_get_mode();
private void ega_set_mode(int);

/* Save the EGA mode */
private int ega_save_mode = -1;

/* Reinitialize the EGA for text mode */
int
ega_close(gx_device *dev)
{	if ( ega_save_mode >= 0 ) ega_set_mode(ega_save_mode);
	return 0;
}

/* Map a r-g-b color to an EGA color code. */
gx_color_index
ega_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{
#define c12 (gx_max_color_value / 2)
#define c13 (gx_max_color_value / 3)
#define c23 (gx_max_color_value - c13)
#define cmono() ((gx_color_index)mono_color[r >> (gx_color_value_bits - 2)])
#if ega_bits_of_color == 0		/* monochrome */
	static byte mono_color[4] = { black, white, white, bwhite };
	return cmono();
#else
# if ega_bits_of_color == 1
	static byte mono_color[4] = { black, white, white, bwhite };
	if ( r == g && g == b )
		return cmono();
	return (gx_color_index)
		((r > c12 ? 4 : 0) |
		 (g > c12 ? 10 : 0) |
		 (b > c12 ? 1 : 0));
# else
	static byte g0[3][3] =
	 {{black,blue,lblue},{red,magenta,lmagenta},{lred,lmagenta,lmagenta}};
	static byte g1[3][3] =
	 {{green,cyan,lcyan},{brown,white,lcyan},{yellow,yellow,lmagenta}};
	static byte g2[3][3] =
	 {{lgreen,lgreen,lcyan},{lgreen,lgreen,lcyan},{yellow,yellow,bwhite}};
	return (gx_color_index)
		((g >= c23 ? g2 : g >= c13 ? g1 : g0)
		 [r >= c23 ? 2 : r >= c13 ? 1 : 0]
		 [b >= c23 ? 2 : b >= c13 ? 1 : 0]);
# endif
#endif
#undef c12
#undef c13
#undef c23
}

/* Map a color code to r-g-b. */
int
ega_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{
#define icolor (int)color
#if rgb_max > 1
	gx_color_value one =
		(icolor & 8 ? gx_max_color_value : gx_max_color_value / 3);
#else
#  define one (gx_max_color_value / 2 + 1)
#endif
	prgb[0] = (icolor & 4 ? one : 0);
	prgb[1] = (icolor & 2 ? one : 0);
	prgb[2] = (icolor & 1 ? one : 0);
	return 0;
#undef one
#undef icolor
}

/* Macro for validating rectangle parameters x, y, w, h */
#define validate_rect()\
  if ( w <= 0 || h <= 0 ) return 0;\
  if ( x < 0 || y < 0 || x + w > dev->width || y + h > dev->height ) return -1

/* ------ Internal routines ------ */

/* Read the device mode */
private int
ega_get_mode()
{	registers regs;
	regs.h.ah = 0xf;
	int86(0x10, &regs, &regs);
	return regs.h.al;
}

/* Set the device mode */
private void
ega_set_mode(int mode)
{	registers regs;
	regs.h.ah = 0;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

/* Structure for operation parameters. */
/* Note that this structure is known to assembly code. */
/* Not all parameters are used for every operation. */
typedef struct rop_params_s {
	fb_ptr dest;			/* pointer to frame buffer */
	int draster;			/* raster of frame buffer */
	byte far *src;			/* pointer to source data */
	int sraster;			/* source raster */
	int width;			/* width in bytes */
	int height;			/* height in scan lines */
	int shift;			/* amount to right shift source */
	int invert;			/* 0 or -1 to invert source */
	int data;			/* data for fill */
} rop_params;
typedef rop_params _ss *rop_ptr;

/* Assembly language routines */

#if USE_ASM
void memsetcol(P1(rop_ptr)); /* dest, draster, height, data */
#else
#define memsetcol cmemsetcol
private void
cmemsetcol(rop_ptr rop)
{	byte far *addr = rop->dest;
	int yc = rop->height;
	byte data = rop->data;
	int draster = rop->draster;
	while ( yc-- )
	 { byte_discard(*addr);
	   *addr = data;
	   addr += draster;
	 }
}
#endif

#if USE_ASM
void memsetrect(P1(rop_ptr)); /* dest, draster, width, height, data */
#else
#define memsetrect cmemsetrect
private void
cmemsetrect(rop_ptr rop)
{	int yc = rop->height;
	int width = rop->width;
	if ( yc <= 0 || width <= 0 ) return;
	   {	byte far *addr = rop->dest;
		byte data = rop->data;
		if ( width > 5 )	/* use memset */
		   {	int skip = rop->draster;
			do
			   {	memset(addr, data, width);
				addr += skip;
			   }
			while ( --yc );
		   }
		else			/* avoid the fixed overhead */
		   {	int skip = rop->draster - width;
			do
			   {	int cnt = width;
				do { *addr++ = data; } while ( --cnt );
				addr += skip;
			   }
			while ( --yc );
		   }
	   }
}
#endif

#if USE_ASM
void memrwcol(P1(rop_ptr)); /* dest, draster, src, sraster, height, shift, invert */
#  define memrwcol0(rop) memrwcol(rop)	/* same except shift = 0 */
#else
#  define memrwcol cmemrwcol
#  define memrwcol0 cmemrwcol0
private void
cmemrwcol(rop_ptr rop)
{	byte far *dp = rop->dest, *sp = rop->src;
	int yc = rop->height;
	int shift = rop->shift;
	byte invert = rop->invert;
	int sraster = rop->sraster, draster = rop->draster;
	while ( yc-- )
	 { byte_discard(*dp);
	   *dp = ((*sp >> shift) + (*sp << (8 - shift))) ^ invert;
	   dp += draster, sp += sraster;
	 }
}
private void
cmemrwcol0(rop_ptr rop)
{	byte far *dp = rop->dest, *sp = rop->src;
	int yc = rop->height;
	byte invert = rop->invert;
	int sraster = rop->sraster, draster = rop->draster;
	if ( yc > 0 ) do
	 { byte_discard(*dp);
	   *dp = *sp ^ invert;
	   dp += draster, sp += sraster;
	 }
	while ( --yc );
}
#endif

#if USE_ASM
void memrwcol2(P1(rop_ptr)); /* dest, draster, src, sraster, height, shift, invert */
#else
#define memrwcol2 cmemrwcol2
private void
cmemrwcol2(rop_ptr rop)
{	byte far *dp = rop->dest, *sp = rop->src;
	int yc = rop->height;
	int shift = rop->shift;
	byte invert = rop->invert;
	int sraster = rop->sraster, draster = rop->draster;
	while ( yc-- )
	 { byte_discard(*dp);
	   *dp = ((sp[1] >> shift) + (*sp << (8 - shift))) ^ invert;
	   dp += draster, sp += sraster;
	 }
}
#endif

/* Forward definitions */
void fill_rectangle(P4(rop_ptr, int, int, int));
void fill_row(P3(byte far *, int, int));

/* Define the device port and register numbers, and the regen map base */
#define out2(port, index, data)\
  (outportb(port, index), outportb((port)+1, data))
#define seq_addr 0x3c4
#define s_map 2
#define set_s_map(mask) out2(seq_addr, s_map, mask)
#define graph_addr 0x3ce
#define g_const 0			/* set/reset */
#define set_g_const(color) out2(graph_addr, g_const, color)
#define g_const_map 1			/* enable set/reset */
#define set_g_const_map(map) out2(graph_addr, g_const_map, map)
#define g_function 3
#define set_g_function(func) out2(graph_addr, g_function, func)
#define g_read_plane 4
#define set_g_read_plane(plane) out2(graph_addr, g_read_plane, plane)
#  define gf_WRITE 0
#  define gf_AND 8
#  define gf_OR 0x10
#  define gf_XOR 0x18
#define g_mode 5
#define set_g_mode(mode) out2(graph_addr, g_mode, mode)
#  define gm_DATA 0
#  define gm_FILL 2
#define g_mask 8
#define set_g_mask(mask) out2(graph_addr, g_mask, mask)
#define regen 0xa000

/* Clean up after writing */
#define dot_end()\
  set_g_mask(0xff)			/* all bits on */

/* Initialize the EGA for graphics mode */
int
ega_open(gx_device *dev)
{	if ( ega_save_mode < 0 ) ega_save_mode = ega_get_mode();
	ega_set_mode(fb_dev->video_mode);
	set_s_map(-1);			/* enable all maps */
	return 0;
}

/* Write a dot using the EGA color codes. */
/* This doesn't have to be efficient. */
int
ega_write_dot(gx_device *dev, int x, int y, gx_color_index color)
{	byte data = (byte)color;
	return ega_copy_color(dev, &data, 1, 1, gx_no_bitmap_id, x, y, 1, 1);
}

/* Macro for testing bit-inclusion */
#define bit_included_in(x,y) !((x)&~(y))

/* Copy a monochrome bitmap.  The colors are given explicitly. */
/* Color = gx_no_color_index means transparent (no effect on the image). */
int
ega_copy_mono(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index izero, gx_color_index ione)
{	rop_params params;
#define czero (int)izero
#define cone (int)ione
	int dleft, count;
	byte mask, rmask;
	fb_ptr save_dest;
	int other_color = -1;
	validate_rect();
	while ( h > fb_dev->max_rop_height )
	   {	unsigned mrh = fb_dev->max_rop_height;
		int code = ega_copy_mono(dev, base, sourcex, raster, id,
				x, y, w, mrh, izero, ione);
		if ( code != 0 ) return code;
		base = (byte *)((byte huge *)base +
				raster * (long)mrh);
		id = gx_no_bitmap_id;
		y += mrh;
		h -= mrh;
	   }
	params.dest = mk_fb_ptr(x, y);
	params.draster = fb_dev->raster;
	params.src = base + (sourcex >> 3);
	params.sraster = raster;
	params.height = h;
	params.shift = (x - sourcex) & 7;
	/* Analyze the 16 possible cases: each of izero and ione may be */
	/* 0, 0xf, transparent, or some other color. */
	switch ( czero )
	   {
	case no_color:
		switch ( cone )
		   {
		default:		/* (T, other) */
			/* Must do 2 passes */
			other_color = cone;
			save_dest = params.dest;
			/* falls through */
		case 0:			/* (T, 0) */
			set_g_function(gf_AND);
			params.invert = -1;
			break;
		case 0xf:		/* (T, 0xf) */
			set_g_function(gf_OR);
			params.invert = 0;
			break;
		case no_color:		/* (T, T) */
			return 0;	/* nothing to do */
		   }
		break;
	case 0:
		params.invert = 0;
		switch ( cone )
		   {
		default:		/* (0, other) */
			set_g_const(0);
			set_g_const_map(cone ^ 0xf);
			/* falls through */
		case 0xf:		/* (0, 0xf) */
			break;
		case no_color:		/* (0, T) */
			set_g_function(gf_AND);
			break;
		   }
		break;
	case 0xf:
		params.invert = -1;
		switch ( cone )
		   {
		case 0:			/* (0xf, 0) */
			break;
		default:		/* (0xf, other) */
			set_g_const(0xf);
			set_g_const_map(cone);
			break;
		case no_color:		/* (0xf, T) */
			set_g_function(gf_OR);
			/* falls through */
		   }
		break;
	default:
		switch ( cone )
		   {
		default:		/* (other, not T) */
			if ( bit_included_in(czero, cone) )
			   {	set_g_const(czero);
				set_g_const_map(czero ^ cone ^ 0xf);
				params.invert = 0;
				break;
			   }
			else if ( bit_included_in(cone, czero) )
			   {	set_g_const(cone);
				set_g_const_map(cone ^ czero ^ 0xf);
				params.invert = -1;
				break;
			   }
			/* No way around it, fill with one color first. */
			save_dest = params.dest;
			fill_rectangle((rop_ptr)&params, x & 7, w, cone);
			params.dest = save_dest;
			set_g_function(gf_XOR);
			set_s_map(czero ^ cone);
			other_color = -2;	/* must reset s_map at end */
			params.invert = -1;
			break;
		case no_color:		/* (other, T) */
			/* Must do 2 passes */
			other_color = czero;
			save_dest = params.dest;
			set_g_function(gf_AND);
			params.invert = 0;
			break;
		   }
		break;
	   }
	/* Actually copy the bits. */
	dleft = 8 - (x & 7);
	mask = 0xff >> (8 - dleft);
	count = w - dleft;
	if ( count < 0 )
		mask -= mask >> w,
		rmask = 0;
	else
		rmask = 0xff00 >> (count & 7);
	/* params: dest, src, sraster, height, shift, invert */
	/* Smashes params.src, params.dest, count. */
copy:	set_g_mask(mask);
	if ( params.shift == 0 )	/* optimize the aligned case */
	   {	/* Do left column */
		memrwcol0((rop_ptr)&params);
		/* Do center */
		if ( (count -= 8) >= 0 )
		   {	set_g_mask(0xff);
			do
			   {	params.src++, params.dest++;
				memrwcol0((rop_ptr)&params);
			   }
			while ( (count -= 8) >= 0 );
		   }
		/* Do right column */
		if ( rmask )
		   {	params.src++, params.dest++;
			set_g_mask(rmask);
			memrwcol0((rop_ptr)&params);
		   }
	   }
	else
	   {	/* Do left column */
		int sleft = 8 - (sourcex & 7);
		if ( sleft >= dleft )
		   {	/* Source fits in one byte */
			memrwcol((rop_ptr)&params);
		   }
		else if ( w <= sleft )
		   {	/* Source fits in one byte, thin case */
			memrwcol((rop_ptr)&params);
			goto fin;
		   }
		else
		   {	memrwcol2((rop_ptr)&params);
			params.src++;
		   }
		/* Do center */
		if ( (count -= 8) >= 0 )
		   {	set_g_mask(0xff);
			do
			   {	params.dest++;
				memrwcol2((rop_ptr)&params);
				params.src++;
			   }
			while ( (count -= 8) >= 0 );
		   }
		/* Do right column */
		if ( rmask )
		   {	set_g_mask(rmask);
			params.dest++;
			if ( count + 8 <= params.shift )
				memrwcol((rop_ptr)&params);
			else
				memrwcol2((rop_ptr)&params);
		   }
	   }
fin:	if ( other_color != -1 )
	   {	if ( other_color >= 0 )
		   {	/* Do the second pass on (T, other) or (other, T). */
			count = w - dleft;
			params.src = base + (sourcex >> 3);
			params.dest = save_dest;
			params.invert ^= -1;
			set_s_map(other_color);
			set_g_function(gf_OR);
			other_color = -2;
			goto copy;
		   }
		else
		   {	/* Finished second pass, restore s_map */
			set_s_map(-1);
		   }
	   }
	set_g_function(gf_WRITE);
	set_g_const_map(0);
	dot_end();
	return 0;
#undef czero
#undef cone
}

/* Copy a color pixelmap.  This is just like a bitmap, */
/* except that each pixel takes 4 bits instead of 1. */
int
ega_copy_color(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	byte *line = base + (sourcex >> 1);
	unsigned lmask = 0x80 >> (x & 7);
	int bitx = sourcex & 1;
	int ex = w + bitx;
	fb_ptr fbline;
	validate_rect();
	while ( h > fb_dev->max_rop_height )
	   {	unsigned mrh = fb_dev->max_rop_height;
		int code = ega_copy_color(dev, base, sourcex, raster, id,
				x, y, w, mrh);
		if ( code != 0 ) return code;
		base = (byte *)((byte huge *)base +
				raster * (long)mrh);
		id = gx_no_bitmap_id;
		y += mrh;
		h -= mrh;
	   }
	fbline = mk_fb_ptr(x, y);
	set_g_mode(gm_FILL);
	outportb(graph_addr, g_mask);	/* first half of set_g_mask */
	while ( --h >= 0 )
	{	byte *bptr = line;
		int px = bitx;
		/* Turbo C will put an unsigned in a register, */
		/* but not a byte! */
		register unsigned mask = lmask;
		fb_ptr fbptr = fbline;
		do
		   {	byte_discard(*fbptr);	/* latch frame buffer data */
			outportb(graph_addr+1, mask);	/* 2nd half of set_g_mask */
			*fbptr = (px & 1 ? *bptr++ & 0xf : *bptr >> 4);
			if ( (mask >>= 1) == 0 )
				mask = 0x80, fbptr++;
		   }
		while ( ++px < ex );
		line += raster;
		fbline += fb_dev->raster;
	}
	set_g_mode(gm_DATA);
	dot_end();
	return 0;
}

/* Fill a rectangle. */
int
ega_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	rop_params params;
	validate_rect();
	while ( h > fb_dev->max_rop_height )
	   {	unsigned mrh = fb_dev->max_rop_height;
		int code = ega_fill_rectangle(dev,
			x, y, w, mrh, color);
		if ( code != 0 ) return code;
		y += mrh;
		h -= mrh;
	   }
	params.dest = mk_fb_ptr(x, y);
	if ( h == 1 )
	   {	set_g_const((int)color);
		fill_row(params.dest, x & 7, w);
	   }
	else
	   {	params.draster = fb_dev->raster;
		params.height = h;
		fill_rectangle((rop_ptr)&params, x & 7, w, (int)color);
	   }
	dot_end();
	return 0;
}

/* Tile a rectangle.  Note that the two colors must both be supplied, */
/* i.e. neither one can be gx_no_color_index (transparent): */
/* a transparent color means that the tile is colored, not a mask. */
int
ega_tile_rectangle(gx_device *dev, gx_bitmap *tile,
  int x, int y, int w, int h, gx_color_index czero, gx_color_index cone,
  int px, int py)
#define zero (int)czero
#define one (int)cone
{	rop_params params;
	int xmod, width_bytes;
	int tile_height = tile->size.y;
	int xbit;
	int lcount;
	int mask, rmask;
	byte narrow;
	byte again;
	int const_bits, maps;
	int ymod, yleft;
	while ( h > fb_dev->max_rop_height )
	   {	int mod_height =
			fb_dev->max_rop_height / tile_height * tile_height;
		int code = ega_tile_rectangle(dev, tile,
			x, y, w, mod_height, czero, cone, px, py);
		if ( code != 0 ) return code;
		y += mod_height;
		h -= mod_height;
	   }
	/* We only handle the easiest cases directly. */
	if ( (tile->size.x & 7) || one == -1 || zero == -1 || px || py )
		return gx_default_tile_rectangle(dev, tile, x, y, w, h,
			czero, cone, px, py);
	/* Following is similar to aligned case of copy_mono */	
	validate_rect();
	params.dest = mk_fb_ptr(x, y);
	params.draster = fb_dev->raster;
	params.sraster = tile->raster;
	params.shift = 0;
	xbit = x & 7;
	/* Set up the graphics registers */
	const_bits = (zero ^ one) ^ 0xf;
	if ( const_bits )
	   {	set_g_const(zero);	/* either color will do */
		set_g_const_map(const_bits);
	   }
	if ( (maps = zero & ~one) != 0 )
	   {	set_s_map(maps += const_bits);
		params.invert = -1;
		again = one & ~zero;
	   }
	else
	   {	maps = one & ~zero;
		set_s_map(maps += const_bits);
		params.invert = 0;
		again = 0;
	   }
	xmod = (x % tile->size.x) >> 3;
	width_bytes = tile->size.x >> 3;
	mask = 0xff >> xbit;
	if ( w + xbit <= 8 )
		mask -= mask >> w,
		rmask = 0,
		narrow = 1;
	else
	   {	rmask = (0xff00 >> ((w + x) & 7)) & 0xff;
		if ( xbit )	w += xbit - 8;
		else		mask = 0, --xmod, --params.dest;
		narrow = 0;
	   }
	ymod = y % tile_height;
tile:	yleft = tile_height - ymod;
	params.src = tile->data + ymod * params.sraster + xmod;
	lcount = h;
	if ( narrow )			/* Optimize narrow case */
	   {	set_g_mask(mask);
		if ( lcount > yleft )
		   {	params.height = yleft;
			memrwcol0((rop_ptr)&params);
			params.dest += yleft * params.draster;
			params.src = tile->data + xmod;
			params.height = tile_height;
			lcount -= yleft;
			while ( lcount >= tile_height )
			   {	memrwcol0((rop_ptr)&params);
				params.dest += tile_height * params.draster;
				lcount -= tile_height;
			   }
		   }
		if ( lcount )
		   {	params.height = lcount;
			memrwcol0((rop_ptr)&params);
		   }
	   }
	else
	   {	fb_ptr line = params.dest;
		int xpos = width_bytes - xmod;
		while ( 1 )
		   {	int xleft = xpos;
			int count = w;
			params.height = (lcount > yleft ? yleft : lcount);
			/* Do first byte, if not a full byte. */
			if ( mask )
			   {	set_g_mask(mask);
				memrwcol0((rop_ptr)&params);
			   }
			/* Do full bytes */
			if ( (count -= 8) >= 0 )
			   {	set_g_mask(0xff);
				do
				   {	if ( !--xleft )
						xleft = width_bytes,
						params.src -= width_bytes;
					++params.src, ++params.dest;
					memrwcol0((rop_ptr)&params);
				   }
				while ( (count -= 8) >= 0 );
			   }
			/* Do last byte */
			if ( rmask )
			   {	if ( !--xleft )
					xleft = width_bytes,
					params.src -= width_bytes;
				set_g_mask(rmask);
				++params.src, ++params.dest;
				memrwcol0((rop_ptr)&params);
			   }
			if ( (lcount -= params.height) == 0 ) break;
			params.dest = line += params.height * params.draster;
			params.src = tile->data + xmod;
			yleft = tile_height;
		   }
	   }
	/* Now do the second color if needed */
	if ( again )
	   {	maps = again + const_bits;
		set_s_map(maps);
		again = 0;
		params.dest = mk_fb_ptr(x, y);
		if ( mask == 0 ) params.dest--;
		params.invert = 0;
		goto tile;
	   }
	if ( maps != 0xf )
		set_s_map(-1);
	if ( const_bits )
		set_g_const_map(0);
	dot_end();
	return 0;
}

/* Read scan lines back from the frame buffer. */
int
ega_get_bits(gx_device *dev, int y, byte *data, uint size, int pad_to_word)
{	/* We don't have to worry about padding, because we read back */
	/* 4 bits per pixel and the frame buffer width is always */
	/* a multiple of 8 pixels. */
	uint bytes_per_row = dev->width >> 1;
	uint count = min(dev->height - y, size / bytes_per_row);
	int j, plane;
	byte *drow = data;
	memset(data, 0, size);
	for ( j = count; j--; drow += bytes_per_row, y++ )
	  for ( plane = 0; plane < 4; plane++ )
	   {	fb_ptr src = mk_fb_ptr(0, y);
		ushort *dest = (ushort *)drow;
		int i;
		/* Plane 0 is the least significant plane. */
		/* We know we're on a little-endian machine.... */
		static ushort spread4[16] =
		   {	0x0, 0x800, 0x8000, 0x8800,
			0x8, 0x808, 0x8008, 0x8808,
			0x80, 0x880, 0x8080, 0x8880,
			0x88, 0x888, 0x8088, 0x8888
		   };
		set_g_read_plane(plane);
		for ( i = 0; i < dev->width; i += 8, src++, dest += 2 )
		   {	byte b = *src;
			dest[0] = (dest[0] >> 1) + spread4[b >> 4];
			dest[1] = (dest[1] >> 1) + spread4[b & 0xf];
		   }
	   }
	return count;
}

/* ------ Internal routines ------ */

/* Mask table for rectangle fill. */
static byte rmask_tab[9] =
   {	0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff
   };

/* Fill a rectangle specified by pointer into frame buffer, */
/* starting bit within byte, width, and height. */
/* Smashes rop->dest. */
private void
fill_rectangle(register rop_ptr rop, int bit, int w, int color)
  /* rop: dest, draster, height */
{	set_g_const(color);
	set_g_const_map(0xf);
	if ( bit + w <= 8 )
	   {	/* Less than one byte */
		set_g_mask(rmask_tab[w] >> bit);
		memsetcol(rop);
	   }
	else
	   {	byte right_mask;
		if ( bit )
		   {	set_g_mask(0xff >> bit);
			memsetcol(rop);
			rop->dest++;
			w += bit - 8;
		   }
		if ( w >= 8 )
		   {	set_g_mask(0xff);	/* all bits */
			rop->width = w >> 3;
			memsetrect(rop);
			rop->dest += rop->width;
			w &= 7;
		   }
		if ( (right_mask = rmask_tab[w]) != 0 )
		   {	set_g_mask(right_mask);
			memsetcol(rop);
		   }
	   }
	set_g_const_map(0);
}

/* Fill a single row specified by pointer into frame buffer, */
/* starting bit within byte, and width. */
/* Caller has already set g_const. */
#define r_m_w(ptr) (*(ptr))++		/* read & write, data irrelevant */
private void
fill_row(byte far *dest, int bit, int w)
  /* rop: dest */
{	set_g_const_map(0xf);
	if ( bit + w <= 8 )
	   {	/* Less than one byte */
		set_g_mask(rmask_tab[w] >> bit);
		r_m_w(dest);
	   }
	else
	   {	byte right_mask;
		if ( bit )
		   {	set_g_mask(0xff >> bit);
			r_m_w(dest);
			dest++;
			w += bit - 8;
		   }
		if ( w >= 8 )
		   {	int byte_count = w >> 3;
			set_g_mask(0xff);	/* all bits */
			memset(dest, 0, byte_count);	/* data irrelevant */
			dest += byte_count;
			w &= 7;
		   }
		if ( (right_mask = rmask_tab[w]) != 0 )
		   {	set_g_mask(right_mask);
			r_m_w(dest);
		   }
	   }
	set_g_const_map(0);
}
