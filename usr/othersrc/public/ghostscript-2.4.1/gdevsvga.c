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

/* gdevsvga.c */
/* SuperVGA display drivers for Ghostscript */
#include "dos_.h"
typedef union REGS registers;
#include "memory_.h"
#include "gs.h"
#include "gxdevice.h"

#ifndef USE_ASM
#  define USE_ASM 0			/* don't use assembly language */
#endif

/* Define the short (integer) version of "transparent" color. */
/* ****** Depends on gx_no_color_index being all 1's. ******/
#define no_color ((int)gx_no_color_index)

/* Procedures */

	/* See gxdevice.h for the definitions of the procedures. */

private dev_proc_close_device(svga_close);
private dev_proc_map_rgb_color(svga_map_rgb_color);
private dev_proc_map_color_rgb(svga_map_color_rgb);
private dev_proc_fill_rectangle(svga_fill_rectangle);
private dev_proc_copy_mono(svga_copy_mono);
private dev_proc_copy_color(svga_copy_color);
private dev_proc_get_bits(svga_get_bits);

/* Type for frame buffer pointers. */
/*** Intimately tied to the 80x86 (x<2) addressing architecture. ***/
typedef byte far *fb_ptr;

/* The device descriptor */
typedef struct gx_device_svga_s gx_device_svga;
struct gx_device_svga_s {
	gx_device_common;
	int (*get_mode)(P0());
	void (*set_mode)(P1(int));
	void (*set_page)(P3(gx_device_svga *, int, int));
	uint raster;			/* frame buffer bytes per line */
	int page;			/* current page */
	int wnum_read, wnum_write;	/* window #s for read vs. write */
	/* Following are device-specific. */
	union {
	  struct {
		void (*bios_set_page)(P2(int, int));	/* set-page function */
	  } vesa;
	  struct {
		int select_reg;			/* page-select register */
	  } atiw;
	  struct {
		int et_model;			/* 4 for ET4000, */
						/* 3 for ET3000 */
	  } tseng;
	} info;
};

/* The color map for dynamically assignable colors. */
#define first_color_index 64
private int next_color_index;
private ushort dynamic_colors[256 - first_color_index];

/* Macro for casting gx_device argument */
#define fb_dev ((gx_device_svga *)dev)

/* Procedure records */
#define svga_procs(open) {\
	open, gx_default_get_initial_matrix,\
	gx_default_sync_output, gx_default_output_page, svga_close,\
	svga_map_rgb_color, svga_map_color_rgb,\
	svga_fill_rectangle, gx_default_tile_rectangle,\
	svga_copy_mono, svga_copy_color, gx_default_draw_line,\
	svga_get_bits, gx_default_get_props, gx_default_put_props\
}

/* The initial parameters map an appropriate fraction of */
/* the screen to an 8.5" x 11" coordinate space. */
/* This may or may not be what is desired! */
#define svga_device(procs, name, get_mode, set_mode, set_page) {\
	sizeof(gx_device_svga),\
	&procs,\
	name,\
	640, 480,		/* screen size */\
	480 / 11.0, 480 / 11.0,	/* resolution */\
	no_margins,\
	dci_color(8, 31, 4),\
	0,			/* not opened yet */\
	get_mode, set_mode, set_page\
   }

/* Save the controller mode */
private int svga_save_mode = -1;

/* Macro for validating rectangle parameters x and w. */
/* set_pixel_ptr implicitly validates y and h. */
#define validate_rect()\
  if ( w <= 0 ) return 0;\
  if ( x < 0 || x + w > dev->width ) return -1

/* ------ Internal routines ------ */

#define regen 0xa000

/* Construct a pointer for writing a pixel. */
/* Assume 64K pages, 64K granularity. */
#define set_pixel_ptr(ptr, fbdev, x, y, wnum)\
{	ulong index = (ulong)(y) * fbdev->raster + (uint)(x);\
	if ( (uint)(index >> 16) != fbdev->page )\
	   {	if ( y < 0 || y >= fbdev->height ) return -1;\
		(*fbdev->set_page)(fbdev, (fbdev->page = index >> 16), wnum);\
	   }\
	ptr = (fb_ptr)MK_PTR(regen, (ushort)index);\
}
#define set_pixel_write_ptr(ptr, fbdev, x, y)\
  set_pixel_ptr(ptr, fbdev, x, y, fbdev->wnum_write)
#define set_pixel_read_ptr(ptr, fbdev, x, y)\
  set_pixel_ptr(ptr, fbdev, x, y, fbdev->wnum_read)

/* Table structure for looking up graphics modes. */
typedef struct {
	int width, height;		/* "key" */
	int mode;			/* "value" */
} mode_info;

/* Find the graphics mode for a desired width and height. */
/* Return the graphics mode or -1. */
private int
svga_find_mode(gx_device *dev, mode_info _ds *mip)
{	for ( ; mip->mode >= 0; mip++ )
	   {	if ( mip->width == fb_dev->width &&
		     mip->height == fb_dev->height
		   )
			return mip->mode;
	   }
	return -1;
}

/* Set the index for writing into the color DAC. */
#define svga_dac_set_write_index(i) outportb(0x3c8, i)

/* Write 6-bit R,G,B values into the color DAC. */
#define svga_dac_write(r, g, b)\
  (outportb(0x3c9, r), outportb(0x3c9, g), outportb(0x3c9, b))

/* ------ Common procedures ------ */

/* Initialize the device structure and the DACs. */
private int
svga_open(gx_device *dev, int mode)
{	fb_dev->raster = fb_dev->width;
	fb_dev->x_pixels_per_inch =
	  fb_dev->y_pixels_per_inch =
	    fb_dev->height / 11.0;
	/* Set the display mode. */
	if ( svga_save_mode < 0 )
		svga_save_mode = (*fb_dev->get_mode)();
	(*fb_dev->set_mode)(mode);
	/* Load the color DAC. */
	svga_dac_set_write_index(0);
	   {	int c;
		for ( c = 0; c < 64; c++ )
		   {	static byte c2[10] =
			   { 0, 42, 0, 0, 0, 0, 0, 0, 21, 63 };
			svga_dac_write(c2[(c >> 2) & 9], c2[(c >> 1) & 9],
				       c2[c & 9]);
		   }
	   }
	/* Initialize the dynamic color table. */
	next_color_index = first_color_index;
	fb_dev->page = -1;
	return 0;
}

/* Close the device; reinitialize the display for text mode. */
private int
svga_close(gx_device *dev)
{	if ( svga_save_mode >= 0 )
		(*fb_dev->set_mode)(svga_save_mode);
	svga_save_mode = -1;
	return 0;
}

/* Map a r-g-b color to a palette index. */
/* The first 64 entries of the color map are set */
/* for compatibility with the older display modes: */
/* these are indexed as 0.0.R0.G0.B0.R1.G1.B1. */
private gx_color_index
svga_map_rgb_color(gx_device *dev, ushort r, ushort g, ushort b)
{
#define cv_bits(v,n) (v >> (gx_color_value_bits - n))
	ushort r5 = cv_bits(r, 5), g5 = cv_bits(g, 5), b5 = cv_bits(b, 5);
	static byte cube_bits[32] =
	   {	0, 128, 128, 128, 128, 128, 128, 128, 128, 128,
		8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
		1, 128, 128, 128, 128, 128, 128, 128, 128, 128,
		9
	   };
	uint cx = ((uint)cube_bits[r5] << 2) + ((uint)cube_bits[g5] << 1) +
		  (uint)cube_bits[b5];
	ushort rgb;
	/* Check for a color on the cube. */
	if ( cx < 64 ) return (gx_color_index)cx;
	/* Not on the cube, check the dynamic color table. */
	rgb = (r5 << 10) + (g5 << 5) + b5;
	   {	int i = next_color_index - first_color_index;
		register ushort _ds *pdc = dynamic_colors + i;
		while ( --i >= 0 )
		  if ( *--pdc == rgb )
		    return (gx_color_index)(i + first_color_index);
	   }
	/* Not on the cube, and not in the dynamic table. */
	/* Put in the dynamic table if space available. */
	if ( next_color_index < 255 )
	   {	int i = next_color_index++;
		dynamic_colors[i - first_color_index] = rgb;
		svga_dac_set_write_index(i);
		svga_dac_write(cv_bits(r, 6), cv_bits(g, 6), cv_bits(b, 6));
		return (gx_color_index)i;
	   }
	/* No space left, report failure. */
	return gx_no_color_index;
}

/* Map a color code to r-g-b. */
/* This routine must invert the transformation of the one above. */
/* Since this is practically never used, we just read the DAC. */
private int
svga_map_color_rgb(gx_device *dev, gx_color_index color, ushort prgb[3])
{	uint cval;
	outportb(0x3c7, (byte)color);
#define dacin() (cval = inportb(0x3c9) >> 1,\
  ((cval << 11) + (cval << 6) + (cval << 1) + (cval >> 4)) >>\
   (16 - gx_color_value_bits))
	prgb[0] = dacin();
	prgb[1] = dacin();
	prgb[2] = dacin();
#undef dacin
	return 0;
}

/* Copy a monochrome bitmap.  The colors are given explicitly. */
/* Color = gx_no_color_index means transparent (no effect on the image). */
private int
svga_copy_mono(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index czero, gx_color_index cone)
{	register int xi;
	uint skip = fb_dev->raster - w;
	int yi;
	fb_ptr ptr = (fb_ptr)0;
	byte *srow = base + (sourcex >> 3);
	uint imask = 0x80 >> (sourcex & 7);
	validate_rect();
#define izero (int)czero
#define ione (int)cone
	for ( yi = 0; yi < h; yi++ )
	   {	byte *sptr = srow;
		uint bits = *sptr;
		register uint mask = imask;
		if ( PTR_OFF(ptr) <= skip )
			set_pixel_write_ptr(ptr, fb_dev, x, y + yi);
		for ( xi = 0; xi < w; xi++ )
		   {	if ( PTR_OFF(ptr) == 0 )
				set_pixel_write_ptr(ptr, fb_dev, x + xi, y + yi);
			if ( bits & mask )
			   {	if ( ione != no_color ) *ptr = (byte)ione;
			   }
			else
			   {	if ( izero != no_color ) *ptr = (byte)izero;
			   }
			if ( !(mask >>= 1) ) mask = 0x80, bits = *++sptr;
			ptr++;
		   }
		ptr += skip;
		srow += raster;
	   }
#undef izero
#undef ione
	return 0;
}

/* Copy a color pixelmap.  This is just like a bitmap, */
/* except that each pixel takes 8 bits instead of 1. */
private int
svga_copy_color(gx_device *dev,
  byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	int xi, yi;
	int skip = raster - w;
	byte *sptr = base + sourcex;
	fb_ptr ptr;
	validate_rect();
	for ( yi = y; yi - y < h; yi++ )
	   {	ptr = 0;
		for ( xi = x; xi - x < w; xi++ )
		   {	if ( PTR_OFF(ptr) == 0 )
				set_pixel_write_ptr(ptr, fb_dev, xi, yi);
			*ptr++ = *sptr++;
		   }
		sptr += skip;
	   }
	return 0;
}

/* Fill a rectangle. */
private int
svga_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	uint raster = fb_dev->raster;
	ushort limit = (ushort)-raster;
	int yi;
	fb_ptr ptr;
	if ( x < 0 || x + w > dev->width ) return -1;	/* skip w */
	set_pixel_write_ptr(ptr, fb_dev, x, y);
	/* Most fills are very small and don't cross a page boundary. */
	yi = h;
	switch ( w )
	   {
	case 0: return 0;		/* no-op */
	case 1:
		while ( --yi >= 0 && PTR_OFF(ptr) < limit )
			ptr[0] = (byte)color,
			ptr += raster;
		if ( !++yi ) return 0;
		break;
	case 2:
		while ( --yi >= 0 && PTR_OFF(ptr) < limit )
			ptr[0] = ptr[1] = (byte)color,
			ptr += raster;
		if ( !++yi ) return 0;
		break;
	case 3:
		while ( --yi >= 0 && PTR_OFF(ptr) < limit )
			ptr[0] = ptr[1] = ptr[2] = (byte)color,
			ptr += raster;
		if ( !++yi ) return 0;
		break;
	case 4:
		while ( --yi >= 0 && PTR_OFF(ptr) < limit )
			ptr[0] = ptr[1] = ptr[2] = ptr[3] = (byte)color,
			ptr += raster;
		if ( !++yi ) return 0;
		break;
	default:
		if ( w < 0 ) return 0;
	   }
	while ( --yi >= 0 )
	   {	if ( PTR_OFF(ptr) < limit )
		   {	memset(ptr, (byte)color, w);
			ptr += raster;
		   }
		else if ( PTR_OFF(ptr) <= (ushort)(-w) )
		   {	memset(ptr, (byte)color, w);
			if ( yi > 0 )
				set_pixel_write_ptr(ptr, fb_dev, x, y + h - yi);
		   }
		else
		   {	uint left = (uint)0x10000 - PTR_OFF(ptr);
			memset(ptr, (byte)color, left);
			set_pixel_write_ptr(ptr, fb_dev, x + left, y + h - 1 - yi);
			memset(ptr, (byte)color, w - left);
			ptr += raster - left;
		   }
	   }
	return 0;
}

/* Read scan lines back from the frame buffer. */
int
svga_get_bits(gx_device *dev, int y, byte *data, uint size, int pad_to_word)
{	/* We don't have to worry about padding, because we read back */
	/* a byte per pixel and the frame buffer width is always */
	/* a multiple of 8 pixels. */
	uint bytes_per_row = dev->width;
	uint count = min(dev->height - y, size / bytes_per_row);
	byte *dest = data;
	ushort limit = (ushort)-bytes_per_row;
	int j;
	for ( j = count; j--; dest += bytes_per_row, y++ )
	   {	fb_ptr src;
		set_pixel_read_ptr(src, fb_dev, 0, y);
		/* The logic here is similar to fill_rectangle. */
		if ( PTR_OFF(src) <= limit )
			memcpy(dest, src, bytes_per_row);
		else
		   {	uint left = (uint)0x10000 - PTR_OFF(src);
			memcpy(dest, src, left);
			set_pixel_read_ptr(src, fb_dev, left, y);
			memcpy(dest + left, src, bytes_per_row - left);
		   }
	   }
	return count;
}

/* ------ The VESA device ------ */

private dev_proc_open_device(vesa_open);
private gx_device_procs vesa_procs = svga_procs(vesa_open);
private int vesa_get_mode(P0());
private void vesa_set_mode(P1(int));
private void vesa_set_page(P3(gx_device_svga *, int, int));
gx_device_svga gs_vesa_device =
	svga_device(vesa_procs, "vesa", vesa_get_mode, vesa_set_mode, vesa_set_page);

/* Define the structure for information returned by the BIOS. */
#define bits_include(a, m) !(~(a) & (m))
typedef enum {
	m_supported = 1,
	m_graphics = 0x10
} mode_attribute;
typedef enum {
	w_supported = 1,
	w_readable = 2,
	w_writable = 4
} win_attribute;
typedef struct {
	ushort mode_attributes;
	byte win_a_attributes;
	byte win_b_attributes;
	ushort win_granularity;
	ushort win_size;
	ushort win_a_segment;
	ushort win_b_segment;
	void (*win_func_ptr)(P2(int, int));
	ushort bytes_per_line;
		/* Optional information */
	ushort x_resolution;
	ushort y_resolution;
	byte x_char_size;
	byte y_char_size;
	byte number_of_planes;
	byte bits_per_pixel;
	byte number_of_banks;
	byte memory_model;
	byte bank_size;
} vesa_info;

/* Read the device mode */
private int
vesa_get_mode()
{	registers regs;
	regs.h.ah = 0x4f;
	regs.h.al = 0x03;
	int86(0x10, &regs, &regs);
	return regs.rshort.bx;
}

/* Set the device mode */
private void
vesa_set_mode(int mode)
{	registers regs;
	regs.h.ah = 0x4f;
	regs.h.al = 0x02;
	regs.rshort.bx = mode;
	int86(0x10, &regs, &regs);
}

/* Read information about a device mode */
private int
vesa_get_info(int mode, vesa_info _ss *info)
{	registers regs;
	struct SREGS sregs;
	regs.h.ah = 0x4f;
	regs.h.al = 0x01;
	regs.rshort.cx = mode;
	regs.rshort.di = PTR_OFF(info);
	segread(&sregs);
	sregs.es = sregs.ss;
	int86x(0x10, &regs, &regs, &sregs);
#ifdef DEBUG
	if ( regs.h.ah == 0 && regs.h.al == 0x4f )
		dprintf8("vesa_get_info(%x): ma=%x wa=%x/%x wg=%x ws=%x wseg=%x/%x\n",
			 mode, info->mode_attributes,
			 info->win_a_attributes, info->win_b_attributes,
			 info->win_granularity, info->win_size,
			 info->win_a_segment, info->win_b_segment);
	else
		dprintf3("vesa_get_info(%x) failed: ah=%x al=%x\n",
			 mode, regs.h.ah, regs.h.al);
#endif
	return (regs.h.ah == 0 && regs.h.al == 0x4f ? 0 : -1);
}

/* Initialize the graphics mode. */
private int
vesa_open(gx_device *dev)
{	/* Select the proper video mode */
	   {	vesa_info info;
		static mode_info mode_table[6] = {
		   {	 640,  400, 0x100	},
		   {	 640,  480, 0x101	},
		   {	 800,  600, 0x103	},
		   {	1024,  768, 0x105	},
		   {	1280, 1024, 0x107	},
		   {	-1, -1, -1	}
		};
		mode_info _ds *mip;
		for ( mip = mode_table; mip->mode >= 0; mip++ )
		   {	if ( mip->width == fb_dev->width &&
			     mip->height == fb_dev->height &&
			     vesa_get_info(mip->mode, &info) >= 0 &&
			     bits_include(info.mode_attributes,
				m_supported | m_graphics) &&
			     info.win_granularity == 64 &&
			     info.win_size == 64 &&
			     bits_include(info.win_a_attributes,
				w_supported) &&
			     info.win_a_segment == regen
			   )
			   {	/* Make sure we can both read & write. */
				/* Initialize for the default case. */
				fb_dev->wnum_read = 0;
				fb_dev->wnum_write = 0;
				if ( bits_include(info.win_a_attributes,
					w_readable | w_writable)
				   )
					break;
				else if ( info.win_b_segment == regen &&
					bits_include(info.win_b_attributes,
						w_supported) &&
					bits_include(info.win_a_attributes |
						info.win_b_attributes,
						w_readable | w_writable)
				   )
				   {	/* Two superimposed windows. */
					if ( !bits_include(info.win_a_attributes,
						w_writable)
					   )
						fb_dev->wnum_write = 1;
					else
						fb_dev->wnum_read = 1;
				   }
				break;
			   }
		   }
		if ( mip->mode < 0 ) return -1;	/* mode not available */
		fb_dev->info.vesa.bios_set_page = info.win_func_ptr;
		return svga_open(dev, mip->mode);
	   }
}

/* Set the current display page. */
private void
vesa_set_page(gx_device_svga *dev, int pn, int wnum)
{
#if USE_ASM
extern void vesa_call_set_page(P3(void (*)(P2(int, int)), int, int));
	if ( dev->info.vesa.bios_set_page != NULL )
		vesa_call_set_page(dev->info.vesa.bios_set_page, pn, wnum);
	else
#endif
	   {	registers regs;
		regs.rshort.dx = pn;
		regs.h.ah = 0x4f;
		regs.h.al = 5;
		regs.rshort.bx = wnum;
		int86(0x10, &regs, &regs);
	   }
}

/* ------ The ATI Wonder device ------ */

private dev_proc_open_device(atiw_open);
private gx_device_procs atiw_procs = svga_procs(atiw_open);
private int atiw_get_mode(P0());
private void atiw_set_mode(P1(int));
private void atiw_set_page(P3(gx_device_svga *, int, int));
gx_device_svga gs_atiw_device =
	svga_device(atiw_procs, "atiw", atiw_get_mode, atiw_set_mode, atiw_set_page);

/* Read the device mode */
private int
atiw_get_mode()
{	registers regs;
	regs.h.ah = 0xf;
	int86(0x10, &regs, &regs);
	return regs.h.al;
}

/* Set the device mode */
private void
atiw_set_mode(int mode)
{	registers regs;
	regs.h.ah = 0;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

/* Initialize the graphics mode. */
private int
atiw_open(gx_device *dev)
{	/* Select the proper video mode */
	   {	static mode_info mode_table[4] = {
		   {	 640,  400, 0x61	},
		   {	 640,  480, 0x62	},
		   {	 800,  600, 0x63	},
		   {	-1, -1, -1	}
		};
		int mode = svga_find_mode(dev, mode_table);
		if ( mode < 0 ) return -1;	/* mode not available */
		fb_dev->info.atiw.select_reg = *(int *)MK_PTR(0xc000, 0x10);
		return svga_open(dev, mode);
	   }
}

/* Set the current display page. */
private void
atiw_set_page(gx_device_svga *dev, int pn, int wnum)
{	int select_reg = dev->info.atiw.select_reg;
	byte reg;
	disable();
	outportb(select_reg, 0xb2);
	reg = inportb(select_reg + 1);
	outportb(select_reg, 0xb2);
	outportb(select_reg + 1, (reg & 0xe1) + (pn << 1));
	enable();
}

/* ------ The Tseng Labs ET3000/4000 device ------ */

private dev_proc_open_device(tseng_open);
private gx_device_procs tseng_procs = svga_procs(tseng_open);
/* We can use the tseng_get/set_mode procedures. */
private void tseng_set_page(P3(gx_device_svga *, int, int));
gx_device_svga gs_tseng_device =
	svga_device(tseng_procs, "tseng", atiw_get_mode, atiw_set_mode, tseng_set_page);

/* Initialize the graphics mode. */
private int
tseng_open(gx_device *dev)
{	fb_dev->wnum_read = 1;
	fb_dev->wnum_write = 0;
	/* Select the proper video mode */
	   {	static mode_info mode_table[5] = {
		   {	 640,  350, 0x2d	},
		   {	 640,  480, 0x2e	},
		   {	 800,  600, 0x30	},
		   {	 1024, 768, 0x38	},
		   {	-1, -1, -1	}
		};
		int mode = svga_find_mode(dev, mode_table);
		fb_ptr p0 = (fb_ptr)MK_PTR(regen, 0);
		if ( mode < 0 ) return -1;	/* mode not available */
		/* Figure out whether we have an ET3000 or an ET4000 */
		/* by playing with the segment register. */
		outportb(0x3cd, 0x44);
		*p0 = 4;		/* byte 0, page 4 */
		outportb(0x3cd, 0x40);
		*p0 = 3;		/* byte 0, page 0 */
		fb_dev->info.tseng.et_model = *p0;
					/* read page 0 if ET3000, */
					/* page 4 if ET4000 */
		return svga_open(dev, mode);
	   }
}

/* Set the current display page. */
private void
tseng_set_page(gx_device_svga *dev, int pn, int wnum)
{	/* The ET3000 has read page = 5:3, write page = 2:0; */
	/* the ET4000 has read page = 7:4, write page = 3:0. */
	int shift = dev->info.tseng.et_model;
	int mask = (1 << shift) - 1;
	if ( wnum ) pn <<= shift, mask <<= shift;
	outportb(0x3cd, (inportb(0x3cd) & ~mask) + pn);
}
