/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevpbm.c */
/* Portable Bit/Gray/PixMap devices for Ghostscript. */
#include "gdevprn.h"
#include "gxlum.h"

/* Thanks are due to Jos Vos (jos@bull.nl) for an earlier P*M driver, */
/* on which this one is based. */

/* Structure for P*M devices, which extend the generic printer device. */

#define MAX_COMMENT 70			/* max user-supplied comment */
struct gx_device_pbm_s {
	gx_device_common;
	gx_prn_device_common;
	/* Additional state for P*M devices */
	const char *magic;		/* "Pn" */
	char comment[MAX_COMMENT + 1];	/* comment for head of file */
	byte is_raw;			/* 1 if raw format, 0 if plain */
	byte is_pbm;			/* 1 if bitmap, 0 if gray or pixmap */
};
typedef struct gx_device_pbm_s gx_device_pbm;

/* ------ The device descriptors ------ */

/*
 * Standard U.S. page width and height.  A4 paper is 8.4" x 11.7".
 */
#define WIDTH_10THS 85
#define HEIGHT_10THS 110

/*
 * Default X and Y resolution.
 */
#define X_DPI 72
#define Y_DPI 72

/* Macro for generating P*M device descriptors. */
#define pbm_prn_device(procs, dev_name, magic, is_raw, num_comp, depth, max_gray, max_rgb, print_page)\
{	prn_device_body(gx_device_pbm, procs, dev_name,\
	  WIDTH_10THS, HEIGHT_10THS, X_DPI, Y_DPI,\
	  0, 0, 0, 0,\
	  num_comp, depth, max_gray, max_rgb, max_gray + 1, max_rgb + 1,\
	  print_page),\
	magic,\
	 { 0 },\
	is_raw,\
	(depth == 1)\
}

/* For all PBM variants we do some extra things at opening time. */
/* private dev_proc_open_device(gdev_pbm_open); */
#define gdev_pbm_open gdev_prn_open		/* no we don't! */

/* For PGM and PPM we need our own color mapping procedures. */
private dev_proc_map_rgb_color(pgm_map_rgb_color);
private dev_proc_map_rgb_color(ppm_map_rgb_color);
private dev_proc_map_color_rgb(pgm_map_color_rgb);
private dev_proc_map_color_rgb(ppm_map_color_rgb);

/* And of course we need our own print-page routines. */
private dev_proc_print_page(pbm_print_page);
private dev_proc_print_page(pbm_raw_print_page);
private dev_proc_print_page(pgm_print_page);
private dev_proc_print_page(ppm_print_page);

/* The device procedures */
private gx_device_procs pbm_procs =
    prn_procs(gdev_pbm_open, gdev_prn_output_page, gdev_prn_close);
private gx_device_procs pgm_procs =
    prn_color_procs(gdev_pbm_open, gdev_prn_output_page, gdev_prn_close,
	pgm_map_rgb_color, pgm_map_color_rgb);
private gx_device_procs ppm_procs =
    prn_color_procs(gdev_pbm_open, gdev_prn_output_page, gdev_prn_close,
	ppm_map_rgb_color, ppm_map_color_rgb);

/* The device descriptors themselves */
gx_device_pbm gs_pbm_device =
  pbm_prn_device(pbm_procs, "pbm", "P1", 0, 1, 1, 1, 0,
	  pbm_print_page);
gx_device_pbm gs_pbmraw_device =
  pbm_prn_device(pbm_procs, "pbmraw", "P4", 1, 1, 1, 1, 1,
	  pbm_raw_print_page);
gx_device_pbm gs_pgm_device =
  pbm_prn_device(pgm_procs, "pgm", "P2", 0, 1, 8, 255, 0,
	  pgm_print_page);
gx_device_pbm gs_pgmraw_device =
  pbm_prn_device(pgm_procs, "pgmraw", "P5", 1, 1, 8, 255, 0,
	  pgm_print_page);
gx_device_pbm gs_ppm_device =
  pbm_prn_device(ppm_procs, "ppm", "P3", 0, 3, 24, 255, 255,
	  ppm_print_page);
gx_device_pbm gs_ppmraw_device =
  pbm_prn_device(ppm_procs, "ppmraw", "P6", 1, 3, 24, 255, 255,
	  ppm_print_page);

/* ------ Color mapping routines ------ */

/* Map an RGB color to a PGM gray value. */
private gx_color_index
pgm_map_rgb_color(gx_device *dev, ushort r, ushort g, ushort b)
{	/* We round the value rather than truncating it. */
	return ((r * (ulong)lum_red_weight) +
		(g * (ulong)lum_green_weight) +
		(b * (ulong)lum_blue_weight) +
		(lum_all_weights / 2))
	       / lum_all_weights * dev->color_info.max_gray / gx_max_color_value;
}

/* Map a PGM gray value back to an RGB color. */
private int
pgm_map_color_rgb(gx_device *dev, gx_color_index color, ushort prgb[3])
{	gx_color_value gray =
		color * gx_max_color_value / dev->color_info.max_gray;
	prgb[0] = gray;
	prgb[1] = gray;
	prgb[2] = gray;
	return 0;
}

/* Map an RGB color to a PPM color tuple. */
private gx_color_index
ppm_map_rgb_color(gx_device *dev, ushort r, ushort g, ushort b)
{	ushort bitspercolor = dev->color_info.depth / 3;
	ulong max_value = (1 << bitspercolor) - 1;
	return ((r * max_value / gx_max_color_value) << (bitspercolor * 2)) +
	       ((g * max_value / gx_max_color_value) << bitspercolor) +
	       (b * max_value / gx_max_color_value);
}

/* Map a PPM color tuple back to an RGB color. */
private int
ppm_map_color_rgb(gx_device *dev, gx_color_index color, ushort prgb[3])
{	ushort bitspercolor = dev->color_info.depth / 3;
	ushort colormask = (1 << bitspercolor) - 1;

	prgb[0] = ((color >> (bitspercolor * 2)) & colormask) *
		(ulong)gx_max_color_value / colormask;
	prgb[1] = ((color >> bitspercolor) & colormask) *
		(ulong)gx_max_color_value / colormask;
	prgb[2] = (color & colormask) *
		(ulong)gx_max_color_value / colormask;
	return 0;
}

/* ------ Internal routines ------ */

/* Define a "cursor" that keeps track of where we are in the page. */
typedef struct pbm_cursor_s {
	gx_device_pbm *dev;
	int bpp;			/* bits per pixel */
	uint line_size;			/* bytes per scan line */
	byte *data;			/* output row buffer */
	int lnum;			/* row within page */
} pbm_cursor;

/* Begin a P*M output page. */
/* Write the header information and initialize the cursor. */
private int
pbm_begin_page(gx_device_pbm *bdev, FILE *pstream, pbm_cursor _ss *pcur)
{	uint line_size =
		gdev_prn_bytes_per_scan_line((gx_device_printer *)bdev);
	byte *data = (byte *)gs_malloc(line_size, 1, "pbm_begin_page");
	if ( data == 0 ) return -1;
	fprintf(pstream, "%s\n", bdev->magic);
	if ( bdev->comment[0] )
		fprintf(pstream, "# %s\n", bdev->comment);
	else
		fprintf(pstream, "# Image generated by Ghostscript (device=%s)\n",
			bdev->dname);
	fprintf(pstream, "%d %d\n", bdev->width, bdev->height);
	if ( !bdev->is_pbm )
		fprintf(pstream, "%d\n", bdev->color_info.max_gray);
	/* Initialize the cursor. */
	pcur->dev = bdev;
	pcur->bpp = bdev->color_info.depth;
	pcur->line_size = line_size;
	pcur->data = data;
	pcur->lnum = 0;
	return 0;
}

/* Advance to the next row.  Return 0 if more, 1 if done. */
private int
pbm_next_row(pbm_cursor _ss *pcur)
{	if ( pcur->lnum >= pcur->dev->height )
	   {	gs_free((char *)pcur->data, pcur->line_size, 1,
			"pbm_next_row(done)");
		return 1;
	   }
	gdev_prn_copy_scan_lines((gx_device_printer *)pcur->dev,
				 pcur->lnum++, pcur->data, pcur->line_size);
	return 0;
}

/* ------ Individual page printing routines ------ */

#define bdev ((gx_device_pbm *)pdev)

/* Print a raw monobit page. */
private int
pbm_raw_print_page(gx_device_printer *pdev, FILE *pstream)
{	/* All the print_page routines have the same structure; */
	/* only the indicated section is different. */
	pbm_cursor cur;
	int code = pbm_begin_page(bdev, pstream, &cur);
	if ( code < 0 ) return code;
	while ( !(code = pbm_next_row(&cur)) )
	   {
		/* ---- This section changes. ---- */

		fwrite(cur.data, 1, cur.line_size, pstream);

		/* ---- End of changing section. ---- */
	   }
	return (code < 0 ? code : 0);
}

/* Print an ASCII monobit page. */
private int
pbm_print_page(gx_device_printer *pdev, FILE *pstream)
{	pbm_cursor cur;
	int code = pbm_begin_page(bdev, pstream, &cur);
	if ( code < 0 ) return code;
	while ( !(code = pbm_next_row(&cur)) )
	   {	byte *bp;
		uint x, mask;
		for ( bp = cur.data, x = 0, mask = 0x80; x < bdev->width;
		      (mask >>= 1) != 0 || (bp++, mask = 0x80)
		    )
		   {	putc((*bp & mask ? '1' : '0'), pstream);
			if ( ++x == bdev->width || !(x & 63) )
				putc('\n', pstream);
		   }
	   }
	return (code < 0 ? code : 0);
}

/* Print a gray-mapped page. */
private int
pgm_print_page(gx_device_printer *pdev, FILE *pstream)
{	pbm_cursor cur;
	int code = pbm_begin_page(bdev, pstream, &cur);
	uint mask;
	if ( code < 0 ) return code;
	/* Note that bpp <= 8 for raw format, bpp <= 16 for plain. */
	mask = (1 << cur.bpp) - 1;
	while ( !(code = pbm_next_row(&cur)) )
	   {	byte *bp;
		uint x;
		int shift;
		for ( bp = cur.data, x = 0, shift = 8 - cur.bpp;
		      x < bdev->width;
		    )
		   {	uint pixel;
			if ( shift < 0 )	/* bpp = 16 */
			   {	pixel = ((uint)*bp << 8) + bp[1];
				bp += 2;
			   }
			else
			   {	pixel = (*bp >> shift) & mask;
				if ( (shift -= cur.bpp) < 0 )
					bp++, shift += 8;
			   }
			++x;
			if ( bdev->is_raw )
				putc(pixel, pstream);
			else
				fprintf(pstream, "%d%c", pixel,
					(x == bdev->width || !(x & 15) ?
					 '\n' : ' '));
		   }
	   }
	return (code < 0 ? code : 0);
}

/* Print a color-mapped page. */
private int
ppm_print_page(gx_device_printer *pdev, FILE *pstream)
{	pbm_cursor cur;
	int code = pbm_begin_page(bdev, pstream, &cur);
	uint bpe, mask;
	if ( code < 0 ) return code;
	/* Note that bpp <= 24 for raw format, bpp <= 32 for plain. */
	bpe = cur.bpp / 3;		/* bits per r/g/b element */
	mask = (1 << bpe) - 1;
	while ( !(code = pbm_next_row(&cur)) )
	   {	byte *bp;
		uint x;
		int shift;
		for ( bp = cur.data, x = 0, shift = 8 - cur.bpp;
		      x < bdev->width;
		    )
		   {	ulong pixel = 0;
			uint r, g, b;
			switch ( cur.bpp >> 3 )
			   {
			case 3:
				pixel = (ulong)*bp << 16; bp++;
				/* falls through */
			case 2:
				pixel += (uint)*bp << 8; bp++;
				/* falls through */
			case 1:
				pixel += *bp; bp++;
				break;
			case 0:			/* bpp == 4, bpe == 1 */
				pixel = *bp >> shift;
				if ( (shift -= cur.bpp) < 0 )
					bp++, shift += 8;
				break;
			   }
			++x;
			b = pixel & mask;  pixel >>= bpe;
			g = pixel & mask;  pixel >>= bpe;
			r = pixel & mask;
			if ( bdev->is_raw )
			   {	putc(r, pstream);
				putc(g, pstream);
				putc(b, pstream);
			   }
			else
				fprintf(pstream, "%d %d %d%c", r, g, b,
					(x == bdev->width || !(x & 7) ?
					 '\n' : ' '));
		   }
	   }
	return (code < 0 ? code : 0);
}
