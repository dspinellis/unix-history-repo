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

/* gdevdjet.c */
/* HP LaserJet/DeskJet driver for Ghostscript */
#include "gdevprn.h"
#include "gdevpcl.h"

/* Thanks to Jim Mayer, Xerox Webster Research Center, */
/* and to Jan-Mark Wams (jms@cs.vu.nl) for improvements. */

/*
 * You may select a resolution of 75, 100, 150, or 300 DPI.
 * Normally you would do this in the makefile or on the gs command line,
 * not here.
 */
/*#define X_DPI 300*/
/*#define Y_DPI 300*/

/*
 * Standard U.S. page width and height.  A4 paper is 8.4" x 11.7".
 */
#define WIDTH_10THS 85
#define HEIGHT_10THS 110

#define X_DPI_MAX 300
#define Y_DPI_MAX 300
/* We round up the LINE_SIZE to a multiple of a ulong for faster scanning. */
#define W sizeof(word)
#define LINE_SIZE ((X_DPI_MAX * 85 / 10 + W * 8 - 1) / (W * 8) * W)
#ifndef X_DPI
#  define X_DPI X_DPI_MAX
#endif
#ifndef Y_DPI
#  define Y_DPI Y_DPI_MAX
#endif

/* Printer types */
#define LJ	0
#define LJplus	1
#define LJ2p	2
#define LJ3	3
#define DJ	4
#define DJ500	5

/* The printer initialization strings. */
private char *init_strings[] = {
	/* LaserJet PCL 3, no compression */
		"\033*r0A\033&l0E\033*p0x0Y\033*b0M",
	/* LaserJet Plus PCL 3, no compression */
		"\033*r0A\033&l0E\033*p0x0Y\033*b0M",
	/* LaserJet IIP PCL 4, mode 2 compression */
		"\033*r0f0A\033&l0E\033&l-185U\033*p0x0Y\033*b2M",
	/* LaserJet III PCL 5, mode 2&3 compression */
		"\033*r0f0A\033&l0E\033&l-185U\033*p0x0Y",
	/* DeskJet almost PCL 4, mode 2 compression */
		/* Key to codes: set 8.5x11 paper; portrait orientation; */
		/* bidirectional printing; no perf-skip; move to top left; */
		/* start raster graphics; mode 2 compression. */
		"\033&l2A\033&l0O\033&k1W\033&l0L\033*p0x0Y\033*r0A\033*b2M",
	/* DeskJet 500 almost PCL 4, mode 2&3 compression */
		"\033&l2A\033&l0O\033&k1W\033&l0L\033*p0x0Y\033*r0A",
};

/* The patterns for skipping blank lines. */
/* (These are all the same now, but I'm not 100% sure this is right.) */
private char *skip_patterns[] = {
	/* LaserJet PCL 3 */
		"\033*p+%dY",
	/* LaserJet Plus PCL 3 */
		"\033*p+%dY",
	/* LaserJet IIP PCL 4 */
		"\033*p+%dY",
	/* LaserJet III PCL 5 */
		"\033*p+%dY",
	/* DeskJet almost PCL 4 */
		"\033*p+%dY",
	/* DeskJet 500 almost PCL 4 */
		"\033*p+%dY"
};

/* The device descriptors */
private dev_proc_print_page(djet_print_page);
private dev_proc_print_page(djet500_print_page);
private dev_proc_print_page(ljet_print_page);
private dev_proc_print_page(ljetplus_print_page);
private dev_proc_print_page(ljet2p_print_page);
private dev_proc_print_page(ljet3_print_page);

/*
 * HP printers need their own get_initial_matrix procedure.  See the
 * definition of hp_get_initial_matrix for the reason why.
 */
private dev_proc_get_initial_matrix(hp_get_initial_matrix);

/* See gdevprn.h and gdevprn.c to understand the NULL entries below. */
gx_device_procs prn_hp_procs = {
	gdev_prn_open,
	hp_get_initial_matrix,
	gx_default_sync_output,
	gdev_prn_output_page,
	gdev_prn_close,
	gdev_prn_map_rgb_color,
	gdev_prn_map_color_rgb,
	NULL,	/* fill_rectangle */
	NULL,	/* tile_rectangle */
	NULL,	/* copy_mono */
	NULL,	/* copy_color */
	NULL,	/* draw_line */
	NULL,	/* get_bits */
	gdev_prn_get_props,
	gdev_prn_put_props
};

gx_device_printer gs_deskjet_device =
  prn_device(prn_hp_procs, "deskjet",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.25, 0.50, 0.25, 0.0625,	/* margins */
	1, djet_print_page);

gx_device_printer gs_djet500_device =
  prn_device(prn_hp_procs, "djet500",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.25, 0.50, 0.25, 0.0625,	/* margins */
	1, djet500_print_page);

gx_device_printer gs_laserjet_device =
  prn_device(prn_hp_procs, "laserjet",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.05, 0.25, 0.55, 0.25,		/* margins */
	1, ljet_print_page);

gx_device_printer gs_ljetplus_device =
  prn_device(prn_hp_procs, "ljetplus",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.05, 0.25, 0.55, 0.25,		/* margins */
	1, ljetplus_print_page);

gx_device_printer gs_ljet2p_device =
  prn_device(prn_hp_procs, "ljet2p",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.20, 0.25, 0.25, 0.25,		/* margins */
	1, ljet2p_print_page);

gx_device_printer gs_ljet3_device =
  prn_device(prn_hp_procs, "ljet3",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0.20, 0.25, 0.25, 0.25,		/* margins */
	1, ljet3_print_page);

/* Forward references */
private int hpjet_print_page(P3(gx_device_printer *, FILE *, int));

/* ------ Internal routines ------ */

/* The DeskJet can compress (mode 2) */
private int
djet_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, DJ);
}
/* The DeskJet500 can compress (mode 3) */
private int
djet500_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, DJ500);
}
/* The LaserJet series II can't compress */
private int
ljet_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, LJ);
}
/* The LaserJet Plus can't compress */
private int
ljetplus_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, LJplus);
}
/* All LaserJet series IIIs (III,IIId,IIIp,IIIsi) compress (mode 3) */
private int
ljet3_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, LJ3);
}
/* LaserJet series IIp & IId compress (mode 2) */
private int
ljet2p_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hpjet_print_page(pdev, prn_stream, LJ2p);
}

/* Send the page to the printer.  For speed, compress each scan line, */
/* since computer-to-printer communication time is often a bottleneck. */
private int
hpjet_print_page(gx_device_printer *pdev, FILE *prn_stream, int compress)
{	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	int line_size_words = (line_size + W - 1) / W;
	uint storage_size_words = line_size_words * 8; /* data, out_row, out_row_alt, prev_row */
	word *storage = (ulong *)gs_malloc(storage_size_words, W,
					   "hpjet_print_page");
	word
	  *data_words,
	  *out_row_words,
	  *out_row_alt_words,
	  *prev_row_words;
#define data ((char *)data_words)
#define out_row ((char *)out_row_words)
#define out_row_alt ((char *)out_row_alt_words)
#define prev_row ((char *)prev_row_words)
	char *out_data;
	int x_dpi = pdev->x_pixels_per_inch;
	int out_count;
	int compression = -1;

	if ( storage == 0 ) return -1; /* can't allocate working area */
	data_words = storage;
	out_row_words = data_words + (line_size_words * 2);
	out_row_alt_words = out_row_words + (line_size_words * 2);
	prev_row_words = out_row_alt_words + (line_size_words * 2);
	/* Clear temp storage */
	memset(data, 0, storage_size_words * W);

	/* Initialize printer. */
	fputs("\033E", prn_stream);		/* reset printer */
	fputs("\033*rB", prn_stream);		/* end raster graphics */
	fprintf(prn_stream, "\033*t%dR", x_dpi);	/* set resolution */
	fputs(init_strings[compress], prn_stream);

	/* Send each scan line in turn */
	   {	int lnum;
		int num_blank_lines = 0;
		word rmask = ~(word)0 << (-pdev->width & (W * 8 - 1));
		/* look for top margin white space... You would think that
		   the normal (raster) white space mechanism would work... it
		   doesn't... Sometimes PCL printers are brain-dead */
		for ( lnum = 0; lnum < pdev->height; lnum++ )
		   {	register word *end_data = data_words + line_size_words;
			gdev_prn_copy_scan_lines(pdev, lnum,
						 (byte *)data, line_size);
		   	/* Mask off 1-bits beyond the line width. */
			end_data[-1] &= rmask;
			/* Remove trailing 0s. */
			while ( end_data > data_words && end_data[-1] == 0 )
			  end_data--;
			if ( end_data == data_words )
			   /* Blank line */
			   num_blank_lines++;
			else
			   break;
		   }
		/* Skip blank lines if any */
		if ( num_blank_lines > 0 )
		  {	/* move down from current position */
		     	fprintf(prn_stream,"\033*p+%dY", num_blank_lines);
		     	num_blank_lines = 0;
		  }
		     /* transfer raster graphics */
		for ( ; lnum < pdev->height; lnum++ )
		   {	register word *end_data = data_words + line_size_words;
			gdev_prn_copy_scan_lines(pdev, lnum,
						 (byte *)data, line_size);
		   	/* Mask off 1-bits beyond the line width. */
			end_data[-1] &= rmask;
			/* Remove trailing 0s. */
			while ( end_data > data_words && end_data[-1] == 0 )
			  end_data--;
			if ( end_data == data_words )
			   {	/* Blank line */
				num_blank_lines++;
				continue;
			   }
			switch (compress)
			  {
			  case LJ3:
			  case DJ500:
			   {	/* Compression modes 2 and 3 are both */
				/* available.  Try both and see which one */
				/* produces the least output data. */
				int count3 = gdev_pcl_mode3compress(line_size, data,
							   prev_row, out_row);
				int count2 = gdev_pcl_mode2compress(data_words, end_data,
							   out_row_alt);
				int penalty3 = (compression == 3 ? 0 : 5);
				int penalty2 = (compression == 2 ? 0 : 5);
				if ( count3 + penalty3 < count2 + penalty2)
				   {	if ( compression != 3 )
					    fputs("\033*b3M", prn_stream);
					compression = 3;
					out_data = out_row;
					out_count = count3;
				   }
				else
				   {	if ( compression != 2 )
					    fputs("\033*b2M", prn_stream);
					compression = 2;
					out_data = out_row_alt;
					out_count = count2;
				   }
				break;
			   }
			  case DJ:
			  case LJ2p:
				out_data = out_row;
			   	out_count = gdev_pcl_mode2compress(data_words, end_data,
							  out_row);
				break;
			  default:
				out_data = data;
				out_count = (char *)end_data - data;
			  }
			/* Skip blank lines if any */
			if ( num_blank_lines > 0 )
			   {	/* move down from current position */
				fprintf(prn_stream, skip_patterns[compress],
					num_blank_lines);
				num_blank_lines = 0;
			   }
			/* transfer raster graphics */
			fprintf(prn_stream, "\033*b%dW", out_count);
			/* send the row */
			fwrite(out_data, sizeof(char), out_count,
			       prn_stream);
		   }
	}

	/* end raster graphics */
	fputs("\033*rB", prn_stream);

	/* eject page */
	fputs("\033&l0H", prn_stream);

	/* free temporary storage */
	gs_free((char *)storage, storage_size_words, W, "hpjet_print_page");

	return 0;
}


/*
 * PCL has the notion of a LOGICAL and a PHYSICAL page.  The PHYSICAL page is
 * the actual paper; the LOGICAL page is the printer specific imageable area.
 * The strange part is that coordinates are all relative to the
 * LOGICAL page.  This means that all PCL code is inherently device dependent.
 * Luckily, in PostScript land, transformation matrices conquer all.
 */

private void
hp_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{       pmat->xx = dev->x_pixels_per_inch / 72.0;
	pmat->xy = 0;
	pmat->yx = 0;
	pmat->yy = dev->y_pixels_per_inch / -72.0;
	pmat->tx = -(dev->l_margin * dev->x_pixels_per_inch);
	pmat->ty = dev->height - (dev->t_margin * dev->y_pixels_per_inch);
}
