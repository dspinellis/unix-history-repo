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

/* gdevpjet.c */
/* H-P PaintJet driver for Ghostscript */
#include "gdevprn.h"
#include "gdevpcl.h"

/* X_DPI and Y_DPI must be the same, and may be either 90 or 180. */
#define X_DPI 180
#define Y_DPI 180

/* We round up LINE_SIZE to a multiple of 8 bytes */
/* because that's the unit of transposition from pixels to planes. */
#define LINE_SIZE ((X_DPI * 85 / 10 + 63) / 64 * 8)

/* The device descriptor */
private dev_proc_print_page(paintjet_print_page);
private gx_device_procs paintjet_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);
gx_device_printer gs_paintjet_device =
  prn_device(paintjet_procs, "paintjet",
	85,				/* width_10ths, 8.5" */
	110,				/* height_10ths, 11" */
	X_DPI, Y_DPI,
	0.25, 0, 0.25, 0,		/* margins */
	3, paintjet_print_page);

/* Forward references */
private int compress1_row(P3(byte _ss *, byte _ss *, byte _ss *));

/* ------ Internal routines ------ */

/* Send the page to the printer.  Compress each scan line. */
private int
paintjet_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
#define DATA_SIZE (LINE_SIZE * 8)
	byte data[DATA_SIZE];
	byte plane_data[LINE_SIZE * 3];

	/* ends raster graphics to set raster graphics resolution */
	fputs("\033*rB", prn_stream);

	/* set raster graphics resolution -- 90 or 180 dpi */
	fprintf(prn_stream, "\033*t%dR", X_DPI);

	/* set the line width */
	fprintf(prn_stream, "\033*r%dS", DATA_SIZE);

	/* set the number of color planes */
	fprintf(prn_stream, "\033*r%dU", 3);		/* always 3 */

	/* move to top left of page */
	fputs("\033&a0H\033&a0V", prn_stream);

	/* select data compression */
	fputs("\033*b1M", prn_stream);

	/* start raster graphics */
	fputs("\033*r1A", prn_stream);

	/* Send each scan line in turn */
	   {	int lnum;
		int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
		int num_blank_lines = 0;
		for ( lnum = 0; lnum < pdev->height; lnum++ )
		   {	byte _ss *end_data = data + line_size;
			gdev_prn_copy_scan_lines(pdev, lnum,
						 (byte *)data, line_size);
			/* Remove trailing 0s. */
			while ( end_data > data && end_data[-1] == 0 )
				end_data--;
			if ( end_data == data )
			   {	/* Blank line */
				num_blank_lines++;
			   }
			else
			   {	int i;
				byte _ss *odp;
				byte _ss *row;

				/* Pad with 0s to fill out the last */
				/* block of 8 bytes. */
				memset(end_data, 0, 7);

				/* Transpose the data to get pixel planes. */
				for ( i = 0, odp = plane_data; i < DATA_SIZE;
				      i += 8, odp++
				    )
				 { /* The following is for 16-bit machines */
#define spread3(c)\
 { 0, c, c*0x100, c*0x101, c*0x10000L, c*0x10001L, c*0x10100L, c*0x10101L }
				   static ulong spr40[8] = spread3(0x40);
				   static ulong spr8[8] = spread3(8);
				   static ulong spr2[8] = spread3(2);
				   register byte _ss *dp = data + i;
				   register ulong pword =
				     (spr40[dp[0]] << 1) +
				     (spr40[dp[1]]) +
				     (spr40[dp[2]] >> 1) +
				     (spr8[dp[3]] << 1) +
				     (spr8[dp[4]]) +
				     (spr8[dp[5]] >> 1) +
				     (spr2[dp[6]]) +
				     (spr2[dp[7]] >> 1);
				   odp[0] = (byte)(pword >> 16);
				   odp[LINE_SIZE] = (byte)(pword >> 8);
				   odp[LINE_SIZE*2] = (byte)(pword);
				 }
				/* Skip blank lines if any */
				if ( num_blank_lines > 0 )
				   {	/* move down from current position */
					fprintf(prn_stream, "\033&a+%dV",
						num_blank_lines * (720 / Y_DPI));
					num_blank_lines = 0;
				   }

				/* Transfer raster graphics */
				/* in the order R, G, B. */
				for ( row = plane_data + LINE_SIZE * 2, i = 0;
				      i < 3; row -= LINE_SIZE, i++
				    )
				   {	byte temp[LINE_SIZE * 2];
					int count = compress1_row(row, row + LINE_SIZE, temp);
					fprintf(prn_stream, "\033*b%d%c",
						count, "VVW"[i]);
					fwrite(temp, sizeof(byte),
					       count, prn_stream);
				   }
			   }
		   }
	   }

	/* end raster graphics */
	fputs("\033*r0B", prn_stream);

	/* eject page */
	fputs("\014", prn_stream);

	return 0;
}

/*
 * Row compression for the H-P PaintJet.
 * Compresses data from row up to end_row, storing the result
 * starting at compressed.  Returns the number of bytes stored.
 * The compressed format consists of a byte N followed by a
 * data byte that is to be repeated N+1 times.
 * In the worst case, the `compressed' representation is
 * twice as large as the input.
 * We complement the bytes at the same time, because
 * we accumulated the image in complemented form.
 */
private int
compress1_row(byte _ss *row, byte _ss *end_row, byte _ss *compressed)
{	register byte _ss *in = row;
	register byte _ss *out = compressed;
	while ( in < end_row )
	   {	byte test = *in++;
		byte _ss *run = in;
		while ( in < end_row && *in == test ) in++;
		/* Note that in - run + 1 is the repetition count. */
		while ( in - run > 255 )
		   {	*out++ = 255;
			*out++ = ~test;
			run += 256;
		   }
		*out++ = in - run;
		*out++ = ~test;
	   }
	return out - compressed;
}
