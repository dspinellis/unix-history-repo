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

/* gdevdjtc.c */
/* HP DeskJet 500C driver for Ghostscript */
#include "gdevprn.h"
#include "gdevpcl.h"

/***
 *** Note: this driver was contributed by a user, Alfred Kayser:
 ***       please contact AKayser@et.tudelft.nl if you have questions.
 ***/

#define X_DPI 300
#define Y_DPI 300
/* bytes per line for DeskJet Color */
#define LINE_SIZE ((X_DPI * 85 / 10 + 63) / 64 * 8)

/* The device descriptors */
private dev_proc_print_page(djet500c_print_page);

private gx_device_procs djet500c_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);

gx_device_printer gs_djet500c_device =
  prn_device(djet500c_procs, "djet500c",
	85,				/* width_10ths, 8.5" */
	120,				/* height_10ths, 12" */
	X_DPI, Y_DPI,
	0.25, 0.25, 0.25, 0.25,		/* margins */
	3, djet500c_print_page);

/* Forward references */
private int djet500c_print_page(P2(gx_device_printer *, FILE *));

/* The DeskJet 500C uses additive colors in separate planes. */
/* We only keep one bit of color, with 1 = R, 2 = G, 4 = B. */
/* Because the buffering routines assume 0 = white, */
/* we complement all the color components. */
#define cv_shift (sizeof(gx_color_value) * 8 - 1)

/* Map an RGB color to a printer color. */
private gx_color_index
djet500c_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	return (((b >> cv_shift) << 2) + ((g >> cv_shift) << 1) + (r >> cv_shift)) ^ 7;
}

/* Map the printer color back to RGB. */
private int
djet500c_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	ushort cc = (ushort)color ^ 7;
	prgb[0] = -(cc & 1);
	prgb[1] = -((cc >> 1) & 1);
	prgb[2] = -(cc >> 2);
	return 0;
}

/* Send the page to the printer.  For speed, compress each scan line, */
/* since computer-to-printer communication time is often a bottleneck. */
/* The DeskJet Color can compress (mode 3) */

private int
djet500c_print_page(gx_device_printer *pdev, FILE *fprn)
{
#define DATA_SIZE (LINE_SIZE*8)
	byte data[DATA_SIZE];
	byte plane_data[DATA_SIZE];

	/* select the most compressed mode available & clear tmp storage */
	/* put printer in known state */
	fputs("\033E",fprn);
	/* ends raster graphics to set raster graphics resolution */
	fputs("\033*rbC", fprn);	/*  was \033*rB  */

	/* set raster graphics resolution -- 300 dpi */
	fputs("\033*t300R", fprn);
	/* move to top left of page & set current position */
	fputs("\033*p0x0Y", fprn); /* cursor pos: 0,0 */
	fputs("\033*r0A", fprn);  /* start graf. left */
	fputs("\033*b2M", fprn);	/*  mode 3 compression for now  */
	fputs("\033*r3U", fprn);    /* RGB Mode */
	fputs("\033&l26a0l1H", fprn); /* A4, skip perf, def. paper tray */ 

	/* Send each scan line in turn */
   	{	int lnum;
		int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
		int num_blank_lines = 0;
		for (lnum=0; lnum<pdev->height; lnum++)
		{	
			byte _ss *end_data = data + line_size;
			gdev_prn_copy_scan_lines(pdev, lnum, (byte *)data, line_size);

			/* Remove trailing 0s. */
			while ( end_data > data && end_data[-1] == 0 )
				end_data--;
			if ( end_data == data )
				num_blank_lines++;
			else
			{	int i;
				byte _ss *odp;
				byte _ss *row;

				/* Pad with 0s to fill out the last */
				/* block of 8 bytes. */
				memset(end_data, 0, 7);

				/* Transpose the data to get pixel planes. */
				for (i=0, odp=plane_data; i<DATA_SIZE; i+=8, odp++)
				{ 
				   register ushort t, r, g, b;
				   for (r=g=b=t=0;t<8;t++)
				   {
						r = (r<<1) | (data[t+i]&4);
						g = (g<<1) | (data[t+i]&2);
						b = (b<<1) | (data[t+i]&1);
				   }
				   odp[0] = (byte)r ^ 0xff;
				   odp[LINE_SIZE] = (byte)(g>>1) ^ 0xff;
				   odp[LINE_SIZE*2] = (byte)(b>>2) ^ 0xff;
				}

				/* Skip blank lines if any */
				if ( num_blank_lines > 0 )
				{	/* move down from current position */
					fprintf(fprn, "\033*b%dY", num_blank_lines);
					num_blank_lines = 0;
				}

				/* Transfer raster graphics */
				/* in the order R, G, B. */
				row=plane_data+LINE_SIZE*2;
				for (i=0; i<3; row-=LINE_SIZE,i++)
				{	
					byte temp[LINE_SIZE * 2];
					int count = gdev_pcl_mode2compress((word *)row, (word *)(row + LINE_SIZE), temp);
					fprintf(fprn, "\033*b%d%c", count, "VVW"[i]);
					fwrite(temp, sizeof(byte), count, fprn); 
				}
			}
		}
	}
	/* end raster graphics */
	fputs("\033*rbC", fprn);	/*  was \033*rB  */
	fputs("\033*r1U", fprn);	/*  back to 1 plane  */

   	/* put printer in known state */
	fputs("\033E",fprn);
	
	/* eject page */
	fputs("\033&l0H", fprn);

	return 0;
}
