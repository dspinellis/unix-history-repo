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

/* gdevlbp8.c */
/* Canon LBP-8II driver for Ghostscript */
#include "gdevprn.h"

#define X_DPI 300
#define Y_DPI 300
#define LINE_SIZE ((X_DPI * 85 / 10 + 7) / 8)	/* bytes per line */

/* The device descriptors */
private dev_proc_print_page(lbp8_print_page);

gx_device_printer gs_lbp8_device =
  prn_device(prn_std_procs, "lbp8",
	83,				/* width_10ths, 8.3" */
	117,				/* height_10ths, 11.7" */
	X_DPI, Y_DPI,
	0.16, 0.20, 0.32, 0.20,		/* margins */
	1, lbp8_print_page);

/* ------ Internal routines ------ */

#define ESC 0x1b
#define CSI 0233
static char can_inits[] = { ESC, ';', ESC, 'c', ESC, ';', /* reset, ISO */
                            CSI, '2', '&', 'z', /* fullpaint mode */
                            CSI, '1', '4', 'p', /* select page type (A4) */
                            CSI, '1', '1', 'h', /* set mode */
                            CSI, '7', ' ', 'I', /* select unit size (300dpi)*/
                          };


/* Send the page to the printer.  */
private int
lbp8_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	char data[LINE_SIZE*2];
	char *out_data;
	int out_count;

#define CSI_print(str) fprintf(prn_stream, str, CSI)
#define CSI_print_1(str,arg) fprintf(prn_stream, str, CSI, arg)

                                /* initialize */
        fwrite(can_inits, sizeof(can_inits), 1, prn_stream);

	/* Send each scan line in turn */
	   {	int lnum;
		int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
		byte rmask = (byte)(0xff << (-pdev->width & 7));

		for ( lnum = 0; lnum < pdev->height; lnum++ )
		   {	char *end_data = data + LINE_SIZE;
			gdev_prn_copy_scan_lines(pdev, lnum,
						 (byte *)data, line_size);
		   	/* Mask off 1-bits beyond the line width. */
			end_data[-1] &= rmask;
			/* Remove trailing 0s. */
			while ( end_data > data && end_data[-1] == 0 )
				end_data--;
			if ( end_data != data )
			   {	
                                int num_cols = 0;

			        out_data = data;
                                while(out_data < end_data && *out_data == 0)
                                  {
                                    num_cols += 8;
                                    out_data++;
                                  }
                                out_count = end_data - out_data;

                                /* move down */
                                CSI_print_1("%c%dd", lnum);
                                /* move across */
                                CSI_print_1("%c%d`", num_cols);

                                /* transfer raster graphics */
                                fprintf(prn_stream, "%c%d;%d;300;.r",
                                        CSI, out_count, out_count);

                                /* send the row */
                                fwrite(out_data, sizeof(char),
                                       out_count, prn_stream);
			   }
		   }
	}

	/* eject page */
        fprintf(prn_stream, "%c=", ESC);

	return 0;
}
