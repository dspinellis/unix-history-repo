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

/* gdevbit.c */
/* Fake bitmapped device to estimate rendering time. */
#include "gdevprn.h"

/* Define the device parameters. */
#define X_DPI 400
#define Y_DPI 400
#define WIDTH_10THS 80			/* 440 */
#define HEIGHT_10THS 80			/* 1080 */

/* The device descriptor */
private dev_proc_print_page(bit_print_page);
gx_device_printer gs_bit_device =
  prn_device(prn_std_procs, "bit",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	1, bit_print_page);

/* Send the page to the printer. */
private int
bit_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	/* Just dump the bits on the file. */
	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	int lnum;
	byte *in = (byte *)gs_malloc(line_size, 1, "bit_print_page(in)");
	if ( in == 0 ) return -1;
	for ( lnum = 0; lnum < pdev->height; lnum++ )
	   {	gdev_prn_copy_scan_lines(pdev, lnum, in, line_size);
/******		fwrite(in, 1, line_size, prn_stream);
 ******/
	   }
	gs_free(in, line_size, 1, "bit_print_page(in)");
	return 0;
}
