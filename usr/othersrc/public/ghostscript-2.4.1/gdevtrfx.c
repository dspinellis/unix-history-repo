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

/* gdevtrfx.c */
/* TruFax driver for Ghostscript. */
/******
 ****** Note: this file requires encode_l.o as supplied by TruFax.
 ******/
#include "gdevprn.h"

/* The device descriptor */
#define X_DPI 204
#define Y_DPI 196
#define LINE_SIZE ((X_DPI * 85 / 10 + 7) / 8)	/* bytes per line */
private dev_proc_print_page(trufax_print_page);
gx_device_printer gs_trufax_device =
  prn_device(prn_std_procs, "TruFax",
	85,				/* width_10ths, 8.5" */
	110,				/* height_10ths, 11" */
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	1, trufax_print_page);

/* ------ Internal routines ------ */

private int
trufax_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	char data[LINE_SIZE + 4];
	int lnum;
	int line_size;
	char out_data[5 * 1734];	/* Sized according to TruFax */
	int out_count;
 
	/* write TruFax header */
	strcpy(out_data, "COSIf2");
	out_data[6]=1;
	out_data[7]=0;
	out_data[8]=1;
	out_data[9]=0;
	fwrite(out_data, sizeof(char), 10, prn_stream);

	line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	for ( lnum = 0; lnum < pdev->height; lnum++ )
	   {
		gdev_prn_copy_scan_lines(pdev, lnum, (byte *)data, line_size);
/* 1728 = Sized according to TruFax */
		out_count = encode_line(1728, data, out_data);
		putc(out_count % 256, prn_stream);
		putc(out_count / 256, prn_stream);
		/* send the row */
		fwrite(out_data, sizeof(char), out_count, prn_stream);
	   }
	return 0;
}
