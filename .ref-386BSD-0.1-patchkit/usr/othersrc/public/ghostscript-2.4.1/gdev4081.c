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

/* gdev4081.c */
/* Ricoh 4081 laser printer driver for Ghostscript */
#include "gdevprn.h"

#define X_DPI 300			/* pixels per inch */
#define Y_DPI 300			/* pixels per inch */

/* The device descriptor */
private dev_proc_print_page(r4081_print_page);
gx_device_printer gs_r4081_device =
  prn_device(prn_std_procs, "r4081",
	85,				/* width_10ths, 8.5" */
	110,				/* height_10ths, 11" */
	X_DPI, Y_DPI,
	0.25, 0.16, 0.25, 0.16,		/* margins */
	1, r4081_print_page);

/* ------ Internal routines ------ */


/* Send the page to the printer. */
private int
r4081_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	
	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	int out_size = ((pdev->width + 7) & -8) ;
	byte *out = (byte *)gs_malloc(out_size, 1, "r4081_print_page(out)");
	int lnum = 0;
	int last = pdev->height;

	/* Check allocations */
	if ( out == 0 )
	   {	if ( out ) gs_free(out, out_size, 1, "r4081_print_page(out)");
		return -1;
	   }

	/* find the first line which has something to print */
	while ( lnum < last )
	{
		gdev_prn_copy_scan_lines(pdev, lnum, (byte *)out, line_size);
		if ( out[0] != 0 ||
		     memcmp((char *)out, (char *)out+1, line_size-1)
		   )
			break;
		lnum ++;
	}

	/* find the last line which has something to print */
	while (last > lnum) {
		gdev_prn_copy_scan_lines(pdev, last-1, (byte *)out, line_size);
		if ( out[0] != 0 ||
		     memcmp((char *)out, (char *)out+1, line_size-1)
		   )
			break;
		last --;
	}

	/* Initialize the printer and set the starting position. */
	fprintf(prn_stream,"\033\rP\033\022YB2 \033\022G3,%d,%d,1,1,1,%d@",
			out_size, last-lnum, (lnum+1)*720/Y_DPI);

	/* Print lines of graphics */
	while ( lnum < last )
	   {
		gdev_prn_copy_scan_lines(pdev, lnum, (byte *)out, line_size);
		fwrite(out, sizeof(char), line_size, prn_stream);
		lnum ++;
	   }

	/* Eject the page and reinitialize the printer */
	fputs("\f\033\rP", prn_stream);

	gs_free(out, out_size, 1, "r4081_print_page(out)");
	return 0;
}
