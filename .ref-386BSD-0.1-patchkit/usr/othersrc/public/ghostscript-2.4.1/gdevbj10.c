/* Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevbj10.c */
/* Canon Bubble Jet BJ-10e printer driver for Ghostscript */
#include "gdevprn.h"

/*
 * The only available resolutions are (180,360)x(180,360).
 */

/* The device descriptor */
private dev_proc_print_page(bj10e_print_page);
gx_device_printer gs_bj10e_device =
  prn_device(prn_std_procs, "bj10e",
	80,				/* width_10ths, 8" */
	105,				/* height_10ths, 10.5" */
	360,				/* x_dpi */
	360,				/* y_dpi */
	0,0,0,0,			/* margins */
	1, bj10e_print_page);

/* ------ internal routines ------ */

/* Send the page to the printer. */
private int
bj10e_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	int line_size = gx_device_bytes_per_scan_line((gx_device *)pdev, 1);
	int xres = pdev->x_pixels_per_inch;
	int yres = pdev->y_pixels_per_inch;
	int mode = (yres == 180 ?
			(xres == 180 ? 11 : 12) :
			(xres == 180 ? 14 : 16));
	int bits_per_column = 24 * (yres / 180);
	int bytes_per_column = bits_per_column / 8;
	int skip_unit = 9 * (xres / 180);
	byte *in = (byte *)gs_malloc(8, line_size, "bj10e_print_page(in)");
	byte *out = (byte *)gs_malloc(bits_per_column, line_size, "bj10e_print_page(out)");
	static char cmp[18] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	int lnum = 0;
	int skip = 0;
	int code = 0;

	if ( in == 0 || out == 0 )
		return -1;

	/* Initialize the printer. */
	fwrite("\033[K\004\000\000\044\000\000", 1, 9, prn_stream);

	/* Set vertical spacing. */
	fwrite("\033[\\\004\000\000\000", 1, 7, prn_stream);
	fputc(yres & 0xff, prn_stream);
	fputc(yres >> 8, prn_stream);

	/* Transfer pixels to printer */
	while ( lnum < pdev->height )
	   {	byte *in_end = in + line_size;
		byte *out_beg = out;
		byte *out_end = out + bytes_per_column * pdev->width;
		byte *outl = out;
		int count, bnum;

		/* Copy 1 scan line and test for all zero. */
		code = gdev_prn_get_bits(pdev, lnum, in, line_size, -1);
		if ( code < 0 ) goto xit;
		/* The mem... or str... functions should be faster than */
		/* the following code, but all systems seem to implement */
		/* them so badly that this code is faster. */
		   {	register long *zip = (long *)in;
			register int zcnt = line_size;
			static long zeroes[4] = { 0, 0, 0, 0 };
			for ( ; zcnt >= 4 * sizeof(long); zip += 4, zcnt -= 4 * sizeof(long) )
			   {	if ( zip[0] | zip[1] | zip[2] | zip[3] )
					goto notz;
			   }
			if ( !memcmp(in, (char *)zeroes, zcnt) )
			   {	/* Line is all zero, skip */
				lnum++;
				skip++;
				continue;
			   }
notz:			;
		   }

		/* Vertical tab to the appropriate position. */
		while ( skip > 255 )
		   {	fputs("\033J\377", prn_stream);
			skip -= 255;
		   }
		if ( skip )
			fprintf(prn_stream, "\033J%c", skip);

		/* Transpose in blocks of 8 scan lines. */
		for ( bnum = 0; bnum < bits_per_column; bnum += 8, lnum += 8 )
		   {	int lcnt = gdev_prn_copy_scan_lines(pdev,
				lnum, in, 8 * line_size);
			byte *inp = in;
			byte *outp = outl;
			if ( lcnt < 0 )
			   {	code = lcnt;
				goto xit;
			   }
			if ( lcnt < 8 )
				memset(in + lcnt * line_size, 0,
				       (8 - lcnt) * line_size);
			for ( ; inp < in_end; inp++, outp += bits_per_column )
			   {	gdev_prn_transpose_8x8(inp, line_size,
					outp, bytes_per_column);
			   }
			outl++;
		   }

		/* Remove trailing 0s. */
		while ( out_end - 6 >= out )
		   {	if ( out_end[-1] | out_end[-2] | out_end[-3] |
			     out_end[-4] | out_end[-5] | out_end[-6]
			   )
				break;
			out_end -= 6;
		   }

		/* Remove leading 0s. */
		while ( out_beg + skip_unit <= out_end )
		   {	if( memcmp(cmp, (char *)out_beg, skip_unit) != 0 )
				break;
			out_beg += skip_unit;
		   }

		/* Transfer the bits */
		count = out_end - out_beg + 1;
		if ( out_beg > out && count > 1 )
		   {	int skip = (out_beg - out) / skip_unit;
			if ( xres == 180 ) skip <<= 1;
			fprintf(prn_stream, "\033d%c%c",
				skip & 0xff, skip >> 8);
		   }
		fprintf(prn_stream, "\033[g%c%c%c",
			count & 0xff, count >> 8, mode);
		fwrite(out_beg, 1, count - 1, prn_stream);
		fputc('\r', prn_stream);
		skip = bits_per_column;
	   }

	/* Eject the page */
xit:	fputc(014, prn_stream);	/* form feed */
	fflush(prn_stream);
	gs_free((char *)out, bits_per_column, line_size, "bj10e_print_page(out)");
	gs_free((char *)in, 8, line_size, "bj10e_print_page(in)");
	return code;
}
