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

/* gdevepsn.c */
/* Epson dot-matrix printer driver for Ghostscript */
#include "gdevprn.h"
/************************************************
 * For 9-pin printers, you may select
 *   X_DPI = 60, 120, or 240
 *   Y_DPI = 60 or 72
 * For 24-pin printers, you may select
 *   X_DPI = 60, 120, 180, 240, or 360
 *   Y_DPI = 60, 72, 180, or 216
 * Note that a given printer implements *either* Y_DPI = 60 | 180 *or*
 * Y_DPI = 72 | 216; no attempt is made to check this here.
 * Note that X_DPI = 180 or 360 requires Y_DPI > 100;
 * this isn't checked either.  Finally, note that X_DPI=240 and
 * X_DPI=360 are double-density modes requiring two passes to print.
 *
 * The values of X_DPI and Y_DPI may be set at compile time:
 * see gdevs.mak.
 * 
 * At some time in the future, we could simulate 24-bit output on
 * 9-pin printers by using fractional vertical positioning;
 * we could even implement an X_DPI=360 mode by using the
 * ESC++ command that spaces vertically in units of 1/360"
 * (not supported on many printers.)
 ************************************************/

#ifndef X_DPI
#  define X_DPI 240			/* pixels per inch */
#endif
#ifndef Y_DPI
#  define Y_DPI 60			/* pixels per inch */
#endif

/* The device descriptors */
private dev_proc_print_page(eps_print_page);
gx_device_printer gs_epson_device =
  prn_device(prn_std_procs, "epson",
	85,				/* width_10ths, 8.5" */
	110,				/* height_10ths, 11" */
	X_DPI, Y_DPI,
	0, 0, 0.5, 0,		/* margins */
	1, eps_print_page);

/* ------ Internal routines ------ */

/* Forward references */
private void eps_output_run(P6(byte *, int, int, char, FILE *, int));

/* Send the page to the printer. */
#define DD 0x80				/* double density flag */
private int
eps_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	static char graphics_modes_9[5] =
	   {	-1, 0 /*60*/, 1 /*120*/, -1, DD+3 /*240*/
	   };
	static char graphics_modes_24[7] =
	   {	-1, 32 /*60*/, 33 /*120*/, 39 /*180*/,
		-1, -1, DD+40 /*360*/
	   };
	int y_24pin = pdev->y_pixels_per_inch > 72;
	int y_mult = (y_24pin ? 3 : 1);
	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	int in_size = line_size * (8 * y_mult);
	byte *in = (byte *)gs_malloc(in_size, 1, "eps_print_page(in)");
	int out_size = ((pdev->width + 7) & -8) * y_mult;
	byte *out = (byte *)gs_malloc(out_size, 1, "eps_print_page(out)");
	int x_dpi = pdev->x_pixels_per_inch;
	char start_graphics =
		(y_24pin ? graphics_modes_24 : graphics_modes_9)[x_dpi / 60];
	int first_pass = (start_graphics & DD ? 1 : 0);
	int last_pass = first_pass * 2;
	int dots_per_space = x_dpi / 10;	/* pica space = 1/10" */
	int bytes_per_space = dots_per_space * y_mult;
	int skip = 0, lnum = 0, pass;

	/* Check allocations */
	if ( in == 0 || out == 0 )
	   {	if ( in ) gs_free((char *)in, in_size, 1, "eps_print_page(in)");
		if ( out ) gs_free((char *)out, out_size, 1, "eps_print_page(out)");
		return -1;
	   }

	/* Initialize the printer and reset the margins. */
	fwrite("\033@\033P\033l\000\r\033Q", 1, 10, prn_stream);
	fputc((int)(pdev->width / pdev->x_pixels_per_inch * 10) + 2,
	      prn_stream);

	/* Print lines of graphics */
	while ( lnum < pdev->height )
	   {	byte *inp = in;
		byte *in_end = in + line_size;
		byte *out_end = out;
		byte *out_blk;
		register byte *outp;
		int lcnt;

		/* Copy 1 scan line and test for all zero. */
		gdev_prn_copy_scan_lines(pdev, lnum, in, line_size);
		if ( in[0] == 0 &&
		     !memcmp((char *)in, (char *)in + 1, line_size - 1)
		   )
		   {	lnum++;
			skip += 3 / y_mult;
			continue;
		   }

		/* Vertical tab to the appropriate position. */
		while ( skip > 255 )
		   {	fputs("\033J\377", prn_stream);
			skip -= 255;
		   }
		if ( skip )
			fprintf(prn_stream, "\033J%c", skip);

		/* Copy the rest of the scan lines. */
		lcnt = 1 + gdev_prn_copy_scan_lines(pdev, lnum + 1, in + line_size, in_size - line_size);
		if ( lcnt < 8 * y_mult )
		  memset(in + lcnt * line_size, 0,
			 in_size - lcnt * line_size);

		/* We have to 'transpose' blocks of 8 pixels x 8 lines, */
		/* because that's how the printer wants the data. */
		/* If we are in a 24-pin mode, we have to transpose */
		/* groups of 3 lines at a time. */

		if ( y_24pin )
		 { for ( ; inp < in_end; inp++, out_end += 24 )
		    { gdev_prn_transpose_8x8(inp, line_size, out_end, 3);
		      gdev_prn_transpose_8x8(inp + line_size * 8, line_size, out_end + 1, 3);
		      gdev_prn_transpose_8x8(inp + line_size * 16, line_size, out_end + 2, 3);
		    }
		   /* Remove trailing 0s. */
		   while ( out_end > out && out_end[-1] == 0 &&
			   out_end[-2] == 0 && out_end[-3] == 0
			 )
		     out_end -= 3;
		 }
		else
		 { for ( ; inp < in_end; inp++, out_end += 8 )
		    { gdev_prn_transpose_8x8(inp, line_size, out_end, 1);
		    }
		   /* Remove trailing 0s. */
		   while ( out_end > out && out_end[-1] == 0 )
		     out_end--;
		 }

		for ( pass = first_pass; pass <= last_pass; pass++ )
		   {
		for ( out_blk = outp = out; outp < out_end; )
		 { /* Skip a run of leading 0s. */
		   /* At least 10 are needed to make tabbing worth it. */
		   /* We do everything by 3's to avoid having to make */
		   /* different cases for 9- and 24-pin. */

		   if ( *outp == 0 && outp + 12 <= out_end &&
			outp[1] == 0 && outp[2] == 0 &&
			(outp[3] | outp[4] | outp[5]) == 0 &&
			(outp[6] | outp[7] | outp[8]) == 0 &&
			(outp[9] | outp[10] | outp[11]) == 0
		      )
		    {	byte *zp = outp;
			int tpos;
			byte *newp;
			outp += 12;
			while ( outp + 3 <= out_end && *outp == 0 &&
				outp[1] == 0 && outp[2] == 0
			      )
				outp += 3;
			tpos = (outp - out) / bytes_per_space;
			newp = out + tpos * bytes_per_space;
			if ( newp > zp + 10 )
			 { /* Output preceding bit data. */
			   if ( zp > out_blk )	/* only false at */
						/* beginning of line */
			     eps_output_run(out_blk, (int)(zp - out_blk),
					    y_mult, start_graphics,
					    prn_stream, pass);
			   /* Tab over to the appropriate position. */
			   fprintf(prn_stream, "\033D%c%c\t", tpos, 0);
			   out_blk = outp = newp;
			 }
		   }
		  else
			outp += y_mult;
		 }
		if ( outp > out_blk )
			eps_output_run(out_blk, (int)(outp - out_blk),
				       y_mult, start_graphics,
				       prn_stream, pass);

		fputc('\r', prn_stream);
		   }
		skip = 24;
		lnum += 8 * y_mult;
	   }

	/* Eject the page and reinitialize the printer */
	fputs("\f\033@", prn_stream);
	fflush(prn_stream);

	gs_free((char *)out, out_size, 1, "eps_print_page(out)");
	gs_free((char *)in, in_size, 1, "eps_print_page(in)");
	return 0;
}

/* Output a single graphics command. */
/* pass=0 for all columns, 1 for even columns, 2 for odd columns. */
private void
eps_output_run(byte *data, int count, int y_mult,
  char start_graphics, FILE *prn_stream, int pass)
{	int xcount = count / y_mult;
	fputc(033, prn_stream);
	if ( !(start_graphics & ~3) )
	   {	fputc("KLYZ"[start_graphics], prn_stream);
	   }
	else
	   {	fputc('*', prn_stream);
		fputc(start_graphics & ~DD, prn_stream);
	   }
	fputc(xcount & 0xff, prn_stream);
	fputc(xcount >> 8, prn_stream);
	if ( !pass )
		fwrite(data, 1, count, prn_stream);
	else
	   {	/* Only write every other column of y_mult bytes. */
		int which = pass;
		byte *dp = data;
		register int i, j;
		for ( i = 0; i < xcount; i++, which++ )
		  for ( j = 0; j < y_mult; j++, dp++ )
		   {	putc(((which & 1) ? *dp : 0), prn_stream);
		   }
	   }
}
