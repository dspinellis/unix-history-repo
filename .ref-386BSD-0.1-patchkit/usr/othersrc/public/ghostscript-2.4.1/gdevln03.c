/* Copyright (C) 1991 Free Software Foundation, Inc.  All rights reserved.

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

/*
gdevln03.c
Ghostscript driver for DEC LN03 printer

Ulrich Mueller, Div. PPE, CERN, CH-1211 Geneva 23 <ulm@vsnhd1.cern.ch>
This code is subject to the GNU General Public License

ulm 91-02-13 created as driver for gs 2.1.1
ulm 91-07-23 adapted to gs 2.2
ulm 91-08-21 changed memory allocation to gs_malloc,
	     ported to VMS (contributed by Martin Stiftinger, TU Vienna)
lpd 91-11-24 sped up by removing multiplies from inner loop
*/

#include "gdevprn.h"

/* The device descriptor */
private dev_proc_print_page(ln03_print_page);
gx_device_printer gs_ln03_device =
    prn_device(prn_std_procs, "ln03",
	       82,			/* width_10ths,  2460 pixel */
	       115,			/* height_10ths, 3450 pixel */
	       300, 300,		/* x_dpi, y_dpi */
	       0, 0.3, 0, 0,		/* margins */
	       1, ln03_print_page);

/* ------ Internal routines ------ */

/* Send the page to the printer. */
private int
ln03_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
    byte *in, *inp;
    int lnum, lcount, l, count, empty, mask, c, oldc, line_size, in_size;

    line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
    in_size = line_size * 6;
    in = (byte *)gs_malloc(in_size, 1, "ln03_print_page");

    /* Check allocation */
    if (!in) return(-1);

    /* switch to graphics mode, 300 dpi
       <ESC>[!p		DECSTR	soft terminal reset
       <ESC>[11h	PUM	select unit of measurement
       <ESC>[7 I	SSU	select pixel as size unit
       <ESC>[?52h	DECOPM	origin is upper-left corner
       <ESC>[3475t	DECSLPP	form length (3475 pixels)
       <ESC>[0;2460s	DECSLRM	left and right margins (0, 2460 pixels)
       <ESC>P0;0;1q		select sixel graphics mode
       "1;1		DECGRA	aspect ratio (1:1)
       *** Parameters are for A4 paper format.
       *** Could someone please check them for american paper format? */
    fputs(
      "\033[!p\033[11h\033[7 I\033[?52h\033[3475t\033[0;2460s\033P0;0;1q\"1;1",
	  prn_stream);

    /* Print lines of graphics */
    for (lnum = lcount = 0; lnum < pdev->height; lnum+=6, lcount++) {
	gdev_prn_copy_scan_lines(pdev, lnum, inp = in, line_size * 6);

	mask = 0200;
	oldc = 077;
	empty = 1;

	for (l = pdev->width, count = 0; --l >= 0; count++) {
	    /* transpose 6*8 rectangle */
	    register byte *iptr = inp;
	    c = 077;
	    if (*iptr & mask)
		c += 1;
	    if (*(iptr += line_size) & mask)
		c += 2;
	    if (*(iptr += line_size) & mask)
		c += 4;
	    if (*(iptr += line_size) & mask)
		c += 010;
	    if (*(iptr += line_size) & mask)
		c += 020;
	    if (*(iptr += line_size) & mask)
		c += 040;
	    if (!(mask >>= 1)) {
		mask = 0200;
		inp++;
	    }

	    if (c != oldc) {
		if (empty) {
		    while (--lcount >= 0) {
#ifdef VMS
			/* terminate record for VMS STREAM-LF file.
			   this LF is ignored by the LN03 */
			fputc('\n', prn_stream);
#endif
			/* terminate previous line */
			fputc('-', prn_stream);
		    }
		    empty = lcount = 0;
		}
		if (count > 3)
		    /* use run length encoding */
		    fprintf(prn_stream, "!%d%c", count, oldc);
		else
		    while (--count >= 0)
			fputc(oldc, prn_stream);
		oldc = c;
		count = 0;
	    }
	}
	if (c != 077) {
	    if (count > 3)
		/* use run length encoding */
		fprintf(prn_stream, "!%d%c", count, c);
	    else
		while (--count >= 0)
		    fputc(c, prn_stream);
	}
    }

    /* leave sixel graphics mode, eject page
       <ESC>\		ST	string terminator
       <FF>		FF	form feed */
    fputs("\033\\\f", prn_stream);
    fflush(prn_stream);

    gs_free((char *)in, in_size, 1, "ln03_print_page");

    return(0);
}
