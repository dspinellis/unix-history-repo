/*
 * Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
 * Distributed by Free Software Foundation, Inc.
 * 
 * This file is part of Ghostscript.
 * 
 * Ghostscript is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY.  No author or distributor accepts responsibility to anyone
 * for the consequences of using it or for whether it serves any particular
 * purpose or works at all, unless he says so in writing.  Refer to the
 * Ghostscript General Public License for full details.
 * 
 * Everyone is granted permission to copy, modify and redistribute Ghostscript,
 * but only under the conditions described in the Ghostscript General Public
 * License.  A copy of this license is supposed to have been given to you
 * along with Ghostscript so you can know your rights and responsibilities.
 * It should be in a file named COPYING.  Among other things, the copyright
 * notice and this notice must be preserved on all copies.
 */

/* gdevcdj.c */
/* H-P DeskJet 500C driver (colour) for Ghostscript */
#include "gdevprn.h"
#include "gdevpcl.h"

/***
 *** Note: this driver was contributed by a user, George Cameron:
 ***       please contact g.cameron@aberdeen.ac.uk if you have questions.
 ***/

/*
 *   Definitions affecting print quality/speed
 */

/* #define A4               * .. if in Europe */
#ifndef SHINGLING        /* Interlaced, multi-pass printing */
#define SHINGLING 1      /* 0 = none, 1 = 50%, 2 = 25%, 2 is best & slowest */
#endif

#ifndef DEPLETION        /* 'Intelligent' dot-removal */
#define DEPLETION 1      /* 0 = none, 1 = 25%, 2 = 50%, 1 best for graphics? */
#endif                   /* Use 0 for transparencies */

/*
 * You may select a resolution of 75, 100, 150, or 300 DPI.
 * Normally you would do this in the makefile or on the gs command line,
 * not here.
 */

#define X_DPI_MAX 300
#define Y_DPI_MAX 300

#ifndef X_DPI
#  define X_DPI X_DPI_MAX
#endif
#ifndef Y_DPI
#  define Y_DPI Y_DPI_MAX
#endif

/* We round up LINE_SIZE to a multiple of 8 bytes */
/* because that's the unit of transposition from pixels to planes. */
#define LINE_SIZE ((X_DPI_MAX * 85 / 10 + 63) / 64 * 8)

/* For all DeskJet Printers:
 *
 *  Maximum printing width               = 2400 dots = 8"
 *  Maximum printing height (colour)     = 3100 dots = 10.3"
 *                          (monochrome) = 3150 dots = 10.5"
 *  All Deskjets have 1/2" unprintable bottom margin
 */
#ifndef A4
#define PAPER_SIZE 2       /* US Letter is default size */
#define WIDTH_10THS 85
#define HEIGHT_10THS 110
#define L_MARGIN 0.25
#define B_MARGIN 0.5
#define R_MARGIN 0.25
#define T_MARGIN 0.2
#else
#define PAPER_SIZE 26      /* A4 size normal in Europe */
#define WIDTH_10THS 83     /* 210mm */
#define HEIGHT_10THS 117   /* 297mm */
#define L_MARGIN 0.134
#define B_MARGIN 0.7
#define R_MARGIN 0.134
#define T_MARGIN 0.66
#endif

/* The device descriptor */
private         dev_proc_print_page(cdeskjet_print_page);
private gx_device_procs cdeskjet_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
		gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);
    gx_device_printer gs_cdeskjet_device =
    prn_device(cdeskjet_procs, "cdeskjet",
	       WIDTH_10THS, HEIGHT_10THS,
	       X_DPI, Y_DPI,
	       L_MARGIN, R_MARGIN, T_MARGIN, B_MARGIN,
	       3, cdeskjet_print_page);

/* Forward references */
private int gdev_pcl_mode9compress(P4(int, const byte *, byte *, byte *));

/* ------ Internal routines ------ */

/* Send the page to the printer.  Compress each scan line. */
private int
cdeskjet_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
#define DATA_SIZE (LINE_SIZE * 8)
    byte  data[DATA_SIZE];
    byte  plane_data[3][LINE_SIZE], prev_plane_data[3][LINE_SIZE];
    int   x_dpi = pdev->x_pixels_per_inch;

    /* Initialize printer. */
    fputs("\033E", prn_stream);		/* reset printer */

    /* ends raster graphics to set raster graphics resolution */
    fputs("\033*rbC", prn_stream);

    /* set raster graphics resolution -- 75, 100, 150 or300 dpi */
    fprintf(prn_stream, "\033*t%dR", x_dpi);

    /* set the line width */
    fprintf(prn_stream, "\033*r%dS", DATA_SIZE);

    /* paper size, orientation, perf skip, feed from tray*/ 
    fprintf(prn_stream, "\033&l%da0o0l1H", PAPER_SIZE);

    /* set the number of color planes */
    fprintf(prn_stream, "\033*r-%dU", 3);	/* always 3, -ve for CMY */

    /* set depletion level */
    fprintf(prn_stream, "\033*o%dD", DEPLETION);

    /* set shingling level */
    fprintf(prn_stream, "\033*o%dQ", SHINGLING);

    /* move to top left of page */
    fputs("\033&a0H\033&a0V", prn_stream);

    /* select data compression */
    fputs("\033*b9M", prn_stream);              /* mode 9 */

    /* start raster graphics */
    fputs("\033*r1A", prn_stream);

    bzero(&prev_plane_data[0][0], LINE_SIZE * 3);

    /* Send each scan line in turn */
    {
	int  lnum;
	int  line_size = gdev_prn_bytes_per_scan_line(pdev);
	int  line_count = line_size / pdev->color_info.depth;
	int  num_blank_lines = 0;

	for (lnum = 0; lnum < pdev->height; lnum++) {
	    byte _ss *end_data = data + line_size;
	    gdev_prn_copy_scan_lines(pdev, lnum,
				     (byte *) data, line_size);
	    /* Remove trailing 0s. */
	    while (end_data > data && end_data[-1] == 0)
		end_data--;
	    if (end_data == data) {	/* Blank line */
		num_blank_lines++;
	    } else {
		int             i;
		byte _ss       *odp;

		/* Pad with 0s to fill out the last */
		/* block of 8 bytes. */
		memset(end_data, 0, 7);

		/* Transpose the data to get pixel planes. */
		for (i = 0, odp = &plane_data[0][0]; i < DATA_SIZE;
		     i += 8, odp++
		    ) {		/* The following is for 16-bit machines */
#define spread3(c)\
 { 0, c, c*0x100, c*0x101, c*0x10000L, c*0x10001L, c*0x10100L, c*0x10101L }
		    static ulong    spr40[8] = spread3(0x40);
		    static ulong    spr8[8] = spread3(8);
		    static ulong    spr2[8] = spread3(2);
		    register byte _ss *dp = data + i;
		    register ulong  pword =
		    (spr40[dp[0]] << 1) +
		    (spr40[dp[1]]) +
		    (spr40[dp[2]] >> 1) +
		    (spr8[dp[3]] << 1) +
		    (spr8[dp[4]]) +
		    (spr8[dp[5]] >> 1) +
		    (spr2[dp[6]]) +
		    (spr2[dp[7]] >> 1);
		    odp[0] = (byte) (pword >> 16);
		    odp[LINE_SIZE] = (byte) (pword >> 8);
		    odp[LINE_SIZE * 2] = (byte) (pword);
		}
		/* Skip blank lines if any */
		if (num_blank_lines > 0) {	/* move down from current
						 * position */
		    fprintf(prn_stream, "\033&a+%dV",
			    num_blank_lines * (720 / Y_DPI));
		    num_blank_lines = 0;
		    bzero(&prev_plane_data[0][0], LINE_SIZE * 3);
		}
		/* Transfer raster graphics */
		/* in the order C, M, Y. */
		for (i = 0; i < 3; i++) {

		    byte temp[LINE_SIZE * 2];

		    int count = gdev_pcl_mode9compress(line_count,
						       plane_data[2 - i],
						       prev_plane_data[2 - i],
						       temp);

		    fprintf(prn_stream, "\033*b%d%c", count, "VVW"[i]);
		    fwrite(temp, sizeof(byte), count, prn_stream);
		}
	    }
	}
    }

    /* end raster graphics */
    fputs("\033*rbC", prn_stream);

    /* reset to monochrome */
    fputs("\033*r1U", prn_stream);

    /* eject page */
    fputs("\033&l0H", prn_stream);

    return 0;
}

private int
gdev_pcl_mode9compress(int bytecount, const byte *current, byte *previous, byte *compressed)
{
  register const byte *cur = current;
  register byte *prev = previous;
  register byte *out = compressed;
  const byte *end = current + bytecount;

  while ( cur < end )
    {	/* Detect a run of unchanged bytes. */
      const byte *run = cur;
      register const byte *diff;
      int offset;
      while ( cur < end && *cur == *prev )
	{	cur++, prev++;
	}
      if ( cur == end ) break;	/* rest of row is unchanged */
      /* Detect a run of changed bytes. */
      /* We know that *cur != *prev. */
      diff = cur;
      do
	{	*prev++ = *cur++;
	}
      while ( cur < end && *cur != *prev );
      /* Now [run..diff) are unchanged, and */
      /* [diff..cur) are changed. */
      offset = diff - run;
      {
	const byte  *stop_test = cur - 4;
	int   dissimilar, similar;
	
	while (diff < cur) {
	  const byte *compr = diff;
	  const byte *next;	/* end of run */
	  byte value;
	  while (diff <= stop_test &&
		 ((value = *diff) != diff[1] ||
		  value != diff[2] ||
		  value != diff[3]))
	    diff++;
	  
	  /* Find out how long the run is */
	  if (diff > stop_test)	/* no run */
	    next = diff = cur;
	  else {
	    next = diff + 4;
	    while (next < cur && *next == value)
	      next++;
	  }
	  
#define MAXOFFSETU 15
#define MAXCOUNTU 7
	  /* output 'dissimilar' bytes, uncompressed */
	  if ((dissimilar = diff - compr))
	    {
	      int temp, i;

	      if ((temp = --dissimilar) > MAXCOUNTU) temp = MAXCOUNTU;
	      if (offset < MAXOFFSETU)
		*out++ = (offset << 3) | (byte)temp;
	      else {
		*out++ = (MAXOFFSETU << 3) | (byte)temp;
		offset -= MAXOFFSETU;
		while (offset >= 255) {
		  *out++ = 255;
		  offset -= 255;
		}
		*out++ = offset;
	      }
	      if (temp == MAXCOUNTU) {
		temp = dissimilar - MAXCOUNTU;
		while (temp >= 255) {
		  *out++ = 255;
		  temp -= 255;
		}
		*out++ = (byte)temp;
	      }
	      for (i = 0; i <= dissimilar; i++) *out++ = *compr++;
	      offset = 0;
	    } /* end uncompressed */
	  
#define MAXOFFSETC 3
#define MAXCOUNTC 31
	  /* output 'similar' bytes, run-length endcoded */
	  if ((similar = next - diff))
	    {
	      int temp;

	      if ((temp = (similar -= 2)) > MAXCOUNTC) temp = MAXCOUNTC;
	      if (offset < MAXOFFSETC)
		*out++ = 0x80 | (offset << 5) | (byte)temp;
	      else {
		*out++ = 0x80 | (MAXOFFSETC << 5) | (byte)temp;
		offset -= MAXOFFSETC;
		while (offset >= 255) {
		  *out++ = 255;
		  offset -= 255;
		}
		*out++ = offset;
	      }
	      if (temp == MAXCOUNTC) {
		temp = similar - MAXCOUNTC;
		while (temp >= 255) {
		  *out++ = 255;
		  temp -= 255;
		}
		*out++ = (byte)temp;
	      }
	      *out++ = value;
	      offset = 0;
	    } /* end compressed */
	  
	  diff = next;
	}
      }
    }
  return out - compressed;
}
