/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevpcl.c */
/* Utilities for PCL printers */
#include "gdevprn.h"
#include "gdevpcl.h"

/* ------ Color mapping ------ */

/* The PaintJet and DeskJet 500C use additive colors in separate planes. */
/* We only keep one bit of color, with 1 = R, 2 = G, 4 = B. */
/* Because the buffering routines assume 0 = white, */
/* we complement all the color components. */
#define cv_shift (sizeof(gx_color_value) * 8 - 1)

/* Map an RGB color to a printer color. */
gx_color_index
gdev_pcl_3bit_map_rgb_color(gx_device *dev,
  gx_color_value r, gx_color_value g, gx_color_value b)
{	return (((b >> cv_shift) << 2) + ((g >> cv_shift) << 1) + (r >> cv_shift)) ^ 7;
}

/* Map the printer color back to RGB. */
int
gdev_pcl_3bit_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	ushort cc = (ushort)color ^ 7;
	prgb[0] = -(cc & 1);
	prgb[1] = -((cc >> 1) & 1);
	prgb[2] = -(cc >> 2);
	return 0;
}

/* ------ Compression ------ */

/*
 * Mode 2 Row compression routine for the HP DeskJet & LaserJet IIp.
 * Compresses data from row up to end_row, storing the result
 * starting at compressed.  Returns the number of bytes stored.
 * Runs of K<=127 literal bytes are encoded as K-1 followed by
 * the bytes; runs of 2<=K<=127 identical bytes are encoded as
 * 257-K followed by the byte.
 * In the worst case, the result is N+(N/127)+1 bytes long,
 * where N is the original byte count (end_row - row).
 * To speed up the search, we examine an entire word at a time.
 * We will miss a few blocks of identical bytes; tant pis.
 */
int
gdev_pcl_mode2compress(const word *row, const word *end_row, char *compressed)
{	register const word *exam = row; /* word being examined in the row to compress */
	register char *cptr = compressed; /* output pointer into compressed bytes */

	while ( exam < end_row )
	   {	/* Search ahead in the input looking for a run */
		/* of at least 4 identical bytes. */
		const char *compr = (const char *)exam;
		const char *end_dis;
		const word *next;
		register word test;
		while ( exam < end_row )
		  { test = *exam;
		    if ( ((test << 8) ^ test) <= 0xff )
		      break;
		    exam++;
		  }

		/* Find out how long the run is */
		end_dis = (const char *)exam;
		if ( exam == end_row )	/* no run */
		  { /* See if any of the last 3 "dissimilar" bytes are 0. */
		    if ( end_dis > compr && end_dis[-1] == 0 )
		      { if ( end_dis[-2] != 0 ) end_dis--;
			else if ( end_dis[-3] != 0 ) end_dis -= 2;
			else end_dis -= 3;
		      }
		    next = --end_row;
		  }
		else
		  { next = exam + 1;
		    while ( next < end_row && *next == test )
		      next++;
		    /* See if any of the last 3 "dissimilar" bytes */
		    /* are the same as the repeated byte. */
		    if ( end_dis > compr && end_dis[-1] == (byte)test )
		      { if ( end_dis[-2] != (byte)test ) end_dis--;
			else if ( end_dis[-3] != (byte)test ) end_dis -= 2;
			else end_dis -= 3;
		      }
		  }

		/* Now [compr..end_dis) should be encoded as dissimilar, */
		/* and [end_dis..next) should be encoded as similar. */
		/* Note that either of these ranges may be empty. */

		for ( ; ; )
		   {	/* Encode up to 127 dissimilar bytes */
			uint count = end_dis - compr; /* uint for faster switch */
			switch ( count )
			  { /* Use memcpy only if it's worthwhile. */
			  case 6: cptr[6] = compr[5];
			  case 5: cptr[5] = compr[4];
			  case 4: cptr[4] = compr[3];
			  case 3: cptr[3] = compr[2];
			  case 2: cptr[2] = compr[1];
			  case 1: cptr[1] = compr[0];
			    *cptr = count - 1;
			    cptr += count + 1;
			  case 0: /* all done */
			    break;
			  default:
			    if ( count > 127 ) count = 127;
			    *cptr++ = count - 1;
			    memcpy(cptr, compr, count);
			    cptr += count, compr += count;
			    continue;
			  }
			break;
		   }

		   {	/* Encode up to 127 similar bytes. */
			/* Note that count may be <0 at end of row. */
			int count = (const char *)next - end_dis;
			while ( count > 0 )
			  { int this = (count > 127 ? 127 : count);
			    *cptr++ = 257 - this;
			    *cptr++ = (byte)test;
			    count -= this;
			  }
			exam = next;
		   }
	   }
	return (cptr - compressed);
}

/*
 * Mode 3 compression routine for the HP LaserJet III family.
 * Compresses bytecount bytes starting at current, storing the result
 * in compressed, comparing against and updating previous.
 * Returns the number of bytes stored.  In the worst case,
 * the number of bytes is bytecount+(bytecount/8)+1.
 */
int
gdev_pcl_mode3compress(int bytecount, const char *current, char *previous, char *compressed)
{	register const char *cur = current;
	register char *prev = previous;
	register char *out = compressed;
	const char *end = current + bytecount;
	while ( cur < end )
	   {	/* Detect a maximum run of unchanged bytes. */
		const char *run = cur;
		register const char *diff;
		const char *stop;
		int offset, cbyte;
		while ( cur < end && *cur == *prev )
		   {	cur++, prev++;
		   }
		if ( cur == end ) break;	/* rest of row is unchanged */
		/* Detect a run of up to 8 changed bytes. */
		/* We know that *cur != *prev. */
		diff = cur;
		stop = (end - cur > 8 ? cur + 8 : end);
		do
		   {	*prev++ = *cur++;
		   }
		while ( cur < stop && *cur != *prev );
		/* Now [run..diff) are unchanged, and */
		/* [diff..cur) are changed. */
		/* Generate the command byte(s). */
		offset = diff - run;
		cbyte = (cur - diff - 1) << 5;
		if ( offset < 31 )
			*out++ = cbyte + offset;
		else
		   {	*out++ = cbyte + 31;
			offset -= 31;
			while ( offset >= 255 )
				*out++ = 255, offset -= 255;
			*out++ = offset;
		   }
		/* Copy the changed data. */
		while ( diff < cur )
			*out++ = *diff++;
	   }
	return out - compressed;
}
