/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gsim2out.c */
/* Image to outline conversion for GhostScript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gsstate.h"
#include "gscoord.h"
#include "gxfixed.h"
#include "gxtype1.h"

/*
 * Convert a bitmap image to an outline (path) representation.
 * The outline representation is in Adobe Type 1 CharString format.
 * See ghost.doc for more details.
 */

/* Define the state of the conversion process. */
typedef struct {
	/* The following are set at the beginning of the conversion. */
	gs_matrix ifm;			/* inverse of (CTM */
					/* scaled by width/height * 4). */
	byte *limit;			/* stop output here */
	int ox, oy;			/* X/Y pixel offset of char origin */
	/* The following are updated dynamically. */
	byte *next;			/* next byte goes here */
	int px, py;			/* X/Y position at start of run */
	int cpx, cpy;			/* px/py in character coordinates */
	int dx, dy;			/* X/Y increment of current run */
	int count;			/* # of steps in current run */
} status;

/* Define the scaling for the path tracer. */
#define outline_scale 4

/* Forward declarations */
private int round_coord(P1(floatp));
private int put_int(P2(status *, int));
private void fill_cells(P4(byte *, byte *, int, int));
private int trace_cells(P4(byte *, int, int, status *));

/*
 * gs_type1imagepath encodes an image into a byte string supplied
 * by the caller.  If the string is not big enough, the procedure
 * returns gs_error_limitcheck.  Otherwise, the procedure returns
 * the actual number of bytes of data stored.
 */
int
gs_type1imagepath(gs_state *pgs, byte *data, int width, int height,
  floatp wx, floatp wy, floatp origin_x, floatp origin_y,
  byte *str, uint maxlen)
{	uint csize;
	byte *cells;
	status stat;
	status *out = &stat;
	int lsbx;
	int iwx, iwy, ilsbx, ilsby;
	int code;
	/* Construct the coordinate transformation. */
	   {	float hsc = height * outline_scale;
		gs_matrix mat;
		gs_currentmatrix(pgs, &stat.ifm);
#ifdef DEBUG
if ( gs_debug['0'] )
		dprintf6("[0]ctm=[%g %g %g %g %g %g]\n",
			 stat.ifm.xx, stat.ifm.xy, stat.ifm.yx, stat.ifm.yy,
			 stat.ifm.tx, stat.ifm.ty);
#endif
		if (	(code = gs_make_scaling(hsc, hsc, &mat)) < 0 ||
			(code = gs_matrix_multiply(&mat, &stat.ifm, &stat.ifm)) < 0 ||
			(code = gs_matrix_invert(&stat.ifm, &stat.ifm)) < 0
		   )
			return code;
	   }
	/* Allocate and fill in the cell matrix. */
	csize = (width + 2) * (height + 2);
	cells = (byte *)gs_malloc(csize, 1, "gsim2out cells");
	if ( cells == 0 ) return_error(gs_error_VMerror);
	fill_cells(cells, data, width, height);
	/* Initialize the rest of the state. */
	stat.next = str;
	stat.limit = str + maxlen;
	/* Determine the left side bearing by looking for */
	/* the leftmost column with any 1-bits. */
	for ( lsbx = 0; lsbx < width; lsbx++ )
	   {	int y;
		for ( y = 1; y <= height; y++ )
		  if ( cells[y * (width + 2) + lsbx + 1] ) goto xit;
	   }
xit:	/* Encode the origin, width, and side bearing. */
	   {	gs_point opt, wpt, lsbpt;
		if (	(code = gs_distance_transform(origin_x * outline_scale,
						      origin_y * outline_scale,
						      &stat.ifm, &opt)) < 0 ||
			(code = gs_distance_transform(wx * outline_scale,
						      wy * outline_scale,
						      &stat.ifm, &wpt)) < 0 ||
			(code = gs_distance_transform((lsbx - origin_x) *
						       outline_scale, (floatp)0,
						      &stat.ifm, &lsbpt)) < 0
		   )
			return code;
		stat.ox = round_coord(opt.x);
		stat.oy = round_coord(opt.y);
		iwx = round_coord(wpt.x);
		iwy = round_coord(wpt.y);
		ilsbx = round_coord(lsbpt.x);
		ilsby = round_coord(lsbpt.y);
#ifdef DEBUG
if ( gs_debug['0'] )
	   {	int cy, cx;
		byte *cp = data;
		dprintf6("[0]w=%d h=%d oxy=(%g,%g) wxy=(%g,%g)\n",
			 width, height, origin_x, origin_y, wx, wy);
		dprintf6("   io=(%d,%d) iw=(%d,%d) ilsb=(%d,%d)\n",
			 stat.ox, stat.oy, iwx, iwy, ilsbx, ilsby);
		for ( cy = 0; cy < height; cy++ )
		   {	dprintf1("[0]%3d ", cy);
			for ( cx = 0; cx < width; cx += 8 )
				dprintf1("%02x", (int)*cp++);
			dputc('\n');
		   }
	   }
#endif
		if ( (code = put_int(out, ilsbx)) < 0 ) return code;
		if ( iwy != 0 || ilsby != 0 )
		   {	if (	(code = put_int(out, ilsby)) < 0 ||
				(code = put_int(out, iwx)) < 0 ||
				(code = put_int(out, iwy)) < 0
			   )
				return code;
			if ( stat.next + 2 > stat.limit )
				return_error(gs_error_limitcheck);
			*stat.next++ = (byte)c_escape;
			*stat.next++ = (byte)ce_sbw;
		   }
		else
		   {	if ( (code = put_int(out, iwx)) < 0 ) return code;
			if ( stat.next + 1 > stat.limit )
				return_error(gs_error_limitcheck);
			*stat.next++ = (byte)c_hsbw;
		   }
	   }
	/* Since all further movements are relative, we can account */
	/* for the origin by simply setting px/py to the lsb, */
	/* and cpx/cpy to the lsb plus the origin. */
	stat.px = (lsbx * outline_scale);
	stat.py = (int)(origin_y * outline_scale);
	stat.cpx = ilsbx + stat.ox;
	stat.cpy = ilsby + stat.oy;
	/* Trace the outline of the cells. */
	code = trace_cells(cells, width, height, out);
	gs_free((char *)cells, csize, 1, "gsim2out cells");
	if ( code < 0 ) return code;
	if ( stat.next >= stat.limit ) return_error(gs_error_limitcheck);
	*stat.next++ = (byte)c_endchar;
	return stat.next - str;
}

/* Fill the cell matrix with the image being traced. */
/* The cell matrix has a row and column of zero padding on each side, */
/* so we don't have to check for boundary conditions all the time. */
/* Note that the image data are in PostScript / Ghostscript standard */
/* order (left to right, top row first), but the cells are stored */
/* bottom row first. */
private void
fill_cells(byte *cells, byte *data, int width, int height)
{	int y;
	byte *dptr = data - 1;
	byte *cptr = cells + (width + 2) * height + 1;
	memset(cells, 0, (width + 2) * (height + 2));
	for ( y = 0; y < height; y++ )
	   {	register int mask = 0;
		register int b;
		register int x;
		for ( x = 0; x < width; x++, mask >>= 1, cptr++ )
		   {	if ( mask == 0 ) mask = 0x80, b = *++dptr;
			if ( b & mask ) *cptr = 1;
		   }
		cptr -= width * 2 + 2;	/* back up 1 row */
	   }
}

/* Trace the cells to form an outline.  The trace goes in clockwise */
/* order, always starting by going west along a bottom edge. */
/* All the subsidiary routines return 0 on success, */
/* -1 if the output buffer overflowed. */
private int trace_from(P3(status *, byte *, int));
private int add_dxdy(P4(status *, int, int, int));
#define add_deltas(s, dx, dy, n)\
  if ( (code = add_dxdy(s, dx, dy, n)) < 0 ) return code
private int put_dxdy(P4(status *, int, int, int));
#define put_deltas(s, dx, dy, moving)\
  if ( (code = put_dxdy(s, dx, dy, moving)) < 0 ) return code
private int
trace_cells(byte *cells, int width, int height, register status *out)
{	byte *cptr;
	int code;
	for ( cptr = cells + (width + 2) * (height + 1) - 2;
	      cptr >= cells;  cptr--
	    )
	   {	if ( *cptr == 1 && cptr[-(width+2)] == 0 )
		   {	/* Found a starting point */
			int x = (cptr-cells) % (width+2) - 1;
			int y = (cptr-cells) / (width+2) - 1;
			put_deltas(out,
				   x * outline_scale + 1 - out->px,
				   y * outline_scale - out->py,
				   1);
			out->count = 0;
			if ( (code = trace_from(out, cptr, width)) < 0 )
				return code;
			if ( out->next >= out->limit )
				return_error(gs_error_limitcheck);
			*out->next++ = (byte)c_closepath;
		   }
	   }
	return 0;
}

/* Trace a path */
private int
trace_from(register status *out, byte *cptr, int width)
{	typedef enum {			/* must be in this order */
		north = 0, east = 1, south = 2, west = 3
	} direction;
	direction dir;
	int w2 = width + 2;		/* actual width of cell rows */
	int part;			/* how far along edge we are */
	int code;
	/* Movement tables */
	typedef struct {
		short tx, ty;		/* relative index of first cell */
					/* to test (counter-clockwise move) */
		short dx, dy;		/* continue in same direction */
	   } dir_descr;
	static dir_descr nesw[4+1] =
	   {	/* Going north (along a western edge) */
		   { -1, 1,   0, 1 },
		/* Going east (along a northern edge) */
			{ 1, 1,   1, 0 },
		/* Going south (along an eastern edge) */
		   { 1, -1,   0, -1 },
		/* Going west (along a southern edge) */
		   { -1, -1,   -1, 0 },
		/* An extra copy of north */
		   { -1, 1,   0, 1 }
	   };
	for ( dir = west, part = 1; ; )
	   {	register dir_descr *pd = &nesw[(int)dir];
		int dx = pd->dx, dy = pd->dy;
		int delta;
		if ( dir == west )
		   {	/* This is the only case that has to check */
			/* for the end of a subpath. */
			if ( *cptr == 2 ) return 0;
			*cptr = 2;
		   }
		delta = pd->ty * w2 + pd->tx;
		if ( cptr[delta] )	/* go counter-clockwise */
		   {	cptr += delta;
			add_deltas(out, dx, dy, 1 - part);
			add_deltas(out, pd->tx, pd->ty, outline_scale - 1);
			dir = (direction)(((int)dir - 1) & 3);
			part = outline_scale - 1;
			continue;
		   }
		delta = dy * w2 + dx;
		if ( !cptr[delta] )	/* go clockwise */
		   {	add_deltas(out, dx, dy, outline_scale - 1 - part);
			add_deltas(out, dx + pd[1].dx, dy + pd[1].dy, 1);
			dir = (direction)(((int)dir + 1) & 3);
			part = 1;
			continue;
		   }
		cptr += delta;		/* go in same direction */
		add_deltas(out, dx, dy, outline_scale);
	   }
}

/* Add a (dx, dy) pair to the path being formed. */
/* Accumulate successive segments in the same direction. */
private int
add_dxdy(register status *out, int dx, int dy, int count)
{	int code;
	if ( count != 0 )
	   {	if ( dx == out->dx && dy == out->dy )
			out->count += count;
		else
		   {	if ( out->count != 0 )
				put_deltas(out, out->dx * out->count,
					   out->dy * out->count, 0);
			out->dx = dx, out->dy = dy;
			out->count = count;
		   }
	   }
	return 0;
}

/* Encode a (dx, dy) pair onto the path. */
/* If there isn't enough space, return -1. */
private int
put_dxdy(register status *out, int dx, int dy, int moving)
{	int code;
	/* We do the arithmetic in the 1/4-pixel coordinate system, */
	/* and then transform the result, to avoid accumulating */
	/* rounding errors. */
	int npx = out->px + dx, npy = out->py + dy;
	gs_point npt;
	int ncpx, ncpy;
	int cdx, cdy;
	gs_distance_transform((floatp)npx, (floatp)npy, &out->ifm, &npt);
	ncpx = round_coord(npt.x);
	ncpy = round_coord(npt.y);
	cdx = ncpx - out->cpx;
	cdy = ncpy - out->cpy;
#ifdef DEBUG
if ( gs_debug['0'] )
	dprintf8("[0]  pxy=(%d,%d)+(%d,%d)  cpxy=(%d,%d)+(%d,%d)\n",
		 out->px, out->py, dx, dy, out->cpx, out->cpy, cdx, cdy);
#endif
	if ( cdx != 0 || cdy == 0 )	/* encode dx if needed */
	  if ( (code = put_int(out, cdx)) < 0 ) return code;
	if ( cdy != 0 )			/* encode dy if needed */
	  if ( (code = put_int(out, cdy)) < 0 ) return code;
	if ( out->next == out->limit ) return_error(gs_error_limitcheck);
	*out->next++ = (byte)
		(cdy == 0 ?		/* use hmove/lineto */
			(moving ? c_hmoveto : c_hlineto) :
		cdx == 0 ?		/* use vmove/lineto */
			(moving ? c_vmoveto : c_vlineto) :
		(moving ? c_rmoveto : c_rlineto));
	out->px = npx, out->py = npy;
	out->cpx = ncpx, out->cpy = ncpy;
	return 0;
}

/* Round a floating point coordinate.  If it is out of range, */
/* return a limiting value. */
private int
round_coord(floatp v)
{	long c = (long)(v + 0.5);
	return( c > 0x7fff ? 0x7fff :
		c < -0x7fff ? -0x7fff :
		(int)c );
}
/* Encode a single number in Type 1 representation. */
private int
put_int(status *out, register int v)
{
#define min_enc_num1 ((c_num1 - c_num2 + 1) / 2)
#define max_enc_num1 ((c_num2 - c_num1 - 1) / 2)
#define min_enc_num2 (max_enc_num1 + 1)
#define max_enc_num2 (min_enc_num2 + (c_num3 - c_num2) * 256 - 1)
#define min_enc_num3 (-max_enc_num2)
#define max_enc_num3 (-min_enc_num2)
	register byte *ptr = out->next;
	if ( ptr + 5 > out->limit )	/* conservative test is faster */
		return_error(gs_error_limitcheck);
	if ( v >= min_enc_num1 && v <= max_enc_num1 )
		*ptr++ = v - min_enc_num1 + c_num1;
	else if ( v >= min_enc_num2 && v <= max_enc_num2 )
	   {	v -= min_enc_num2;
		*ptr++ = (v >> 8) + c_num2;
		*ptr++ = v & 0xff;
	   }
	else if ( v >= min_enc_num3 && v <= max_enc_num3 )
	   {	v = -(v - max_enc_num3);
		*ptr++ = (v >> 8) + c_num3;
		*ptr++ = v & 0xff;
	   }
	else
	   {	*ptr++ = c_num4;
		*ptr++ = ((long)v >> 24) & 0xff;
		*ptr++ = ((long)v >> 16) & 0xff;
		*ptr++ = (v >> 8) & 0xff;
		*ptr++ = v & 0xff;
	   }
	out->next = ptr;
	return 0;
}
