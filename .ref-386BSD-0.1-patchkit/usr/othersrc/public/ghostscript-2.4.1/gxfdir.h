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

/* gxfdir.h */
/* Font directory (font/character cache manager) definitions */
/* Requires gsfont.h. */

/* A font directory (font/character cache manager). */
struct gs_font_dir_s {
	proc_alloc_t alloc;
	proc_free_t free;
	/* Font directory */
	long next_id;			/* next unique ID */
	/* Scaled font cache */
	gs_font *scaled_fonts;		/* list of recently scaled fonts */
	uint ssize, smax;
	/* Character cache parameters and statistics */
	uint bsize, bmax;		/* # of bytes for cached chars */
	uint msize, mmax;		/* # of cached font/matrix pairs */
	uint csize, cmax;		/* # of cached chars */
	uint lower;			/* min size at which cached chars */
					/* should be stored compressed */
	uint upper;			/* max size of a single cached char */
	/* Character cache */
	struct cached_char_s **chars;	/* chain heads */
	uint chars_mask;		/* (a power of 2 -1) */
	struct cached_fm_pair_s *mdata;
	uint mnext;			/* rover for allocating font/matrix pairs */
	byte *cdata;		/* struct cached_char_head_s * */
	uint cdata_size;
	uint cnext;		/* rover for allocating cached characters */
};
