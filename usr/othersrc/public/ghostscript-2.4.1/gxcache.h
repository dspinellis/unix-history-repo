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

/* gxcache.h */
/* Definitions for character cache */
/* Requires gxchar.h */

/* The character cache contains both used and free blocks. */
/* All blocks have a common header. */
typedef struct cached_char_head_s {
	uint size;		/* total block size in bytes */
	cached_fm_pair *pair;		/* font/matrix pair, 0 if free */
} cached_char_head;
#define cc_head_is_free(cch) ((cch)->pair == 0)
#define cc_head_set_free(cch) ((cch)->pair = 0)
/* A cached bitmap for an individual character. */
/* The bits immediately follow the structure. */
struct cached_char_s {
	/* The code and font/matrix pair are the 'key' in the cache. */
	cached_char_head head;		/* (must be first) */
				/* references font/matrix pair */
	char_code code;			/* character code */
	cached_char *next;		/* next in replacement ring */
	/* The rest of the structure is the 'value'. */
	ushort raster, height;		/* dimensions of bitmap */
	ushort width;
	gx_bitmap_id id;
	gs_fixed_point wxy;		/* width in device coords */
	gs_fixed_point offset;		/* (-llx, -lly) in device coords */
};
#define cc_is_free(cc) cc_head_is_free(&(cc)->head)
#define cc_set_free(cc) cc_head_set_free(&(cc)->head)
