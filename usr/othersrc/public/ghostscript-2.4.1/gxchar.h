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

/* gxchar.h */
/* Internal character definition for Ghostscript library */
/* Requires gsmatrix.h, gxfixed.h, gzdevice.h */
#include "gschar.h"

/* An entry for a (font,matrix) pair in the character cache. */
/* Either font == 0 and UniqueID != -1, or font != 0 and UniqueID == -1. */
typedef struct cached_fm_pair_s cached_fm_pair;
struct cached_fm_pair_s {
	struct gs_font_s *font;		/* base font */
	long UniqueID;			/* font UniqueID */
	char * /* void * */ encoding;	/* encoding vector (opaque) */
	float mxx, mxy, myx, myy;	/* transformation */
	int num_chars;			/* # of cached chars with this */
					/* f/m pair */
};
/* If font == 0 and UniqueID == -1, this is a free entry. */
#define fm_pair_is_free(pair) ((pair)->font == 0 && (pair)->UniqueID == -1)
#define fm_pair_set_free(pair) ((pair)->font = 0, (pair)->UniqueID = -1)

/* The type of cached characters is opaque. */
typedef struct cached_char_s cached_char;
extern const uint cached_char_sizeof;

/* An enumeration object for string display. */
typedef enum {
	sws_none,
	sws_cache,			/* setcachedevice */
	sws_no_cache			/* setcharwidth */
} show_width_status;
struct gs_show_enum_s {
	/* Following are set at creation time */
	gs_state *pgs;
	int level;			/* save the level of pgs */
	byte *str;			/* char may be signed! */
	uint size;
	float wcx, wcy;			/* for widthshow */
	char_code wchr;			/* ditto */
	float ax, ay;			/* for ashow */
	int add;			/* true if a[width]show */
	int do_kern;			/* true if kshow */
	int slow_show;			/* [a][width]show or kshow */
	int charpath_flag;		/* 0 for show, 1 for false */
					/* charpath, 2 for true charpath */
	int stringwidth_flag;		/* 0 for show/charpath, */
					/* 1 for stringwidth */
	int can_cache;			/* true if can cache chars */
	int cxmin, cymin, cxmax, cymax;	/* int version of quick-check */
					/* clipping box */
	int is_composite;		/* true if composite font */
	int ftx, fty;			/* transformed font translation */
	/* Following are set at most once */
	gx_device_memory dev_cache_info;
	device dev_cache_dev;
	int dev_cache_set;
	/* Following are updated dynamically */
	uint index;			/* index within string */
	gs_fixed_point wxy;		/* for current char in device coords */
	cached_char *cc;		/* being accumulated */
	gs_point width;			/* total width of string, set at end */
	show_width_status width_status;
	gs_fixed_point metrics_sb, metrics_width;	/* width and side */
					/* bearing from metrics, in */
					/* *character* coordinates */
					/* (only used for Type 1 fonts) */
	byte sb_set, width_set;		/* true if metrics set */
					/* (only used for Type 1 fonts) */
	int color_loaded;		/* true if have done gx_color_render */
	int (*continue_proc)(P1(struct gs_show_enum_s *));	/* continuation procedure */
	/* Following are dynamic, for composite fonts only */
#define max_font_depth 5
	struct gs_font_s *fstack[max_font_depth];
	int fdepth;
	struct gs_font_s *pfont;
};
#define gs_show_enum_s_DEFINED

/* Cached character procedures (in gxfont.c) */
#ifndef gs_font_dir_DEFINED
#  define gs_font_dir_DEFINED	
typedef struct gs_font_dir_s gs_font_dir;
#endif
void	gx_char_cache_init(P1(gs_font_dir *));
cached_char *
	gx_alloc_char_bits(P4(gs_font_dir *, gx_device_memory *, ushort, ushort));
void	gx_free_cached_char(P2(gs_font_dir *, cached_char *));
cached_fm_pair *
	gx_lookup_fm_pair(P1(gs_state *));
void	gx_add_cached_char(P4(gs_font_dir *, gx_device_memory *, cached_char *, cached_fm_pair *));
cached_char *
	gx_lookup_cached_char(P3(gs_state *, cached_fm_pair *, char_code));
int	gx_image_cached_char(P2(gs_show_enum *, cached_char *));
