/* Copyright (C) 1989, 1991 Aladdin Enterprises.  All rights reserved.
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

/* gsfont.h */
/* Font operations for Ghostscript library */
/* Requires gsmatrix.h */

/* A 'font directory' object (to avoid making fonts global). */
/* 'directory' is something of a misnomer: this structure */
/* actually just keeps track of the scaled font and */
/* rendered character caches. */
#ifndef gs_font_dir_DEFINED
#  define gs_font_dir_DEFINED	
typedef struct gs_font_dir_s gs_font_dir;
#endif

/* Font objects */
typedef struct gs_font_s gs_font;

/* Initialization */
/* These procedures return 0 if they fail. */
gs_font_dir	*gs_font_dir_alloc(P2(proc_alloc_t, proc_free_t));
gs_font_dir	*gs_font_dir_alloc_limits(P7(proc_alloc_t, proc_free_t,
			uint /*smax*/, uint /*bmax*/, uint /*mmax*/,
			uint /*cmax*/, uint /*upper*/));

/* Font manipulations */
/* gs_scalefont and gs_makefont return 0 if the scaled font */
/* was already in the cache, 1 if a new font was created. */
/* The second gs_font ** argument returns a font discarded */
/* from the cache (or 0) in the latter case.  This is for */
/* the benefit of reference-counted clients. */
int	gs_scalefont(P5(gs_font_dir *, gs_font *, floatp, gs_font **, gs_font **));
int	gs_makefont(P5(gs_font_dir *, gs_font *, gs_matrix *, gs_font **, gs_font **));
int	gs_setfont(P2(gs_state *, gs_font *));
gs_font *	gs_currentfont(P1(gs_state *));
void	gs_purge_font_from_caches(P2(gs_font_dir *, gs_font *));

/* Font cache parameter operations */
void	gs_cachestatus(P2(gs_font_dir *, uint [7]));
int	gs_setcachelimit(P2(gs_font_dir *, uint));
uint	gs_currentcachelower(P1(gs_font_dir *));
int	gs_setcachelower(P2(gs_font_dir *, uint));
uint	gs_currentcacheupper(P1(gs_font_dir *));
int	gs_setcacheupper(P2(gs_font_dir *, uint));
