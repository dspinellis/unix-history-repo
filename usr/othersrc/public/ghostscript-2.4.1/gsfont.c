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

/* gsfont.c */
/* Font operators for GhostScript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"			/* must precede gxdevice */
#include "gxdevice.h"			/* must precede gxfont */
#include "gschar.h"
#include "gxfont.h"
#include "gxfdir.h"

/* Imported procedures */
void	gs_purge_font_from_char_caches(P2(gs_font_dir *, gs_font *));

/* Size of cache structures */
extern const uint cached_char_sizeof;
extern const uint cached_fm_pair_sizeof;

/* Define the sizes of the various aspects of the font/character cache. */
/*** Big memory machines ***/
#define smax_LARGE 50		/* smax - # of scaled fonts */
#define bmax_LARGE 500000	/* bmax - space for cached chars */
#define mmax_LARGE 200		/* mmax - # of cached font/matrix pairs */
#define cmax_LARGE 5000		/* cmax - # of cached chars */
#define blimit_LARGE 1000	/* blimit/upper - max size of a single cached char */
/*** Small memory machines ***/
#define smax_SMALL 20		/* smax - # of scaled fonts */
#define bmax_SMALL 25000	/* bmax - space for cached chars */
#define mmax_SMALL 40		/* mmax - # of cached font/matrix pairs */
#define cmax_SMALL 500		/* cmax - # of cached chars */
#define blimit_SMALL 100	/* blimit/upper - max size of a single cached char */

/* Allocate a font directory */
gs_font_dir *
gs_font_dir_alloc(proc_alloc_t palloc, proc_free_t pfree)
{	/* Try allocating a very large cache. */
	/* If this fails, allocate a small one. */
	gs_font_dir *pdir;
#if !arch_ints_are_short
	pdir = gs_font_dir_alloc_limits(palloc, pfree,
					smax_LARGE, bmax_LARGE, mmax_LARGE,
					cmax_LARGE, blimit_LARGE);
	if ( pdir != 0 ) return pdir;
#endif
	return gs_font_dir_alloc_limits(palloc, pfree,
					smax_SMALL, bmax_SMALL, mmax_SMALL,
					cmax_SMALL, blimit_SMALL);
}
gs_font_dir *
gs_font_dir_alloc_limits(proc_alloc_t palloc, proc_free_t pfree,
  uint smax, uint bmax, uint mmax, uint cmax, uint upper)
{	register gs_font_dir *pdir = (gs_font_dir *)(*palloc)(1, sizeof(gs_font_dir), "font_dir_alloc(dir)");
	uint cdcount = bmax / cached_char_sizeof + 1;
	uint cdsize = cdcount * cached_char_sizeof;
	uint chsize = (cmax / 5) | 31;		/* a guess */
	struct cached_fm_pair_s *mdata;
	byte *cdata;
	struct cached_char_s **chars;
	if ( pdir == 0 ) return 0;
	/* Round up chsize to a power of 2. */
	while ( chsize & (chsize + 1) ) chsize |= chsize >> 1;
	chsize++;
	mdata = (struct cached_fm_pair_s *)(*palloc)(mmax, cached_fm_pair_sizeof, "font_dir_alloc(mdata)");
	cdata = (byte *)(*palloc)(cdcount, cached_char_sizeof, "font_dir_alloc(cdata)");
	chars = (struct cached_char_s **)(*palloc)(chsize, sizeof(struct cached_char_s *), "font_dir_alloc(chars)");
	if ( mdata == 0 || cdata == 0 || chars == 0 )
	   {	if ( chars != 0 ) (*pfree)((char *)chars, chsize, sizeof(struct cached_char_s *), "font_dir_alloc(chars)");
		if ( cdata != 0 ) (*pfree)((char *)cdata, cdcount, cached_char_sizeof, "font_dir_alloc(cdata)");
		if ( mdata != 0 ) (*pfree)((char *)mdata, mmax, cached_fm_pair_sizeof, "font_dir_alloc(mdata)");
		(*pfree)((char *)pdir, 1, sizeof(gs_font_dir), "font_dir_alloc(dir)");
		return 0;
	   }
	memset((char *)pdir, 0, sizeof(gs_font_dir));	/* easiest to clear everything first */
	pdir->alloc = palloc;
	pdir->free = pfree;
	pdir->smax = smax;
	pdir->bmax = bmax;
	pdir->mmax = mmax;
	pdir->cmax = cmax;
	pdir->lower = upper / 10;
	pdir->upper = upper;
	pdir->mdata = mdata;
	pdir->cdata = cdata;
	pdir->cdata_size = cdsize;
	pdir->chars = chars;
	pdir->chars_mask = chsize - 1;
	gx_char_cache_init(pdir);
	return pdir;
}

/* Macro for linking an element at the head of a chain */
#define link_first(first, elt)\
  if ( (elt->next = first) != NULL ) first->prev = elt;\
  elt->prev = 0;\
  first = elt

/* scalefont */
int
gs_scalefont(gs_font_dir *pdir, gs_font *pfont, floatp scale,
  gs_font **ppfont, gs_font **pdfont)
{	gs_matrix mat;
	gs_make_scaling(scale, scale, &mat);
	return gs_makefont(pdir, pfont, &mat, ppfont, pdfont);
}

/* makefont */
int
gs_makefont(gs_font_dir *pdir, gs_font *pfont, gs_matrix *pmat,
  gs_font **ppfont, gs_font **pdfont)
{	int code;
	gs_font *prev = 0, *pf_out = pdir->scaled_fonts;
	gs_matrix newmat;
	*pdfont = 0;
	gs_make_identity(&newmat);	/* fill in tags */
	if ( (code = gs_matrix_multiply(&pfont->FontMatrix, pmat, &newmat)) < 0 )
	  return code;
	/* Check for the font already being in the scaled font cache. */
	/* Only attempt to share fonts if the current font has */
	/* a real UniqueID (i.e., not -1). */
#ifdef DEBUG
if ( gs_debug['m'] )
   {	dprintf2("[m]UniqueID=%ld, FontType=%d,\n",
	  pfont->data.base.UniqueID, pfont->FontType);
	dprintf6("[m]  new FontMatrix=[%g %g %g %g %g %g]\n",
	  pmat->xx, pmat->xy, pmat->yx, pmat->yy,
	  pmat->tx, pmat->ty);
   }
#endif
	if ( pfont->data.base.UniqueID != -1 )
	  for ( ; pf_out != 0; prev = pf_out, pf_out = pf_out->next )
		if (	pf_out->data.base.UniqueID == pfont->data.base.UniqueID &&
			pf_out->FontType == pfont->FontType &&
			pf_out->FontMatrix.xx == newmat.xx &&
			pf_out->FontMatrix.xy == newmat.xy &&
			pf_out->FontMatrix.yx == newmat.yx &&
			pf_out->FontMatrix.yy == newmat.yy &&
			pf_out->FontMatrix.tx == newmat.tx &&
			pf_out->FontMatrix.ty == newmat.ty
		   )
		   {	*ppfont = pf_out;
#ifdef DEBUG
if ( gs_debug['m'] )
			dprintf1("[m]found font=%lx\n", (ulong)pf_out);
#endif
			return 0;
		   }
	pf_out = (gs_font *)(*pdir->alloc)(1, sizeof(gs_font), "gs_makefont");
	if ( !pf_out ) return_error(gs_error_VMerror);
	*pf_out = *pfont;
	pf_out->FontMatrix = newmat;
	if ( pdir->ssize == pdir->smax )
	  { /* Must discard a cached scaled font. */
	    /* Scan for the oldest font if we didn't already. */
	    if ( !prev )
	      for ( prev = pdir->scaled_fonts;
		    prev->next != 0;
		    prev = prev->next
		  ) ;
#ifdef DEBUG
if ( gs_debug['m'] )
		dprintf1("[m]discarding font %lx\n", (ulong)prev);
#endif
	    *pdfont = prev;
	    prev->prev->next = 0;
	  }
	else
	  pdir->ssize++;
	link_first(pdir->scaled_fonts, pf_out);
	pf_out->base = pfont->base;
	pf_out->dir = pdir;
	*ppfont = pf_out;
#ifdef DEBUG
if ( gs_debug['m'] )
	dprintf1("[m]new font=%lx\n", (ulong)pf_out);
#endif
	return 1;
}

/* setfont */
int
gs_setfont(gs_state *pgs, gs_font *pfont)
{	pgs->font = pfont;
	pgs->char_tm_valid = 0;
	return 0;
}

/* currentfont */
gs_font *
gs_currentfont(gs_state *pgs)
{	return pgs->font;
}

/* cachestatus */
void
gs_cachestatus(register gs_font_dir *pdir, register uint pstat[7])
{	pstat[0] = pdir->bsize;
	pstat[1] = pdir->bmax;
	pstat[2] = pdir->msize;
	pstat[3] = pdir->mmax;
	pstat[4] = pdir->csize;
	pstat[5] = pdir->cmax;
	pstat[6] = pdir->upper;
}

/* setcachelimit */
int
gs_setcachelimit(gs_font_dir *pdir, uint size)
{	pdir->upper = size;
	return 0;
}

/* setcacheparams */
int
gs_setcachelower(gs_font_dir *pdir, uint size)
{	pdir->lower = size;
	return 0;
}
int
gs_setcacheupper(gs_font_dir *pdir, uint size)
{	pdir->upper = size;
	return 0;
}

/* currentcacheparams */
uint
gs_currentcachelower(gs_font_dir *pdir)
{	return pdir->lower;
}
uint
gs_currentcacheupper(gs_font_dir *pdir)
{	return pdir->upper;
}

/* Dummy (ineffective) BuildChar procedure */
int
gs_no_build_char_proc(struct gs_show_enum_s *penum, gs_state *pgs,
  gs_font *pfont, char_code chr, char *data)
{	return 1;			/* failure, but not error */
}

/* Purge a font from the font- and character-related caches. */
/* This is only used by restore (and, someday, the GC). */
void
gs_purge_font_from_caches(gs_font_dir *pdir, gs_font *pfont)
{
	/* Purge the font from the scaled font cache. */
	gs_font *pf = pdir->scaled_fonts;
	while ( pf != 0 )
	  { if ( pf == pfont )
	      { pf = pf->next;
		if ( pfont->prev ) pfont->prev->next = pf;
		else pdir->scaled_fonts = pf;
		if ( pf ) pf->prev = pfont->prev;
		pdir->ssize--;
	      }
	    else if ( pf->base == pfont )
	      { gs_purge_font_from_caches(pdir, pf->base);
		pf = pdir->scaled_fonts; /* start over */
	      }
	    else
	      pf = pf->next;
	   }

	/* Purge the font from the font/matrix pair cache, */
	/* including all cached characters rendered with that font. */
	gs_purge_font_from_char_caches(pdir, pfont);

}
