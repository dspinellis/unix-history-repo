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

/* gxcache.c */
/* Character cache routines for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"
#include "gzpath.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxcache.h"
#include "gxfont.h"
#include "gxfdir.h"

extern ulong gs_next_ids(P1(uint));

/* Define the size of the cache structures. */
/* We round the size of a cached_char so that */
/* an immediately following bitmap will be properly aligned. */
const uint cached_char_sizeof =
  sizeof(cached_char) + (-sizeof(cached_char) & 3);
#define cc_bits(cc) ((byte *)(cc) + cached_char_sizeof)
const uint cached_fm_pair_sizeof = sizeof(cached_fm_pair);

/* Define the hash chain for a (code, fm_pair) key. */
#define chars_head(dir, code, pair)\
  &(dir)->chars[((uint)(code) + ((uint)(pair) << 4)) & (dir)->chars_mask]

/* Forward references */
private void shorten_cached_char(P3(gs_font_dir *, cached_char *, uint));
private void purge_fm_pair(P2(gs_font_dir *, cached_fm_pair *));

/* Initialize the character cache. */
void
gx_char_cache_init(register gs_font_dir *dir)
{	cached_char_head *cdata = (cached_char_head *)dir->cdata;
	int i;
	cached_fm_pair *pair;
	dir->bsize = 0;
	dir->msize = 0;
	dir->csize = 0;
	dir->mnext = 0;
	dir->cnext = 0;
	cdata->pair = 0;
	cdata->size = dir->cdata_size;
	memset((char *)dir->chars, 0,
	       (dir->chars_mask + 1) * sizeof(cached_char *));
	for ( i = dir->mmax, pair = dir->mdata; --i >= 0; pair++ )
	  fm_pair_set_free(pair);
}

/* Allocate storage for caching a rendered character, */
/* and set up the memory device. */
/* Return the cached_char if OK, 0 if too big. */
cached_char *
gx_alloc_char_bits(gs_font_dir *dir, gx_device_memory *dev,
  ushort iwidth, ushort iheight)
{	ulong isize, cdsize;
	cached_char_head *cch;
#define hcc ((cached_char *)cch)
	cached_char *cc;
	uint fsize = 0;
	byte *bits;
	dev->width = iwidth;
	dev->height = iheight;
	isize = gdev_mem_bitmap_size(dev);	/* sets raster */
	if ( dev->raster != 0 && iheight > dir->upper / dev->raster )
		return 0;		/* too big */
	cdsize = isize + cached_char_sizeof;
	if ( cdsize >= dir->cmax )
		return 0;		/* too big */
	/* Look for and/or free enough space. */
	cch = (cached_char_head *)(dir->cdata + dir->cnext);
	cc = hcc;
	for ( ; ; )
	  { if ( (byte *)cc + fsize == dir->cdata + dir->cdata_size )
	      cch = (cached_char_head *)dir->cdata, cc = hcc, fsize = 0;
	    if ( !cc_head_is_free(cch) )
	      { /* Free the character */
		cached_char **pcc = chars_head(dir, hcc->code, cch->pair);
		while ( *pcc != hcc )
		  pcc = &(*pcc)->next;
		*pcc = hcc->next; /* remove from chain */
		gx_free_cached_char(dir, hcc);
	      }
	    fsize += cch->size;
	    if_debug2('K', "[K]merging free 0x%lx(%u)\n",
		      (ulong)cch, cch->size);
	    cc->head.size = fsize;
	    if ( fsize == cdsize ||
		 fsize >= cdsize + sizeof(cached_char_head)
	       )
	      break;		/* enough room here */
	    cch = (cached_char_head *)((byte *)cc + fsize);
	  }
#undef hcc
	if ( fsize > cdsize )
	  { shorten_cached_char(dir, cc, fsize - cdsize);
	    if_debug2('K', "[K]shortening 0x%lx by %u (initial)\n",
		      (ulong)cc, (uint)(fsize - cdsize));
	  }
	if_debug4('k', "[k]adding 0x%lx:%u(%u,%u)\n",
		  (ulong)cc, (uint)cdsize, iwidth, iheight);
	bits = cc_bits(cc);
	memset((char *)bits, 0, (uint)isize);
	cc->width = iwidth;
	cc->height = iheight;
	cc->raster = dev->raster;
	cc->head.pair = 0;	/* not linked in yet */
	dev->base = bits;
	(*dev->procs->open_device)((gx_device *)dev);	/* initialize */
	dir->csize++;
	dir->bsize += cdsize;
	dir->cnext = (byte *)cc + cdsize - dir->cdata;
	return cc;
}

/* Remove a character from the cache. */
void
gx_free_cached_char(gs_font_dir *dir, cached_char *cc)
{	dir->cnext = (byte *)cc - dir->cdata;
	dir->csize--;
	dir->bsize -= cc->head.size;
	if ( cc->head.pair != 0 )
	   {	/* might be allocated but not added to table yet */
		cc->head.pair->num_chars--;
	   }
	if_debug2('k', "[k]freeing 0x%lx, pair=0x%lx\n",
		  (ulong)cc, (ulong)cc->head.pair);
	cc_set_free(cc);
}

/* Look up, and if necessary add, a font/matrix pair in the cache */
cached_fm_pair *
gx_lookup_fm_pair(register gs_state *pgs)
{	float	mxx = pgs->char_tm.xx, mxy = pgs->char_tm.xy,
		myx = pgs->char_tm.yx, myy = pgs->char_tm.yy;
	gs_font *font = pgs->font;
	register gs_font_dir *dir = font->dir;
	register cached_fm_pair *pair = dir->mdata + dir->mnext;
	int count = dir->mmax;
	long uid = -1;
	cached_fm_pair *mend;
	if ( font->FontType != ft_composite )
	   {	uid = font->data.base.UniqueID;
		if ( uid != -1 ) font = 0;
	   }
	while ( count-- )
	   {	if ( pair == dir->mdata ) pair += dir->mmax;
		pair--;
		if (	pair->font == font && pair->UniqueID == uid &&
			pair->mxx == mxx && pair->mxy == mxy &&
			pair->myx == myx && pair->myy == myy
		   )
		  return pair;
	   }
	/* Add the pair to the cache */
	mend = dir->mdata + dir->mmax;
	if ( dir->msize == dir->mmax ) /* cache is full */
	  { /* Prefer an entry with num_chars == 0, if any. */
	    for ( count = dir->mmax; --count >= 0 && pair->num_chars != 0; )
	      if ( ++pair == mend ) pair = dir->mdata;
	    purge_fm_pair(dir, pair);
	  }
	else
	  { /* Look for an empty entry.  (We know there is one.) */
	    while ( !fm_pair_is_free(pair) )
	      if ( ++pair == mend ) pair = dir->mdata;
	  }
	dir->msize++;
	dir->mnext = pair + 1 - dir->mdata;
	if ( dir->mnext == dir->mmax ) dir->mnext = 0;
	pair->font = font;
	pair->UniqueID = uid;
	pair->mxx = mxx, pair->mxy = mxy;
	pair->myx = myx, pair->myy = myy;
	pair->num_chars = 0;
	return pair;
}

/* Add a character to the cache */
void
gx_add_cached_char(gs_font_dir *dir, gx_device_memory *dev,
  cached_char *cc, cached_fm_pair *pair)
{	cc->id = gs_next_ids(1);
	/* Make sure the bits are in the right order */
	/* to use as a source. */
	gdev_mem_ensure_byte_order(dev);
	/* Add the new character at the tail of its chain. */
	   {	register cached_char **head =
		  chars_head(dir, cc->code, pair);
		while ( *head != 0 ) head = &(*head)->next;
		*head = cc;
		cc->next = 0;
		cc->head.pair = pair;
		pair->num_chars++;
	   }
	/* Discard the memory device overhead that follows the bits. */
	 { uint diff = gdev_mem_bitmap_size(dev) - cc->raster * cc->height;
	   if ( diff >= sizeof(cached_char_head) )
	     { shorten_cached_char(dir, cc, diff);
	       dir->bsize -= diff;
	       if_debug2('K', "[K]shortening 0x%lx by %u (mdev overhead)\n",
		         (ulong)cc, diff);
	     }
	 }
}

/* Look up a character in the cache. */
/* Return the cached_char or 0. */
cached_char *
gx_lookup_cached_char(gs_state *pgs, cached_fm_pair *pair, char_code ccode)
{	gs_font_dir *dir = pgs->font->dir;
	register cached_char *cc = *chars_head(dir, ccode, pair);
	while ( cc != 0 )
	  { if ( cc->code == ccode && cc->head.pair == pair )
	      return cc;
	    cc = cc->next;
	  }
	return 0;
}

/* Copy a cached character to the screen. */
/* Assume the caller has already done gx_color_load, */
/* and the color is not a halftone. */
/* Return 0 if OK, 1 if we couldn't do the operation but no error */
/* occurred, or a negative error code. */
int
gx_image_cached_char(register gs_show_enum *penum, register cached_char *cc)
{	register gs_state *pgs = penum->pgs;
	int x, y, w, h;
	int code;
	gs_fixed_point pt;
	gx_device *dev = pgs->device->info;
	gx_device_clip cdev;
	code = gx_path_current_point_inline(pgs->path, &pt);
	if ( code < 0 ) return code;
	/* Abort if the device color isn't pure. */
	if ( !penum->color_loaded )
	   {	if ( !color_is_pure(pgs->dev_color) )
			return 1;	/* can't use cache */
		penum->color_loaded = 1;
	   }
	/* If the character doesn't lie entirely within the */
	/* quick-check clipping rectangle, we have to */
	/* set up an intermediate clipping device. */
	pt.x -= cc->offset.x;
	x = fixed2int_var_rounded(pt.x) + penum->ftx;
	pt.y -= cc->offset.y;
	y = fixed2int_var_rounded(pt.y) + penum->fty;
	w = cc->width;
	h = cc->height;
#ifdef DEBUG
if ( gs_debug['K'] )
	dprintf3("[K]copying 0x%lx, offset=(%g,%g)\n", (ulong)cc,
		 fixed2float(-cc->offset.x), fixed2float(-cc->offset.y)),
	dprintf6("   at (%g,%g)+(%d,%d)->(%d,%d)\n", fixed2float(pt.x),
		 fixed2float(pt.y), penum->ftx, penum->fty, x, y);
#endif
	if (	x < penum->cxmin || x + w > penum->cxmax ||
		y < penum->cymin || y + h > penum->cymax
	   )
	   {	cdev = gs_clip_device;
		cdev.target = dev;
		cdev.list = pgs->clip_path->list;
		dev = (gx_device *)&cdev;
		(*dev->procs->open_device)(dev);
		if_debug0('K', "[K](clipping)\n");
	   }
	/* Copy the bits. */
	code = (*dev->procs->copy_mono)
		(dev, cc_bits(cc), 0, cc->raster, cc->id,
		 x, y, w, h,
		 gx_no_color_index, pgs->dev_color->color1);
	return ( code < 0 ? code : 0 );
}

/* Purge from the caches all references to a given font. */
void
gs_purge_font_from_char_caches(gs_font_dir *dir, gs_font *font)
{	cached_fm_pair *pair = dir->mdata;
	int count = dir->mmax;
	if_debug1('k', "[k]purging font 0x%lx\n",
		  (ulong)font);
	while ( count-- )
	  { if ( pair->font == font ) purge_fm_pair(dir, pair);
	    pair++;
	  }
}

/* ------ Internal routines ------ */

/* Shorten a cached character. */
/* diff >= sizeof(cached_char_head). */
private void
shorten_cached_char(gs_font_dir *dir, cached_char *cc, uint diff)
{	cached_char_head *next;
	if ( (byte *)cc + cc->head.size == dir->cdata + dir->cnext )
	  dir->cnext -= diff;
	cc->head.size -= diff;
	next = (cached_char_head *)((byte *)cc + cc->head.size);
	if_debug2('K', "[K]shortening creates free block 0x%lx(%u)\n",
		  (ulong)next, diff);
	cc_head_set_free(next);
	next->size = diff;
}

/* Purge from the caches all references to a given font/matrix pair. */
private void
purge_fm_pair(gs_font_dir *dir, cached_fm_pair *pair)
{	int chi;
	if_debug1('k', "[k]purging pair 0x%lx\n",
		  (ulong)pair);
	for ( chi = dir->chars_mask; pair->num_chars != 0; )
	  { cached_char **pcc = dir->chars + chi--;
	    while ( *pcc != 0 )
	      { cached_char *cc = *pcc;
		if ( cc->head.pair == pair )
		  { gx_free_cached_char(dir, cc);
		    *pcc = cc->next;
		  }
		else
		  pcc = &cc->next;
	      }
	  }
	fm_pair_set_free(pair);
	dir->msize--;
}
