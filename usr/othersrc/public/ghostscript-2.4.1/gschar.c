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

/* gschar.c */
/* Character writing operators for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "string_.h"
#include "gserrors.h"
#include "gxfixed.h"			/* ditto */
#include "gxarith.h"
#include "gxmatrix.h"
#include "gzstate.h"			/* must precede gzdevice */
#include "gzdevice.h"			/* must precede gxchar */
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxcache.h"
#include "gxfont.h"
#include "gspath.h"
#include "gzpath.h"
#include "gzcolor.h"

/* Exported size of enumerator */
const uint gs_show_enum_sizeof = sizeof(gs_show_enum);

/* Imported procedures */
extern void gx_set_black(P1(gs_state *));

/* Forward declarations */
private int continue_show(P1(gs_show_enum *));
private int continue_show_update(P1(gs_show_enum *));
private int show_setup(P3(gs_show_enum *, gs_state *, char *));
private int stringwidth_setup(P3(gs_show_enum *, gs_state *, char *));

/* Print the ctm if debugging */
#define print_ctm(s,pgs)\
  dprintf7("[p]%sctm=[%g %g %g %g %g %g]\n", s,\
	   pgs->ctm.xx, pgs->ctm.xy, pgs->ctm.yx, pgs->ctm.yy,\
	   pgs->ctm.tx, pgs->ctm.ty)

/* ------ String writing operators ------ */

/* Setup macros for show operators */
#define setup_show()\
  penum->size = strlen(str)
#define setup_show_n()\
  penum->size = size
#define setup_a()\
  penum->add = 1, penum->ax = ax, penum->ay = ay,\
  penum->slow_show = 1
#define setup_width()\
  penum->wchr = chr, penum->wcx = cx, penum->wcy = cy,\
  penum->slow_show = 1
#define no_chr ~(char_code)0

/* show[_n] */
int
gs_show_init(register gs_show_enum *penum,
  gs_state *pgs, char *str)
{	setup_show();
	penum->slow_show = 0;
	return show_setup(penum, pgs, str);
}
int
gs_show_n_init(register gs_show_enum *penum,
  gs_state *pgs, char *str, uint size)
{	setup_show_n();
	penum->slow_show = 0;
	return show_setup(penum, pgs, str);
}

/* ashow[_n] */
int
gs_ashow_init(register gs_show_enum *penum,
  gs_state *pgs, floatp ax, floatp ay, char *str)
{	int code;
	setup_show();
	code = show_setup(penum, pgs, str);
	setup_a();
	return code;
}
int
gs_ashow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp ax, floatp ay, char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_a();
	return code;
}

/* widthshow[_n] */
int
gs_widthshow_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, char chr, char *str)
{	int code;
	setup_show();
	code = show_setup(penum, pgs, str);
	setup_width();
	return code;
}
int
gs_widthshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, char chr, char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_width();
	return code;
}

/* awidthshow[_n] */
int
gs_awidthshow_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, char chr, floatp ax, floatp ay,
  char *str)
{	int code;
	setup_show();
	code = show_setup(penum, pgs, str);
	setup_a();
	setup_width();
	return code;
}
int
gs_awidthshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, char chr, floatp ax, floatp ay,
  char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_a();
	setup_width();
	return code;
}

/* kshow[_n] */
int
gs_kshow_init(register gs_show_enum *penum,
  gs_state *pgs, char *str)
{	int code;
	if ( pgs->font->FontType == ft_composite)
		return_error(gs_error_invalidfont);
	setup_show();
	code = show_setup(penum, pgs, str);
	penum->do_kern = penum->slow_show = 1;
	return code;
}
int
gs_kshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, char *str, uint size)
{	int code;
	if ( pgs->font->FontType == ft_composite)
		return_error(gs_error_invalidfont);
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->do_kern = penum->slow_show = 1;
	return code;
}

/* ------ Related operators ------ */

/* stringwidth[_n] */
int
gs_stringwidth_init(gs_show_enum *penum, gs_state *pgs, char *str)
{	setup_show();
	return stringwidth_setup(penum, pgs, str);
}
int
gs_stringwidth_n_init(gs_show_enum *penum, gs_state *pgs, char *str, uint size)
{	setup_show_n();
	return stringwidth_setup(penum, pgs, str);
}

/* Common code for stringwidth[_n] */
private int
stringwidth_setup(gs_show_enum *penum, gs_state *pgs, char *str)
{	int code = show_setup(penum, pgs, str);
	if ( code < 0 ) return_error(code);
	penum->stringwidth_flag = 1;
	/* Do an extra gsave and suppress output */
	if ( (code = gs_gsave(pgs)) < 0 ) return code;
	penum->level = pgs->level;	/* for level check in show_update */
	gx_device_no_output(pgs);
	/* Establish an arbitrary current point. */
	return gx_path_add_point(pgs->path, pgs->ctm.tx_fixed, pgs->ctm.ty_fixed);
}

/* charpath[_n] */
int
gs_charpath_init(gs_show_enum *penum, gs_state *pgs,
  char *str, int bool)
{	int code;
	setup_show();
	code = show_setup(penum, pgs, str);
	penum->charpath_flag = (bool ? 2 : 1);
	penum->can_cache = 0;
	return code;
}
int
gs_charpath_n_init(gs_show_enum *penum, gs_state *pgs,
  char *str, uint size, int bool)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->charpath_flag = (bool ? 2 : 1);
	penum->can_cache = 0;
	return code;
}

/* ------ Width/cache operators ------ */

/* setcachedevice */
int
gs_setcachedevice(register gs_show_enum *penum, gs_state *pgs,
  floatp wx, floatp wy, floatp llx, floatp lly, floatp urx, floatp ury)
{	int code = gs_setcharwidth(penum, pgs, wx, wy);	/* default is don't cache */
	if ( code < 0 ) return code;
	/* See if we want to cache this character. */
	if ( pgs->in_cachedevice )		/* no recursion! */
		return 0;
	pgs->in_cachedevice = 1;	/* disable color/gray/image operators */
	/* We can only use the cache if ctm is unchanged */
	/* (aside from a possible translation), */
	/* and if the extent of the box is non-negative. */
	if ( !penum->can_cache || !pgs->char_tm_valid ||
	     llx > urx || lly > ury
	   )
		return 0;
	   {	gs_font_dir *dir = pgs->font->dir;
		gs_fixed_point cbox_ll, cbox_ur, cdim;
		long iwidth, iheight;
		cached_char *cc;
		gs_fixed_rect clip_box;
		gs_distance_transform2fixed(&pgs->ctm, llx, lly, &cbox_ll);
		gs_distance_transform2fixed(&pgs->ctm, urx, ury, &cbox_ur);
		cdim.x = cbox_ur.x - cbox_ll.x;
		cdim.y = cbox_ur.y - cbox_ll.y;
		if ( cdim.x < 0 ) cdim.x = -cdim.x;
		if ( cdim.y < 0 ) cdim.y = -cdim.y;
#ifdef DEBUG
if ( gs_debug['k'] )
   {	dprintf4("[k]cbox=[%g %g %g %g]\n",
		 fixed2float(cbox_ll.x), fixed2float(cbox_ll.y),
		 fixed2float(cbox_ur.x), fixed2float(cbox_ur.y));
	print_ctm("  ", pgs);
   }
#endif
		iwidth = fixed2long(cdim.x) + 2;
		iheight = fixed2long(cdim.y) + 2;
		if (	iwidth != (ushort)iwidth ||
			iheight != (ushort)iheight
		   )
		  return 0;		/* much too big */
		if ( !penum->dev_cache_set )
		   {	/* Set up the memory device for the character cache */
			device *dev = &penum->dev_cache_dev;
			penum->dev_cache_info = mem_mono_device;
			dev->info = (gx_device *)&penum->dev_cache_info;
			dev->is_band_device = 0;
			dev->white = 1;
			dev->black = 1;
			penum->dev_cache_set = 1;
		   }
		if ( (cc = gx_alloc_char_bits(dir,
					(gx_device_memory *)&penum->dev_cache_info,
					(ushort)iwidth,
					(ushort)iheight)) == 0 )
		  return 0;		/* too big for cache */
		/* The mins handle transposed coordinate systems.... */
		/* Truncate the offsets to avoid artifacts later. */
		cc->offset.x = fixed_ceiling(-min(cbox_ll.x, cbox_ur.x));
		cc->offset.y = fixed_ceiling(-min(cbox_ll.y, cbox_ur.y));
#ifdef DEBUG
if ( gs_debug['k'] )
		dprintf2("[k]offset=[%g %g]\n", fixed2float(cc->offset.x),
			 fixed2float(cc->offset.y));
#endif
		if ( !color_is_pure(pgs->dev_color) )	/* can't use cache */
		   {	gx_free_cached_char(dir, cc);
			return code;
		   }
		if ( (code = gs_gsave(pgs)) < 0 )
		   {	gx_free_cached_char(dir, cc);
			return code;
		   }
		/* Nothing can go wrong now.... */
		penum->cc = cc;
		cc->code = gs_show_current_char(penum);
		cc->wxy = penum->wxy;
		/* Install the device */
		pgs->device = &penum->dev_cache_dev;
		pgs->device_is_shared = 1;	/* don't deallocate */
		/* Adjust the translation in the graphics context */
		/* so that the character lines up with the cache. */
		gs_translate_to_fixed(pgs, cc->offset.x, cc->offset.y);
		/* Reset the clipping path to match the metrics. */
		clip_box.p.x = clip_box.p.y = 0;
		clip_box.q.x = int2fixed(iwidth);
		clip_box.q.y = int2fixed(iheight);
		if ( (code = gx_clip_to_rectangle(pgs, &clip_box)) < 0 )
		  return code;
		gx_set_black(pgs);	/* Set the color to black. */
	   }
	penum->width_status = sws_cache;
	return 0;
}

/* setcharwidth */
int
gs_setcharwidth(register gs_show_enum *penum, gs_state *pgs, floatp wx, floatp wy)
{	if ( penum->width_status != sws_none )
		return_error(gs_error_undefined);
	gs_distance_transform2fixed(&pgs->ctm, wx, wy, &penum->wxy);
	penum->width_status = sws_no_cache;
	return 0;
}

/* setmetrics */
int
gs_setmetrics(register gs_show_enum *penum, gs_state *pgs,
  gs_point *psbxy, gs_point *pwxy)
{	if ( penum->width_status != sws_none )
		return_error(gs_error_undefined);
	if ( psbxy != 0 )
	   {	penum->metrics_sb.x = float2fixed(psbxy->x);
		penum->metrics_sb.y = float2fixed(psbxy->y);
		penum->sb_set = 1;
	   }
	if ( pwxy != 0 )
	   {	penum->metrics_width.x = float2fixed(pwxy->x);
		penum->metrics_width.y = float2fixed(pwxy->y);
		penum->width_set = 1;
	   }
	return 0;
}

/* ------ Enumerator ------ */

/* Do the next step of a show (or stringwidth) operation */
int
gs_show_next(gs_show_enum *penum)
{	return (*penum->continue_proc)(penum);
}

/* Continuation procedures */
#define show_fast_move(wxy, pgs)\
  gx_path_add_rel_point_inline(pgs->path, wxy.x, wxy.y)
private int show_update(P1(register gs_show_enum *penum));
private int show_move(P1(register gs_show_enum *penum));
private int show_proceed(P1(register gs_show_enum *penum));
private int show_finish(P1(register gs_show_enum *penum));
private int
continue_show_update(register gs_show_enum *penum)
{	int code = show_update(penum);
	if ( code < 0 ) return code;
	code = show_move(penum);
	if ( code != 0 ) return code;
	return show_proceed(penum);
}
private int
continue_show(register gs_show_enum *penum)
{	return show_proceed(penum);
}

/* Update position */
private int
show_update(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	/* Update position for last character */
	switch ( penum->width_status )
	   {
	case sws_none:
		/* Adobe interpreters assume a character width of 0, */
		/* even though the documentation says this is an error.... */
		penum->wxy.x = penum->wxy.y = 0;
		break;
	case sws_cache:
	   {	/* Finish installing the cache entry. */
		cached_char *cc = penum->cc;
		int code;
		/* If the BuildChar procedure did a save and a restore, */
		/* it already undid the gsave in setcachedevice. */
		/* We have to check for this by comparing levels. */
		switch ( pgs->level - penum->level )
		   {
		default:
			return_error(gs_error_invalidfont);	/* WRONG */
		case 2:
			code = gs_grestore(pgs);
			if ( code < 0 ) return code;
		case 1:
			;
		   }
		gx_add_cached_char(pgs->font->dir, &penum->dev_cache_info,
				   cc, gx_lookup_fm_pair(pgs));
		if ( !penum->stringwidth_flag && !penum->charpath_flag )
		  { /* Copy the bits to the real output device. */
		    penum->color_loaded = 0;	/* force gx_color_render */
		    code = gs_grestore(pgs);
		    if ( code < 0 ) return code;
		    return gx_image_cached_char(penum, cc);
		  }
	   }
	case sws_no_cache: ;
	   }
	return gs_grestore(pgs);
}

/* Move to next character */
private int
show_move(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	if ( penum->add )
		gs_rmoveto(pgs, penum->ax, penum->ay);
	if ( penum->str[penum->index - 1] == penum->wchr )
		gs_rmoveto(pgs, penum->wcx, penum->wcy);
	/* wxy is in device coordinates */
	   {	int code = show_fast_move(penum->wxy, pgs);
		if ( code < 0 ) return code;
	   }
	/* Check for kerning, but not on the last character. */
	if ( penum->do_kern && penum->index < penum->size )
	   {	penum->continue_proc = continue_show;
		return gs_show_kern;
	   }
	return 0;
}
/* Process next character */
private int
show_proceed(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	byte *str = penum->str;
	uint index;
	cached_fm_pair *pair = 0;
	char_code chr;
	int code;
	penum->color_loaded = 0;
more:	/* Proceed to next character */
	if ( penum->can_cache )
	   {	/* Loop with cache */
		if ( pair == 0 )
		  pair = gx_lookup_fm_pair(pgs);
		if ( penum->stringwidth_flag )
		  while ( (index = penum->index++) != penum->size )
		    {	cached_char *cc;
			chr = str[index];
			cc = gx_lookup_cached_char(pgs, pair, chr);
			if ( cc == 0 ) goto no_cache;
			/* Character is in cache. */
			code = show_fast_move(cc->wxy, pgs);
			if ( code ) return code;
		    }
		else
		  while ( (index = penum->index++) != penum->size )
		    {	cached_char *cc;
			chr = str[index];
			cc = gx_lookup_cached_char(pgs, pair, chr);
			if ( cc == 0 ) goto no_cache;
			/* Character is in cache. */
			code = gx_image_cached_char(penum, cc);
			if ( code < 0 ) return code;
			else if ( code > 0 ) goto no_cache;
			if ( penum->slow_show )
			   {	penum->wxy = cc->wxy;
				code = show_move(penum);
			   }
			else
				code = show_fast_move(cc->wxy, pgs);
			if ( code ) return code;
		    }
		/* All done. */
		return show_finish(penum);
	   }
	else
	   {	/* Can't use cache */
		if ( (index = penum->index++) == penum->size )
		  {	/* All done. */
			return show_finish(penum);
		  }
		chr = str[index];
	   }
no_cache:
	/* Character is not cached, client must render it. */
	if ( (code = gs_gsave(pgs)) < 0 ) return code;
	/* Set the charpath flag in the graphics context if necessary, */
	/* so that fill and stroke will add to the path */
	/* rather than having their usual effect. */
	pgs->in_charpath = penum->charpath_flag;
	   {	gs_fixed_point cpt;
		gx_path *ppath = pgs->path;
		if ( (code = gx_path_current_point_inline(ppath, &cpt)) < 0 )
			return code;
		cpt.x -= pgs->ctm.tx_fixed;
		cpt.y -= pgs->ctm.ty_fixed;
		gs_setmatrix(pgs, &pgs->char_tm);
		cpt.x += pgs->ctm.tx_fixed;
		cpt.y += pgs->ctm.ty_fixed;
		if ( !penum->stringwidth_flag && !penum->charpath_flag )
		  { /* Round the translation in the graphics state. */
		    /* This helps prevent rounding artifacts later. */
		    cpt.x = fixed_rounded(cpt.x);
		    cpt.y = fixed_rounded(cpt.y);
		  }
		gs_translate_to_fixed(pgs, cpt.x, cpt.y);
		gs_newpath(pgs);
		gx_path_add_point(ppath, pgs->ctm.tx_fixed,
				  pgs->ctm.ty_fixed);
	   }
	penum->width_status = sws_none;
	penum->width_set = penum->sb_set = 0;
	penum->continue_proc = continue_show_update;
	/* Try using the build procedure in the font. */
	/* < 0 means error, 0 means success, 1 means failure. */
	   {	gs_font *pfont = pgs->font;
		code = (*pfont->build_char_proc)(penum, pgs, pfont, chr, pfont->build_char_data);
		if ( code < 0 ) return_error(code);
		if ( code == 0 )
		   {	code = show_update(penum);
			if ( code < 0 ) return code;
			penum->color_loaded = 0;
			code = show_move(penum);
			if ( code ) return code;
			goto more;
		   }
	   }
	return gs_show_render;
}

/* Finish show or stringwidth */
private int
show_finish(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	int code;
	if ( !penum->stringwidth_flag ) return 0;
	/* Save the accumulated width before returning, */
	/* and undo the extra gsave. */
	code = gs_currentpoint(pgs, &penum->width);
	if ( code < 0 ) return code;
	return gs_grestore(pgs);
}

/* Return the current character for rendering. */
char_code
gs_show_current_char(gs_show_enum *penum)
{	return penum->str[penum->index - 1];
}

/* Return the just-displayed character for kerning. */
char_code
gs_kshow_previous_char(gs_show_enum *penum)
{	return penum->str[penum->index - 1];
}

/* Return the about-to-be-displayed character for kerning. */
char_code
gs_kshow_next_char(gs_show_enum *penum)
{	return penum->str[penum->index];
}

/* Return the accumulated width for stringwidth. */
void
gs_show_width(gs_show_enum *penum, gs_point *ppt)
{	*ppt = penum->width;
}

/* Return the charpath flag. */
int
gs_show_in_charpath(gs_show_enum *penum)
{	return penum->charpath_flag;
}

/* ------ Internal routines ------ */

/* Initialize a show enumerator */
private int
show_setup(register gs_show_enum *penum, gs_state *pgs, char *str)
{	int code;
	gs_font *pfont = pgs->font;
	penum->pgs = pgs;
	penum->level = pgs->level;
	penum->str = (byte *)str;	/* avoid signed chars */
	penum->wchr = no_chr;
	penum->add = 0;
	penum->do_kern = 0;
	penum->charpath_flag = 0;
	penum->stringwidth_flag = 0;
	penum->dev_cache_set = 0;
	penum->index = 0;
	penum->continue_proc = continue_show;
	if ( (penum->is_composite = pfont->FontType == ft_composite) )
	   {	gs_font *rfont = pgs->font;
		penum->fstack[0] = rfont;
		penum->fdepth = 0;
		penum->pfont =
		  rfont->data.type0_data.FDepVector[rfont->data.type0_data.Encoding[0]];
	   }
	if ( !pgs->char_tm_valid )
	   {	/* Compute combined transformation */
		gs_make_identity(&pgs->char_tm);	/* make sure type */
					/* fields are set in char_tm! */
		code = gs_matrix_multiply(&pgs->font->FontMatrix,
					  &ctm_only(pgs), &pgs->char_tm);
		if ( code < 0 ) return code;
		pgs->char_tm_valid = 1;
	   }
	if ( penum->can_cache =	/* no skewing or non-rectangular rotation */
		(is_fzero2(pgs->char_tm.xy, pgs->char_tm.yx) ||
		 is_fzero2(pgs->char_tm.xx, pgs->char_tm.yy)) )
	   {	gs_fixed_rect cbox;
		gx_cpath_box_for_check(pgs->clip_path, &cbox);
		penum->cxmin = fixed2int_var(cbox.p.x);
		penum->cymin = fixed2int_var(cbox.p.y);
		penum->cxmax = fixed2int_var(cbox.q.x);
		penum->cymax = fixed2int_var(cbox.q.y);
		penum->ftx = (int)fixed2long(float2fixed(pgs->char_tm.tx) - pgs->ctm.tx_fixed);
		penum->fty = (int)fixed2long(float2fixed(pgs->char_tm.ty) - pgs->ctm.ty_fixed);
	   }
	return 0;
}
