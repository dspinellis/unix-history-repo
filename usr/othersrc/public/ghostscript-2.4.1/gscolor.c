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

/* gscolor.c */
/* Color and halftone operators for GhostScript library */
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"			/* ditto */
#include "gxmatrix.h"			/* for gzstate.h */
#include "gxdevice.h"			/* for gx_color_index */
#include "gzstate.h"
#include "gzcolor.h"
#include "gzht.h"

/* Halftone enumeration structure */
struct gs_screen_enum_s {
	halftone_params ht;		/* constructed here */
	gs_matrix mat;		/* for mapping device x,y to rotated cell */
	int x, y;
	gs_state *pgs;
};

/* Exported values */
const uint gs_screen_enum_sizeof = sizeof(gs_screen_enum);
const uint gs_color_sizeof = sizeof(gs_color);

/* Forward declarations */
private void tri_param(P4(floatp, floatp, floatp, color_param [3]));
private void tri_return(P4(color_param, color_param, color_param, float [3]));
private void set_phase(P1(gs_state *));
private void load_transfer_map(P2(gs_state *, gx_transfer_map *));
private int unshare_transfer(P1(gs_state *));
/* Imported from gxcolor.c */
void	gx_color_from_hsb(P4(gs_color *, color_param, color_param, color_param)),
	gx_color_from_rgb(P1(gs_color *)),
	gx_color_to_hsb(P2(gs_color *, color_param [3])),
	gx_sort_ht_order(P2(ht_bit *, uint));

/* setgray */
int
gs_setgray(gs_state *pgs, floatp gray)
{	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	gx_set_gray_only(pgs->color, gx_color_unit_param(gray));
	return gx_remap_color(pgs);
}

/* currentgray */
float
gs_currentgray(gs_state *pgs)
{	return (float)(color_luminance(pgs->color) / max_color_param_float);
}

/* sethsbcolor */
int
gs_sethsbcolor(gs_state *pgs, floatp h, floatp s, floatp b)
{	color_param params[3];
	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	tri_param(h, s, b, params);
	gx_color_from_hsb(pgs->color, params[0], params[1], params[2]);
	return gx_remap_color(pgs);
}

/* currenthsbcolor */
int
gs_currenthsbcolor(gs_state *pgs, float pr3[3])
{	color_param hsb[3];
	gx_color_to_hsb(pgs->color, hsb);
	tri_return(hsb[0], hsb[1], hsb[2], pr3);
	return 0;
}

/* setrgbcolor */
int
gs_setrgbcolor(gs_state *pgs, floatp r, floatp g, floatp b)
{	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	gx_set_rgb_only(pgs->color, gx_color_unit_param(r),
			gx_color_unit_param(g), gx_color_unit_param(b));
	return gx_remap_color(pgs);
}

/* currentrgbcolor */
int
gs_currentrgbcolor(gs_state *pgs, float pr3[3])
{	gs_color *pcolor = pgs->color;
	tri_return(pcolor->red, pcolor->green, pcolor->blue, pr3);
	return 0;
}

/* currentcolorspace */
int
gs_currentcolorspace(gs_state *pgs, gs_color_space *pcs)
{	*pcs = (gs_color_space)pgs->color->space;
	return 0;
}

/* setscreen */
int
gs_setscreen(gs_state *pgs,
  floatp freq, floatp angle, float (*proc)(P2(floatp, floatp)))
{	gs_screen_enum senum;
	gs_point pt;
	int code = gs_screen_init(&senum, pgs, freq, angle);
	if ( code < 0 ) return code;
	while ( (code = gs_screen_currentpoint(&senum, &pt)) == 0 )
		if ( (code = gs_screen_next(&senum, (*proc)(pt.x, pt.y))) < 0 )
			return code;
	if ( code < 0 ) return code;
	pgs->ht_proc = proc;
	set_phase(pgs);
	return 0;
}

/* currentscreen */
int
gs_currentscreen(gs_state *pgs,
  float *pfreq, float *pangle, float (**pproc)(P2(floatp, floatp)))
{	halftone_params *pht = pgs->halftone;
	*pfreq = pht->frequency;
	*pangle = pht->angle;
	*pproc = pgs->ht_proc;
	return 0;
}

/* settransfer */
/* Remap=0 is used by the interpreter. */
int
gs_settransfer_remap(gs_state *pgs, gs_transfer_proc tproc, int remap)
{	int code = unshare_transfer(pgs);
	gx_transfer *ptran;
	if ( code < 0 ) return code;
	ptran = pgs->transfer;
	ptran->gray.proc = tproc;
	if ( remap )
	  load_transfer_map(pgs, &ptran->gray);
	ptran->red = ptran->gray;
	ptran->green = ptran->gray;
	ptran->blue = ptran->gray;
	return (remap ? gx_remap_color(pgs) : 0);
}
int
gs_settransfer(gs_state *pgs, gs_transfer_proc tproc)
{	return gs_settransfer_remap(pgs, tproc, 1);
}

/* currenttransfer */
gs_transfer_proc
gs_currenttransfer(gs_state *pgs)
{	return pgs->transfer->gray.proc;
}

/* setcolortransfer */
/* Remap=0 is used by the interpreter. */
int
gs_setcolortransfer_remap(gs_state *pgs, gs_transfer_proc red_proc,
  gs_transfer_proc green_proc, gs_transfer_proc blue_proc,
  gs_transfer_proc gray_proc, int remap)
{	int code = unshare_transfer(pgs);
	gx_transfer *ptran;
	if ( code < 0 ) return code;
	ptran = pgs->transfer;
	ptran->red.proc = red_proc;
	ptran->green.proc = green_proc;
	ptran->blue.proc = blue_proc;
	ptran->gray.proc = gray_proc;
	if ( remap )
	  { load_transfer_map(pgs, &ptran->red);
	    load_transfer_map(pgs, &ptran->green);
	    load_transfer_map(pgs, &ptran->blue);
	    load_transfer_map(pgs, &ptran->gray);
	    return gx_remap_color(pgs);
	  }
	else
	  return 0;
}
int
gs_setcolortransfer(gs_state *pgs, gs_transfer_proc red_proc,
  gs_transfer_proc green_proc, gs_transfer_proc blue_proc,
  gs_transfer_proc gray_proc)
{	return gs_setcolortransfer_remap(pgs, red_proc, green_proc,
					 blue_proc, gray_proc, 1);
}

/* currentcolortransfer */
void
gs_currentcolortransfer(gs_state *pgs, gs_transfer_proc procs[4])
{	gx_transfer *ptran = pgs->transfer;
	procs[0] = ptran->red.proc;
	procs[1] = ptran->green.proc;
	procs[2] = ptran->blue.proc;
	procs[3] = ptran->gray.proc;
}

/* sethalftonephase */
int
gs_sethalftonephase(gs_state *pgs, int x, int y)
{	pgs->ht_phase.x = x;
	pgs->ht_phase.y = y;
	set_phase(pgs);
	return 0;
}

/* currenthalftonephase */
int
gs_currenthalftonephase(gs_state *pgs, gs_int_point *pphase)
{	*pphase = pgs->ht_phase;
	return 0;
}

/* ------ Halftone sampling ------ */

/* Set up for halftone sampling */
int
gs_screen_init(gs_screen_enum *penum, gs_state *pgs,
  floatp freq, floatp angle)
{	int cwidth, cheight;
	int code;
	ht_bit *order;
	if ( freq < 0.0 ) return_error(gs_error_rangecheck);
	/* Convert the frequency to cell width and height */
	   {	float cell_size = 72.0 / freq;
		gs_point pcwh;
		gs_matrix imat;
		gs_deviceinitialmatrix(gs_currentdevice(pgs), &imat);
		if ( (code = gs_distance_transform(cell_size, cell_size,
						   &imat, &pcwh)) < 0
		    ) return code;
		/* It isn't clear to me whether we should round the */
		/* width and height, truncate them, or do something */
		/* more complicated.  All the problems arise from devices */
		/* whose X and Y resolutions aren't the same: */
		/* the halftone model isn't really designed for this. */
		/* For the moment, truncate and hope for the best. */
#define abs_round(z) (z < 0 ? -(int)(z) : (int)(z))
/*#define abs_round(z) (z < 0 ? -(int)(z - 0.5) : (int)(z + 0.5))*/
		cwidth = abs_round(pcwh.x);
		cheight = abs_round(pcwh.y);
#undef abs_round
	   }
	/* Force a halfway reasonable cell size. */
	if ( cwidth <= 4 ) cwidth = 4;
	if ( cheight <= 4 ) cheight = 4;
	if ( cwidth > max_ushort / cheight )
		return_error(gs_error_rangecheck);
	order = (ht_bit *)gs_malloc(cwidth * cheight, sizeof(ht_bit),
				    "halftone samples");
	if ( order == 0 ) return_error(gs_error_VMerror);
	penum->ht.frequency = freq;
	penum->ht.angle = angle;
	penum->ht.order = order;
	penum->ht.width = cwidth;
	penum->ht.height = cheight;
	penum->ht.order_size = cwidth * cheight;
	penum->x = penum->y = 0;
	penum->pgs = pgs;
	/* The transformation matrix must include normalization to the */
	/* interval (-1..1), and rotation by the negative of the angle. */
	   {	float xscale = 2.0 / cwidth;
		float yscale = 2.0 / cheight;
		gs_matrix mat;
		gs_make_identity(&mat);
		mat.xx = xscale;
		mat.yy = yscale;
		mat.tx = xscale * 0.5 - 1.0;
		mat.ty = yscale * 0.5 - 1.0;
		if ( (code = gs_matrix_rotate(&mat, -angle, &penum->mat)) < 0 )
			return code;
#ifdef DEBUG
if ( gs_debug['h'] )
	dprintf8("[h]Screen: w=%d h=%d [%f %f %f %f %f %f]\n",
		 cwidth, cheight, penum->mat.xx, penum->mat.xy,
		 penum->mat.yx, penum->mat.yy, penum->mat.tx, penum->mat.ty);
#endif
	   }
	return 0;
}

/* Report current point for sampling */
private int gx_screen_finish(P1(gs_screen_enum *));
int
gs_screen_currentpoint(gs_screen_enum *penum, gs_point *ppt)
{	gs_point pt;
	int code;
	if ( penum->y >= penum->ht.height )	/* all done */
		return gx_screen_finish(penum);
	if ( (code = gs_point_transform((floatp)penum->x, (floatp)penum->y, &penum->mat, &pt)) < 0 )
		return code;
	while ( pt.x < -1.0 ) pt.x += 2.0;
	while ( pt.x >= 1.0 ) pt.x -= 2.0;
	while ( pt.y < -1.0 ) pt.y += 2.0;
	while ( pt.y >= 1.0 ) pt.y -= 2.0;
	*ppt = pt;
	return 0;
}

/* Record next halftone sample */
int
gs_screen_next(gs_screen_enum *penum, floatp value)
{	ushort sample;
	if ( value < -1.0 || value > 1.0 )
	  return_error(gs_error_rangecheck);
	/* The following statement was split into two */
	/* to work around a bug in the Siemens C compiler. */
	sample = (ushort)(value * (float)(int)(max_ushort >> 1));
	sample += (max_ushort >> 1);	/* convert from signed to biased */
#ifdef DEBUG
if ( gs_debug['h'] )
   {	gs_point pt;
	gs_screen_currentpoint(penum, &pt);
	dprintf6("[h]sample x=%d y=%d (%f,%f): %f -> %u\n",
		 penum->x, penum->y, pt.x, pt.y, value, sample);
   }
#endif
	penum->ht.order[penum->y * penum->ht.width + penum->x].mask = sample;
	if ( ++(penum->x) >= penum->ht.width )
		penum->x = 0, ++(penum->y);
	return 0;
}

/* All points have been sampled. */
/* Finish constructing the halftone. */
private int
gx_screen_finish(gs_screen_enum *penum)
{	ht_bit *order = penum->ht.order;
	uint size = penum->ht.order_size;
	uint i;
	int code;
	/* Label each element with its ordinal position. */
	for ( i = 0; i < size; i++ )
		order[i].offset = i;
	/* Sort the samples in increasing order by value. */
	gx_sort_ht_order(order, size);
	/* Set up the actual halftone description. */
	code = gx_ht_construct_order(order, penum->ht.width, penum->ht.height);
	if ( code < 0 ) return code;
	*penum->pgs->halftone = penum->ht;
	return 1;			/* all done */
}

/* ------ Internal routines ------ */

/* Get 3 real parameters in the range [0..1], */
/* and convert them to color_params. */
private void
tri_param(floatp p1, floatp p2, floatp p3, color_param pq3[3])
{	pq3[0] = gx_color_unit_param(p1);
	pq3[1] = gx_color_unit_param(p2);
	pq3[2] = gx_color_unit_param(p3);
}

/* Convert 3 color_params to reals */
private void
tri_return(color_param p1, color_param p2, color_param p3, float pr3[3])
{	pr3[0] = p1 / max_color_param_float;
	pr3[1] = p2 / max_color_param_float;
	pr3[2] = p3 / max_color_param_float;
}

/* Compute the negated halftone phase mod the tile size. */
/* This is the displacement of the tile relative to the device coordinates. */
private void
set_phase(gs_state *pgs)
{	halftone_params *pht = pgs->halftone;
	if ( pht->width == 0 )
		pgs->phase_mod.x = 0;
	else
	   {	if ( (pgs->phase_mod.x = -pgs->ht_phase.x % pht->width) < 0 )
			pgs->phase_mod.x += pht->width;
	   }
	if ( pht->height == 0 )
		pgs->phase_mod.y = 0;
	else
	   {	if ( (pgs->phase_mod.y = -pgs->ht_phase.y % pht->height) < 0 )
			pgs->phase_mod.y += pht->height;
	   }
}

/* Load one cached transfer map. */
private void
load_transfer_map(gs_state *pgs, gx_transfer_map *pmap)
{	gs_transfer_proc proc = pmap->proc;
	color_param *values = pmap->values;
	int i;
	for ( i = 0; i < transfer_map_size; i++ )
	  values[i] = gx_color_unit_param((*proc)(pgs, (float)i / transfer_map_size));
}

/* Ensure that the transfer map is not shared. */
private int
unshare_transfer(gs_state *pgs)
{	if ( pgs->transfer->ref_count > 1 )
	   {	gx_transfer *ptran = (gx_transfer *)(*pgs->memory_procs.alloc)(1, sizeof(gx_transfer), "unshare_transfer");
		if ( ptran == 0 ) return_error(gs_error_VMerror);
		ptran->ref_count = 1;
		pgs->transfer->ref_count--;
		pgs->transfer = ptran;
	   }
	return 0;
}
