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

/* gscoord.c */
/* Coordinate system operators for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxarith.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate */
#include "gscoord.h"			/* requires gsmatrix, gsstate */

/* Choose whether to enable the new rounding code in update_ctm_fixed. */
/* I'm pretty sure this is the right thing to do, but since this change */
/* is being made 1 day before releasing version 2.4, I'm feeling cautious. */
#define round_ctm_fixed 1

/* Forward declarations */
#ifdef DEBUG
private void trace_ctm(P1(const gs_state *));
private void trace_matrix(P1(const gs_matrix *));
#endif

/* Macro for ensuring ctm_inverse is valid */
#ifdef DEBUG
#define print_inverse(pgs)\
if ( gs_debug['x'] )\
	dprintf("[x]Inverting:\n"), trace_ctm(pgs), trace_matrix(&pgs->ctm_inverse)
#else
#define print_inverse(pgs) 0
#endif
#define ensure_inverse_valid(pgs)\
	if ( !pgs->inverse_valid )\
	   {	int code = ctm_set_inverse(pgs);\
		if ( code < 0 ) return code;\
	   }

private int
ctm_set_inverse(gs_state *pgs)
{	int code = gs_matrix_invert(&ctm_only(pgs), &pgs->ctm_inverse);
	print_inverse(pgs);
	if ( code < 0 ) return code;
	pgs->inverse_valid = 1;
	return 0;
}

/* Machinery for updating fixed version of ctm. */
/*
 * We (conditionally) adjust the floating point translation
 * so that it exactly matches the (rounded) fixed translation.
 * This avoids certain unpleasant rounding anomalies, such as
 * 0 0 moveto currentpoint not returning 0 0, and () stringwidth
 * not returning 0 0.
 */
#if round_ctm_fixed			/* ****** NOTA BENE ****** */
#  define update_t_fixed(mat, t, t_fixed)\
    (mat).t = fixed2float((mat).t_fixed = float2fixed((mat).t))
#else					/* !round_update_fixed */
#  define update_t_fixed(mat, t, t_fixed)\
    (mat).t_fixed = float2fixed((mat).t)
#endif					/* (!)round_update_fixed */
#define update_matrix_fixed(mat)\
  update_t_fixed(mat, tx, tx_fixed),\
  update_t_fixed(mat, ty, ty_fixed)
#define update_ctm(pgs)\
  update_matrix_fixed(pgs->ctm),\
  pgs->inverse_valid = 0,\
  pgs->char_tm_valid = 0

void
gs_update_matrix_fixed(gs_matrix_fixed *pmat)
{	update_matrix_fixed(*pmat);
}

/* ------ Coordinate system definition ------ */

int
gs_initmatrix(gs_state *pgs)
{	gx_device *dev = pgs->device->info;
	(*dev->procs->get_initial_matrix)(dev, &ctm_only(pgs));
	update_ctm(pgs);
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf("[x]initmatrix:\n"), trace_ctm(pgs);
#endif
	return 0;
}

int
gs_defaultmatrix(const gs_state *pgs, gs_matrix *pmat)
{	gx_device *dev = pgs->device->info;
	(*dev->procs->get_initial_matrix)(dev, pmat);
	return 0;
}

int
gs_currentmatrix(const gs_state *pgs, gs_matrix *pmat)
{	*pmat = ctm_only(pgs);
	return 0;
}

int
gs_setmatrix(gs_state *pgs, const gs_matrix *pmat)
{	ctm_only(pgs) = *pmat;
	update_ctm(pgs);
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf("[x]setmatrix:\n"), trace_ctm(pgs);
#endif
	return 0;
}

int
gs_translate(gs_state *pgs, floatp dx, floatp dy)
{	gs_point pt;
	int code;
	if ( (code = gs_distance_transform(dx, dy, &ctm_only(pgs), &pt)) < 0 )
		return code;
	pgs->ctm.tx += pt.x;
	pgs->ctm.ty += pt.y;
	update_ctm(pgs);
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf4("[x]translate: %f %f -> %f %f\n",
		 dx, dy, pt.x, pt.y),
	trace_ctm(pgs);
#endif
	return 0;
}

int
gs_scale(gs_state *pgs, floatp sx, floatp sy)
{	pgs->ctm.xx *= sx;
	pgs->ctm.xy *= sx;
	pgs->ctm.yx *= sy;
	pgs->ctm.yy *= sy;
	pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf2("[x]scale: %f %f\n", sx, sy), trace_ctm(pgs);
#endif
	return 0;
}

int
gs_rotate(gs_state *pgs, floatp ang)
{	int code = gs_matrix_rotate(&ctm_only(pgs), ang, &ctm_only(pgs));
	pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf1("[x]rotate: %f\n", ang), trace_ctm(pgs);
#endif
	return code;
}

int
gs_concat(gs_state *pgs, const gs_matrix *pmat)
{	int code = gs_matrix_multiply(pmat, &ctm_only(pgs), &ctm_only(pgs));
	update_ctm(pgs);
#ifdef DEBUG
if ( gs_debug['x'] )
	dprintf("[x]concat:\n"), trace_matrix(pmat), trace_ctm(pgs);
#endif
	return code;
}

/* ------ Coordinate transformation ------ */

int
gs_transform(gs_state *pgs, floatp x, floatp y, gs_point *pt)
{	return gs_point_transform(x, y, &ctm_only(pgs), pt);
}

int
gs_dtransform(gs_state *pgs, floatp dx, floatp dy, gs_point *pt)
{	return gs_distance_transform(dx, dy, &ctm_only(pgs), pt);
}

int
gs_itransform(gs_state *pgs, floatp x, floatp y, gs_point *pt)
{	/* If the matrix isn't skewed, we get more accurate results */
	/* by using transform_inverse than by using the inverse matrix. */
	if ( !is_skewed(&pgs->ctm) )
	   {	return gs_point_transform_inverse(x, y, &ctm_only(pgs), pt);
	   }
	else
	   {	ensure_inverse_valid(pgs);
		return gs_point_transform(x, y, &pgs->ctm_inverse, pt);
	   }
}

int
gs_idtransform(gs_state *pgs, floatp dx, floatp dy, gs_point *pt)
{	/* If the matrix isn't skewed, we get more accurate results */
	/* by using transform_inverse than by using the inverse matrix. */
	if ( !is_skewed(&pgs->ctm) )
	   {	return gs_distance_transform_inverse(dx, dy,
						     &ctm_only(pgs), pt);
	   }
	else
	   {	ensure_inverse_valid(pgs);
		return gs_distance_transform(dx, dy, &pgs->ctm_inverse, pt);
	   }
}

/* ------ For internal use only ------ */

/* Set the translation to a fixed value, */
/* and mark char_tm as valid. */
/* Used by gschar.c to prepare for a BuildChar procedure. */
int
gs_translate_to_fixed(register gs_state *pgs, fixed px, fixed py)
{	pgs->ctm.tx = fixed2float(pgs->ctm.tx_fixed = px);
	pgs->ctm.ty = fixed2float(pgs->ctm.ty_fixed = py);
	pgs->inverse_valid = 0;
	pgs->char_tm_valid = 1;
	return 0;
}

/* Compute the coefficients for fast fixed-point distance transformations */
/* from a transformation matrix. */
/* We should cache the coefficients with the ctm.... */
int
gx_matrix_to_fixed_coeff(const gs_matrix *pmat, register fixed_coeff *pfc,
  int max_bits)
{	gs_matrix ctm;
	int scale = -10000;
	int expt, shift;
	ctm = *pmat;
	pfc->skewed = 0;
	if ( !is_fzero(ctm.xx) )
	   {	(void)frexp(ctm.xx, &scale);
	   }
	if ( !is_fzero(ctm.xy) )
	   {	(void)frexp(ctm.xy, &expt);
		if ( expt > scale ) scale = expt;
		pfc->skewed = 1;
	   }
	if ( !is_fzero(ctm.yx) )
	   {	(void)frexp(ctm.yx, &expt);
		if ( expt > scale ) scale = expt;
		pfc->skewed = 1;
	   }
	if ( !is_fzero(ctm.yy) )
	   {	(void)frexp(ctm.yy, &expt);
		if ( expt > scale ) scale = expt;
	   }
	scale = sizeof(long) * 8 - 1 - max_bits - scale;
	shift = scale - _fixed_shift;
	if ( shift > 0 )
	   {	pfc->shift = shift;
		pfc->round = (fixed)1 << (shift - 1);
	   }
	else
	   {	pfc->shift = 0;
		pfc->round = 0;
		scale -= shift;
	   }
	pfc->xx = (is_fzero(ctm.xx) ? 0 : (long)ldexp(ctm.xx, scale));
	pfc->yy = (is_fzero(ctm.yy) ? 0 : (long)ldexp(ctm.yy, scale));
	if ( pfc->skewed )
	   {	pfc->xy = (is_fzero(ctm.xy) ? 0 : (long)ldexp(ctm.xy, scale));
		pfc->yx = (is_fzero(ctm.yx) ? 0 : (long)ldexp(ctm.yx, scale));
	   }
	else
		pfc->xy = pfc->yx = 0;
#ifdef DEBUG
if ( gs_debug['x'] )
   {	dprintf7("[x]ctm: [%6g %6g %6g %6g ; %6g %6g] scale=%d\n",
		 ctm.xx, ctm.xy, ctm.yx, ctm.yy, ctm.tx, ctm.ty, scale);
	dprintf5("   fc: [%lx %lx %lx %lx] shift=%d\n",
		 pfc->xx, pfc->xy, pfc->yx, pfc->yy,
		 pfc->shift);
   }
#endif
	pfc->max_bits = max_bits;
	return 0;
}

/* ------ Debugging printout ------ */

#ifdef DEBUG

/* Print a matrix */
private void
trace_ctm(const gs_state *pgs)
{	const gs_matrix_fixed *pmat = &pgs->ctm;
	trace_matrix((gs_matrix *)pmat);
	dprintf2("\t\tt_fixed: [%6g %6g]\n",
		 fixed2float(pmat->tx_fixed), fixed2float(pmat->ty_fixed));
}
private void
trace_matrix(register const gs_matrix *pmat)
{	dprintf6("\t[%6g %6g %6g %6g %6g %6g]\n",
		 pmat->xx, pmat->xy, pmat->yx, pmat->yy, pmat->tx, pmat->ty);
}

#endif
