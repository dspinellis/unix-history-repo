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

/* gsmatrix.c */
/* Matrix operators for GhostScript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"

/* The identity matrix */
/* This should be private (static), but the interpreter */
/* has to be able to fill in the "unused" words. */
/*static*/ gs_matrix gs_identity_matrix =
	{ identity_matrix_body };

/* ------ Matrix creation ------ */

/* Create an identity matrix */
void
gs_make_identity(gs_matrix *pmat)
{	*pmat = gs_identity_matrix;
}

/* Create a translation matrix */
int
gs_make_translation(floatp dx, floatp dy, register gs_matrix *pmat)
{	*pmat = gs_identity_matrix;
	pmat->tx = dx;
	pmat->ty = dy;
	return 0;
}

/* Create a scaling matrix */
int
gs_make_scaling(floatp sx, floatp sy, register gs_matrix *pmat)
{	*pmat = gs_identity_matrix;
	pmat->xx = sx;
	pmat->yy = sy;
	return 0;
}

/* Create a rotation matrix. */
/* The angle is in degrees. */
int
gs_make_rotation(floatp ang, register gs_matrix *pmat)
{	float theta = ang * (M_PI / 180.0);
	*pmat = gs_identity_matrix;
	pmat->xx = pmat->yy = cos(theta);
	pmat->yx = -(pmat->xy = sin(theta));
	return 0;
}

/* ------ Matrix arithmetic ------ */

/* Multiply two matrices.  We should check for floating exceptions, */
/* but for the moment it's just too awkward. */
/* Since this is used heavily, we check for shortcuts. */
int
gs_matrix_multiply(const gs_matrix *pm1, const gs_matrix *pm2, register gs_matrix *pmr)
{	float xx1 = pm1->xx, yy1 = pm1->yy;
	float tx1 = pm1->tx, ty1 = pm1->ty;
	float xx2 = pm2->xx, yy2 = pm2->yy;
	float xy2 = pm2->xy, yx2 = pm2->yx;
	if ( !is_skewed(pm1) )
	   {	pmr->tx = tx1 * xx2 + pm2->tx;
		pmr->ty = ty1 * yy2 + pm2->ty;
		if ( is_fzero(xy2) )
			pmr->xy = 0;
		else
			pmr->xy = xx1 * xy2,
			pmr->ty += tx1 * xy2;
		pmr->xx = xx1 * xx2;
		if ( is_fzero(yx2) )
			pmr->yx = 0;
		else
			pmr->yx = yy1 * yx2,
			pmr->tx += ty1 * yx2;
		pmr->yy = yy1 * yy2;
	   }
	else
	   {	pmr->xx = xx1 * xx2 + pm1->xy * yx2;
		pmr->xy = xx1 * xy2 + pm1->xy * yy2;
		pmr->yy = pm1->yx * xy2 + yy1 * yy2;
		pmr->yx = pm1->yx * xx2 + yy1 * yx2;
		pmr->tx = tx1 * xx2 + ty1 * yx2 + pm2->tx;
		pmr->ty = tx1 * xy2 + ty1 * yy2 + pm2->ty;
	   }
	return 0;
}

/* Invert a matrix.  Return gs_error_undefinedresult if not invertible. */
int
gs_matrix_invert(register const gs_matrix *pm, register gs_matrix *pmr)
{	/* We have to be careful about fetch/store order, */
	/* because pm might be the same as pmr. */
	if ( !is_skewed(pm) )
	   {	if ( is_fzero(pm->xx) || is_fzero(pm->yy) )
			return_error(gs_error_undefinedresult);
		pmr->tx = - (pmr->xx = 1.0 / pm->xx) * pm->tx;
		pmr->xy = 0.0;
		pmr->yx = 0.0;
		pmr->ty = - (pmr->yy = 1.0 / pm->yy) * pm->ty;
	   }
	else
	   {	double det = pm->xx * pm->yy - pm->xy * pm->yx;
		float mxx = pm->xx, mtx = pm->tx;
		if ( det == 0 ) return_error(gs_error_undefinedresult);
		pmr->xx = pm->yy / det;
		pmr->xy = - pm->xy / det;
		pmr->yx = - pm->yx / det;
		pmr->yy = mxx / det;	/* xx is already changed */
		pmr->tx = - (mtx * pmr->xx + pm->ty * pmr->yx);
		pmr->ty = - (mtx * pmr->xy + pm->ty * pmr->yy);	/* tx is already changed */
	   }
	return 0;
}

/* Rotate a matrix, possibly in place.  The angle is in degrees. */
int
gs_matrix_rotate(register const gs_matrix *pm, floatp ang, register gs_matrix *pmr)
{	float mxx, mxy;
	int quads;
	float tsin, tcos;
	/* We do some special checking for multiples of 90, */
	/* so we don't get any rounding errors. */
	if (	ang >= -360 && ang <= 360 &&
		ang == (quads = (int)ang / 90) * 90
	   )
	   {	int isin = 0, icos = 1, t;
		quads &= 3;
		while ( quads-- ) t = isin, isin = icos, icos = -t;
		tsin = isin, tcos = icos;
	   }
	else
	   {	float theta = ang * (M_PI / 180.0);
		tsin = sin(theta);
		tcos = cos(theta);
	   }
	mxx = pm->xx, mxy = pm->xy;
	pmr->xx = tcos * mxx + tsin * pm->yx;
	pmr->xy = tcos * mxy + tsin * pm->yy;
	pmr->yx = tcos * pm->yx - tsin * mxx;
	pmr->yy = tcos * pm->yy - tsin * mxy;
	pmr->tx = pm->tx;
	pmr->ty = pm->ty;
	return 0;
}

/* ------ Coordinate transformations (floating point) ------ */

/* Note that all the transformation routines take separate */
/* x and y arguments, but return their result in a point. */

/* Transform a point. */
int
gs_point_transform(floatp x, floatp y, register const gs_matrix *pmat,
  register gs_point *ppt)
{	ppt->x = x * pmat->xx + pmat->tx;
	ppt->y = y * pmat->yy + pmat->ty;
	if ( !is_fzero(pmat->yx) )
		ppt->x += y * pmat->yx;
	if ( !is_fzero(pmat->xy) )
		ppt->y += x * pmat->xy;
	return 0;
}

/* Inverse-transform a point. */
/* Return gs_error_undefinedresult if the matrix is not invertible. */
int
gs_point_transform_inverse(floatp x, floatp y, register const gs_matrix *pmat,
  register gs_point *ppt)
{	if ( !is_skewed(pmat) )
	   {	if ( is_fzero(pmat->xx) || is_fzero(pmat->yy) )
			return_error(gs_error_undefinedresult);
		ppt->x = (x - pmat->tx) / pmat->xx;
		ppt->y = (y - pmat->ty) / pmat->yy;
		return 0;
	   }
	else
	   {	/* There are faster ways to do this, */
		/* but we won't implement one unless we have to. */
		gs_matrix imat;
		int code = gs_matrix_invert(pmat, &imat);
		if ( code < 0 ) return code;
		return gs_point_transform(x, y, &imat, ppt);
	   }
}

/* Transform a distance. */
int
gs_distance_transform(floatp dx, floatp dy, register const gs_matrix *pmat,
  register gs_point *pdpt)
{	pdpt->x = dx * pmat->xx;
	pdpt->y = dy * pmat->yy;
	if ( !is_fzero(pmat->yx) )
		pdpt->x += dy * pmat->yx;
	if ( !is_fzero(pmat->xy) )
		pdpt->y += dx * pmat->xy;
	return 0;
}

/* Inverse-transform a distance. */
/* Return gs_error_undefinedresult if the matrix is not invertible. */
int
gs_distance_transform_inverse(floatp dx, floatp dy,
  register const gs_matrix *pmat, register gs_point *pdpt)
{	if ( !is_skewed(pmat) )
	   {	if ( is_fzero(pmat->xx) || is_fzero(pmat->yy) )
			return_error(gs_error_undefinedresult);
		pdpt->x = dx / pmat->xx;
		pdpt->y = dy / pmat->yy;
	   }
	else
	   {	double det = pmat->xx * pmat->yy - pmat->xy * pmat->yx;
		if ( det == 0 ) return_error(gs_error_undefinedresult);
		pdpt->x = (dx * pmat->yy - dy * pmat->yx) / det;
		pdpt->y = (dy * pmat->xx - dx * pmat->xy) / det;
	   }
	return 0;
}

/* Inverse-transform a bounding box. */
/* Return gs_error_undefinedresult if the matrix is not invertible. */
int
gs_bbox_transform_inverse(gs_rect *pbox_in, gs_matrix *pmat,
  gs_rect *pbox_out)
{	int code;
	gs_point p, w, h;
	double xmin, ymin, xmax, ymax;	/* gs_point uses double */
	/* We must recompute the max and min after transforming, */
	/* since a rotation may be involved. */
	if (	(code = gs_point_transform_inverse(pbox_in->p.x, pbox_in->p.y, pmat, &p)) < 0 ||
		(code = gs_distance_transform_inverse(pbox_in->q.x - pbox_in->p.x, 0.0, pmat, &w)) < 0 ||
		(code = gs_distance_transform_inverse(0.0, pbox_in->q.y - pbox_in->p.y, pmat, &h)) < 0
	   )
		return code;
	xmin = xmax = p.x;
	if ( w.x < 0 )	xmin += w.x;
	else		xmax += w.x;
	if ( h.x < 0 )	xmin += h.x;
	else		xmax += h.x;
	ymin = ymax = p.y;
	if ( w.y < 0 )	ymin += w.y;
	else		ymax += w.y;
	if ( h.y < 0 )	ymin += h.y;
	else		ymax += h.y;
	pbox_out->p.x = xmin, pbox_out->p.y = ymin;
	pbox_out->q.x = xmax, pbox_out->q.y = ymax;
	return 0;
}

/* ------ Coordinate transformations (to fixed point) ------ */

/* Transform a point with a fixed-point result. */
int
gs_point_transform2fixed(register gs_matrix_fixed *pmat,
  floatp x, floatp y, gs_fixed_point *ppt)
{	ppt->x = float2fixed(x * pmat->xx) + pmat->tx_fixed;
	ppt->y = float2fixed(y * pmat->yy) + pmat->ty_fixed;
	if ( !is_fzero(pmat->yx) )
		ppt->x += float2fixed(y * pmat->yx);
	if ( !is_fzero(pmat->xy) )
		ppt->y += float2fixed(x * pmat->xy);
	return 0;
}

/* Transform a distance with a fixed-point result. */
int
gs_distance_transform2fixed(register gs_matrix_fixed *pmat,
  floatp dx, floatp dy, gs_fixed_point *ppt)
{	ppt->x = float2fixed(dx * pmat->xx);
	ppt->y = float2fixed(dy * pmat->yy);
	if ( !is_fzero(pmat->yx) )
		ppt->x += float2fixed(dy * pmat->yx);
	if ( !is_fzero(pmat->xy) )
		ppt->y += float2fixed(dx * pmat->xy);
	return 0;
}
