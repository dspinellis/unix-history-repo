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

/* gsmatrix.h */
/* Definition of matrices and matrix routines for Ghostscript library */

#ifndef gsmatrix_INCLUDED
#  define gsmatrix_INCLUDED

/* See p. 65 of the PostScript manual for the semantics of */
/* transformation matrices. */

/* Structure for a transformation matrix. */
/* This is a machine-dependent hack to avoid importing */
/* the full definition of a Ghostscript type-tagged reference. */
#define _matrix_body\
	long _xx /*skip*/; float xx;\
	long _xy /*skip*/; float xy;\
	long _yx /*skip*/; float yx;\
	long _yy /*skip*/; float yy;\
	long _tx /*skip*/; float tx;\
	long _ty /*skip*/; float ty
typedef struct gs_matrix_s {
	_matrix_body;
} gs_matrix;
/* Macro for initializing constant matrices */
#define constant_matrix_body(xx, xy, yx, yy, tx, ty)\
	0L,(float)(xx), 0L,(float)(xy), 0L,(float)(yx),\
	0L,(float)(yy), 0L,(float)(tx), 0L,(float)(ty)

/* The identity matrix (for structure initialization) */
#define identity_matrix_body\
	constant_matrix_body(1, 0, 0, 1, 0, 0)

/* Matrix creation */
void	gs_make_identity(P1(gs_matrix *));
int	gs_make_translation(P3(floatp, floatp, gs_matrix *)),
	gs_make_scaling(P3(floatp, floatp, gs_matrix *)),
	gs_make_rotation(P2(floatp, gs_matrix *));

/* Matrix arithmetic */
int	gs_matrix_multiply(P3(const gs_matrix *, const gs_matrix *, gs_matrix *)),
	gs_matrix_invert(P2(const gs_matrix *, gs_matrix *)),
	gs_matrix_rotate(P3(const gs_matrix *, floatp, gs_matrix *));

/* Coordinate transformation */
int	gs_point_transform(P4(floatp, floatp, const gs_matrix *, gs_point *)),
	gs_point_transform_inverse(P4(floatp, floatp, const gs_matrix *, gs_point *)),
	gs_distance_transform(P4(floatp, floatp, const gs_matrix *, gs_point *)),
	gs_distance_transform_inverse(P4(floatp, floatp, const gs_matrix *, gs_point *)),
	gs_bbox_transform_inverse(P3(gs_rect *, gs_matrix *, gs_rect *));

#endif					/* gsmatrix_INCLUDED */
