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

/* gscie.h */
/* Interface to Ghostscript CIE color algorithms */

/* ------ Common definitions ------ */

/* A 3-element vector. */
typedef struct gs_vector3_s {
	float u, v, w;
} gs_vector3;

/* A 3x3 matrix, stored in column order. */
typedef struct gs_matrix3_s {
	gs_vector3 cu, cv, cw;
} gs_matrix3;

/* A 3-element vector of ranges. */
typedef struct gs_range_s {
	float rmin, rmax;
} gs_range;
typedef struct gs_range3_s {
	gs_range u, v, w;
} gs_range3;

/* A 3-element procedure vector. */
typedef float (*gs_float_proc)(P1(floatp));
typedef struct gs_float_proc3_s {
	gs_float_proc u, v, w;
} gs_float_proc3;

/* CIE white and black points. */
typedef struct gs_cie_wb_s {
	gs_vector3 WhitePoint;
	gs_vector3 BlackPoint;
} gs_cie_wb;

/* ------ Color space dictionaries ------ */

/* A CIEBasedABC dictionary. */
typedef struct gs_cie_abc_s {
	gs_range3 RangeABC;
	gs_float_proc3 DecodeABC;
	gs_matrix3 MatrixABC;
	gs_range3 RangeLMN;
	gs_float_proc3 DecodeLMN;
	gs_matrix3 MatrixLMN;
	gs_cie_wb points;
} gs_cie_abc;

/* A CIEBasedA dictionary. */
typedef struct gs_cie_a_s {
	gs_range RangeA;
	gs_float_proc DecodeA;
	gs_vector3 MatrixA;
	gs_range3 RangeLMN;
	gs_float_proc3 DecodeLMN;
	gs_matrix3 MatrixLMN;
	gs_cie_wb points;
} gs_cie_a;

/* Default values for components */
extern gs_range3 Range3_default;
extern gs_float_proc3 Decode3_default;
extern gs_matrix3 Matrix3_default;
extern gs_range RangeA_default;
extern gs_float_proc DecodeA_default;
extern gs_vector3 MatrixA_default;
extern gs_vector3 BlackPoint_default;

/* ------ Rendering dictionaries ------ */

typedef struct gs_cie_wbsd_s {
	struct { gs_vector3 xyz, pqr; } ws, bs, wd, bd;
} gs_cie_wbsd;
typedef float (*gs_cie_transform_proc)(P2(gs_cie_wbsd *, floatp));
typedef struct gs_cie_render_table_s {
	int NA, NB, NC;			/* >1 */
	byte **table;			/* [NA][m * NB * NC] */
					/* 0 means no table */
	int m;				/* 3 or 4 */
	gs_float_proc T[4];		/* [m] */
} gs_cie_render_table;
/* The main dictionary */
typedef struct gs_cie_render_s {
	gs_matrix3 MatrixLMN;
	gs_float_proc3 EncodeLMN;
	gs_range3 RangeLMN;
	gs_matrix3 MatrixABC;
	gs_float_proc3 EncodeABC;
	gs_range3 RangeABC;
	gs_cie_wb points;
	gs_matrix3 MatrixPQR;
	gs_range3 RangePQR;
	struct { gs_cie_transform_proc u, v, w; } TransformPQR;
	gs_cie_render_table RenderTable;
		/* Following are computed when table is initialized. */
	gs_matrix3 MatrixPQR_inverse;
	gs_vector3 wdpqr, bdpqr;
} gs_cie_render;

/* ------ Procedures ------ */

/*
 * The decoding and rendering algorithms involve user-defined procedures.
 * Since the interpreter handles these specially, the algorithms *return*
 * to the caller between steps, rather than using a call-back.
 * The scenario for decoding:
 *	gs_cie_abc_decode1(abc, tabc)
 *	... DecodeABC(tabc) ...
 *	gs_cie_abc_decode2(tabc, tlmn)
 *	... DecodeLMN(tlmn) ...
 *	gs_cie_abc_decode3(tlmn, xyz)
 * or:
 *	gs_cie_a_decode1(a, ta)
 *	... DecodeA(ta) ...
 *	gs_cie_a_decode2(ta, tlmn)
 *	... DecodeLMN(tlmn) ...
 *	gs_cie_a_decode3(tlmn, xyz) [same as abc_decode3]
 * The scenario for rendering:
 *	gs_cie_render_colors1(xyz, wbsd, tpqr)
 *	... TransformPQR(wbsd, tpqr) ...
 *	gs_cie_render_colors2(tpqr, tlmn)
 *	... EncodeLMN(tlmn) ...
 *	gs_cie_render_colors3(tlmn, tabc)
 *	... EncodeABC(tabc) ...
 *	gs_cie_render_colors4(tabc, colors)
 */

/* Decode ABC values to XYZ. */
extern int gs_cie_abc_decode1(P3(gs_vector3 *pabc, gs_vector3 *ptabc, gs_cie_abc *pcie));
extern int gs_cie_abc_decode2(P3(gs_vector3 *ptabc, gs_vector3 *ptlmn, gs_cie_abc *pcie));
extern int gs_cie_abc_decode3(P3(gs_vector3 *ptlmn, gs_vector3 *pxyz, gs_cie_abc *pcie));

/* Decode A value to XYZ. */
extern int gs_cie_a_decode1(P3(floatp va, float *pta, gs_cie_a *pcie));
extern int gs_cie_a_decode2(P3(floatp ta, gs_vector3 *ptlmn, gs_cie_a *pcie));
#define gs_cie_a_decode3(ptlmn, pxyz, pcie) gs_cie_abc_decode3(ptlmn, pxyz, pcie)

/* Compute the cached values in a CIE rendering table. */
extern int gs_cie_render_init(P1(gs_cie_render *pcie));

/* Render CIE colors */
extern int gs_cie_render_colors1(P5(gs_vector3 *pxyz, gs_cie_wbsd *pwbsd, gs_vector3 *ppqr, gs_cie_wb *points, gs_cie_render *pcie));
extern int gs_cir_render_colors2(P3(gs_vector3 *ptpqr, gs_vector3 *ptlmn, gs_cie_render *pcie));
extern int gs_cir_render_colors3(P3(gs_vector3 *ptlmn, gs_vector3 *ptabc, gs_cie_render *pcie));
extern int gs_cie_render_colors4(P3(gs_vector3 *ptabc, float *colors, gs_cie_render *pcie));
