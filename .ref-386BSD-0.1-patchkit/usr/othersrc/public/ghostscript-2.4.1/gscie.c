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

/* gscie.c */
/* CIE color algorithms for Ghostscript */
#include "std.h"
#include "gscie.h"

/* Default values for components. */
private float
fp_identity(floatp value)
{	return (float)value;
}
gs_range3 Range3_default = { {0,1}, {0,1}, {0,1} };
gs_float_proc3 Decode3_default = { fp_identity, fp_identity, fp_identity };
gs_matrix3 Matrix3_default = { {1,0,0}, {0,1,0}, {0,0,1} };
gs_range RangeA_default = {0,1};
gs_float_proc DecodeA_default = fp_identity;
gs_vector3 MatrixA_default = { 1, 1, 1 };
gs_vector3 BlackPoint_default = { 0, 0, 0 };

/* Apply procedures to a vector. */
private void
cie_apply3(gs_vector3 *in, gs_float_proc3 *procs, gs_vector3 *out)
{	out->u = (*procs->u)(in->u);
	out->v = (*procs->v)(in->v);
	out->w = (*procs->w)(in->w);
}

/* Multiply a vector by a matrix. */
private void
cie_mult3(gs_vector3 *in, register gs_matrix3 *mat, gs_vector3 *out)
{	float u = in->u, v = in->v, w = in->w;
	out->u = (u * mat->cu.u) + (v * mat->cu.v) + (w * mat->cu.w);
	out->v = (u * mat->cv.u) + (v * mat->cv.v) + (w * mat->cv.w);
	out->w = (u * mat->cw.u) + (v * mat->cw.v) + (w * mat->cw.w);
}

/* Invert a matrix. */
private void
cie_invert3(register gs_matrix3 *in, register gs_matrix3 *out)
{	/* This is a brute force algorithm; maybe there are better. */
	/* We label the array elements */
	/*   [ A B C ]   */
	/*   [ D E F ]   */
	/*   [ G H I ]   */
#define A cu.u
#define B cv.u
#define C cw.u
#define D cu.v
#define E cv.v
#define F cw.v
#define G cu.w
#define H cv.w
#define I cw.w
	double AE = in->A * in->E, AF = in->A * in->F,
		AH = in->A * in->H, AI = in->A * in->I;
	double BD = in->B * in->D, BF = in->B * in->F,
		BG = in->B * in->G, BI = in->B * in->I;
	double CD = in->C * in->D, CE = in->C * in->E,
		CG = in->C * in->G, CH = in->C * in->H;
	double DH = in->D * in->H, DI = in->D * in->I;
	double EG = in->E * in->G, EI = in->E * in->I;
	double FG = in->F * in->G, FH = in->F * in->H;
	double coA = EI - FH, coB = FG - DI, coC = DH - EG;
	double det = in->A * coA + in->B * coB + in->C * coC;
	out->A = coA / det;
	out->D = coB / det;
	out->G = coC / det;
	out->B = (CH - BI) / det;
	out->E = (AI - CG) / det;
	out->H = (BG - AH) / det;
	out->C = (BF - CE) / det;
	out->F = (CD - AF) / det;
	out->I = (AE - BD) / det;
#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef G
#undef H
#undef I
}

/* Force values within bounds. */
#define restrict(v, r)\
  ((v) < (r).rmin ? (r).rmin : (v) > (r).rmax ? (r).rmax : (v))
private void
cie_restrict3(gs_vector3 *in, gs_range3 *range, gs_vector3 *out)
{	float temp;
	temp = in->u; out->u = restrict(temp, range->u);
	temp = in->v; out->v = restrict(temp, range->v);
	temp = in->w; out->w = restrict(temp, range->w);
}

/* Decode ABC values to XYZ. */
int
gs_cie_abc_decode1(gs_vector3 *pabc, gs_vector3 *ptabc, gs_cie_abc *pcie)
{	cie_restrict3(pabc, &pcie->RangeABC, ptabc);
	return 0;
}
/*
 * Client:
	cie_apply3(ptabc, &pcie->DecodeABC, ptabc);
 */
int
gs_cie_abc_decode2(gs_vector3 *ptabc, gs_vector3 *ptlmn, gs_cie_abc *pcie)
{	cie_mult3(ptabc, &pcie->MatrixABC, ptlmn);
	cie_restrict3(ptlmn, &pcie->RangeLMN, ptlmn);
	return 0;
}
/*
 * Client:
	cie_apply3(ptlmn, &pcie->DecodeLMN, ptlmn);
 */
int
gs_cie_abc_decode3(gs_vector3 *ptlmn, gs_vector3 *pxyz, gs_cie_abc *pcie)
{	cie_mult3(ptlmn, &pcie->MatrixLMN, pxyz);
	return 0;
}

/* Decode an A value to XYZ. */
int
gs_cie_a_decode1(floatp va, float *pta, gs_cie_a *pcie)
{	*pta = restrict(va, pcie->RangeA);
	return 0;
}
/*
 * Client:
	ta = (*pcie->DecodeA)(*pta);
 */
int
gs_cie_a_decode2(floatp ta, gs_vector3 *ptlmn, gs_cie_a *pcie)
{	gs_vector3 lmn;
	lmn.u = ta * pcie->MatrixA.u;
	lmn.v = ta * pcie->MatrixA.v;
	lmn.w = ta * pcie->MatrixA.w;
	cie_restrict3(&lmn, &pcie->RangeLMN, ptlmn);
	return 0;
}
/*
 * Client:
	cie_apply3(ptlmn, &pcie->DecodeLMN, ptlmn);
 */
/* gs_cie_a_decode3 is the same as gs_cie_abc_decode3. */

/* Initialize the computed fields of a CIE color rendering structure. */
int
gs_cie_render_init(gs_cie_render *pcie)
{	cie_invert3(&pcie->MatrixPQR, &pcie->MatrixPQR_inverse);
	cie_mult3(&pcie->points.WhitePoint, &pcie->MatrixPQR, &pcie->wdpqr);
	cie_mult3(&pcie->points.BlackPoint, &pcie->MatrixPQR, &pcie->bdpqr);
	return 0;
}

/* Render CIE colors */
int
gs_cie_render_colors1(gs_vector3 *pxyz, gs_cie_wbsd *pwbsd, gs_vector3 *ptpqr, gs_cie_wb *points, gs_cie_render *pcie)
{	cie_mult3(pxyz, &pcie->MatrixPQR, ptpqr);
	pwbsd->ws.xyz = points->WhitePoint;
	cie_mult3(&pwbsd->ws.xyz, &pcie->MatrixPQR, &pwbsd->ws.pqr);
	pwbsd->bs.xyz = points->BlackPoint;
	cie_mult3(&pwbsd->bs.xyz, &pcie->MatrixPQR, &pwbsd->bs.pqr);
	pwbsd->wd.xyz = pcie->points.WhitePoint;
	pwbsd->wd.pqr = pcie->wdpqr;
	pwbsd->bd.xyz = pcie->points.BlackPoint;
	pwbsd->bd.pqr = pcie->bdpqr;
	return 0;
}
/*
 * Client:
	ptpqr->u = (*pcie->TransformPQR.u)(pwbsd, ptpqr->u);
	ptpqr->v = (*pcie->TransformPQR.v)(pwbsd, ptpqr->v);
	ptpqr->w = (*pcie->TransformPQR.w)(pwbsd, ptpqr->w);
 */
int
gs_cie_render_colors2(gs_vector3 *ptpqr, gs_vector3 *ptlmn, gs_cie_render *pcie)
{	gs_vector3 xyzd;
	cie_mult3(ptpqr, &pcie->MatrixPQR_inverse, &xyzd);
	cie_mult3(&xyzd, &pcie->MatrixLMN, ptlmn);
	return 0;
}
/*
/* Client:
	cie_apply3(ptlmn, &pcie->EncodeLMN, ptlmn);
 */
int
gs_cir_render_colors3(gs_vector3 *ptlmn, gs_vector3 *ptabc, gs_cie_render *pcie)
{	gs_vector3 lmn;
	cie_restrict3(ptlmn, &pcie->RangeLMN, &lmn);
	cie_mult3(&lmn, &pcie->MatrixABC, ptabc);
	return 0;
}
/*
 * Client:
	cie_apply3(ptabc, &pcie->EncodeABC, ptabc);
 */
int
gs_cie_render_colors4(gs_vector3 *ptabc, float *colors, gs_cie_render *pcie)
{	gs_vector3 abc;
	cie_restrict3(ptabc, &pcie->RangeABC, &abc);
	if ( pcie->RenderTable.table == 0 )
	   {	/* No further transformation */
		colors[0] = abc.u;
		colors[1] = abc.v;
		colors[2] = abc.w;
	   }
	else
	   {	/* Use the RenderTable. */
		int m = pcie->RenderTable.m;
#define ri(s,n)\
  (int)((abc.s - pcie->RangeABC.s.rmin) * (pcie->RenderTable.n - 1) /\
	(pcie->RangeABC.s.rmax - pcie->RangeABC.s.rmin) + 0.5)
		int ia = ri(u, NA);
		int ib = ri(v, NB);
		int ic = ri(w, NC);
		int j;
		byte *pdc = pcie->RenderTable.table[ia] +
			m * (ib * pcie->RenderTable.NC + ic);
		for ( j = 0; j < m; j++ )
			colors[j] = (*pcie->RenderTable.T[j])(pdc[j] / 255.0);
	   }
	return 0;
}
