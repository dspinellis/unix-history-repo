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

/* zcolor.c */
/* Color operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "estack.h"
#include "iutil.h"
#include "store.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gxdevice.h"			/* for gzcolor.h */
#include "gzcolor.h"
#include "state.h"

/* Import the 'for' operator */
extern int
  zfor(P1(os_ptr)),
  i_zfor;

/* Define the number of estack slots needed for remap_one. */
#define remap_one_estack 3

/* Forward declarations */
private void tri_put(P2(os_ptr, float [3]));
private int remap_one(P3(ref *, os_ptr, gx_transfer_map *));
private int
  remap_one_finish(P1(os_ptr)),
  i_remap_one_finish,
  remap_color(P1(os_ptr)),
  i_remap_color;

/* Transfer functions for the library layer. */
/* These just return what's already in the map. */
#define tr_map(pgs,v,c)\
  (pgs->transfer->c.values[(int)((v) * transfer_map_size + 0.5)] / max_color_param_float)
private float
transfer_red(gs_state *pgs, floatp value)
{	return tr_map(pgs, value, red);
}
private float
transfer_green(gs_state *pgs, floatp value)
{	return tr_map(pgs, value, green);
}
private float
transfer_blue(gs_state *pgs, floatp value)
{	return tr_map(pgs, value, blue);
}
private float
transfer_gray(gs_state *pgs, floatp value)
{	return tr_map(pgs, value, gray);
}
#undef tr_map

/* currentcolortransfer */
int
zcurrentcolortransfer(register os_ptr op)
{	push(4);
	op[-3] = istate.transfer_procs.red;
	op[-2] = istate.transfer_procs.green;
	op[-1] = istate.transfer_procs.blue;
	*op = istate.transfer_procs.gray;
	return 0;
}

/* currentgray */
int
zcurrentgray(register os_ptr op)
{	push(1);
	make_real(op, gs_currentgray(igs));
	return 0;
}

/* currenthsbcolor */
int
zcurrenthsbcolor(register os_ptr op)
{	float par[3];
	gs_currenthsbcolor(igs, par);
	push(3);
	tri_put(op, par);
	return 0;
}

/* currentrgbcolor */
int
zcurrentrgbcolor(register os_ptr op)
{	float par[3];
	gs_currentrgbcolor(igs, par);
	push(3);
	tri_put(op, par);
	return 0;
}

/* currenttransfer */
int
zcurrenttransfer(register os_ptr op)
{	push(1);
	*op = istate.transfer_procs.gray;
	return 0;
}

/* setcolortransfer */
int
zsetcolortransfer(register os_ptr op)
{	int code;
	check_proc(op[-3]);
	check_proc(op[-2]);
	check_proc(op[-1]);
	check_proc(*op);
	istate.transfer_procs.red = op[-3];
	istate.transfer_procs.green = op[-2];
	istate.transfer_procs.blue = op[-1];
	istate.transfer_procs.gray = *op;
	pop(4);  op -= 4;
	check_estack(1 + remap_one_estack * 4);
	push_op_estack(remap_color, i_remap_color);
	(code = gs_setcolortransfer_remap(igs, transfer_red, transfer_green, transfer_blue, transfer_gray, 0)) < 0 ||
		/* Use osp rather than op here, because remap_one pushes. */
	(code = remap_one(&istate.transfer_procs.red, osp, &igs->transfer->red)) < 0 ||
	(code = remap_one(&istate.transfer_procs.green, osp, &igs->transfer->green)) < 0 ||
	(code = remap_one(&istate.transfer_procs.blue, osp, &igs->transfer->blue)) < 0 ||
	(code = remap_one(&istate.transfer_procs.gray, osp, &igs->transfer->gray)) < 0;
	return code;
}

/* setgray */
int
zsetgray(register os_ptr op)
{	float gray;
	int code;
	if ( (code = real_param(op, &gray)) < 0 ||
	     (code = gs_setgray(igs, gray)) < 0
	   )
		return code;
	pop(1);
	return 0;
}

/* sethsbcolor */
int
zsethsbcolor(register os_ptr op)
{	float par[3];
	int code;
	if (	(code = num_params(op, 3, par)) < 0 ||
		(code = gs_sethsbcolor(igs, par[0], par[1], par[2])) < 0
	   )
		return code;
	pop(3);
	return 0;
}

/* setrgbcolor */
int
zsetrgbcolor(register os_ptr op)
{	float par[3];
	int code;
	if (	(code = num_params(op, 3, par)) < 0 ||
		(code = gs_setrgbcolor(igs, par[0], par[1], par[2])) < 0
	   )
		return code;
	pop(3);
	return 0;
}

/* settransfer */
int
zsettransfer(register os_ptr op)
{	int code;
	check_proc(*op);
	istate.transfer_procs.red = istate.transfer_procs.green =
	  istate.transfer_procs.blue = istate.transfer_procs.gray = *op;
	pop(1);  op--;
	check_estack(1 + remap_one_estack);
	code = gs_settransfer_remap(igs, transfer_gray, 0);
	if ( code < 0 ) return code;
	push_op_estack(remap_color, i_remap_color);
	return remap_one(&istate.transfer_procs.gray, op, &igs->transfer->gray);
}

/* ------ Internal routines ------ */

/* Put 3 reals on the operand stack. */
private void
tri_put(register os_ptr op, float pf3[3])
{	make_real(op - 2, pf3[0]);
	make_real(op - 1, pf3[1]);
	make_real(op, pf3[2]);
}

/* Prepare to remap one color component. */
/* Use the 'for' operator to gather the values. */
/* The caller must have done the necessary check_estack. */
private int
remap_one(ref *pproc, register os_ptr op, gx_transfer_map *pmap)
{	push(4);
	make_real(op - 3, 1.0);
	make_real(op - 2, -0.999999 / (transfer_map_size - 1));
	make_real(op - 1, 0.0);
	*op = *pproc;
	++esp;
	make_tasv(esp, t_string, 0, sizeof(*pmap), bytes, (byte *)pmap);
	push_op_estack(remap_one_finish, i_remap_one_finish);
	push_op_estack(zfor, i_zfor);
	return o_push_estack;
}

/* Store the result of remapping a component. */
private int
remap_one_finish(os_ptr op)
{	int i;
	gx_transfer_map *pmap = (gx_transfer_map *)esp->value.bytes;
	for ( i = 0; i < transfer_map_size; i++ )
	   {	float v;
		int code = real_param(op - i, &v);
		if ( code < 0 ) return 0;
		pmap->values[i] = gx_color_unit_param(v);
	   }
	pop(transfer_map_size);
	esp--;				/* pop pointer to transfer map */
	return o_pop_estack;
}

/* Finally, remap the current color. */
private int
remap_color(os_ptr op)
{	/* If we just did settransfer, as opposed to setcolortransfer, */
	/* we need to copy the gray map to the r/g/b maps. */
	gx_transfer *ptran = igs->transfer;
	if ( ptran->red.proc == ptran->gray.proc &&
	     ptran->green.proc == ptran->gray.proc &&
	     ptran->blue.proc == ptran->gray.proc
	   )
	   {	ptran->red = ptran->gray;
		ptran->green = ptran->gray;
		ptran->blue = ptran->gray;
	   }
	return gx_remap_color(igs);
}

/* ------ Initialization procedure ------ */

op_def zcolor_op_defs[] = {
	{"0currentcolortransfer", zcurrentcolortransfer},
	{"0currentgray", zcurrentgray},
	{"0currenthsbcolor", zcurrenthsbcolor},
	{"0currentrgbcolor", zcurrentrgbcolor},
	{"0currenttransfer", zcurrenttransfer},
	{"4setcolortransfer", zsetcolortransfer},
	{"1setgray", zsetgray},
	{"3sethsbcolor", zsethsbcolor},
	{"3setrgbcolor", zsetrgbcolor},
	{"1settransfer", zsettransfer},
		/* Internal operators */
	{"1%remap_one_finish", remap_one_finish, &i_remap_one_finish},
	{"0%remap_color", remap_color, &i_remap_color},
	op_def_end(0)
};
