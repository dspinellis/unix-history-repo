/* Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
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

/* zdps1.c */
/* Display PostScript graphics extensions */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gspath.h"
#include "gsstate.h"
#include "state.h"
#include "store.h"
#include "stream.h"
#include "bnum.h"

/* ------ Graphics states ------ */

/* Structure for saving a graphics state. */
typedef struct gstate_obj_s {
	int_gstate istate;
	gs_state *pgs;
} gstate_obj;

/* gstate */
int
zgstate(register os_ptr op)
{	gs_state *pnew = gs_gstate(igs);
	gstate_obj *pso =
		(gstate_obj *)alloc(1, sizeof(gstate_obj), "gstate");
	if ( pnew == 0 || pso == 0 ) return e_VMerror;
	pso->istate = istate;
	int_gstate_map_refs(&pso->istate, ref_mark_new);
	pso->pgs = pnew;
	push(1);
	make_tv(op, t_gstate, pgstate, pso);
	return 0;
}

/* currentgstate */
int
zcurrentgstate(register os_ptr op)
{	int code;
	int_gstate *pistate;
	check_type(*op, t_gstate);
	/****** DOESN'T GET FULLY UNDONE BY RESTORE ******/
	code = gs_currentgstate(op->value.pgstate->pgs, igs);
	if ( code < 0 ) return code;
	pistate = &op->value.pgstate->istate;
#define gsref_save(p) ref_save(p, "currentgstate")
	int_gstate_map_refs(pistate, gsref_save);
#undef gsref_save
	*pistate = istate;
	int_gstate_map_refs(pistate, ref_mark_new);
	return 0;
}

/* setgstate */
int
zsetgstate(register os_ptr op)
{	int code;
	check_type(*op, t_gstate);
	code = gs_setgstate(igs, op->value.pgstate->pgs);
	if ( code < 0 ) return code;
	istate = op->value.pgstate->istate;
	pop(1);
	return 0;
}

/* ------ Rectangles ------- */

/* We preallocate a short list for rectangles, because */
/* the rectangle operators usually will involve very few rectangles. */
#define max_local_rect 5
typedef struct local_rects_s {
	gs_rect *pr;
	uint count;
	gs_rect rl[max_local_rect];
} local_rects;

/* Forward references */
private int rect_get(P2(local_rects *, os_ptr));
private void rect_release(P1(local_rects *));

/* rectappend */
int
zrectappend(os_ptr op)
{	local_rects lr;
	int npop = rect_get(&lr, op);
	int code;
	if ( npop < 0 ) return npop;
	code = gs_rectappend(igs, lr.pr, lr.count);
	rect_release(&lr);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* rectclip */
int
zrectclip(os_ptr op)
{	local_rects lr;
	int npop = rect_get(&lr, op);
	int code;
	if ( npop < 0 ) return npop;
	code = gs_rectclip(igs, lr.pr, lr.count);
	rect_release(&lr);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* rectfill */
int
zrectfill(os_ptr op)
{	local_rects lr;
	int npop = rect_get(&lr, op);
	int code;
	if ( npop < 0 ) return npop;
	code = gs_rectfill(igs, lr.pr, lr.count);
	rect_release(&lr);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* rectstroke */
int
zrectstroke(os_ptr op)
{	gs_matrix mat;
	local_rects lr;
	int npop, code;
	if ( read_matrix(op, &mat) >= 0 )
	   {	/* Concatenate the matrix to the CTM just before */
		/* stroking the path. */
		npop = rect_get(&lr, op - 1);
		if ( npop < 0 ) return npop;
		code = gs_rectstroke(igs, lr.pr, lr.count, &mat);
		npop++;
	   }
	else
	   {	/* No matrix. */
		npop = rect_get(&lr, op);
		if ( npop < 0 ) return npop;
		code = gs_rectstroke(igs, lr.pr, lr.count, (gs_matrix *)0);
	   }
	rect_release(&lr);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* --- Internal routines --- */

/* Get rectangles from the stack. */
/* Return the number of elements to pop (>0) if OK, <0 if error. */
private int
rect_get(local_rects *plr, os_ptr op)
{	int code, npop;
	stream st;
	uint n, count;
	gs_rect *pr;
	switch ( r_type(op) )
	   {
	case t_array:
	case t_string:
		code = sread_num_array(&st, op);
		if ( code < 0 ) return code;
		count = scount_num_stream(&st);
		if ( count % 4 ) return e_typecheck;
		count /= 4, npop = 1;
		break;
	default:			/* better be 4 numbers */
		sread_string(&st, (byte *)(op - 3), sizeof(ref) * 4);
		st.num_format = num_array;
		count = 1, npop = 4;
		break;
	   }
	plr->count = count;
	if ( count <= max_local_rect )
		pr = plr->rl;
	else
	   {	pr = (gs_rect *)alloc(count, sizeof(gs_rect), "rect_get");
		if ( pr == 0 ) return e_VMerror;
	   }
	plr->pr = pr;
	for ( n = 0; n < count; n++, pr++ )
	   {	ref rnum;
		float rv[4];
		int i;
		for ( i = 0; i < 4; i++ )
		   {	switch ( code = sget_encoded_number(&st, &rnum) )
			   {
			case t_integer:
				rv[i] = rnum.value.intval;
				break;
			case t_real:
				rv[i] = rnum.value.realval;
				break;
			default:	/* code < 0 */
				return code;
			   }
		   }
		pr->q.x = (pr->p.x = rv[0]) + rv[2];
		pr->q.y = (pr->p.y = rv[1]) + rv[3];
	   }
	return npop;
}

/* Release the rectangle list if needed. */
private void
rect_release(local_rects *plr)
{	if ( plr->pr != plr->rl )
		alloc_free((char *)plr->pr, plr->count, sizeof(gs_rect),
			   "rect_release");
}

/* ------ Graphics state ------ */

/* currentstrokeadjust */
int
zcurrentstrokeadjust(register os_ptr op)
{	push(1);
	make_bool(op, gs_currentstrokeadjust(igs));
	return 0;
}

/* setbbox */
int
zsetbbox(register os_ptr op)
{	float box[4];
	int code = num_params(op, 4, box);
	if ( code < 0 ) return code;
	if ( (code = gs_setbbox(igs, box[0], box[1], box[2], box[3])) < 0 )
		return code;
	pop(4);
	return 0;
}

/* setstrokeadjust */
int
zsetstrokeadjust(register os_ptr op)
{	check_type(*op, t_boolean);
	gs_setstrokeadjust(igs, op->value.index);
	pop(1);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zdps1_op_defs[] = {
		/* Graphics state objects */
	{"0gstate", zgstate},
	{"1currentgstate", zcurrentgstate},
	{"1setgstate", zsetgstate},
		/* Rectangles */
	{"1rectappend", zrectappend},
	{"1rectclip", zrectclip},
	{"1rectfill", zrectfill},
	{"1rectstroke", zrectstroke},
		/* Graphics state components */
	{"0currentstrokeadjust", zcurrentstrokeadjust},
	{"4setbbox", zsetbbox},
	{"1setstrokeadjust", zsetstrokeadjust},
	op_def_end(0)
};
