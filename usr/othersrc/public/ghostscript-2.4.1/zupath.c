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

/* zupath.c */
/* Operators related to user paths */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "dict.h"
#include "dstack.h"
#include "iutil.h"
#include "state.h"
#include "store.h"
#include "stream.h"
#include "bnum.h"
#include "gsmatrix.h"
#include "gspath.h"
#include "gsstate.h"
#include "gscoord.h"
#include "gspaint.h"
#include "gxfixed.h"
#include "gxdevice.h"
#include "gxpath.h"

/* Forward references */
private int upath_append(P1(os_ptr));
private int upath_stroke(P1(os_ptr));

/* ------ Insideness testing ------ */

/* Forward references */
private int in_test(P2(os_ptr, int (*)(P1(gs_state *))));
private int in_path(P1(os_ptr));
private int in_path_result(P3(os_ptr, int, int));
private int in_utest(P2(os_ptr, int (*)(P1(gs_state *))));
private int in_upath(P1(os_ptr));
private int in_upath_result(P3(os_ptr, int, int));

/* We use invalidexit, which the painting procedures cannot generate, */
/* as an "error" to indicate that the hit detection device found a hit. */
#define e_hit e_invalidexit

/* ineofill */
int
zineofill(os_ptr op)
{	return in_test(op, gs_eofill);
}

/* infill */
int
zinfill(os_ptr op)
{	return in_test(op, gs_fill);
}

/* instroke */
int
zinstroke(os_ptr op)
{	return in_test(op, gs_stroke);
}

/* inueofill */
int
zinueofill(os_ptr op)
{	return in_utest(op, gs_eofill);
}

/* inufill */
int
zinufill(os_ptr op)
{	return in_utest(op, gs_fill);
}

/* inustroke */
int
zinustroke(os_ptr op)
{	/* This is different because of the optional matrix operand. */
	int code = gs_gsave(igs);
	int spop, npop;
	if ( code < 0 ) return code;
	if ( (spop = upath_stroke(op)) < 0 ||
	     (npop = in_path(op - spop)) < 0
	   )
	   {	gs_grestore(igs);
		return code;
	   }
	code = gs_stroke(igs);
	return in_upath_result(op, npop + spop, code);
}

/* ------ Internal routines ------ */

/* Define a minimal device for insideness testing. */
/* It returns e_hit whenever it is asked to actually paint any pixels. */
private dev_proc_fill_rectangle(hit_fill_rectangle);
private gx_device_procs hit_procs = {
	NULL,				/* open_device */
	NULL,				/* get_initial_matrix */
	NULL,				/* sync_output */
	NULL,				/* output_page */
	NULL,				/* close_device */
	gx_default_map_rgb_color,
	gx_default_map_color_rgb,
	hit_fill_rectangle,
	NULL,				/* tile_rectangle */
	NULL,				/* copy_mono */
	NULL,				/* copy_color */
	gx_default_draw_line
};
private gx_device hit_device =
{	sizeof(gx_device),
	&hit_procs,
	"hit detector",
	0, 0, 1, 1, no_margins, dci_black_and_white, 0	/* generic */
};
/* Test for a hit when filling a rectangle. */
private int
hit_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	return (w > 0 && h > 0 ? e_hit : 0);
}

/* Do the work of the non-user-path insideness operators. */
private int
in_test(os_ptr op, int (*paintproc)(P1(gs_state *)))
{	int npop = in_path(op);
	int code;
	if ( npop < 0 ) return npop;
	code = (*paintproc)(igs);
	return in_path_result(op, npop, code);
}

/* Set up a clipping path and device for insideness testing. */
private int
in_path(os_ptr op)
{	int code = gs_gsave(igs);
	int npop;
	float uxy[2];
	if ( code < 0 ) return code;
	code = num_params(op, 2, uxy);
	if ( code >= 0 )
	   {	/* Aperture is a single pixel. */
		gs_point dxy;
		gs_fixed_rect fr;
		gs_transform(igs, uxy[0], uxy[1], &dxy);
		fr.p.x = fixed_truncated(float2fixed(dxy.x));
		fr.p.y = fixed_truncated(float2fixed(dxy.y));
		fr.q.x = fr.p.x + int2fixed(1);
		fr.q.y = fr.p.y + int2fixed(1);
		code = gx_clip_to_rectangle(igs, &fr);
		npop = 2;
	   }
	else
	   {	/* Aperture is a user path. */
		if ( (code = upath_append(op)) >= 0 )
			code = gx_clip_to_path(igs);
		npop = 1;
	   }
	if ( code < 0 )
	   {	gs_grestore(igs);
		return code;
	   }
	/* Install the hit detection device. */
	gs_setgray(igs, 0.0);
	gx_set_device_only(igs, &hit_device);
	return npop;
}

/* Finish an insideness test. */
private int
in_path_result(os_ptr op, int npop, int code)
{	int result;
	gs_grestore(igs);
	switch ( code )
	   {
	case e_hit:			/* found a hit */
		result = 1;
		break;
	case 0:				/* completed painting without a hit */
		result = 0;
		break;
	default:			/* error */
		return code;
	   }
	npop--;
	pop(npop); op -= npop;
	make_bool(op, result);
	return 0;
		
}

/* Do the work of the user-path insideness operators. */
private int
in_utest(os_ptr op, int (*paintproc)(P1(gs_state *)))
{	int npop = in_upath(op);
	int code;
	if ( npop < 0 ) return npop;
	code = (*paintproc)(igs);
	return in_upath_result(op, npop, code);
}

/* Set up a clipping path and device for insideness testing */
/* with a user path. */
private int
in_upath(os_ptr op)
{	int code = gs_gsave(igs);
	int npop;
	if ( code < 0 ) return code;
	if ( (code = upath_append(op)) < 0 ||
	     (npop = in_path(op - 1)) < 0
	   )
	   {	gs_grestore(igs);
		return code;
	   }
	return npop + 1;
}

/* Finish an insideness test with a user path. */
private int
in_upath_result(os_ptr op, int npop, int code)
{	gs_grestore(igs);		/* undo the extra gsave */
	return in_path_result(op, npop, code);
}

/* ------ User paths ------ */

/* User path operator codes */
typedef enum {
  upath_setbbox = 0,
  upath_moveto = 1,
  upath_rmoveto = 2,
  upath_lineto = 3,
  upath_rlineto = 4,
  upath_curveto = 5,
  upath_rcurveto = 6,
  upath_arc = 7,
  upath_arcn = 8,
  upath_arct = 9,
  upath_closepath = 10,
  upath_ucache = 11
} upath_op;
#define upath_op_max 11
#define upath_repeat 32
static byte up_nargs[upath_op_max + 1] =
   { 4, 2, 2, 2, 2, 6, 6, 5, 5, 5, 0, 0 };
#define zp(proc) extern int proc(P1(os_ptr))
	zp(zsetbbox); zp(zmoveto); zp(zrmoveto);
	zp(zlineto); zp(zrlineto); zp(zcurveto); zp(zrcurveto);
	zp(zarc); zp(zarcn); zp(zarct); zp(zclosepath); zp(zucache);
#undef zp
static op_proc_p up_ops[upath_op_max + 1] =
   {	zsetbbox, zmoveto, zrmoveto, zlineto, zrlineto,
	zcurveto, zrcurveto, zarc, zarcn, zarct,
	zclosepath, zucache
   };

/* Imported procedures */
extern int array_get(P3(ref *, long, ref *));

/* ucache */
int
zucache(os_ptr op)
{	/* A no-op for now. */
	return 0;
}

/* uappend */
int
zuappend(register os_ptr op)
{	int code = gs_gsave(igs);
	if ( code < 0 ) return code;
	if ( (code = upath_append(op)) >= 0 )
		code = gs_upmergepath(igs);
	gs_grestore(igs);
	if ( code < 0 ) return code;
	pop(1);
	return 0;
}

/* ueofill */
int
zueofill(register os_ptr op)
{	int code = gs_gsave(igs);
	if ( code < 0 ) return code;
	if ( (code = upath_append(op)) >= 0 )
		code = gs_eofill(igs);
	gs_grestore(igs);
	if ( code < 0 ) return code;
	pop(1);
	return 0;
}

/* ufill */
int
zufill(register os_ptr op)
{	int code = gs_gsave(igs);
	if ( code < 0 ) return code;
	if ( (code = upath_append(op)) >= 0 )
		code = gs_fill(igs);
	gs_grestore(igs);
	if ( code < 0 ) return code;
	pop(1);
	return 0;
}

/* ustroke */
int
zustroke(register os_ptr op)
{	int code = gs_gsave(igs);
	int npop;
	if ( code < 0 ) return code;
	if ( (code = npop = upath_stroke(op)) >= 0 )
		code = gs_stroke(igs);
	gs_grestore(igs);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* ustrokepath */
int
zustrokepath(register os_ptr op)
{	int code = gs_gsave(igs);
	int npop;
	if ( code < 0 ) return code;
	if ( (code = npop = upath_stroke(op)) < 0 ||
	     (code = gs_strokepath(igs)) < 0 ||
	     (code = gs_upmergepath(igs)) < 0
	   )
		;
	gs_grestore(igs);
	if ( code < 0 ) return code;
	pop(npop);
	return 0;
}

/* --- Internal routines --- */

/* Append a user path to the current path. */
private int
upath_append(os_ptr op)
{	check_read(*op);
	gs_newpath(igs);
	/****** ROUND tx AND ty ******/
	if ( r_has_type(op, t_array) && r_size(op) == 2 &&
	     r_has_type(op->value.refs + 1, t_string)
	   )
	   {	/* 1st element is operators, 2nd is operands */
		stream st;
		int code;
		int repcount = 1;
		byte *opp;
		uint ocount;
		code = sread_num_array(&st, op->value.refs);
		if ( code < 0 ) return code;
		opp = op->value.refs[1].value.bytes;
		ocount = r_size(&op->value.refs[1]);
		while ( ocount-- )
		   {	byte opx = *opp++;
			if ( opx > 32 )
				repcount = opx - 32;
			else if ( opx > upath_op_max )
				return e_typecheck;
			else		/* operator */
			   {	do
				   {	byte opargs = up_nargs[opx];
					while ( opargs-- )
					   {	push(1);
						code = sget_encoded_number(&st, op);
						switch ( code )
						   {
						case t_integer:
							r_set_type_attrs(op, t_integer, 0);
							break;
						case t_real:
							r_set_type_attrs(op, t_real, 0);
							break;
						default:
							return e_typecheck;
						   }
					   }
					code = (*up_ops[opx])(op);
					if ( code < 0 ) return code;
					op = osp;	/* resync */
				   }
				while ( --repcount );
				repcount = 1;
			   }
		   }
	   }
	else
	   {	/* Ordinary executable array */
		ref *arp = op;
		ref *defp;
		ref rup;
		uint ocount = r_size(op);
		long index = 0;
		int argcount = 0;
		int (*oproc)(P1(os_ptr));
		int opx, code;
		for ( ; index < ocount; index++ )
		 switch ( array_get(arp, index, &rup), r_type(&rup) )
		   {
		case t_integer:
		case t_real:
			argcount++;
			push(1);
			*op = rup;
			break;
		case t_name:
			if ( !r_has_attr(&rup, a_executable) )
				return e_typecheck;
			if ( dict_find(&systemdict, &rup, &defp) <= 0 )
				return e_undefined;
			if ( r_btype(defp) != t_operator )
				return e_typecheck;
			goto xop;
		case t_operator:
			defp = &rup;
xop:			if ( !r_has_attr(defp, a_executable) )
				return e_typecheck;
			oproc = real_opproc(defp);
			for ( opx = 0; opx <= upath_op_max; opx++ )
				if ( oproc == up_ops[opx] ) break;
			if ( opx > upath_op_max || argcount != up_nargs[opx] )
				return e_typecheck;
			code = (*oproc)(op);
			if ( code < 0 ) return code;
			op = osp;	/* resync ostack pointer */
			argcount = 0;
			break;
		default:
			return e_typecheck;
		   }
		if ( argcount ) return e_typecheck;	/* leftover args */
	   }
	return 0;
}

/* Append a user path to the current path, and then apply */
/* a transformation if one is supplied. */
private int
upath_stroke(register os_ptr op)
{	int code, npop;
	gs_matrix mat;
	if ( (code = read_matrix(op, &mat)) >= 0 )
	   {	if ( (code = upath_append(op - 1)) >= 0 )
			code = gs_concat(igs, &mat);
		npop = 2;
	   }
	else
	   {	code = upath_append(op);
		npop = 1;
	   }
	return (code < 0 ? code : npop);
}

/* ------ Initialization procedure ------ */

op_def zupath_op_defs[] = {
		/* Insideness testing */
	{"1ineofill", zineofill},
	{"1infill", zinfill},
	{"1instroke", zinstroke},
	{"2inueofill", zinueofill},
	{"2inufill", zinufill},
	{"2inustroke", zinustroke},
		/* User paths */
	{"1uappend", zuappend},
	{"1ueofill", zueofill},
	{"1ufill", zufill},
	{"1ustroke", zustroke},
	{"1ustrokepath", zustrokepath},
	{"0ucache", zucache},
	op_def_end(0)
};
