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

/* zpath2.c */
/* Non-constructor path operators for GhostScript */
#include "ghost.h"
#include "oper.h"
#include "errors.h"
#include "alloc.h"
#include "estack.h"	/* for pathforall */
#include "gspath.h"
#include "state.h"
#include "store.h"

/* flattenpath */
int
zflattenpath(register os_ptr op)
{	return gs_flattenpath(igs);
}

/* reversepath */
int
zreversepath(register os_ptr op)
{	return gs_reversepath(igs);
}

/* strokepath */
int
zstrokepath(register os_ptr op)
{	return gs_strokepath(igs);
}

/* clippath */
int
zclippath(register os_ptr op)
{	return gs_clippath(igs);
}

/* pathbbox */
int
zpathbbox(register os_ptr op)
{	gs_rect box;
	int code = gs_pathbbox(igs, &box);
	if ( code < 0 ) return code;
	push(4);
	make_real(op - 3, box.p.x);
	make_real(op - 2, box.p.y);
	make_real(op - 1, box.q.x);
	make_real(op, box.q.y);
	return 0;
}

/* pathforall */
private int path_continue(P1(os_ptr));
private int i_path_continue;
int
zpathforall(register os_ptr op)
{	gs_path_enum *penum;
	check_proc(op[-3]);
	check_proc(op[-2]);
	check_proc(op[-1]);
	check_proc(*op);
	check_estack(8);
	if ( (penum = (gs_path_enum *)alloc(1, gs_path_enum_sizeof, "pathforall")) == 0 )
	  return e_VMerror;
	gs_path_enum_init(penum, igs);
	/* Push a mark, the four procedures, and a pseudo-integer */
	/* in which value.bytes points to the path enumerator, */
	/* and then call the continuation procedure. */
	mark_estack(es_for);	/* iterator */
	*++esp = op[-3];	/* moveto proc */
	*++esp = op[-2];	/* lineto proc */
	*++esp = op[-1];	/* curveto proc */
	*++esp = *op;		/* closepath proc */
	++esp;
	make_tv(esp, t_integer, bytes, (byte *)penum);
	pop(4);  op -= 4;
	return path_continue(op);
}
/* Continuation procedure for pathforall */
private int pf_push(P3(gs_point *, int, os_ptr));
private int
path_continue(register os_ptr op)
{	gs_path_enum *penum = (gs_path_enum *)esp->value.bytes;
	gs_point ppts[3];
	int code;
	code = gs_path_enum_next(penum, ppts);
	switch ( code )
	  {
	case 0:			/* all done */
	    { alloc_free((char *)penum, 1, gs_path_enum_sizeof, "pathforall");
	      esp -= 6;
	      return o_pop_estack;
	    }
	default:		/* error */
	    return code;
	case gs_pe_moveto:
	    esp[2] = esp[-4];			/* moveto proc */
	    code = pf_push(ppts, 1, op);
	    break;
	case gs_pe_lineto:
	    esp[2] = esp[-3];			/* lineto proc */
	    code = pf_push(ppts, 1, op);
	    break;
	case gs_pe_curveto:
	    esp[2] = esp[-2];			/* curveto proc */
	    code = pf_push(ppts, 3, op);
	    break;
	case gs_pe_closepath:
	    esp[2] = esp[-1];			/* closepath proc */
	    code = 0;
	    break;
	  }
	if ( code < 0 ) return code;	/* ostack overflow in pf_push */
	push_op_estack(path_continue, i_path_continue);
	++esp;		/* include pushed procedure */
	return o_push_estack;
}
/* Internal procedure to push one or more points */
private int
pf_push(gs_point *ppts, int n, os_ptr op)
{	while ( n-- )
	  { push(2);
	    make_real(op - 1, ppts->x);
	    make_real(op, ppts->y);
	    ppts++;
	  }
	return 0;
}

/* initclip */
int
zinitclip(register os_ptr op)
{	return gs_initclip(igs);
}

/* clip */
int
zclip(register os_ptr op)
{	return gs_clip(igs);
}

/* eoclip */
int
zeoclip(register os_ptr op)
{	return gs_eoclip(igs);
}

/* ------ Initialization procedure ------ */

op_def zpath2_op_defs[] = {
	{"0clip", zclip},
	{"0clippath", zclippath},
	{"0eoclip", zeoclip},
	{"0flattenpath", zflattenpath},
	{"0initclip", zinitclip},
	{"0pathbbox", zpathbbox},
	{"4pathforall", zpathforall},
	{"0reversepath", zreversepath},
	{"0strokepath", zstrokepath},
		/* Internal operators */
	{"0%path_continue", path_continue, &i_path_continue},
	op_def_end(0)
};
