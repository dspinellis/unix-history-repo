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

/* zvmem.c */
/* "Virtual memory" operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "estack.h"			/* for checking in restore */
#include "dict.h"			/* ditto */
#include "dstack.h"
#include "save.h"
#include "state.h"
#include "store.h"
#include "gsmatrix.h"			/* for gsstate.h */
#include "gsstate.h"

/* Imported operators */
extern int zgsave(P1(os_ptr));
extern int zgrestore(P1(os_ptr));

/* Import the routine for setting the attributes of the identity matrix. */
extern void init_identity_matrix(P0());

/* 'Save' structure */
typedef struct vm_save_s vm_save;
struct vm_save_s {
	alloc_save *asave;		/* allocator save */
	int_gstate isave;		/* old interpreter state */
	gs_state *gsave;		/* old graphics state */
};

/* save */
int
zsave(register os_ptr op)
{	vm_save *vmsave = (vm_save *)alloc(1, sizeof(vm_save), "zsave");
	alloc_save *asave;
	int code;
	gs_state *prev, *prev2;
	if ( vmsave == 0 ) return_error(e_VMerror);
	asave = alloc_save_state();
	if ( asave == 0 )
	   {	alloc_free((char *)vmsave, 1, sizeof(vm_save), "zsave");
		return_error(e_VMerror);
	   }
	vmsave->asave = asave;
	/* Save the old interpreter state, */
	/* and cut the chains so we can't grestore past here. */
	vmsave->isave = istate;
	code = zgsave(op);
	if ( code < 0 ) return code;
	/* Swap the contents of the old and new states, */
	/* so the new state points to the newly allocated components. */
	prev = gs_state_swap_saved(igs, (gs_state *)0);
	prev2 = gs_state_swap_saved(prev, (gs_state *)0);
	gs_state_swap_saved(igs, prev2);
	gs_state_swap(igs, prev);
	vmsave->gsave = igs;
	igs = prev;
	istate.saved = 0;
	push(1);
	make_tv(op, t_save, psave, vmsave);
	init_identity_matrix();		/* update l_new attribute */
	return zgsave(op);
}

/* restore */
private int restore_check_stack(P3(ref *, ref *, alloc_save *));
private void restore_fix_stack(P3(ref *, ref *, alloc_save *));
int
zrestore(register os_ptr op)
{	vm_save *vmsave;
	alloc_save *asave;
	check_type(*op, t_save);
	vmsave = op->value.psave;
	if ( vmsave == 0 )		/* invalidated save */
		return_error(e_invalidrestore);
	asave = vmsave->asave;
	/* Check the contents of the stacks. */
	   {	int code;
		if ( (code = restore_check_stack(osbot, op, asave)) < 0 ||
		     (code = restore_check_stack(esbot, esp + 1, asave)) < 0 ||
		     (code = restore_check_stack(dstack, dsp + 1, asave)) < 0
		   )
			return code;
	   }
	if ( alloc_restore_state_check(asave) < 0 )
		return_error(e_invalidrestore);
	/* Invalidate any other copies of this save object on the stacks, */
	/* and reset l_new in all stack entries if the new restore level */
	/* is zero. */
	restore_fix_stack(osbot, op, asave);
	restore_fix_stack(esbot, esp + 1, asave);
	restore_fix_stack(dstack, dsp + 1, asave);
	/* Now it's safe to restore the state of memory. */
	alloc_restore_state(asave);
	/* Restore the interpreter and graphics state. */
	istate = vmsave->isave;
	igs = vmsave->gsave;
	alloc_free((char *)vmsave, 1, sizeof(vm_save), "zrestore");
	pop(1);
	init_identity_matrix();		/* update l_new attribute */
	return 0;
}
/* Check a stack to make sure all its elements are older than a save. */
private int
restore_check_stack(ref *bot, ref *top, alloc_save *asave)
{	ref *stkp;
	for ( stkp = bot; stkp < top; stkp++ )
	   {	char *ptr;
		switch ( r_type(stkp) )
		   {
		case t_array: ptr = (char *)stkp->value.refs; break;
		case t_condition: ptr = (char *)stkp->value.pcond; break;
		case t_dictionary: ptr = (char *)stkp->value.pdict; break;
		case t_fontID: ptr = (char *)stkp->value.pfont; break;
		case t_gstate: ptr = (char *)stkp->value.pgstate; break;
		/* case t_file: ****** WHAT? ****** */
		case t_lock: ptr = (char *)stkp->value.plock; break;
		case t_name:
			/* Names are special because of how they are allocated. */
			if ( alloc_name_is_since_save(stkp, asave) )
				return_error(e_invalidrestore);
			continue;
		case t_save: ptr = (char *)stkp->value.psave; break;
		case t_string: ptr = (char *)stkp->value.bytes; break;
		case t_mixedarray: case t_shortarray:
			ptr = (char *)stkp->value.packed; break;
		case t_color: ptr = (char *)stkp->value.pcolor; break;
		case t_device: ptr = (char *)stkp->value.pdevice; break;
		default: continue;
		   }
		if ( alloc_is_since_save(ptr, asave) )
			return_error(e_invalidrestore);
	   }

	return 0;			/* OK */
}
/* Fix up the contents of a stack by invalidating */
/* any save objects newer than the save being restored, */
/* and, if the new save level is zero, clearing the l_new */
/* bit in all the entries (since we can't tolerate values with */
/* l_new set if the save level is zero). */
private void
restore_fix_stack(ref *bot, ref *top, alloc_save *asave)
{	ref *stkp;
	for ( stkp = bot; stkp < top; stkp++ )
	   {	if ( r_type(stkp) == t_save &&
		     stkp->value.psave != 0 &&
		     (stkp->value.psave->asave == asave ||
		      alloc_is_since_save((char *)stkp->value.psave, asave))
		   )
			stkp->value.psave = 0;
		r_clear_attrs(stkp, l_new);	/* always do it, no harm */
	   }
}

/* vmstatus */
int
zvmstatus(register os_ptr op)
{	long used, total;
	alloc_status(&used, &total);
	push(3);
	make_int(op - 2, alloc_save_level());
	make_int(op - 1, used);
	make_int(op, total);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zvmem_op_defs[] = {
	{"1restore", zrestore},
	{"0save", zsave},
	{"0vmstatus", zvmstatus},
	op_def_end(0)
};
