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

/* iinit.c */
/* Initialize internally known objects for Ghostscript interpreter */
#include "string_.h"
#include "ghost.h"
#include "alloc.h"
#include "dict.h"
#include "dstack.h"
#define INCLUDE_ERROR_NAMES		/* see errors.h */
#include "errors.h"
#include "name.h"
#include "oper.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"

/* Implementation parameters. */
/* The size of systemdict can be set in the makefile. */
#ifndef SYSTEMDICT_SIZE
#  define SYSTEMDICT_SIZE 449		/* a nice prime (?) number */
#endif
#define systemdict_size SYSTEMDICT_SIZE
#define op_array_table_size 100		/* arbitrary */

/* Standard dictionaries */
ref name_errordict;
/* Error names */
ref name_ErrorNames;

/* The operator tables */
op_def_ptr *op_def_table;
uint op_def_count;
ref op_array_table;	/* t_array, definitions of `operator' procedures */
ushort *op_array_nx_table;		/* name indices for same */
uint op_array_count;

/* Enter a name and value into systemdict */
void
initial_enter_name(const char *nstr, ref *pref)
{	ref nref;
	name_enter(nstr, &nref);
	if ( dict_put(&systemdict, &nref, pref) )
		lprintf("dict_put failed!\n"),
		gs_exit(1);
}

/* Initialize objects other than operators */
void
obj_init()
{
	/* Initialize the standard objects */
	ref vmark, vnull;
	make_tv(&vmark, t_mark, intval, 0);
	make_tv(&vnull, t_null, intval, 0);

	/* Create the system dictionary */
	dict_create(systemdict_size, &systemdict);
	dstack[1] = dstack[0];		/* just during initialization */

	/* Initialize the predefined names other than operators */
	initial_enter_name("mark", &vmark);
	initial_enter_name("null", &vnull);

	/* Create other system-known names */
	name_enter("errordict", &name_errordict);
	name_enter("ErrorNames", &name_ErrorNames);

	/* Create the error name table */
	   {	int n = sizeof(gs_error_names) / sizeof(char _ds *) - 1;
		int i;
		ref era;
		make_tasv(&era, t_array, a_read + a_execute, n, refs,
			  alloc_refs(n, "obj_init(ErrorNames)"));
		for ( i = 0; i < n; i++ )
		  name_enter((char *)gs_error_names[i], era.value.refs + i);
		dict_put(&systemdict, &name_ErrorNames, &era);
	   }
}

/* Optional devices and .ps files are handled in gconfig.c. */
#define device_(dev)
#define psfile_(fns)

/* Initialize the operators */
#define oper_(defs) defs[],
	/* Non-graphics operators */
extern op_def
#include "gconfig.h"
  interp_op_defs[],
  zarith_op_defs[], zarray_op_defs[], zcontrol_op_defs[],
  zdict_op_defs[], zfile_op_defs[], zfileio_op_defs[],
  zfilter_op_defs[], zgeneric_op_defs[],
  zmath_op_defs[], zmisc_op_defs[], zpacked_op_defs[], zprops_op_defs[],
  zrelbit_op_defs[], zstack_op_defs[], zstring_op_defs[],
  ztype_op_defs[], zvmem_op_defs[],
	/* Graphics operators */
  zchar_op_defs[], zcolor_op_defs[], zdevice_op_defs[],
  zfont_op_defs[], zfont1_op_defs[], zfont2_op_defs[],
  zgstate_op_defs[], zht_op_defs[],
  zmatrix_op_defs[], zpaint_op_defs[], zpath_op_defs[],
  zpath2_op_defs[];
#undef oper_
#define oper_(defs) defs,
private op_def_ptr op_defs_all[] = {
#include "gconfig.h"
	/* Non-graphics operators */
  interp_op_defs,
  zarith_op_defs, zarray_op_defs, zcontrol_op_defs,
  zdict_op_defs, zfile_op_defs, zfileio_op_defs,
  zfilter_op_defs, zgeneric_op_defs,
  zmath_op_defs, zmisc_op_defs, zpacked_op_defs, zprops_op_defs,
  zrelbit_op_defs, zstack_op_defs, zstring_op_defs,
  ztype_op_defs, zvmem_op_defs,
	/* Graphics operators */
  zchar_op_defs, zcolor_op_defs, zdevice_op_defs,
  zfont_op_defs, zfont1_op_defs, zfont2_op_defs,
  zgstate_op_defs, zht_op_defs,
  zmatrix_op_defs, zpaint_op_defs, zpath_op_defs,
  zpath2_op_defs,
	/* end marker */
  (op_def_ptr)0
};
#undef oper_
#undef device_

/* Run the initialization procedures of the individual operator files. */
void
zop_init()
{	op_def_ptr _ds *tptr;
	op_def_ptr def;
	for ( tptr = op_defs_all; *tptr != 0; tptr++ )
	   {	for ( def = *tptr; def->oname != 0; def++ ) ;
		if ( def->proc != 0 )
			((void (*)(P0()))(def->proc))();
	   }
}
/* Initialize the operator table. */
void
op_init()
{	int count = 1;
	op_def_ptr _ds *tptr;
	op_def_ptr def;
	const char _ds *nstr;

	/* Do a first pass just to count the operators. */

	for ( tptr = op_defs_all; *tptr != 0; tptr ++ )
	 for ( def = *tptr; def->oname != 0; count++, def++ )
	  ;

	/* Do a second pass to construct the operator table, */
	/* and enter the operators in systemdict. */

	op_def_table = (op_def_ptr *)alloc(count, sizeof(op_def_ptr),
					   "op_init(op_def_table)");
	op_def_count = count;
	count = 1;
	for ( tptr = op_defs_all; *tptr != 0; tptr ++ )
	 for ( def = *tptr; (nstr = def->oname) != 0; count++, def++ )
	   {	ref nref, oper;
		make_oper(&oper, count, (dummy_op_proc_p)(def->proc));
		interp_fix_op(&oper);		/* optimize if possible */
		/* The first character of the name is a digit */
		/* giving the minimum acceptable number of operands. */
		/* For now, we just skip over it. */
		nstr++;
		/* Don't enter internal operators into systemdict. */
		if ( *nstr == '%' )
			name_enter(nstr, &nref);
		else
			initial_enter_name(nstr, &oper);
		if ( def->oindex != 0 )
			*def->oindex = count;
		op_def_table[count] = def;
	   }

	/* Allocate the table for `operator' procedures. */
	   {	ref *tbody =
		  alloc_refs(op_array_table_size, "op_array table");
		make_tasv(&op_array_table, t_array, a_read+a_execute,
			  op_array_table_size, refs, tbody);
		refset_null(tbody, op_array_table_size);
		op_array_nx_table =
		  (ushort *)alloc(op_array_table_size, sizeof(ushort),
				  "op_array nx table");
		op_array_count = 0;
	   }

}

/* Initialize variables that hold name constants. */
void
init_names(register const names_def _ds *pnd)
{	for ( ; pnd->vname != 0; pnd++ )
		name_enter(pnd->vname, pnd->pvref);
}
