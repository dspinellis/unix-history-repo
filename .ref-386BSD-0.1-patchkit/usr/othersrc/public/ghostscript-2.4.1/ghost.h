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

/* ghost.h */
/* Common definitions for Ghostscript */
#include "gx.h"

/* The typedef for object references */
typedef struct ref_s ref;

/*
 * Object types.  This should be an enum, but there is no way
 * to declare an enum a subrange of byte rather than int....
 * The types marked with + use the read/write/execute attributes;
 * the rest only use the executable attribute.
 */
typedef enum {
	t_array,			/* + value.refs, uses size */
	t_boolean,			/* value.index */
	t_condition,			/* value.pcond */
	t_dictionary,			/* + value.pdict */
	t_file,				/* + value.pfile */
	t_fontID,			/* value.pfont */
	t_gstate,			/* value.pgstate */
	t_integer,			/* value.intval */
	t_lock,				/* value.plock */
	t_mark,				/* (no value) */
	t_name,				/* value.pname */
	t_null,				/* (no value) */
	t_operator,			/* value.opproc, uses size */
	t_real,				/* value.realval */
	t_save,				/* value.psave */
	t_string,			/* + value.bytes, uses size */
/* The following are the two implementations of packed arrays. */
	t_mixedarray,			/* + value.packed, uses size */
	t_shortarray,			/* + value.packed, uses size */
/*
 * The following are extensions to the PostScript type set.
 * When adding new types, be sure to edit the table in gs_init.ps
 * (==only operator), the printing routine in idebug.c, the dispatch
 * in interp.c, obj_eq in iutil.c, restore_check_stack in zvmem.c,
 * and also type_name_strings and type_print_strings below.
 */
	t_color,			/* value.pcolor */
	t_device,			/* value.pdevice */
	t_oparray,			/* (no value), uses size */
	t_next_index		/*** first available index ***/
} ref_type;
/*
 * The interpreter uses types starting at t_next_index for representing
 * a few high-frequency operators.
 * Since there are no operations specifically on operators,
 * there is no need for any operators to check specifically for these
 * types.  The r_btype macro takes care of the conversion when required.
 */
/*
 * Define the types that use the size field.
 */
#define case_types_with_size\
  case t_array: case t_operator: case t_string:\
  case t_mixedarray: case t_shortarray: case t_oparray
/*
 * Define the type names for debugging printout.
 * All names must be the same length, so that columns will line up.
 */
#define type_print_strings\
  "arry","bool","cond","dict","file","font","gstt","int ","lock","mark",\
  "name","null","oper","real","save","str ","mpry","spry","colr","devc",\
  "opry"
/*
 * Define the type names for the type operator.
 */
#define type_name_strings\
  "arraytype","booleantype","conditiontype","dicttype","filetype",\
  "fonttype","gstatetype","integertype","locktype","marktype",\
  "nametype","nulltype","operatortype","realtype","savetype",\
  "stringtype","packedarraytype","packedarraytype","colortype","devicetype",\
  "operatortype"

/*
 * The encoding of attributes is constrained by two factors:
 *
 *	- The packed array format requires the high-order bits of the
 *	  type/attributes field to be 0.  (see packed.h)
 *
 *	- The interpreter wants the type, executable bit, and execute
 *	  permission to be adjacent, and in that order from high to low.
 *
 * The layout given below is the one that leads to the most efficient
 * dispatching in the interpreter.
 */

/* Location attributes */
/* Note that these are associated with the *location*, not with the */
/* ref that is *stored* in that location. */
#define l_mark 1			/* mark for garbage collector */
					/* (not used yet) */
#define l_new 2				/* stored into since last save */
#define l_space 4			/* local vs. global space */
					/* (not used yet) */
#define a_write 8
#define a_read 0x10
#define a_execute 0x20
#define a_executable 0x40
#define a_all (a_write+a_read+a_execute)
#define r_type_shift 7
#define r_type_bits 6

/* Define the attribute names for debugging printout. */
#define attr_print_string "mnswrxe......???"

/* Abstract types */
typedef struct dict_s dict;
typedef struct name_s name;
/* We define a dummy type for op_proc_p so that */
/* we don't have to include oper.h. */
typedef int (*dummy_op_proc_p)();
#define real_opproc(pref) (*(op_proc_p *)&(pref)->value)

/* Object reference */
/*
 * Note that because of the way packed arrays are represented,
 * the type_attrs member must be the first one in the ref structure.
 */
struct stream_s;
struct gs_font_s;
struct gs_color_s;
struct gs_condition_s;
struct gs_lock_s;
struct gx_device_s;
struct gstate_obj_s;
struct vm_save_s;
struct tas_s {
	ushort type_attrs;
	ushort rsize;
};
struct ref_s {

	struct tas_s tas;

#define r_size(rp) ((rp)->tas.rsize)
#define r_inc_size(rp,inc) ((rp)->tas.rsize += (inc))
#define r_set_size(rp,siz) ((rp)->tas.rsize = (siz))
/* type_attrs is a single element for fast dispatching in the interpreter */
#define r_type(rp) ((rp)->tas.type_attrs >> r_type_shift)
#define r_has_type(rp,typ) r_has_type_attrs(rp,typ,0)	/* see below */
#define r_set_type(rp,typ) ((rp)->tas.type_attrs = (typ) << r_type_shift)
#define r_btype(rp)\
 ((rp)->tas.type_attrs >= (t_next_index << r_type_shift) ?\
  t_operator : r_type(rp))
#define type_xe(tas) ((tas) >> (r_type_shift - 2))
#define r_type_xe(rp) type_xe((rp)->tas.type_attrs)
#define type_xe_value(t,xe) type_xe(((t) << r_type_shift) + (xe))
#define r_type_attrs(rp) ((rp)->tas.type_attrs)	/* reading only */
#define r_has_attrs(rp,mask) !(~r_type_attrs(rp) & (mask))
#define r_has_attr(rp,mask1)		/* optimize 1-bit case */\
   (r_type_attrs(rp) & (mask1))
#define r_has_type_attrs(rp,typ,mask)\
 (((rp)->tas.type_attrs & ((((1 << r_type_bits) - 1) << r_type_shift)\
    + (mask))) ==\
  (((typ) << r_type_shift) + (mask)))
#define r_set_attrs(rp,mask) ((rp)->tas.type_attrs |= (mask))
#define r_clear_attrs(rp,mask) ((rp)->tas.type_attrs &= ~(mask))
#define r_set_type_attrs(rp,typ,mask)\
 ((rp)->tas.type_attrs = ((typ) << r_type_shift) + (mask))

	union v {			/* name the union to keep gdb happy */
		long intval;
		ushort index;		/* for enumerated things */
		float realval;
		byte *bytes;
		struct ref_s *refs;
		name *pname;
		dict *pdict;
		ushort *packed;
		dummy_op_proc_p opproc;
		struct stream_s *pfile;
		struct gs_font_s *pfont;
		struct gs_color_s *pcolor;
		struct gs_condition_s *pcond;
		struct gs_lock_s *plock;
		struct gx_device_s *pdevice;
		struct gstate_obj_s *pgstate;
		struct vm_save_s *psave;
	} value;
};
