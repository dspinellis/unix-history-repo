/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* opdef.h */
/* Operator definition interface for Ghostscript */

/* Typedef for an operator procedure. */
/*
 * Operator procedures return 0 for success, a negative code for an error, 
 * or a positive code for some uncommon situations (see below).
 */
typedef int (*op_proc_p)(P1(os_ptr));

/* Structure for initializing the operator table. */
/*
 * Each operator file declares an array of these, of the following kind:

op_def my_defs[] = {
	{"1name", zname},   --or--  {"1%name", zname, &iname},
	    ...
	op_def_end(iproc)
}

 * where iproc is an initialization procedure for the file, or 0.
 * This definition always appears at the END of the file,
 * to avoid the necessity for forward declarations for all the
 * operator procedures.
 *
 * The second form of definition is for internal operators, such as
 * continuation operators, that do not appear in systemdict and whose
 * name indices must be stored in a static variable.  Ghostscript assumes
 * that these operators cannot appear anywhere (in executable form)
 * except on the e-stack; to maintain this invariant, the execstack
 * operator converts them to literal form, and cvx refuses to convert
 * them back.  As a result of this invariant, they do not need to
 * push themselves back on the e-stack when executed, since the only
 * place they could have come from was the e-stack.
 */
typedef struct {
	const char _ds *oname;
	op_proc_p proc;
	int _ds *oindex;
} op_def;
typedef op_def const _ds *op_def_ptr;
#define op_def_end(iproc) {(char _ds *)0, (op_proc_p)iproc}

/*
 * All operators are catalogued in a table, primarily so
 * that they can have a convenient packed representation.
 * The `size' of an operator is its index in this table.
 */
#define op_index(opref) r_size(opref)
/*
 * There are actually two kinds of operators: the real ones (t_operator),
 * and ones defined by procedures (t_oparray).  The catalog for the former
 * is op_def_table, and their index is in the range [1..op_def_count).
 */
extern op_def_ptr *op_def_table;
extern uint op_def_count;
#define op_num_args(opref) (op_def_table[op_index(opref)]->oname[0] - '0')
/*
 * The catalog for the latter is op_array_table, and their index is in
 * the range [op_def_count..op_def_count+op_array_count).  The actual
 * index in op_array_table is the operator index minus op_def_count.
 */
extern ref op_array_table;		/* t_array */
extern ushort *op_array_nx_table;
extern uint op_array_count;
#define op_index_ref(index,pref)\
  ((index) < op_def_count ?\
   make_oper(pref, index, op_def_table[index]->proc) :\
   (r_set_type_attrs(pref, t_oparray, a_executable), r_set_size(pref, index)))
