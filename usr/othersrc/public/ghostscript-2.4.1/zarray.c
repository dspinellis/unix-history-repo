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

/* zarray.c */
/* Array operators for GhostScript */
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "oper.h"
#include "packed.h"
#include "store.h"

/* The generic operators (copy, get, put, getinterval, putinterval, */
/* length, and forall) are implemented in zgeneric.c. */

/* Forward references */
private int make_array(P4(os_ptr, int, int, const char *));

/* array */
int
zarray(register os_ptr op)
{	int code = make_array(op, t_array, a_all, "array");
	if ( code < 0 ) return code;
	refset_null(op->value.refs, r_size(op));
	return 0;
}

/* aload */
int
zaload(register os_ptr op)
{	ref aref;
	int type;
	aref = *op;
	switch ( (type = r_type(&aref)) )
	   {
	default: return e_typecheck;
	case t_array: case t_mixedarray: case t_shortarray: ;
	   }
	check_read(aref);
#define asize r_size(&aref)
	if ( asize > ostop - op ) return e_rangecheck;
	if ( type == t_array )
		memcpy((char *)op, (char *)aref.value.refs,
		       asize * sizeof(ref));
	else
	   {	register ushort i;
		ushort *packed = aref.value.packed;
		os_ptr pdest = op;
		for ( i = 0; i < asize; i++, pdest++ )
			packed_get(packed, pdest),
			packed = packed_next(packed);
	   }
	push(asize);
#undef asize
	*op = aref;
	return 0;
}

/* astore */
int
zastore(register os_ptr op)
{	uint size;
	check_type(*op, t_array);
	check_write(*op);
	size = r_size(op);
	if ( size > op - osbot ) return e_stackunderflow;
	refcpy_to_old(op->value.refs, op - size, size, "astore");
	op[-(int)size] = *op;
	pop(size);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zarray_op_defs[] = {
	{"1aload", zaload},
	{"1array", zarray},
	{"1astore", zastore},
	op_def_end(0)
};

/* ------ Internal procedures ------ */

/* Make an array from the operand stack. */
/* Don't fill it in. */
private int
make_array(register os_ptr op, int type, int attrs, const char *client_name)
{	ref *abody;
	uint size;
	check_type(*op, t_integer);
	if ( op->value.intval < 0 ||
	     op->value.intval > max_uint / sizeof(ref) - 1
	   )
		return e_rangecheck;
	size = op->value.intval;
	abody = alloc_refs(size, client_name);
	if ( abody == 0 ) return e_VMerror;
	make_tasv(op, type, attrs, size, refs, abody);
	return 0;
}
