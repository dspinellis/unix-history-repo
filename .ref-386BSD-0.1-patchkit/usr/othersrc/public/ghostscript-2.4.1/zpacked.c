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

/* zpacked.c */
/* Packed array operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "alloc.h"
#include "dict.h"
#include "name.h"
#include "oper.h"
#include "packed.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"

/* Import the array packing flag */
extern ref array_packing;

/* currentpacking */
int
zcurrentpacking(register os_ptr op)
{	push(1);
	make_bool(op, array_packing.value.index);
	return 0;
}

/* packedarray */
int
zpackedarray(register os_ptr op)
{	int code;
	uint size;
	os_ptr aop;
	check_type(*op, t_integer);
	if ( op->value.intval < 0 ||
	     op->value.intval > max_uint / sizeof(ref) - 1
	   )
		return e_rangecheck;
	size = op->value.intval;
	if ( size > op - osbot ) return e_stackunderflow;
	aop = op - size;
	code = make_packed_array(aop, size, aop, "packedarray");
	if ( code >= 0 )
	   {	pop(size);
	   }
	return code;
}

/* setpacking */
int
zsetpacking(register os_ptr op)
{	check_type(*op, t_boolean);
	ref_save(&array_packing, "setpacking");
	array_packing.value.index = op->value.index;
	pop(1);
	return 0;
}

/* ------ Non-operator routines ------ */

/* Make a packed array.  See the comment in packed.h about */
/* ensuring that refs in mixed arrays are properly aligned. */
int
make_packed_array(ref *elts, uint size, ref *paref, const char *client_name)
{	/* Check whether we can make a packed array. */
	ref *endp = elts + size;
	ref *pref = elts;
	ushort *pbody, *pdest;
	ushort *pshort;			/* points to start of */
					/* last run of short elements */
	int atype;
	/* Allocate a maximum-size array first, */
	/* shorten later if needed. */
	pbody = (ushort *)alloc(size, sizeof(ref), client_name);
	if ( pbody == 0 ) return e_VMerror;
	pshort = pdest = pbody;
	for ( ; pref < endp; pref++ )
	   {	switch ( r_btype(pref) )	/* not r_type, opers are special */
		   {
		case t_name:
			if ( name_index(pref) >= packed_max_name_index )
				break;	/* can't pack */
			*pdest = name_index(pref) +
			  (r_has_attr(pref, a_executable) ?
			   pt_tag(pt_executable_name) :
			   pt_tag(pt_literal_name));
			pdest++;
			continue;
		case t_integer:
			if ( pref->value.intval < packed_min_intval ||
			     pref->value.intval > packed_max_intval
			   )
				break;
			*pdest = pt_tag(pt_integer) +
			  ((short)pref->value.intval - packed_min_intval);
			pdest++;
			continue;
		case t_oparray:
		case t_operator:
		   {	uint oidx;
			if ( !r_has_attr(pref, a_executable) ) break;
			oidx = op_index(pref);
			if ( oidx == 0 || oidx > packed_int_mask ) break;
			*pdest = pt_tag(pt_executable_operator) + oidx;
		   }	pdest++;
			continue;
		default: ;
		   }
		/* Can't pack this element, use a full ref. */
		/* We may have to unpack up to 3 preceding short elements. */
		   {	int i = (pdest - pshort) & 3;
			ref *pnext = (ref *)(pdest + (packed_per_ref - 1) * i);
			ref *pnext1 = pnext;
			ref temp;	/* source & dest might overlap */
			while ( --i >= 0 )
			   {	packed_get(--pdest, &temp);
				--pnext;
				ref_assign(pnext, &temp);
			   }
			pdest = (ushort *)pnext1;
			ref_assign(&temp, pref);
			ref_assign_new(pnext1, &temp);
		   }
		pdest += packed_per_ref;
		pshort = pdest;
	   }
	atype = (pdest == pbody + size ? t_shortarray : t_mixedarray);
	pbody = (ushort *)alloc_shrink((byte *)pbody, size * packed_per_ref,
				       (uint)(pdest - pbody), sizeof(ushort),
				       client_name);
	make_tasv_new(paref, atype, a_read+a_execute, size, packed, pbody);
	return 0;
}

/* Get an element from a packed array. */
/* (This works for ordinary arrays too.) */
void
packed_get(const ushort *packed, ref *pref)
{	ushort elt = *packed;
	switch ( elt >> packed_type_shift )
	   {
	case pt_executable_operator:
		op_index_ref(elt & packed_int_mask, pref);
		break;
	case pt_integer:
		make_int(pref, (elt & packed_int_mask) + packed_min_intval);
		break;
	case pt_literal_name:
	case pt_literal_name + 1:
		name_index_ref(elt & packed_max_name_index, pref);
		break;
	case pt_executable_name:
	case pt_executable_name + 1:
		name_index_ref(elt & packed_max_name_index, pref);
		r_set_attrs(pref, a_executable);
		break;
	default:			/* (shouldn't happen) */
	case pt_full_ref:
		ref_assign(pref, (const ref *)packed);
	   }
}

/* Skip N elements in a packed array. */
/* (This works for ordinary arrays too.) */
void
packed_skip(ushort **pp, uint count)
{	ushort *packed = *pp;
	while ( count-- > 0 )
	  if ( *packed <= packed_max_full_ref )
		packed += packed_per_ref;	/* full ref */
	  else
		packed++;
	*pp = packed;
}

/* ------ Initialization procedure ------ */

op_def zpacked_op_defs[] = {
	{"0currentpacking", zcurrentpacking},
	{"1packedarray", zpackedarray},
	{"1setpacking", zsetpacking},
	op_def_end(0)
};
