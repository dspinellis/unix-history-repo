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

/* iutil.c */
/* Utilities for Ghostscript interpreter */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "errors.h"
#include "alloc.h"
#include "dict.h"
#include "iutil.h"			/* for checking prototypes */
#include "name.h"
#include "ostack.h"			/* for opdef.h */
#include "opdef.h"			/* for obj_cvs */
#include "store.h"
#include "gsmatrix.h"
#include "gxdevice.h"			/* for gx_color_index */
#include "gzcolor.h"

/* ------ Object utilities ------ */

/* Copy refs from one place to another. */
void
refcpy_to_old(register ref *to, register const ref *from, register uint size,
  const char *cname)
{	while ( size-- ) ref_assign_old(to, from, cname), to++, from++;
}
void
refcpy_to_new(register ref *to, register const ref *from, register uint size)
{	while ( size-- ) ref_assign_new(to, from), to++, from++;
}

/* Fill a new object with nulls. */
void
refset_null(register ref *to, register uint size)
{	while ( size-- ) make_null_new(to), to++;
}

/* Compare two objects for equality.  Return 1 if equal, 0 if not. */
int
obj_eq(register const ref *pref1, register const ref *pref2)
{	ref nref;
	if ( r_btype(pref1) != r_btype(pref2) )
	   {	/* Only a few cases need be considered here: */
		/* integer/real, name/string, and vice versa. */
		switch ( r_type(pref1) )
		   {
		case t_integer:
			return (r_has_type(pref2, t_real) &&
				pref2->value.realval == pref1->value.intval);
		case t_real:
			return (r_has_type(pref2, t_integer) &&
				pref2->value.intval == pref1->value.realval);
		case t_name:
			if ( !r_has_type(pref2, t_string) ) return 0;
			name_string_ref(pref1, &nref);
			pref1 = &nref;
			break;
		case t_string:
			if ( !r_has_type(pref2, t_name) ) return 0;
			name_string_ref(pref2, &nref);
			pref2 = &nref;
			break;
		default:
			return 0;
		   }
	   }
	/* Now do a type-dependent comparison. */
	/* This would be very simple if we always filled in */
	/* all 8 bytes of a ref, but we currently don't. */
	switch ( r_btype(pref1) )
	   {
	case t_array:
		return (pref1->value.refs == pref2->value.refs &&
			r_size(pref1) == r_size(pref2));
	case t_mixedarray:
	case t_shortarray:
		return (pref1->value.packed == pref2->value.packed &&
			r_size(pref1) == r_size(pref2));
	case t_boolean:
		return (pref1->value.index == pref2->value.index);
	case t_condition:
		return (pref1->value.pcond == pref2->value.pcond);
	case t_dictionary:
		return (pref1->value.pdict == pref2->value.pdict);
	case t_file:
		return (pref1->value.pfile == pref2->value.pfile);
	case t_fontID:
		return (pref1->value.pfont == pref2->value.pfont);
	case t_gstate:
		return (pref1->value.pgstate == pref2->value.pgstate);
	case t_integer:
		return (pref1->value.intval == pref2->value.intval);
	case t_lock:
		return (pref1->value.plock == pref2->value.plock);
	case t_mark:
	case t_null:
		return 1;
	case t_name:
		return (pref1->value.pname == pref2->value.pname);
	case t_oparray:
	case t_operator:
		return (op_index(pref1) == op_index(pref2));
	case t_real:
		return (pref1->value.realval == pref2->value.realval);
	case t_save:
		return (pref1->value.psave == pref2->value.psave);
	case t_string:
		return (!bytes_compare(pref1->value.bytes, r_size(pref1),
				       pref2->value.bytes, r_size(pref2)));
	case t_color:
	   {	struct gs_color_s
		  *pc1 = pref1->value.pcolor,
		  *pc2 = pref2->value.pcolor;
		return
		  ( pc1->red == pc2->red && pc1->green == pc2->green &&
		    pc1->blue == pc2->blue && pc1->not_black == pc2->not_black
		  );
	   }
	case t_device:
		return (pref1->value.pdevice == pref2->value.pdevice);
	   }
	return 0;			/* shouldn't happen! */
}

/* Create a printable representation of an object, a la cvs. */
/* Return 0 if OK, <0 if the destination wasn't large enough. */
int
obj_cvs(const ref *op, byte *str, uint len, uint *prlen)
{	char buf[30];			/* big enough for any float */
	byte *pstr = (byte *)buf;
	uint plen;
	ref nref;
	switch ( r_btype(op) )
	   {
	case t_boolean:
		pstr = (byte *)(op->value.index ? "true" : "false");
		break;
	case t_integer:
		sprintf(buf, "%ld", op->value.intval);
		break;
	case t_name:
		name_string_ref(op, &nref);	/* name string */
cvname:		pstr = nref.value.bytes;
		plen = r_size(&nref);
		goto nl;
	case t_oparray:
		name_index_ref(op_array_nx_table[op_index(op) - op_def_count], &nref);
		name_string_ref(&nref, &nref);
		goto cvname;
	case t_operator:
	   {	/* Recover the name from the initialization table. */
		uint index = op_index(op);
		if ( index != 0 )
		   {	pstr = (byte *)(op_def_table[index]->oname + 1);
			break;
		   }
	   }
		/* Internal operator, no name. */
		sprintf(buf, "operator_%lx", (ulong)op->value.opproc);
		break;
	case t_real:
		sprintf(buf, "%g", op->value.realval);
		break;
	case t_string:
		pstr = op->value.bytes;
		plen = r_size(op);
		goto nl;
	default:
		pstr = (byte *)"--nostringval--";
	   }
	plen = strlen((char *)pstr);
nl:	if ( plen > len ) return e_rangecheck;
	memcpy(str, pstr, plen);
	*prlen = plen;
	return 0;
}

/* ------ String utilities ------ */

/* Convert a C string to a Ghostscript string */
int
string_to_ref(const char *cstr, ref *pref, const char *cname)
{	uint size = strlen(cstr);
	char *str = alloc(size, 1, cname);
	if ( str == 0 ) return e_VMerror;
	memcpy(str, cstr, size);
	make_tasv(pref, t_string, a_all, size, bytes, (byte *)str);
	return 0;
}

/* Convert a Ghostscript string to a C string. */
/* Return 0 iff the buffer can't be allocated. */
char *
ref_to_string(const ref *pref, const char *client_name)
{	uint size = r_size(pref);
	char *str = alloc(size + 1, 1, client_name);
	if ( str == 0 ) return 0;
	memcpy(str, (char *)pref->value.bytes, size);
	str[size] = 0;
	return str;
}

/* ------ Operand utilities ------ */

/* Get N numeric operands from the stack. */
/* Return a bit-mask indicating which ones are integers, */
/* or a (negative) error indication. */
/* The 1-bit in the bit-mask refers to the bottommost stack entry. */
/* Store float versions of the operands at pval. */
int
num_params(const ref *op, int count, float *pval)
{	int mask = 0;
	pval += count;
	while ( --count >= 0 )
	   {	mask <<= 1;
		switch ( r_type(op) )
		   {
		case t_real:
			*--pval = op->value.realval;
			break;
		case t_integer:
			*--pval = op->value.intval;
			mask++;
			break;
		default:
			return e_typecheck;
		   }
		op--;
	   }
	return mask;
}

/* Get a real parameter. */
/* If an error is returned, the return value is not updated. */
int
real_param(const ref *op, float *pparam)
{	switch ( r_type(op) )
	   {
	case t_integer: *pparam = op->value.intval; break;
	case t_real: *pparam = op->value.realval; break;
	default: return e_typecheck;
	   }
	return 0;
}

/* ------ Matrix utilities ------ */

/* Check for a matrix operand with read access. */
/* Return 0 if OK, error code if not. */
/* Store an all-float version of the matrix in *pmat. */
int
read_matrix(const ref *op, gs_matrix *pmat)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_array: ;
	   }
	if ( r_size(op) != 6 ) return e_rangecheck;
	if ( !r_has_attr(op, a_read) ) return e_invalidaccess;
	*pmat = *(gs_matrix *)op->value.refs;
	   {	ref *pel = (ref *)pmat;
		int i;
		for ( i = 0; i < 6; i++ )
		   {	switch ( r_type(pel) )
			   {
			default: return e_typecheck;
			case t_integer:
				make_real(pel, pel->value.intval);
			case t_real: ;
			   }
			pel++;
		   }
	   }
	return 0;
}

/* Check for a matrix operand with write access. */
/* Return 0 if OK, error code if not. */
/* Initialize the matrix to an identity matrix */
/* (to set the types and attributes properly.) */
int
write_matrix(register ref *op)
{	ref *aptr;
	int i;
	if ( !r_has_type(op, t_array) ) return e_typecheck;
	if ( !r_has_attr(op, a_write) ) return e_invalidaccess;
	if ( r_size(op) != 6 ) return e_rangecheck;
	aptr = op->value.refs;
	for ( i = 5; i >= 0; i--, aptr++ )
	  { ref_save(aptr, "write_matrix");	/* we're going to overwrite */
	  }
	gs_make_identity((gs_matrix *)op->value.refs);
	return 0;
}
