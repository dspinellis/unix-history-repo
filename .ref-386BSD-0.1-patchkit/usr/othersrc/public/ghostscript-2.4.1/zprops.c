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

/* zprops.c */
/* Device property operators */
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "dict.h"
#include "errors.h"
#include "oper.h"
#include "name.h"
#include "store.h"
#include "gsprops.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"

/* Imported procedures */
extern int array_get(P3(ref *, long, ref *));

/* Forward references */
private int props_to_stack(P3(gs_prop_item *, os_ptr, int));
private int props_from_stack(P3(gs_prop_item *, os_ptr, int));

/* getdeviceprops */
int
zgetdeviceprops(os_ptr op)
{	gx_device *dev;
	gs_prop_item *plist;
	int count;
	int code;
	check_type(*op, t_device);
	dev = op->value.pdevice;
	count = (*dev->procs->get_props)(dev, NULL);
	plist = (gs_prop_item *)alloc(count, sizeof(gs_prop_item), "getdeviceprops");
	if ( plist == 0 ) return_error(e_VMerror);
	(*dev->procs->get_props)(dev, plist);
	code = props_to_stack(plist, op + 1, count);
	alloc_free((char *)plist, count, sizeof(gs_prop_item), "getdeviceprops");
	if ( code >= 0 )
	   {	make_mark(op);
		push(code);
		code = 0;
	   }
	return code;
}

/* putdeviceprops */
int
zputdeviceprops(os_ptr op)
{	gx_device *dev;
	gs_prop_item *plist;
	os_ptr mp;
	int count, acount = 0;
	int code;
	check_type(*op, t_device);
	dev = op->value.pdevice;
	for ( mp = op - 1; !r_has_type(mp, t_mark); mp-- )
	   {	if ( mp <= osbot ) return e_unmatchedmark;
		switch ( r_type(mp) )
		   {
		case t_array:
		case t_mixedarray:
		case t_shortarray:
			acount += r_size(mp);
		   }
	   }
	count = op - mp - 1;
	if ( count & 1 ) return e_rangecheck;
	count >>= 1;
	plist = (gs_prop_item *)alloc(count + acount, sizeof(gs_prop_item), "putdeviceprops");
	if ( plist == 0 ) return_error(e_VMerror);
	code = props_from_stack(plist, mp + 1, count);
	if ( code >= 0 )
		code = (*dev->procs->put_props)(dev, plist, count + acount);
	alloc_free((char *)plist, count + acount, sizeof(gs_prop_item), "putdeviceprops");
	if ( code >= 0 )
	   {	*mp = *op;
		osp = op = mp;
	   }
	return code;
}

/* ------ Initialization procedure ------ */

op_def zprops_op_defs[] = {
	{"1getdeviceprops", zgetdeviceprops},
	{"2putdeviceprops", zputdeviceprops},
	op_def_end(0)
};

/* ------ Internal routines ------ */

/* Get properties from a property list to the stack. */
private int
props_to_stack(gs_prop_item *plist, os_ptr op0, int count)
{	gs_prop_item *pi;
	os_ptr op;
	int i;
	int code;
	for ( op = op0, pi = plist, i = count; i != 0; pi++, i-- )
	   {	ref value;
		const char *nstr = pi->pname;
		int nlen = pi->name_size;
		if ( nstr == 0 ) continue;	/* no name, skip */
		if ( ostop - op < 2 ) return e_stackoverflow;
		if ( nlen < 0 ) nlen = strlen(nstr);
		code = name_ref((const byte *)nstr, nlen, op, 0);
		if ( code < 0 ) return code;
		switch ( pi->type )
		   {
		case (int)prt_int:
			make_int(&value, pi->value.i);
			break;
		case (int)prt_float:
			make_real(&value, pi->value.f);
			break;
		case (int)prt_bool:
			make_bool(&value, pi->value.b);
			break;
		case (int)prt_string:
		   {	ushort size = pi->value.a.size;
			char *str;
			if ( size == (ushort)(-1) )
				size = strlen(pi->value.a.p.s);
			str = alloc(size, 1, "props_to_stack(string)");
			if ( str == 0 ) return e_VMerror;
			memcpy(str, pi->value.a.p.s, size);
			make_tasv(&value, t_string, a_all, size, bytes, (byte *)str);
		   }	break;
		case (int)prt_int_array:
		case (int)prt_float_array:
		   {	uint size = pi->value.a.size;
			ref *arefs = alloc_refs(size, "props_to_stack(array)");
			uint j;
			gs_prop_item *pv = pi->value.a.p.v;
			if ( arefs == 0 ) return e_VMerror;
			make_tasv_new(&value, t_array, a_all, size, refs, arefs);
			for ( j = 0; j < size; j++, arefs++, pv++ )
			  if ( pi->type == prt_int_array )
				make_int_new(arefs, pv->value.i);
			  else
				make_real_new(arefs, pv->value.f);
		   }	break;
		default:
			return e_typecheck;
		   }
		ref_assign(op + 1, &value);
		op += 2;
	   }
	return op - op0;
}

/* Set properties from the stack. */
/* Returns the number of elements copied. */
/* Entries with non-name keys are not copied; */
/* entries with invalid values are copied with status = pv_typecheck. */
private int
props_from_stack(gs_prop_item *plist /* [count + acount] */, os_ptr op0,
  int count)
{	gs_prop_item *pi = plist;
	gs_prop_item *pai = plist + count;
	os_ptr op = op0 + 1;
	for ( ; count; op += 2, count-- )
	   {	ref sref;
		if ( !r_has_type(op - 1, t_name) ) return e_typecheck;
		name_string_ref(op - 1, &sref);
		pi->pname = (char *)sref.value.bytes;
		pi->name_size = r_size(&sref);
		pi->status = pv_set;
		switch ( r_type(op) )
		   {
		case t_null:
			pi->type = prt_null;
			break;
		case t_integer:
			pi->type = prt_int;
			pi->value.i = op->value.intval;
			break;
		case t_real:
			pi->type = prt_float;
			pi->value.f = op->value.realval;
			break;
		case t_boolean:
			pi->type = prt_bool;
			pi->value.b = op->value.index;
			break;
		case t_name:
			name_string_ref(op, &sref);
			goto nst;
		case t_string:
			ref_assign(&sref, op);
			pi->type = prt_string;
			pi->value.a.p.s = (char *)op->value.bytes;
nst:			pi->value.a.size = r_size(&sref);
			break;
		case t_array:
		case t_mixedarray:
		case t_shortarray:
		   {	uint size = r_size(op);
			uint i;
			gs_prop_item *pv;
			gs_prop_type tv = prt_int;
			pi->type = prt_int_array;
			pi->value.a.p.v = pai;
			pi->value.a.size = size;
top:			pv = pai;
			for ( i = 0; i < size; i++ )
			   {	ref rnum;
				array_get(op, (long)i, &rnum);
				pv->pname = 0;
				pv->type = tv;
				switch ( r_type(&rnum) )
				   {
				case t_real:
					if ( tv == prt_int )
					   {	tv = prt_float;
						pi->type = prt_float_array;
						goto top;
					   }
					pv++->value.f = rnum.value.realval;
					break;
				case t_integer:
					if ( tv == prt_int )
					  pv++->value.i = rnum.value.intval;
					else
					  pv++->value.f = rnum.value.intval;
					break;
				default:
					pi->status = pv_typecheck;
				   }
			   }

			pai = pv;
		   }	break;
		default:
			pi->status = pv_typecheck;
		   }
		pi++;
	   }
	return 0;
}
