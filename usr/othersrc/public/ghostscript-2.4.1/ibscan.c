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

/* ibscan.c */
/* Binary token scanner */
#include "math_.h"
#include "ghost.h"
#include "errors.h"
#include "alloc.h"
#include "dict.h"
#include "dstack.h"			/* for immediately evaluated names */
#include "iutil.h"
#include "name.h"
#include "ostack.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"
#include "stream.h"
#include "btoken.h"
#include "bseq.h"
#include "bnum.h"

/* Import the system and user name tables */
extern ref system_names, user_names;

/* Imported procedures */
extern int array_get(P3(ref *, long, ref *));

/* Forward references */
private	int	scan_binary_sequence(P2(stream *, ref *));

/* Scan a binary token.  Called from the main scanner */
/* when it encounters an ASCII code 128-159, */
/* if binary tokens are being recognized (object format != 0). */
int
scan_binary_token(register stream *s, ref *pref, int tcode)
{	int num_format, code;
	uint arg;
	long nidx;
	if ( seofp(s) ) return e_syntaxerror;
	switch ( tcode )
	   {
	case bt_seq_IEEE_msb:
		s->num_format = num_msb + num_float_IEEE; goto bseq;
	case bt_seq_IEEE_lsb:
		s->num_format = num_lsb + num_float_IEEE; goto bseq;
	case bt_seq_native_msb:
		s->num_format = num_msb + num_float_native; goto bseq;
	case bt_seq_native_lsb:
		s->num_format = num_lsb + num_float_native;
bseq:		return scan_binary_sequence(s, pref);
	case bt_int32_msb:
		num_format = num_msb + num_int32; goto num;
	case bt_int32_lsb:
		num_format = num_lsb + num_int32; goto num;
	case bt_int16_msb:
		num_format = num_msb + num_int16; goto num;
	case bt_int16_lsb:
		num_format = num_lsb + num_int16; goto num;
	case bt_int8:
		make_int(pref, (sgetc(s) ^ 128) - 128);
		return 0;
	case bt_fixed:
		num_format = sgetc(s);
		if ( !num_is_valid(num_format) )
			return e_syntaxerror;
		goto num;
	case bt_float_IEEE_msb:
		num_format = num_msb + num_float_IEEE; goto num;
	case bt_float_IEEE_lsb:
		num_format = num_lsb + num_float_IEEE; goto num;
	case bt_float_native:
		num_format = num_float_native;
num:		s->num_format = num_format;
		code = sget_encoded_number(s, pref);
		switch ( code )
		   {
		case t_integer: case t_real:
			r_set_type(pref, code);
			break;
		case t_null: return e_syntaxerror;
		default: return code;
		   }
		return 0;
	case bt_boolean:
		arg = sgetc(s);
		if ( arg & ~1 ) return e_syntaxerror;
		make_bool(pref, arg);
		return 0;
	case bt_string_256:
		arg = sgetc(s); goto str;
	case bt_string_64k_msb:
		arg = sgetc(s) << 8;
		if ( seofp(s) ) return e_syntaxerror;
		arg += sgetc(s);
		goto str;
	case bt_string_64k_lsb:
		arg = sgetc(s);
		if ( seofp(s) ) return e_syntaxerror;
		arg += sgetc(s) << 8;
str:	   {	byte *str = (byte *)alloc(arg, 1, "string token");
		uint rcnt;
		if ( str == 0 ) return e_VMerror;
		rcnt = sgets(s, str, arg);
		if ( rcnt != arg ) return e_syntaxerror;
		make_tasv(pref, t_string, a_all, arg, bytes, str);
	   }	return 0;
	case bt_litname_system:
		nidx = sgetc(s);
		return array_get(&system_names, nidx, pref);
	case bt_execname_system:
		nidx = sgetc(s);
		code = array_get(&system_names, nidx, pref);
		if ( code < 0 ) return code;
		r_set_attrs(pref, a_executable);
		return 0;
	case bt_litname_user:
		nidx = sgetc(s);
		code = array_get(&user_names, nidx, pref);
		if ( code < 0 ) return code;
		if ( !r_has_type(pref, t_name) ) return e_undefined;
		return 0;
	case bt_execname_user:
		nidx = sgetc(s);
		code = array_get(&user_names, nidx, pref);
		if ( code < 0 ) return code;
		if ( !r_has_type(pref, t_name) ) return e_undefined;
		r_set_attrs(pref, a_executable);
		return 0;
	case bt_num_array:
	   {	ref *nap;
		uint i;
		num_format = sgetc(s);
		if ( !num_is_valid(num_format) ) return e_syntaxerror;
		s->num_format = num_format;
		code = sgetshort(s, (short *)&arg);
		if ( code < 0 ) return code;
		nap = alloc_refs(arg, "number array token");
		if ( nap == 0 ) return e_VMerror;
		for ( i = 0; i < arg; i++ )
		   {	ref *np = nap + i;
			int code = sget_encoded_number(s, np);
			switch ( code )
			   {
			case t_integer: case t_real:
				r_set_type(np, code);
				break;
			case t_null: return e_syntaxerror;
			default: return code;
			   }
		   }
		make_tasv(pref, t_array, a_all, arg, refs, nap);
	   }	return 0;
	   }
	return e_syntaxerror;
}

/* Scan a binary object sequence. */
private int
scan_binary_sequence(register stream *s, ref *pref)
{	ushort top_size = sgetc(s);
	ushort size;
	uint max_array_index;
	uint min_string_index;
	uint index;
	int code;
	register os_ptr op = osp;
#if arch_is_big_endian
#  define must_swap_bytes s_is_lsb(s)
#else
#  define must_swap_bytes s_is_msb(s)
#endif
	if ( top_size == 0 )
	   {	/* Extended header (2-byte array size, 4-byte length) */
		ulong lsize;
		if ( (code = sgetshort(s, (short *)&top_size)) < 0 ||
		     (code = sgetlong(s, (long *)&lsize)) < 0
		   )
			return code;
		if ( (size = (ushort)lsize) != lsize )
			return e_limitcheck;
		if ( size < 8 + sizeof(bin_seq_obj) )
			return e_syntaxerror;
		size -= 8;
	   }
	else
	   {	/* Normal header (1-byte array size, 2-byte length) */
		if ( (code = sgetshort(s, (short *)&size)) < 0 )
			return code;
		if ( size < 4 + sizeof(bin_seq_obj) )
			return e_syntaxerror;
		size -= 4;
	   }
	/* First pass: read objects, handle all but composite. */
	max_array_index = top_size;
	min_string_index = size;
	for ( index = 0; index < max_array_index; index++ )
	   {	bin_seq_obj ob;
		byte bt;
		if ( sgets(s, (byte *)&ob, sizeof(bin_seq_obj)) !=
			sizeof(bin_seq_obj)
		   )
			return e_syntaxerror;
		if ( ++op > ostop )
			return e_limitcheck;
#define swap_size()\
  if ( must_swap_bytes )\
    bt = ob.size.b[0], ob.size.b[0] = ob.size.b[1], ob.size.b[1] = bt
#define swap_value()\
  if ( must_swap_bytes )\
    bt = ob.value.b[0], ob.value.b[0] = ob.value.b[3], ob.value.b[3] = bt,\
    bt = ob.value.b[1], ob.value.b[1] = ob.value.b[2], ob.value.b[2] = bt
		switch ( ob.tx & 0x7f )
		   {
		case bs_null:
			make_null(op); break;
		case bs_integer:
			swap_value();
			make_int(op, ob.value.w);
			break;
		case bs_real:
			if ( ob.size.w != 0 )	/* fixed-point number */
			   {	swap_size(); swap_value();
				ob.value.f = (float)ldexp((float)ob.value.w,
							  -ob.size.w);
			   }
			else if ( (s->num_format & ~(num_lsb | num_msb)) !=
				  num_float_native
				)
			   {	swap_value();
#if !arch_floats_are_IEEE
				/* Convert IEEE float to native float. */
				   {	int expt = (ob.value.w >> 23) & 0xff;
					long mant = ob.value.w & 0x7fffff;
					if ( expt == 0 && mant == 0 )
						ob.value.f = 0;
					else
					   {	mant += 0x800000;
						ob.value.f = (float)
						  ldexp((float)mant,
							expt - 127 - 24);
					   }
					if ( ob.value.w < 0 )
						ob.value.f = -ob.value.f;
				   }
#endif
			   }
			make_real(op, ob.value.f);
			break;
		case bs_boolean:
			swap_value();
			make_bool(op, (ob.value.w == 0 ? 0 : 1));
			break;
		case bs_string:
			swap_size(); swap_value();
			if ( ob.value.w < max_array_index ||
			    ob.value.w + ob.size.w > size
			   )
				return e_syntaxerror;
			if ( ob.value.w < min_string_index )
				min_string_index = (uint)ob.value.w;
			make_tasv(op, t_string, (ob.tx < 128 ? a_all :
				  a_all + a_executable), ob.size.w,
				  intval, ob.value.w);
			break;
		case bs_name:
		case bs_eval_name:
			swap_size(); swap_value();
			switch ( ob.size.w )
			   {
			case 0:
			case 0xffff:
				break;
			default:
				if ( ob.value.w < max_array_index ||
				    ob.value.w + ob.size.w > size
				   )
					return e_syntaxerror;
				if ( ob.value.w < min_string_index )
					min_string_index = (uint)ob.value.w;
			   }
			make_tasv(op, t_name,
				  ((ob.tx & 0x7f) == bs_eval_name ? 0 :
				   ob.tx & 0x80 ? a_all + a_executable :
				   a_all), ob.size.w, intval, ob.value.w);
			break;
		case bs_array:
			swap_size(); swap_value();
			if ( ob.value.w + ob.size.w > min_string_index ||
			    ob.value.w & (sizeof(bin_seq_obj) - 1)
			   )
				return e_syntaxerror;
			max_array_index =
			  max(max_array_index,
			      ob.value.w / sizeof(bin_seq_obj) + ob.size.w);
			make_tasv(op, t_array, (ob.tx < 128 ? a_all :
				  a_all + a_executable), ob.size.w,
				  intval, ob.value.w);
			break;
		case bs_mark:
			make_mark(op); break;
		default:
			return e_syntaxerror;
		   }
	   }
	/* Allocate objects and strings. */
	min_string_index =
	  min(min_string_index, max_array_index * sizeof(bin_seq_obj));
	   {	ref *rbase = alloc_refs(max_array_index,
					"binary object sequence(objects)");
		uint str_size = size - min_string_index;
		byte *sbase = (byte *)alloc(str_size, 1,
					    "binary object sequence(strings)");
		os_ptr op_top = op;
		if ( rbase == 0 || sbase == 0 )
		   {	/* SHOULD FREE THEM */
			return e_VMerror;
		   }
		if ( sgets(s, sbase, str_size) != str_size )
			return e_syntaxerror;
		/* Fix up composites. */
		for ( op = osp; op != op_top; )
		  switch ( r_type(++op) )
		   {
		case t_string:
			op->value.bytes =
				sbase + (op->value.intval - min_string_index);
			break;
		case t_array:
			op->value.refs =
				rbase + (op->value.intval / sizeof(bin_seq_obj));
			break;
		case t_name:
		   {	ref rname;
			switch ( r_size(op) )
			   {
			case 0:
				code = array_get(&user_names,
						 op->value.intval, &rname);
				goto usn;
			case 0xffff:
				code = array_get(&system_names,
						 op->value.intval, &rname);
usn:				if ( code >= 0 && !r_has_type(&rname, t_name) )
					return e_undefined;
				break;
			default:	/* ordinary name */
				code = name_ref(sbase + (op->value.intval - min_string_index), r_size(op), &rname, 0);
			   }
			if ( code < 0 ) return code;
			if ( !r_has_attr(op, a_read) )	/* bs_eval_name */
			   {	ref *defp = dict_find_name(&rname);
				if ( defp == 0 ) return e_undefined;
				rname = *defp;
			   }
			else if ( r_has_attr(op, a_executable) )
				r_set_attrs(&rname, a_executable);
			ref_assign(op, &rname);
		   }
			break;
		   }
		refcpy_to_new(rbase, osp + 1, max_array_index);
		make_tasv(pref, t_array, a_all, top_size, refs, rbase);
	   }
	return 0;
}
