/* Copyright (C) 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

/* zbseq.c */
/* Level 2 binary object sequence operators */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "save.h"
#include "store.h"
#include "stream.h"
#include "file.h"
#include "name.h"
#include "bnum.h"
#include "btoken.h"
#include "bseq.h"

/* Current binary format (in iscan.c) */
extern ref binary_object_format;

/* System and user name arrays. */
ref system_names, user_names;

/* Import the binary token scanner. */
extern int scan_binary_token(P3(stream *, ref *, int));
extern int (*scan_btoken_proc)(P3(stream *, ref *, int));

/* Forward references */
private int write_bin_object(P2(stream *, os_ptr));

/* Initialize the binary object machinery. */
private void
zbseq_init()
{	/* Initialize fake system and user name tables. */
	/* PostScript code will install the real ones. */
	make_tasv(&system_names, t_shortarray, a_read+a_execute, 0, packed, NULL);
	make_tasv(&user_names, t_array, a_all, 0, refs, NULL);
	scan_btoken_proc = scan_binary_token;
}

/* .installnames */
int
zinstallnames(register os_ptr op)
{	check_read_type(op[-1], t_shortarray);
	check_type(*op, t_array);
	ref_assign_old(&system_names, op - 1, ".installnames");
	ref_assign_old(&user_names, op, ".installnames");
	pop(2);
	return 0;
}

/* currentobjectformat */
int
zcurrentobjectformat(register os_ptr op)
{	push(1);
	*op = binary_object_format;
	return 0;
}

/* printobject */
int
zprintobject(register os_ptr op)
{	int code = write_bin_object(&std_files[1], op);
	if ( code >= 0 )
	   {	pop(2);
	   }
	return code;
}

/* setobjectformat */
int
zsetobjectformat(register os_ptr op)
{	check_type(*op, t_integer);
	if ( op->value.intval < 0 || op->value.intval > 4 )
		return e_rangecheck;
	ref_save(&binary_object_format, "setobjectformat");
	binary_object_format.value.intval = op->value.intval;
	pop(1);
	return 0;
}

/* writeobject */
int
zwriteobject(register os_ptr op)
{	stream *s;
	int code;
	check_write_file(s, op - 2);
	code = write_bin_object(s, op);
	if ( code >= 0 )
	   {	pop(3);
	   }
	return code;
}

/* ------ Initialization procedure ------ */

op_def zbseq_op_defs[] = {
	{"2.installnames", zinstallnames},
	{"0currentobjectformat", zcurrentobjectformat},
	{"2printobject", zprintobject},
	{"1setobjectformat", zsetobjectformat},
	{"3writeobject", zwriteobject},
	op_def_end(zbseq_init)
};

/* ------ Internal routines ------ */

typedef struct { ulong refs, chars; } bin_space;

/* Compute the size of a binary object sequence */
private int
bin_seq_space(ref *op, int array_ok, bin_space *sp)
{	switch ( r_type(op) )
	  {
	  case t_null: case t_integer: case t_real:
	  case t_boolean: case t_mark:
	    sp->refs++;  break;
	  case t_string:
	    sp->refs++;  sp->chars += r_size(op);  break;
	  case t_name:
	    sp->refs++;
	    { ref nstr;
	      name_string_ref(op, &nstr);
	      sp->chars += r_size(&nstr);
	    }
	    break;
	  case t_array:
	    if ( !array_ok ) return e_limitcheck;
	    { uint i;
	      for ( i = 0; i < r_size(op); i++ )
		{ int code = bin_seq_space(op->value.refs + i, 0, sp);
		  if ( code < 0 ) return code;
		}
	    }
	    break;
	  default:
	    return e_typecheck;
	  }
	return 0;
}

/* Write the objects part of a binary object sequence. */
/* Return the new offset in the string part. */
private uint
bin_seq_write_objects(stream *s, ref *op, byte tag, uint spos)
{	bin_seq_obj ob;
	ref nstr;
	ob.unused = tag;
#define swap_t(a, b) t = a, a = b, b = t
#if arch_is_big_endian
#  define must_swap(s) s_is_lsb(s)
#else
#  define must_swap(s) s_is_msb(s)
#endif
	switch ( r_type(op) )
	  {
	  case t_null: ob.tx = (byte)bs_null; break;
	  case t_mark: ob.tx = (byte)bs_mark; break;
	  case t_integer:
	    ob.tx = (byte)bs_integer;
	    ob.value.w = op->value.intval;
num:	    ob.size.w = 0;		/* (matters for reals) */
swb:				/* swap bytes of value if needed */
	    if ( must_swap(s) )
	     { byte t;
	       swap_t(ob.value.b[0], ob.value.b[3]);
	       swap_t(ob.value.b[1], ob.value.b[2]);
	     }
	    break;
	  case t_real:
	    ob.tx = (byte)bs_real;
	    ob.value.f = op->value.realval;
	    /***** handle non-IEEE native *****/
	    goto num;
	  case t_boolean:
	    ob.tx = (byte)bs_boolean;
	    ob.value.w = op->value.index;
	    goto num;
	  case t_array:
	    { uint i;
	      for ( i = 0; i < r_size(op); i++ )
		spos = bin_seq_write_objects(s, op->value.refs + i, 0, spos);
	    }
	    return spos;
	  case t_string:
	    ob.tx = (byte)bs_string;
	    if ( r_has_attr(op, a_executable) )
	      ob.tx += (byte)bs_executable;
nos:	    ob.size.w = r_size(op);
	    if ( must_swap(s) )
	     { byte t;
	       swap_t(ob.size.b[0], ob.size.b[1]);
	     }
	    ob.value.w = spos;
	    spos += r_size(op);
	    goto swb;
	  case t_name:
	    ob.tx = (byte)bs_name;
	    name_string_ref(op, &nstr);
	    op = &nstr;
	    goto nos;
	  }
	sputs(s, (byte *)&ob, sizeof(bin_seq_obj));
	return spos;
}

/* Write the string part of a binary object sequence */
private void
bin_seq_write_strings(stream *s, ref *op)
{	switch ( r_type(op) )
	  {
	  case t_array:
	    { uint i;
	      for ( i = 0; i < r_size(op); i++ )
		bin_seq_write_strings(s, op->value.refs + i);
	    }
	    break;
	  case t_name:
	    { ref nstr;
	      name_string_ref(op, &nstr);
	      sputs(s, nstr.value.bytes, r_size(&nstr));
	    }
	    break;
	  case t_string:
	    sputs(s, op->value.bytes, r_size(op));
	    break;
	  }
}

/* Top-level routine for writing an object (printobject/writeobject) */
private int
write_bin_object(register stream *s, os_ptr op)
{	int bin_format = (int)binary_object_format.value.intval - 1;
	bin_space space;
	int code;
	ulong total;
	byte tag;
	os_ptr op1 = op - 1;
	static int nfs[4] =
	   {	num_float_IEEE + num_msb,
		num_float_IEEE + num_lsb,
		num_float_native + num_msb,
		num_float_native + num_lsb
	   };
	if ( bin_format < 0 ) return e_undefined;
	check_type(*op, t_integer);
	if ( op->value.intval < 0 || op->value.intval > 255 )
		return e_rangecheck;
	tag = (byte)op->value.intval;
	space.refs = space.chars = 0;
	code = bin_seq_space(op1, 1, &space);
	if ( code < 0 ) return code;
	/* Object has been validated, only possible error now is */
	/* ioerror (which we don't check for). */
	total = space.refs * sizeof(bin_seq_obj) + space.chars;
	if ( r_has_type(op1, t_array) )
		total += sizeof(bin_seq_obj);
	if ( total > 0xffff - 4 ) return e_limitcheck;
	s->num_format = nfs[bin_format];
	sputc(s, (byte)bt_seq + bin_format);
	sputc(s, 1);
	sputshort(s, (ushort)total + 4);
	if ( r_has_type(op1, t_array) )
	  { sputc(s, (byte)bs_array +
		  (r_has_attr(op1, a_executable) ? (byte)bs_executable : 0));
	    sputc(s, tag);
	    sputshort(s, r_size(op1));
	    sputlong(s, sizeof(bin_seq_obj));
	    tag = 0;
	  }
	bin_seq_write_objects(s, op1, tag, (uint)(total - space.chars));
	bin_seq_write_strings(s, op1);
	return 0;
}
