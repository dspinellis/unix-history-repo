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

/* idebug.c */
/* Debugging support for Ghostscript interpreter */
#include "string_.h"
#include "ghost.h"
#include "iutil.h"
#include "dict.h"
#include "name.h"
#include "ostack.h"			/* for opdef.h */
#include "opdef.h"
#include "packed.h"
#include "store.h"		/* for make_oper for opdef.h */

/* Forward references */
void debug_print_string(P2(const byte *, ushort));
#define debug_print_name(pnref)\
  debug_print_string((pnref)->value.pname->string_bytes,\
		     (pnref)->value.pname->string_size)

/* Table of type name strings */
static const char *type_strings[] = { type_print_strings };

/* Print a ref */
void debug_print_ref(P1(const ref *));
void
debug_print_full_ref(const ref *pref)
{	unsigned size = r_size(pref);
	ref nref;
	dprintf1("(%x)", r_type_attrs(pref));
	switch ( r_type(pref) )
	   {
	case t_array:
	  dprintf2("array(%u)0x%lx", size, (ulong)pref->value.refs); break;
	case t_boolean:
	  dprintf1("boolean %x", pref->value.index); break;
	case t_color:
	  dprintf1("color 0x%lx", (ulong)pref->value.pcolor); break;
	case t_condition:
	  dprintf1("condition 0x%lx", (ulong)pref->value.pcond); break;
	case t_device:
	  dprintf1("device 0x%lx", (ulong)pref->value.pdevice); break;
	case t_dictionary:
	  dprintf3("dict(%u/%u)0x%lx",
		   dict_length(pref), dict_maxlength(pref),
		   (ulong)pref->value.pdict);
	  break;
	case t_file:
	  dprintf1("file 0x%lx", (ulong)pref->value.pfile); break;
	case t_gstate:
	  dprintf1("gstate 0x%lx", (ulong)pref->value.pgstate); break;
	case t_integer: dprintf1("int %ld", pref->value.intval); break;
	case t_lock:
	  dprintf1("lock 0x%lx", (ulong)pref->value.plock); break;
	case t_mark: dprintf("mark"); break;
	case t_mixedarray:
	  dprintf2("mixed packedarray(%u)0x%lx", size,
		   (ulong)pref->value.packed); break;
	case t_name:
	  dprintf2("name(0x%lx#%x)", (ulong)pref->value.pname,
		   pref->value.pname->index);
	  debug_print_name(pref);
	  break;
	case t_null: dprintf("null"); break;
	case t_oparray:
	  dprintf1("op_array(0x%x)", size);
	  name_index_ref(op_array_nx_table[size - op_def_count], &nref);
	  debug_print_name(&nref);
	  break;
	case t_operator:
	  dprintf1("op(0x%x", size);
	  if ( size )
	    dprintf1(":%s", (const char *)(op_def_table[size]->oname + 1));
	  dprintf1(")0x%lx", (ulong)pref->value.opproc);
	  break;
	case t_real: dprintf1("real %f", pref->value.realval); break;
	case t_shortarray:
	  dprintf2("short packedarray(%u)0x%lx", size,
		   (ulong)pref->value.packed); break;
	case t_string:
	  dprintf2("string(%u)0x%lx", size, (ulong)pref->value.bytes); break;
	default: dprintf1("type 0x%x", r_type(pref));
	   }
}
void
debug_print_packed_ref(const ref_packed *pref)
{	ushort elt = *pref;
	ref nref;
	switch ( elt >> packed_type_shift )
	   {
	case pt_executable_operator:
	  dprintf("<op_name>");
	  elt &= packed_int_mask;
	  op_index_ref(elt, &nref);
	  debug_print_ref(&nref);
	  break;
	case pt_integer:
	  dprintf1("<int> %d", (elt & packed_int_mask) + packed_min_intval);
	  break;
	case pt_literal_name: case pt_literal_name+1:
	  dprintf("<lit_name>"); elt &= packed_max_name_index; goto ptn;
	case pt_executable_name: case pt_executable_name+1:
	  dprintf("<exec_name>"); elt &= packed_max_name_index;
ptn:	  name_index_ref(elt, &nref);
	  dprintf2("(0x%lx#%x)", (ulong)nref.value.pname, elt);
	  debug_print_name(&nref);
	  break;
	   }
}
void
debug_print_ref(const ref *pref)
{	if ( r_is_packed(pref) )
		debug_print_packed_ref((const ref_packed *)pref);
	else
		debug_print_full_ref(pref);
}

/* Print a string */
void
debug_print_string(const byte *chrs, ushort len)
{	ushort i;
	for ( i = 0; i < len; i++ )
		dputc(chrs[i]);
}

/* Dump one ref */
void
debug_dump_one_ref(const ref *p)
{	uint attrs = r_type_attrs(p);
	uint btype = r_btype(p);
	static const char *as = attr_print_string;
	const char *ap = as;
#define buf_size 30
	char buf[buf_size + 1];
	uint plen;
	if ( btype >= t_next_index )
		dprintf1("0x%02x?? ", btype);
	else
		dprintf1("%s ", type_strings[btype]);
	for ( ; *ap; ap++, attrs >>= 1 )
	  if ( *ap != '.' )
	    dputc(((attrs & 1) ? *ap : '-'));
	dprintf2(" 0x%04x 0x%08lx", r_size(p), *(const ulong *)&p->value);
	if ( obj_cvs(p, (byte *)buf, buf_size, &plen) >= 0 &&
	     ((buf[plen] = 0), strcmp(buf, "--nostringval--"))
	   )
		dprintf1(" = %s", buf);
}

/* Dump a region of memory containing refs */
void
debug_dump_refs(const ref *from, uint size, const char *msg)
{	const ref *p = from;
	uint count = size;
	if ( size && msg )
		dprintf2("%s at 0x%lx:\n", msg, (ulong)from);
	while ( count-- )
	   {	dprintf2("..%04x: 0x%02x ", (uint)p & 0xffff, r_type(p));
		debug_dump_one_ref(p);
		dputc('\n');
		p++;
	   }
}

/* Dump a region of memory */
void
debug_dump_bytes(const byte *from, const byte *to, const char *msg)
{	const byte *p = from;
	if ( from < to && msg )
		dprintf1("%s:\n", msg);
	while ( p != to )
	   {	const byte *q = min(p + 16, to);
		dprintf1("%lx:", (ulong)p);
		while ( p != q ) dprintf1(" %02x", *p++);
		dputc('\n');
	   }
}

/* Dump an array. */
void
debug_dump_array(const ref *array)
{	const ref_packed *pp;
	unsigned int type = r_type(array);
	uint len;

	switch (type)
	   {
	default:
		dprintf2 ("%s at 0x%lx isn't an array.\n",
			  (type < countof(type_strings) ?
			   type_strings[type] : "????"),
			  (ulong)array);
		return;
	case t_oparray:
		/* This isn't really an array, but we'd like to see */
		/* its contents anyway. */
		debug_dump_array(op_array_table.value.refs + op_index(array) -
				 op_def_count);
		return;
	case t_array:
	case t_mixedarray:
	case t_shortarray: 
		;
	   }

	/* This "packed" loop works for all array-types. */
	for ( len = r_size (array), pp = array->value.packed;
	      len > 0;
	      len--, pp = packed_next(pp))
	   {	ref temp;
		packed_get(pp, &temp);
		dprintf3("..%04x%c 0x%02x ", 
			 (uint)pp & 0xffff,
			 ((r_is_packed(pp)) ? '*' : ':'),
			 r_type(&temp));
		debug_dump_one_ref(&temp);
		dputc ('\n');
	   }
}
