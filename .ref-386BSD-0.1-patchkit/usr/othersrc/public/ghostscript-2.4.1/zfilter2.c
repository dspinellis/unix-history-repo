/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* zfilter2.c */
/* Additional filter creation for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "stream.h"

/* Imported from zfilter.c */
int filter_read(P3(os_ptr, stream_procs _ds *, stream **));
int filter_write(P3(os_ptr, stream_procs _ds *, stream **));

/* .filterASCII85Encode */
extern stream_procs s_A85E_procs;
int
zA85E(os_ptr op)
{	return filter_write(op, &s_A85E_procs, NULL);
}

/* .filterASCII85Decode */
extern stream_procs s_A85D_procs;
int
zA85D(os_ptr op)
{	return filter_read(op, &s_A85D_procs, NULL);
}

/* .filterNullEncode */
extern stream_procs s_NullE_procs;
int
zNullE(os_ptr op)
{	return filter_write(op, &s_NullE_procs, NULL);
}

/* .filterRunLengthEncode */
#if 0					/* ****** */
extern stream_procs s_RLE_procs;
extern void s_RLE_init(P2(stream *, uint));
int
zRLE(register os_ptr op)
{	stream *s;
	int code;
	check_type(*op, t_integer);
	if ( (ulong)(op->value.intval) > max_uint )
		return e_rangecheck;
	code = filter_write(op - 1, &s_RLE_procs, &s);
	if ( code < 0 ) return code;
	s_RLE_init(s, (uint)(op->value.intval));
	pop(1);
	return 0;
}
#endif					/* ****** */

/* .filterRunLengthDecode */
#if 0					/* ****** */
extern stream_procs s_RLD_procs;
extern void s_RLD_init(P1(stream *));
int
zRLD(os_ptr op)
{	stream *s;
	int code = filter_write(op, &s_RLD_procs, &s);
	if ( code < 0 ) return code;
	s_RLD_init(s);
	return 0;
}
#endif					/* ****** */

/* .filterSubFileDecode */
/****** Only implemented for null case (count=0, null string) ******/
extern stream_procs s_SFD_procs;
int
zSFD(os_ptr op)
{	int code;
	check_type(op[-1], t_integer);
	check_type(*op, t_string);
	if ( op[-1].value.intval < 0 ) return e_rangecheck;
	if ( op[-1].value.intval != 0 || r_size(op) != 0 )
		return e_undefined;	/* NYI */
	code = filter_read(op - 2, &s_SFD_procs, NULL);
	if ( code < 0 ) return code;
	pop(2);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfilter2_op_defs[] = {
	{"1.filterASCII85Encode", zA85E},
	{"1.filterASCII85Decode", zA85D},
	{"1.filterNullEncode", zNullE},
#if 0					/* ****** */
	{"2.filterRunLengthEncode", zRLE},
	{"1.filterRunLengthDecode", zRLD},
#endif					/* ****** */
	{"1.filterSubFileDecode", zSFD},
	op_def_end(0)
};
