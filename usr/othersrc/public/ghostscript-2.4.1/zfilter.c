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

/* zfilter.c */
/* Filter creation for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "stream.h"

/* Forward references */
int filter_read(P3(os_ptr, stream_procs _ds *, stream **));
int filter_write(P3(os_ptr, stream_procs _ds *, stream **));

/* .filterASCIIHexEncode */
extern stream_procs s_AXE_procs;
int
zAXE(os_ptr op)
{	return filter_write(op, &s_AXE_procs, NULL);
}

/* .filterASCIIHexDecode */
extern stream_procs s_AXD_procs;
extern void s_AXD_init(P1(stream *));
int
zAXD(os_ptr op)
{	stream *s;
	int code = filter_read(op, &s_AXD_procs, &s);
	if ( code < 0 ) return code;
	s_AXD_init(s);
	return code;
}

/* .filtereexecDecode */
extern stream_procs s_exD_procs;
extern void s_exD_init(P2(stream *, ushort));
int
zexD(register os_ptr op)
{	stream *s;
	ushort state;
	int code;
	check_type(*op, t_integer);
	state = op->value.intval;
	if ( op->value.intval != state )
		return e_rangecheck;	/* state value was truncated */
	code = filter_read(op - 1, &s_exD_procs, &s);
	if ( code < 0 ) return code;
	s_exD_init(s, state);
	pop(1);
	return 0;
}

/* .filterPFBDecode */
extern stream_procs s_PFBD_procs;
extern void s_PFBD_init(P1(stream *));
int
zPFBD(os_ptr op)
{	stream *s;
	int code = filter_read(op, &s_PFBD_procs, &s);
	if ( code < 0 ) return code;
	s_PFBD_init(s);
	return code;
}

/* ------ Utilities ------ */

/* Free the underlying string stream if needed. */
private void
filter_free_null(stream *s)
{
}
private void
filter_free_stream(stream *s)
{	alloc_free((char *)s->strm, 1, sizeof(stream),
		   "filter_free_stream(stream)");
}

/* Set up an input filter. */
int
filter_read(os_ptr op, stream_procs _ds *procs, stream **ps)
{	stream *s;
	stream *sstrm;
	int is_temp;
	int code;
	/* Check to make sure that the underlying data */
	/* can function as a source for reading. */
	switch ( r_type(op) )
	   {
	case t_string:
		check_access(*op, a_read);
		sstrm = (stream *)alloc(1, sizeof(stream),
					"filter_read(string stream)");
		if ( sstrm == 0 ) return e_VMerror;
		sread_string(sstrm, op->value.bytes, r_size(op));
		is_temp = 1;
		break;
	case t_file:
		check_access(*op, a_read);
		sstrm = op->value.pfile;
		is_temp = 0;
		break;
	default:
		return e_typecheck;
	   }
	code = file_open((byte *)0, 0, "r", (ref *)op, &s);
	if ( code < 0 ) return code;
	s_std_init(s, s->cbuf, s->bsize, procs, s_mode_read);
	s->end_status = 0;
	s->position = -1;		/* not positionable */
	s->file = 0;			/* not a file stream */
	s->strm = sstrm;
	s->strm_is_temp = is_temp;
	if ( ps != NULL ) *ps = s;
	return 0;
}

/* Set up an output filter. */
int
filter_write(os_ptr op, stream_procs _ds *procs, stream **ps)
{	stream *s;
	stream *sstrm;
	int is_temp;
	int code;
	/* Check to make sure that the underlying data */
	/* can function as a sink for writing. */
	switch ( r_type(op) )
	   {
	case t_string:
		check_access(*op, a_write);
		sstrm = (stream *)alloc(1, sizeof(stream),
					"filter_write(string)");
		if ( sstrm == 0 ) return e_VMerror;
		swrite_string(sstrm, op->value.bytes, r_size(op));
		is_temp = 1;
		break;
	case t_file:
		check_access(*op, a_write);
		sstrm = op->value.pfile;
		is_temp = 0;
		break;
	default:
		return e_typecheck;
	   }
	code = file_open((byte *)0, 0, "w", (ref *)op, &s);
	if ( code < 0 ) return code;
	s_std_init(s, s->cbuf, s->bsize, procs, s_mode_write);
	s->end_status = 0;
	s->position = -1;		/* not positionable */
	s->file = 0;			/* not a file stream */
	s->strm = sstrm;
	s->strm_is_temp = is_temp;
	if ( ps != NULL ) *ps = s;
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfilter_op_defs[] = {
	{"1.filterASCIIHexEncode", zAXE},
	{"1.filterASCIIHexDecode", zAXD},
	{"2.filtereexecDecode", zexD},
	{"1.filterPFBDecode", zPFBD},
	op_def_end(0)
};
