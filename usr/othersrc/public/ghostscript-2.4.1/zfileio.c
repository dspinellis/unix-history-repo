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

/* zfileio.c */
/* File I/O operators for Ghostscript */
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "stream.h"
#include "estack.h"
#include "file.h"
#include "store.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "gxdevmem.h"

/* Forward references */
int zreadline_from(P4(byte *, uint, uint *, stream *));
es_ptr zget_current_file(P0());
private int write_string(P2(os_ptr, stream *));

/* ------ Operators ------ */

/* read */
int
zread(register os_ptr op)
{	stream *s;
	int ch;
	check_read_file(s, op);
	ch = sgetc(s);
	if ( ch == EOFC )
		make_bool(op, 0);
	else
	   {	make_int(op, ch);
		push(1);
		make_bool(op, 1);
	   }
	return 0;
}

/* write */
int
zwrite(register os_ptr op)
{	stream *s;
	ulong ch;
	check_write_file(s, op - 1);
	check_type(*op, t_integer);
	ch = op->value.intval;
	if ( ch > 0xff ) return e_rangecheck;
	sputc(s, (byte)ch);
	pop(2);
	return 0;
}

/* readhexstring */
int
zreadhexstring(register os_ptr op)
{	stream *s;
	int code;
	uint nread;
	int odd = -1;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	code = sreadhex(s, op->value.bytes, r_size(op), &nread, &odd, 1);
	switch ( code )
	   {
	case EOFC:
		/* Reached end-of-file before filling the string. */
		/* Return an appropriate substring. */
		r_set_size(op, nread);
		code = 1;
		break;
	case 0:
		/* Filled the string. */
		break;
	default:			/* Error */
		return e_ioerror;
	   }
	ref_assign(op - 1, op);
	make_bool(op, 1 - code);
	return 0;
}

/* writehexstring */
int
zwritehexstring(register os_ptr op)
{	register stream *s;
	register byte ch;
	register byte *p;
	register const char _ds *hex_digits = "0123456789abcdef";
	register uint len;
	check_write_file(s, op - 1);
	check_read_type(*op, t_string);
	p = op->value.bytes;
	len = r_size(op);
	while ( len-- )
	   {	ch = *p++;
		sputc(s, hex_digits[ch >> 4]);
		sputc(s, hex_digits[ch & 0xf]);
	   }
	pop(2);
	return 0;
}

/* readstring */
int
zreadstring(register os_ptr op)
{	stream *s;
	uint len, rlen;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	len = r_size(op);
	rlen = sgets(s, op->value.bytes, len);
	r_set_size(op, rlen);
	op[-1] = *op;
	make_bool(op, (rlen == len ? 1 : 0));
	return 0;
}

/* writestring */
int
zwritestring(register os_ptr op)
{	stream *s;
	int code;
	check_write_file(s, op - 1);
	code = write_string(op, s);
	if ( code >= 0 ) pop(2);
	return code;
}

/* readline */
int
zreadline(register os_ptr op)
{	stream *s;
	uint count;
	int code;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	code = zreadline_from(op->value.bytes, r_size(op), &count, s);
	if ( code < 0 ) return code;
	r_set_size(op, count);
	op[-1] = *op;
	make_bool(op, code);
	return 0;
}

/* Read a line from stdin.  This is called from gs.c. */
int
zreadline_stdin(byte *ptr, uint size, uint *pcount)
{	return zreadline_from(ptr, size, pcount, &std_files[0]);
}

/* Internal readline routine. */
/* Returns 1 if OK, 0 if end of file, or an error code. */
int
zreadline_from(byte *ptr, uint size, uint *pcount, stream *s)
{	uint count = 0;
	int ch;
	for ( ; ; count++ )
	   {	switch ( ch = sgetc(s) )
		   {
		case '\r':
			ch = sgetc(s);
			if ( ch != '\n' && ch >= 0 ) sputback(s);
			/* falls through */
		case '\n':
			*pcount = count;
			return 1;
		case EOFC:
			*pcount = count;
			return 0;
		   }
		if ( count >= size )	/* filled the string */
		   {	sputback(s);
			return e_rangecheck;
		   }
		*ptr++ = ch;
	   }
	return 0;
}

/* token - this is called from zstring.c */
int
ztoken_file(register os_ptr op)
{	stream *s;
	ref token;
	int code;
	check_read_file(s, op);
	switch ( code = scan_token(s, 0, &token) )
	   {
	case 0:				/* read a token */
		*op = token;
		push(1);
		make_bool(op, 1);
		return 0;
	case 1:				/* no tokens */
		make_bool(op, 0);
		return 0;
	default:			/* error */
		return code;
	   }
}

/* bytesavailable */
int
zbytesavailable(register os_ptr op)
{	stream *s;
	long avail;
	check_read_file(s, op);
	if ( savailable(s, &avail) < 0 ) return e_ioerror;
	make_int(op, avail);
	return 0;
}

/* flush */
int
zflush(register os_ptr op)
{	sflush(&std_files[1]);
	return 0;
}

/* flushfile */
int
zflushfile(register os_ptr op)
{	stream *s;
	check_file(s, op);
	sflush(s);
	if ( !s_is_writing(s) )
		fseek(s->file, 0L, 2);	/* set to end */
	pop(1);
	return 0;
}

/* resetfile */
int
zresetfile(register os_ptr op)
{	NYI("resetfile");
	pop(1);
	return 0;
}

/* status */
int
zstatus(register os_ptr op)
{	switch ( r_type(op) )
	   {
	case t_file:
		make_bool(op, (fptr(op)->bsize != 0 ? 1 : 0));
		return 0;
	case t_string:
	   {	char *fname = ref_to_string(op, "status");
		file_status fstat;
		if ( fname == 0 ) return e_VMerror;
		if ( gp_file_status(fname, &fstat) )
		   {	push(4);
			make_int(op - 4, fstat.size_pages);
			make_int(op - 3, fstat.size_bytes);
			make_int(op - 2, fstat.time_referenced);
			make_int(op - 1, fstat.time_created);
			make_bool(op, 1);
		   }
		else
			make_bool(op, 0);
		alloc_free(fname, r_size(op) + 1, 1, "status");
	   }	return 0;
	default:
		return e_typecheck;
	   }
}

/* currentfile */
int
zcurrentfile(register os_ptr op)
{	es_ptr fp;
	push(1);
	/* Check the cache first */
	if ( esfile != 0 )
		ref_assign(op, esfile);
	else if ( (fp = zget_current_file()) == 0 )
	   {	/* Return an invalid file object. */
		/* This doesn't make a lot of sense to me, */
		/* but it's what the PostScript manual specifies. */
		make_file(op, 0, &invalid_file_entry);
	   }
	else
	   {	ref_assign(op, fp);
		/* Load the cache */
		esfile = fp;
	   }
	/* Make the returned value literal. */
	r_clear_attrs(op, a_executable);
	return 0;
}

/* print */
int
zprint(register os_ptr op)
{	int code = write_string(op, &std_files[1]);
	if ( code >= 0 ) pop(1);
	return code;
}

/* echo */
int
zecho(register os_ptr op)
{	check_type(*op, t_boolean);
	/****** NOT IMPLEMENTED YET ******/
	pop(1);
	return 0;
}

/* ------ Level 2 extensions ------ */

/* fileposition */
int
zfileposition(register os_ptr op)
{	stream *s;
	check_file(s, op);
	if ( !sseekable(s) ) return e_ioerror;
	make_int(op, stell(s));
	return 0;
}

/* setfileposition */
int
zsetfileposition(register os_ptr op)
{	stream *s;
	check_file(s, op - 1);
	check_type(*op, t_integer);
	if ( sseek(s, op->value.intval) < 0 ) return e_ioerror;
	pop(2);
	return 0;
}

/* ------ Ghostscript extensions ------ */

/* unread */
int
zunread(register os_ptr op)
{	stream *s;
	ulong ch;
	check_read_file(s, op - 1);
	check_type(*op, t_integer);
	ch = op->value.intval;
	if ( ch > 0xff ) return e_rangecheck;
	if ( sungetc(s, (byte)ch) < 0 ) return e_ioerror;
	pop(2);
	return 0;
}

/* writeppmfile */
int
zwriteppmfile(register os_ptr op)
{	stream *s;
	int code;
	check_write_file(s, op - 1);
	check_type(*op, t_device);
	if ( !gs_device_is_memory(op->value.pdevice) ) return e_typecheck;
	sflush(s);
	code = gs_writeppmfile((gx_device_memory *)(op->value.pdevice), s->file);
	if ( code >= 0 ) pop(2);
	return code;
}

/* ------ Initialization procedure ------ */

op_def zfileio_op_defs[] = {
	{"1bytesavailable", zbytesavailable},
	{"0currentfile", zcurrentfile},
	{"1echo", zecho},
	{"1fileposition", zfileposition},
	{"0flush", zflush},
	{"1flushfile", zflushfile},
	{"1print", zprint},
	{"1read", zread},
	{"2readhexstring", zreadhexstring},
	{"2readline", zreadline},
	{"2readstring", zreadstring},
	{"1resetfile", zresetfile},
	{"2setfileposition", zsetfileposition},
	{"2unread", zunread},
	{"1status", zstatus},
	{"2write", zwrite},
	{"2writehexstring", zwritehexstring},
	{"2writeppmfile", zwriteppmfile},
	{"2writestring", zwritestring},
	op_def_end(0)
};

/* ------ Non-operator routines ------ */

/* Check a file for reading. */
/* The interpreter calls this to check an executable file. */
int
file_check_read(ref *op, stream **ps)
{	if ( !s_is_reading(*ps = fptr(op)) ) return e_invalidaccess;
	return 0;
}

/* Get the current file from which the interpreter is reading. */
es_ptr
zget_current_file()
{	register es_ptr ep = esp;
	while ( ep >= esbot )
	{	if ( r_has_type_attrs(ep, t_file, a_executable) )
			return ep;
		ep--;
	}
	return (es_ptr)0;
}

/* ------ Internal routines ------ */

/* Write a string on a file.  The file has been checked for validity, */
/* but not the string. */
private int
write_string(os_ptr op, stream *s)
{	uint len;
	check_read_type(*op, t_string);
	len = r_size(op);
	if ( sputs(s, op->value.bytes, len) != len ) return e_ioerror;
	return 0;
}
