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

/* zfile.c */
/* Non-I/O file operators for Ghostscript */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "estack.h"			/* for filenameforall, file_close */
#include "iutil.h"
#include "save.h"			/* for restore */
#include "stream.h"
#include "file.h"			/* must come after stream.h */
#include "store.h"

/* Forward references */
int lib_file_open(P6(const byte *, uint, byte *, uint, uint *, ref *));
int file_open(P5(const byte *, uint, const char *, ref *, stream **));
int file_close(P2(ref *, stream *));
private int open_std_file(P3(os_ptr, const char *, os_ptr));
/* In zfileio.c */
es_ptr zget_current_file(P0());

/* Imported from gs.c */
extern char **gs_lib_paths;		/* search path list, */
					/* terminated by a null pointer */

/* The chain of all open files.  We need this only */
/* so that we can do the right thing for restore (and GC someday). */
private ref all_files;			/* t_file */
#define file_list all_files.value.pfile

/* File buffer sizes.  For real files, this is arbitrary, */
/* since the C library does its own buffering in addition. */
/* stdout and stderr use smaller buffers, */
/* on the assumption that they are usually not real files. */
/* The buffer size for type 1 encrypted files is NOT arbitrary: */
/* it must be at most 512. */
#define buffer_size 512

/* Standard file objects: */
/* 0 is stdin, 1 is stdout, 2 is stderr, 3 is lineedit, 4 is statementedit */
#define num_std_files 5
stream invalid_file_entry;
private byte
#define stdin_buf_size 1
	stdin_buf[stdin_buf_size],
#define stdout_buf_size 128
	stdout_buf[stdout_buf_size],
#define stderr_buf_size 128
	stderr_buf[stderr_buf_size],
#define lineedit_buf_size 160
	lineedit_buf[lineedit_buf_size];
/* statementedit is equivalent to lineedit for now */
stream std_files[num_std_files];
private const char *std_file_names[num_std_files] =
   {	"%stdin",
	"%stdout",
	"%stderr",
	"%lineedit",
	"%statementedit"
   };
private int std_file_attrs[num_std_files] =
   {	a_read+a_execute,
	a_write+a_execute,
	a_write+a_execute,
	a_read+a_execute,
	a_read+a_execute
   };

/* Initialize the file table */
private void
zfile_init()
{	/* Create files for stdin, stdout, and stderr. */
	/****** stdin IS NOT IMPLEMENTED PROPERLY ******/
	/* Note that we use gs_out instead of stdout. */
	sread_file(&std_files[0], stdin, stdin_buf, stdin_buf_size);
	std_files[0].can_close = 0;
	swrite_file(&std_files[1], gs_out, stdout_buf, stdout_buf_size);
	std_files[1].can_close = 0;
	swrite_file(&std_files[2], stderr, stderr_buf, stderr_buf_size);
	std_files[2].can_close = 0;
	make_tav(&all_files, t_file, 0, pfile, (stream *)0);
	s_disable(&invalid_file_entry);
}

/* file */
int
zfile(register os_ptr op)
{	char file_access[3];
	ref fname;
	int code;
	stream *s;
	fname = op[-1];
	check_type(fname, t_string);
	check_type(*op, t_string);
	switch ( r_size(op) )
	   {
/******
	case 2:
		if ( op->value.bytes[1] != '+' )
			return e_invalidfileaccess;
		file_access[1] = '+';
		file_access[2] = 0;
		break;
 ******/
	case 1:
		file_access[1] = 0;
		break;
	default:
		return e_invalidfileaccess;
	   }
	switch ( *op->value.bytes )
	   {
	case 'r': case 'w': /****** case 'a': ******/
		break;
	default:
		return e_invalidfileaccess;
	   }
	file_access[0] = *op->value.bytes;
	code = open_std_file(op - 1, file_access, op - 1);
	switch ( code )
	   {
	case 0:				/* successful open */
		pop(1);
	default:			/* unsuccessful open */
		return code;
	case e_undefinedfilename:	/* not a %file */
		;
	   }
	code = file_open(fname.value.bytes, r_size(&fname),
			 file_access, op - 1, &s);
	if ( code >= 0 ) pop(1);
	return code;
}

/* closefile */
int
zclosefile(register os_ptr op)
{	stream *s;
	int code;
	check_file(s, op);
	if ( s->can_close == 0 )
		return e_invalidaccess;	/* can't close std file */
	if ( (code = file_close(op, s)) >= 0 )
	   {	pop(1);
	   }
	return code;
}

/* ------ Level 2 extensions ------ */

/* deletefile */
int
zdeletefile(register os_ptr op)
{	char *str;
	int stat;
	check_read_type(*op, t_string);
	str = ref_to_string(op, "deletefile");
	if ( str == 0 ) return e_VMerror;
	stat = unlink(str);
	alloc_free(str, r_size(op) + 1, 1, "deletefile");
	if ( stat != 0 ) return e_ioerror;
	pop(1);
	return 0;
}

/* filenameforall */
private int file_continue(P1(os_ptr));
private int i_file_continue;
int
zfilenameforall(register os_ptr op)
{	file_enum *pfen;
	check_write_type(*op, t_string);
	check_proc(op[-1]);
	check_read_type(op[-2], t_string);
	/* Push a mark, the pattern, the scratch string, the enumerator, */
	/* and the procedure, and invoke the continuation. */
	check_estack(7);
	pfen = gp_enumerate_files_init((char *)op[-2].value.bytes, r_size(op - 2), alloc, alloc_free);
	if ( pfen == 0 ) return e_VMerror;
	mark_estack(es_for);
	*++esp = op[-2];
	*++esp = *op;
	++esp;
	make_tasv(esp, t_string, a_read+a_execute+a_executable, 0,
		  bytes, (byte *)pfen);
	*++esp = op[-1];
	pop(3);  op -= 3;
	return file_continue(op);
}
/* Continuation operator for enumerating files */
private int
file_continue(register os_ptr op)
{	es_ptr pscratch = esp - 2;
	file_enum *pfen = (file_enum *)esp[-1].value.bytes;
	uint len = r_size(pscratch);
	uint code = gp_enumerate_files_next(pfen, (char *)pscratch->value.bytes, len);
	if ( code == ~(uint)0 )		/* all done */
	   {	gp_enumerate_files_close(pfen);
		esp -= 4;		/* pop mark, scatch, pfen, proc */
		return o_pop_estack;
	   }
	else if ( code > len )		/* overran string */
	   {	gp_enumerate_files_close(pfen);
		return e_rangecheck;
	   }
	else if ( !string_match(pscratch->value.bytes, code,
				pscratch[-1].value.bytes,
				r_size(&pscratch[-1]),
				gp_file_names_ignore_case)
		)
	   {	/* Enumerator was too liberal, ignore this one. */
		push_op_estack(file_continue, i_file_continue);	/* come again */
		return o_push_estack;
	   }
	else
	   {	push(1);
		ref_assign(op, pscratch);
		r_set_size(op, code);
		push_op_estack(file_continue, i_file_continue);	/* come again */
		*++esp = pscratch[2];	/* proc */
		return o_push_estack;
	   }
}

/* renamefile */
int
zrenamefile(register os_ptr op)
{	char *str1 = 0, *str2 = 0;
	check_read_type(*op, t_string);
	check_read_type(op[-1], t_string);
	str1 = ref_to_string(op - 1, "renamefile(from)");
	str2 = ref_to_string(op, "renamefile(to)");
	if ( str1 != 0 && str2 != 0 && rename(str1, str2) == 0 )
	   {	pop(2);
	   }
	if ( str1 != 0 )
		alloc_free(str1, r_size(op - 1) + 1, 1, "renamefile(from)");
	if ( str2 != 0 )
		alloc_free(str2, r_size(op) + 1, 1, "renamefile(to)");
	return 0;
}	

/* ------ Ghostscript extensions ------ */

/* findlibfile */
int
zfindlibfile(register os_ptr op)
{	int code;
#define maxclen 200
	byte cname[maxclen];
	uint clen;
	check_type(*op, t_string);
	code = open_std_file(op, "r", op - 1);
	switch ( code )
	   {
	case 0:				/* successful open */
		push(1);
		make_bool(op, 1);
	default:			/* unsuccessful open */
		return code;
	case e_undefinedfilename:	/* not a %file */
		;
	   }
	code = lib_file_open(op->value.bytes, r_size(op), cname, maxclen,
			     &clen, op);
	if ( code >= 0 )
	   {	byte *cstr = (byte *)alloc(clen, 1, "findlibfile");
		if ( cstr == 0 ) return e_VMerror;
		memcpy(cstr, cname, clen);
		push(2);
		op[-1] = op[-2];
		make_tasv(op - 2, t_string, a_all, clen, bytes, cstr);
		make_bool(op, 1);
	   }
	else
	   {	push(1);
		make_bool(op, 0);
	   }
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfile_op_defs[] = {
	{"1closefile", zclosefile},
	{"1deletefile", zdeletefile},
	{"2file", zfile},
	{"3filenameforall", zfilenameforall},
	{"1findlibfile", zfindlibfile},
	{"2renamefile", zrenamefile},
		/* Internal operators */
	{"0%file_continue", file_continue, &i_file_continue},
	op_def_end(zfile_init)
};

/* ------ Non-operator routines ------ */

/* Open a file, using the search paths if necessary. */
/* The startup code calls this to open the initialization file gs_init.ps. */
int
lib_file_open(const byte *fname, uint len, byte *cname, uint max_clen,
  uint *pclen, ref *pfile)
{	int code;
	char **ppath;
	stream *s;
	code = file_open(fname, len, "r", pfile, &s);
	if ( code >= 0 )
	   {	memcpy(cname, fname, len);
		*pclen = len;
		return code;
	   }
	if ( gp_file_name_is_absolute((const char *)fname, len) )
		return e_undefinedfilename;
	/* Go through the list of search paths */
	for ( ppath = gs_lib_paths; *ppath != 0; ppath++ )
	   {	char *path = *ppath;
		for ( ; ; )
		   {	/* Find the end of the next path */
			char *npath = path;
			uint plen;
			char *cstr;
			uint clen;
			while ( *npath != 0 && *npath != gp_file_name_list_separator )
				npath++;
			plen = npath - path;
			cstr = gp_file_name_concat_string(path, plen,
							  (const char *)fname,
							  len);
			/* Concatenate the prefix, combiner, and file name. */
			clen = plen + strlen(cstr) + len;
			if ( clen <= max_clen )	/* otherwise punt */
			   {	memcpy(cname, (byte *)path, plen);
				strcpy((char *)cname + plen, cstr);
				memcpy(cname + clen - len, fname, len);
				*pclen = clen;
				code = file_open(cname, clen, "r",
						 pfile, &s);
				if ( code >= 0 ) return code;
			   }
			/****** NYI ******/
			if ( !*npath ) break;
			path = npath + 1;
		   }
	   }
	return code;
}

/* Open a file and create a file object. */
/* Return 0 if successful, error code if not. */
/* If fname==0, set up the file entry, stream, and buffer, */
/* but don't open an OS file or initialize the stream. */
/* The filter routines also use this. */
int
file_open(const byte *fname, uint len, const char *file_access,
  ref *pfile, stream **ps)
{	byte *buffer;
	stream *s;
	int code;
	if ( len >= buffer_size )
		return e_limitcheck;	/* we copy the file name into the buffer */
	/* Allocate the stream first, since it persists */
	/* even after the file has been closed. */
	s = (stream *)alloc(1, sizeof(stream), "file_open(stream)");
	if ( s == 0 ) return e_VMerror;
	/* Allocate the buffer. */
	buffer = (byte *)alloc(buffer_size, 1, "file_open(buffer)");
	/* Set the close procedure in case we have to free the file */
	/* before actually initializing the stream. */
	s->procs.close = s_std_close;
	if ( buffer == 0 )
	   {	alloc_free((char *)s, 1, sizeof(stream), "file_open(stream)");
		return e_VMerror;
	   }
	if ( fname != 0 )
	   {	/* Copy the name (so we can terminate it with a zero byte.) */
		char *file_name = (char *)buffer;
		FILE *file;
		memcpy(file_name, fname, len);
		file_name[len] = 0;		/* terminate string */
		/* Open the file. */
		file = fopen(file_name, file_access);
		code = e_undefinedfilename;
		if ( file == 0 )
		   {	alloc_free((char *)buffer, buffer_size, 1, "file_open(buffer)");
			alloc_free((char *)s, 1, sizeof(stream), "file_open(stream)");
			return code;
		   }
		/* Set up the stream. */
		if ( *file_access == 'r' )	/* reading */
			sread_file(s, file, buffer, buffer_size);
		else
			swrite_file(s, file, buffer, buffer_size);
	   }
	else				/* save the buffer and size */
	   {	s->cbuf = buffer;
		s->bsize = s->cbsize = buffer_size;
	   }
	s->can_close = 1;
	s->strm_is_temp = 0;		/* not a temp stream */
	if ( file_list != 0 )
		file_list->prev = s;
	s->next = file_list;
	s->prev = 0;
	file_list = s;
	make_file(pfile,
		(*file_access == 'r' ? a_read+a_execute : a_write+a_execute),
		s);
	*ps = s;
	return 0;
}

/* Free a stream and its buffer. */
/* Free the stream and buffer in the reverse of the order */
/* in which they were created, and hope for LIFO storage behavior. */
void
file_free_stream(register stream *s)
{	alloc_free((char *)s->cbuf, s->cbsize, 1,
		   "file_free_stream(buffer)");
	alloc_free((char *)s, 1, sizeof(stream),
		   "file_free_stream(stream)");
}

/* Close a file.  The interpreter calls this when */
/* it reaches the end of an executable file. */
int
file_close(ref *fp /* t_file */, stream *s)
{	switch ( s->can_close )
	   {
	case 0:				/* can't close std files, ignore */
		break;
	case -1:			/* ignore on statement/lineedit */
		sclose(s);
		s_disable(s);
		break;
	default:			/* ordinary or filter file */
	   {	char *stemp = (s->strm_is_temp ? (char *)s->strm : 0);
		/* Save the links before freeing the stream. */
		stream *sprev = s->prev, *snext = s->next;
		if ( sclose(s) ) return e_ioerror;
		s_disable(s);
		file_free_stream(s);
		if ( stemp != 0 )
			alloc_free(stemp, 1, sizeof(stream),
				   "file_close(sub-stream)");
		/* Unlink the file from the list of all files. */
		if ( sprev != 0 ) sprev->next = snext;
		else file_list = snext;
		if ( snext != 0 ) snext->prev = sprev;
	   }
	   }
	/* If we just closed the file from which the interpreter */
	/* is reading, zap it on the exec stack. */
	   {	es_ptr cfp = zget_current_file();
		if ( cfp != 0 && fptr(cfp) == fptr(fp) )
		   {	/* A null would confuse the estack parser.... */
			make_tasv(cfp, t_array, a_executable+a_execute, 0, refs, (ref *)0);
			esfile = 0;		/* clear currentfile cache */
		   }
	   }
	return 0;
}

/* ------ Internal routines ------ */

/* If a file name refers to one of the standard %files, */
/* 'open' the file and return 0 or an error code, otherwise */
/* return e_undefinedfilename. */
private int
open_std_file(os_ptr pfname, const char *file_access, os_ptr pfile)
{	int i;
	for ( i = 0; i < num_std_files; i++ )
	  if ( !bytes_compare(pfname->value.bytes, r_size(pfname),
			      (const byte *)std_file_names[i],
			      strlen(std_file_names[i]))
	    )
	   {	/* This is a standard file */
		int attrs = (*file_access == 'r' ? a_read+a_execute : a_write+a_execute);
		stream *s = &std_files[i];
		if ( attrs != std_file_attrs[i] )
			return e_invalidaccess;
		make_file(pfile, attrs, s);
		/* If this is %lineedit or %statementedit, */
		/* read a line now. */
		switch ( i )
		   {
		case 3: case 4:
		   {	uint count;
			int code = zreadline_stdin(lineedit_buf,
				lineedit_buf_size, &count);
			if ( code < 0 ) return code;
			sread_string(s, lineedit_buf, count);
			s->can_close = -1;
			return 0;
		   }
		   }
		return 0;
	   }
	return e_undefinedfilename;
}

/* Close inaccessible files just before a restore, */
/* or all files just before exiting. */
void
file_restore(alloc_save *save)
{	while ( file_list != 0 &&
		(save == 0 || alloc_is_since_save((char *)file_list, save))
	      )
	   {	stream *s = file_list;
		if ( s->can_close > 0 )	/* ignore std & buffered files */
		   {	sclose(s);
		   }
		file_list = s->next;
		if ( s->next != 0 ) s->next->prev = s->prev;
		s_disable(s);
	   }
}
void
file_close_all()
{	file_restore((alloc_save *)0);
}
