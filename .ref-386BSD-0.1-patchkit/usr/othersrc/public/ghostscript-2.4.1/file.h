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

/* file.h */
/* Common declarations for zfile.c and zfileio.c */
/* Requires stream.h */

/* File objects store a pointer to a stream in value.pfile. */
/* A file object is valid if its buffer size is non-zero. */
/* The value.pfile of an invalid file object points to an invalid stream. */
#define fptr(pref) (pref)->value.pfile
#define make_file(pref,a,s)\
  make_tav(pref,t_file,a,pfile,s)

/* The standard files.  0 is %stdin, 1 is %stdout. */
extern stream std_files[];
/* An invalid file. */
extern stream invalid_file_entry;

/* Macros for checking file validity */
#define check_file_access(svar,op,acc)\
   {	svar = fptr(op);	/* do first, acc may refer to it */\
	if ( !(acc) ) return e_invalidaccess;\
   }
#define check_file_ref(svar,op,acc)\
   {	if ( !r_has_type(op, t_file) ) return e_typecheck;\
	check_file_access(svar,op,acc);\
   }
#define check_file(svar,op) check_file_ref(svar,op,s_is_valid(svar))
#define check_read_file(svar,op)\
   {	check_read_type(*(op), t_file);\
	check_file_access(svar,op,s_is_reading(svar));\
   }
#define check_write_file(svar,op)\
   {	check_write_type(*(op), t_file);\
	check_file_access(svar,op,s_is_writing(svar));\
   }
