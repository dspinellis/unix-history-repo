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

/* iutil.h */
/* Prototypes for procedures in iutil.c */

/* ------ Object utilities ------ */

/* Copy refs from one place to another. */
/* (If we are copying to the stack, we can just use memcpy.) */
extern void refcpy_to_new(P3(ref *to, const ref *from, uint size));
extern void refcpy_to_old(P4(ref *to, const ref *from, uint size, const char *client_name));
/* Fill an array with nulls. */
extern void refset_null(P2(ref *to, uint size));

/* Compare two objects for equality.  Return 1 if equal, 0 if not. */
extern int obj_eq(P2(const ref *, const ref *));

/* Create a printable representation of an object, a la cvs. */
/* Return 0 if OK, <0 if the destination wasn't large enough. */
extern int obj_cvs(P4(const ref *, byte *, uint, uint *));

/* ------ String utilities ------ */

/* Convert a C string to a Ghostscript string */
extern int string_to_ref(P3(const char *, ref *, const char *));

/* Convert a Ghostscript string to a C string. */
/* Return 0 iff the buffer can't be allocated. */
extern char *ref_to_string(P2(const ref *, const char *));

/* ------ Operand utilities ------ */

/* Get N numeric operands from the stack. */
/* Note that the first argument must be ref * rather than os_ptr, */
/* because num_params is sometimes used elsewhere than */
/* on the operand stack. */
extern int num_params(P3(const ref *, int, float *));

/* Get a real parameter. */
extern int real_param(P2(const ref *, float *));

/* Check for a matrix operand with read access. */
struct gs_matrix_s;
extern int read_matrix(P2(const ref *, struct gs_matrix_s *));

/* Check for a matrix operand with write access. */
extern int write_matrix(P1(ref *));
