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

/* nsave.h */
/* Interpreter client interface for new Ghostscript allocator */
/* Requires nalloc.h and ghost.h */

/* The routines here parallel those in nalloc.h, but deal with */
/* refs and (unaligned) strings. */

/* ====== Ordinary clients ====== */
/* These are clients who just allocate and deallocate objects. */

/* Allocate/free a string. */
extern byte *gs_alloc_string(P3(as_ptr_t, uint, client_name_t));
extern void gs_free_string(P4(as_ptr_t, byte *, uint, client_name_t));

/* Allocate/free refs. */
extern ref *gs_alloc_refs(P3(as_ptr_t, uint, client_name_t));
extern void gs_free_refs(P4(as_ptr_t, ref *, uint, client_name_t));

/* ====== Root-registering clients ====== */

/* Register a ref root. */
void gs_alloc_ref_root_register(P3(as_ptr_t, gc_root_t *, ref **));
