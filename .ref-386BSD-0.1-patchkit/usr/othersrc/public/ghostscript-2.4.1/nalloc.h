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

/* nalloc.h */
/* Library client interface for new Ghostscript allocator */

/* Define the type for the size of an object. */
typedef uint usize_t;

/* Define the type for identifying an allocator client. */
typedef char _ds *client_name_t;

/* Define the type for an allocator instance. */
struct alloc_state_s;
typedef struct alloc_state_s _ds *as_ptr_t;

/* Define the type for a structure descriptor. */
struct struct_type_s;
typedef struct struct_type_s _ds *at_ptr_t;

/* Define the type for allocator statistics. */
typedef struct alloc_status_s {
	ulong allocated;
	ulong used;
} alloc_status_t;

/* Define the allocator client procedures */
typedef struct alloc_procs_s alloc_procs_t;
struct alloc_procs_s {

		/* Allocate a structure */
#define alloc_proc_alloc_struct(proc)\
  void *proc(P3(as_ptr_t, at_ptr_t, client_name_t))
	alloc_proc_alloc_struct((*alloc_struct));
#define gs_alloc_struct(ap,tp,cn) (*(ap)->procs.alloc_struct)(ap,tp,cn)

		/* Allocate (aligned) bytes */
#define alloc_proc_alloc_bytes(proc)\
  byte *proc(P4(as_ptr_t, uint, uint, client_name_t))
	alloc_proc_alloc_bytes((*alloc_bytes));
#define gs_alloc_bytes(ap,es,n,cn) (*(ap)->procs.alloc_bytes)(ap,es,n,cn)

		/* Change the size of a byte object */
#define alloc_proc_resize(proc)\
  byte *proc(P5(as_ptr_t, byte *, uint, uint, client_name_t))
	alloc_proc_resize((*resize));
#define gs_resize(ap,p,es,n,cn) (*(ap)->procs.resize)(ap,p,es,n,cn)

		/* Free an object (structure or bytes) */
#define alloc_proc_free(proc)\
  void proc(P3(as_ptr_t, void *, client_name_t))
	alloc_proc_free((*free));
#define gs_free(ap,p,cn) (*(ap)->procs.free)(ap,p,cn)

		/* Report status (assigned, used) */
#define alloc_proc_status(proc)\
  void proc(P2(as_ptr_t, alloc_status_t *))
	alloc_proc_status((*status));
#define gs_alloc_status(ap,s) (*(ap)->procs.status)(ap,s)

};

/* Define the generic allocator state. */
/* "Subclasses" will extend this. */
#define alloc_state_common\
	alloc_procs_t procs
struct alloc_state_s {
	alloc_state_common;
};

/* Define the standard Ghostscript allocator implementation. */
extern alloc_procs_t gs_alloc_std_procs;

/* ====== Root-registering clients ====== */
union struct_header_s;
struct ref_s;

/* Define the type for a pointer descriptor. */
typedef struct ptr_procs_s {

		/* Mark the referent of a pointer. */
#define ptr_proc_mark(proc)\
  bool proc(P1(void *))
	ptr_proc_mark((*mark));

		/* Relocate a pointer. */
#define ptr_proc_reloc(proc)\
  void *proc(P1(void *))
	ptr_proc_reloc((*reloc));

} ptr_procs_t;
typedef ptr_procs_t _ds *ptr_type_t;

/* Define the pointer type for ordinary structure pointers. */
extern ptr_procs_t ptr_struct_procs;
#define ptr_struct_type (&ptr_struct_procs)

/* Define the type for a GC root. */
typedef struct gc_root_s gc_root_t;
struct gc_root_s {
  gc_root_t *next;
  ptr_type_t ptype;
  void **p;
};

/* Register/unregister a root. */
void gs_struct_root_register(P3(as_ptr_t, gc_root_t *, union struct_header_s **));
void gs_root_unregister(P2(as_ptr_t, gc_root_t *));

/* ====== Structure-defining clients ====== */
/* These are clients who define new types of structure. */

/* Object contents enumerator type */
#define mark_enum_proc(proc)\
  ptr_type_t proc(P3(void *ptr, uint index, void ***pep))

/* Object type */
typedef struct struct_type_s {
  usize_t size;
  char _ds *sname;
  mark_enum_proc((*gc_mark_proc));
  void (*pre_gc_proc)(P1(void *));
  void (*post_gc_proc)(P1(void *));
  void (*finalize_proc)(P1(void *));
} struct_type_t;
/* Default object procedures */
extern	mark_enum_proc(no_gc_mark_proc);
extern	void	no_pre_gc_proc(P1(void *)),
		no_post_gc_proc(P1(void *)),
		no_finalize_proc(P1(void *));
