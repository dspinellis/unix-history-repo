/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* nmman.h */
/* New memory manager internal definitions for Ghostscript */

/* ====== Allocator ====== */

/* Define the alignment modulus for aligned objects. */
#define log2_align_mod 3		/* big enough for a double */
					/* and for a struct_header */
#define align_mod (1 << log2_align_mod)
#define align_mask (align_mod-1)
#define align_round(siz) (uint)(((siz) + align_mask) & -align_mod)

/* Structure header. */
/* There are 3 different formats, as indicated below. */
/* Note that we force the size of a structure header to be align_mod. */
typedef union struct_header_s struct_header_t;
union struct_header_s {
  struct {
    unsigned mark : 1, large : 1, bytes : 1;
  } h;					/* all formats */
	/* Ordinary typed structure */
  struct {
    unsigned _ : 3, reloc8 : 13;
    at_ptr_t type;
  } s;					/* large = 0, bytes = 0 */
	/* Plain (but aligned) bytes */
  struct {
    unsigned _ : 3, reloc8 : 13;
    ushort size;
  } b;					/* large = 0, bytes = 1 */
	/* Large bytes (in a chunk by themselves) */
  struct {
    unsigned _ : 3, lsize : 13;
    ushort size;
  } l;					/* large = 1, bytes = 1 */
	/* Force size up to align_mod */
  byte _[align_mod];
};
#define struct_size(shp)\
  ((shp)->h.bytes ? (shp)->b.size : (shp)->s.type->size)
#define struct_large_size(shp)\
  (((ulong)(shp)->l.lsize << 16) + (shp)->l.size)
#define struct_next(shp)\
  ((struct_header_t *)((byte *)(shp) + struct_size(shp)))

/* ====== Chunks ====== */

/* Chunks are "objects", i.e. they have a header that identifies */
/* which implementation is being used. */
typedef struct chunk_s chunk_t;
typedef struct chunk_locator_s chunk_locator_t;
typedef chunk_locator_t _ss *cl_ptr_t;

#define declare_chunk_procs(dscope, ctype, init, status, gc_init, gc_trace_from_marked, gc_set_reloc, gc_do_reloc, gc_compact)\
\
		/* Initialize the chunk. */\
	dscope void init(P3(ctype *, byte *, usize_t));\
\
		/* Return the space allocated and used. */\
	dscope void status(P2(ctype *, alloc_status_t *));\
\
	/**** The rest of the procedures are only for the GC. ****/\
\
		/* Initialize for a GC by clearing marks. */\
	dscope void gc_init(P1(ctype *));\
\
		/* Trace from all marked pointers. */\
		/* Return true if any new marks. */\
	dscope bool gc_trace_from_marked(P1(ctype *));\
\
		/* Compute and store relocation amounts. */\
	dscope void gc_set_reloc(P2(ctype *, cl_ptr_t));\
\
		/* Relocate pointers. */\
	dscope void gc_do_reloc(P2(ctype *, cl_ptr_t));\
\
		/* Compact to remove unmarked components. */\
	dscope void gc_compact(P1(ctype *))

#define chunk_procs_struct(ctype)\
  struct {\
    declare_chunk_procs(, ctype, (*init), (*status), (*gc_init), (*gc_trace_from_marked), (*gc_set_reloc), (*gc_do_reloc), (*gc_compact));\
  }
typedef chunk_procs_struct(chunk_t) chunk_procs_t;
#define chunk_common\
  chunk_procs_t _ds *procs;\
  byte *cbot, *ctop;\
  chunk_t *cprev, *cnext		/* sorted by address */
struct chunk_s {
  chunk_common;
};

/* Find the chunk for a pointer. */
#define ptr_is_in_chunk(ptr, cp)\
  ptr_between((byte *)ptr, (cp)->cbot, (cp)->ctop)
struct chunk_locator_s {
  chunk_t *cp;				/* one-element cache */
};
extern bool chunk_locate_ptr(P2(byte *, cl_ptr_t));
#define chunk_locate(ptr, clp)\
  (ptr_is_in_chunk((byte *)(ptr), (clp)->cp) ||\
   chunk_locate_ptr((byte *)(ptr), clp))

/* ------ Concrete chunks ------ */

/* Many small structures. */
typedef struct {
  chunk_common;
  struct_header_t *top;
} chunk_structs_t;

/* One large structure. */
typedef struct {
  chunk_common;
} chunk_large_struct_t;

/* Refs and strings. */
typedef struct {
  chunk_common;
  struct ref_s *rtop;
  byte *ibot, *itop;
  /* Rest is for GC */
  byte *ibits;
  byte *ibase;
  usize_t ibitsize;
  ushort *ireloc;
  uint imove;
} chunk_refs_t;

/* ====== Definition of allocator state ====== */

typedef struct std_alloc_state_s std_alloc_state_t;
struct std_alloc_state_s {
  alloc_state_common;
  chunk_structs_t ccs;			/* current structs chunk */
  chunk_structs_t *pccs;		/* where to put ccs */
  chunk_refs_t ccrs;			/* current refs chunk */
  chunk_refs_t *pccrs;			/* where to put ccrs */
  uint chunk_size;
  uint large_size;			/* min size for large chunk */
  gc_root_t *roots;
  chunk_t chunk_min, chunk_max;		/* head and tail of chunk list */
};
