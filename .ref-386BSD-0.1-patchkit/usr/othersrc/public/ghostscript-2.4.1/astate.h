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

/* astate.h */
/* State structures for the Ghostscript allocator */

/* Define pointers to an allocation state. */
/****** Note the _ds (for Turbo C only). ******/
typedef struct alloc_state_s alloc_state;
typedef alloc_state _ds *alloc_state_ptr;
/* The only instance.... */
extern alloc_state_ptr alloc_state_current;

/* Round up sizes of aligned objects. */
#define log2_align_mod 3		/* log2(sizeof(double)) */
#define align_mod (1<<log2_align_mod)
#define align_mask (align_mod-1)
#define align_round(siz) (uint)(((siz) + align_mask) & -align_mod)

/* Max object size for separate free list */
#define max_chain_size 350

/* Structure for a separately allocated block. */
typedef struct alloc_block_s alloc_block;
struct alloc_block_s {
	alloc_block *next;
	uint size;
	int save_level;
	alloc_state_ptr cap;
};
#define alloc_block_size align_round(sizeof(alloc_block))

/* Structure for a single wholesale allocation 'chunk'. */
typedef struct alloc_chunk_s alloc_chunk;
struct alloc_chunk_s {
	/* Note that allocation takes place both from the bottom up */
	/* (aligned objects) and from the top down (byte objects). */
	byte *base;
	byte *bot;			/* bottom of free area */
					/* (top of aligned objects) */
	byte *top;			/* top of free area */
					/* (bottom of byte objects) */
	byte *limit;
	int save_level;			/* save level when this chunk */
					/* was allocated */
	alloc_chunk *next;		/* chain chunks together */
};

#define ptr_is_in_chunk(ptr, chunk)\
  ptr_between(ptr, (chunk)->base, (chunk)->limit)

/* Structures for save/restore (not defined here). */
struct alloc_save_s;
struct alloc_change_s;

/* Structure for allocator state.  If we multi-thread some day, */
/* this might be instantiated more than once. */
struct alloc_state_s {
	alloc_chunk current;		/* the current chunk */
#define cbase current.base
#define cbot current.bot
#define ctop current.top
#define climit current.limit
	alloc_chunk *current_ptr;	/* where to put current */
	uint chunk_size;		/* unit for wholesale malloc */
	uint big_size;			/* min size for separate malloc */
	proc_alloc_t palloc;		/* proc for malloc */
	proc_free_t pfree;		/* proc for free */
	alloc_chunk *last_freed;	/* cache the last non-current chunk */
					/* at the current save level */
					/* where we freed an object */
	/* Statistics */
	long used;			/* total space used, including */
					/* malloc'ed blocks and all chunks */
					/* other than the current one */
	long total;			/* total space allocated, */
					/* other than malloc'ed blocks */
	unsigned num_chunks;
	/* Chain together freed objects within a save level. */
	/* We only do this for aligned objects. */
#define num_free_chains ((max_chain_size >> log2_align_mod) + 1)
	char *free[num_free_chains];
	/* Chain together any malloc'ed objects */
	alloc_block *malloc_chain;
	/* Keep track of saved states */
	int save_level;
	struct alloc_save_s *saved;
	byte *saved_cbot;		/* cbot at last save */
	byte *saved_ctop;		/* ctop at last save */
	struct alloc_change_s *changes;
};
