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

/* ialloc.c */
/* Memory allocator for Ghostscript interpreter */
#include <stdio.h>		/* for NULL */
#include "gs.h"
#include "memory_.h"
#include "alloc.h"
#include "astate.h"

#ifdef DEBUG
extern char gs_debug[128];
#endif

/* Forward references */
private int alloc_add_chunk(P2(alloc_state_ptr, uint));
private char *alloc_large(P3(alloc_state_ptr, uint, const char *));
private void alloc_free_large(P3(char *, uint, const char *));

/* The only allocator instance (for now). */
private alloc_state as_current;
alloc_state_ptr alloc_state_current = &as_current;

/* Debugging printout */
#ifdef DEBUG
#  define alloc_print(rtag, tag, blk, sz)\
	if ( gs_debug['A'] )\
	  fprintf(gs_debug_out, "[a:%c:%c:%s] %lx(%u)\n", rtag, tag,\
		  client_name, (ulong)blk, sz)
#  define alloc_print_large(rtag, tag, blk, sz)\
	if ( gs_debug['A'] | gs_debug['a'] )\
	  fprintf(gs_debug_out, "[a:%c:%c:%s] %lx(%u)\n", rtag, tag,\
		  client_name, (ulong)blk, sz)
#else
#  define alloc_print(rtag, tag, blk, sz)
#  define alloc_print_large(rtag, tag, blk, sz)
#endif

/* ------ Initialize/status ------ */

/* Initialize the allocator */
void
alloc_init(proc_alloc_t palloc, proc_free_t pfree, uint chunk_size)
{	register alloc_state_ptr ap = &as_current;
	memset(ap, 0, sizeof(alloc_state));	/* do it all at once */
	ap->chunk_size = chunk_size;
	ap->big_size = chunk_size / 4;
	ap->palloc = palloc;
	ap->pfree = pfree;
	ap->last_freed = 0;
	   {	extern void alloc_save_init(P0());
		alloc_save_init();
	   }
}

/* Return the status of the allocator: space used, total space. */
void
alloc_status(long *pused, long *ptotal)
{	register alloc_state_ptr ap = &as_current;
	*pused = (ap->cbot - ap->cbase) + (ap->climit - ap->ctop) + ap->used;
	*ptotal = ap->total;
}

/* ------ Allocation and freeing ------ */

/* Allocate an object.  Return 0 if not enough room. */
char *
alloc(uint num_elts, uint elt_size, const char *client_name)
{	register alloc_state_ptr ap = &as_current;
	uint size = num_elts * elt_size;
	uint block_size;
	uint left;
	if ( size >= ap->big_size )
	   {	/* Large object, do a separate malloc. */
		char *block = alloc_large(ap, size, client_name);
		if ( block != NULL ) return block;
		if ( size > ap->chunk_size )
			return 0;	/* can't alloc */
	   }
	block_size = align_round(size);
	if ( block_size <= max_chain_size )
	   {	/* See if we can use a freed block. */
		char **fptr = &ap->free[block_size >> log2_align_mod];
		char *block = *fptr;
		if ( block != 0 )
		   {	*fptr = *(char **)block;
			alloc_print('+', '#', block, size);
			return block;
		   }
	   }
	left = ap->ctop - ap->cbot;
	if ( block_size > left )
	   {	uint csize = ap->chunk_size;
		while ( !alloc_add_chunk(ap, csize) )
		   {	alloc_print('+', '?', (ulong)0, size);
			/* Things are desperate, but perhaps not hopeless. */
			if ( (csize >>= 1) < block_size )
				return 0;	/* no hope */
		   }
	   }
	if ( elt_size == 1 )
	   {	/* Unaligned block */
		ap->ctop -= size;
		alloc_print('+', '>', ap->ctop, size);
		return (char *)ap->ctop;
	   }
	else
	   {	/* Aligned block */
		char *block = (char *)ap->cbot;
		ap->cbot += block_size;
		alloc_print('+', '<', block, size);
		return block;
	   }
}

/* Free an object, if possible. */
/* Note that if a save is in effect, objects in chunks older than */
/* the save, and objects allocated with malloc before the save, */
/* must not be freed. */
void
alloc_free(char *cobj, uint num_elts, uint elt_size, const char *client_name)
{	register alloc_state_ptr ap = &as_current;
	uint size = num_elts * elt_size;
	uint block_size;
	if ( size >= ap->big_size )
	   {	/* Object was allocated with malloc. */
		alloc_free_large(cobj, size, client_name);
		return;
	   }
#define obj ((byte *)cobj)
	else if ( obj == ap->ctop )
	   {	/* Don't free the object if we're in a save and */
		/* this object wasn't allocated since the save. */
		if ( ap->save_level == 0 ||
		     ap->current.save_level >= ap->save_level ||
		     /* We know the current chunk is the same as */
		     /* the one in as->saved->state */
		     obj < ap->saved_ctop
		   )
			ap->ctop += size;
		alloc_print('-', '>', obj, size);
		return;
	   }
	else if ( obj + (block_size = align_round(size)) == ap->cbot )
	   {	/* Freeing an aligned object.  Same check. */
		if ( ap->save_level == 0 ||
		     ap->current.save_level >= ap->save_level ||
		     obj >= ap->saved_cbot
		   )
			ap->cbot = obj;
		alloc_print('-', '<', obj, size);
		return;
	   }
	else if ( !ptr_is_in_chunk(obj, &ap->current) )
	   {	/* In another segment, check its save level. */
		int level = ap->save_level;
		alloc_chunk *cp = ap->last_freed;
		if ( cp != 0 && ptr_is_in_chunk(obj, cp) )  /* cache hit */
		 { if ptr_lt(obj, cp->bot) goto pxf;
		   else goto pnf;
		 }
		for ( cp = ap->current.next; cp != 0; cp = cp->next )
		 { switch ( cp->save_level - level )
		    {
		    case 0:
		      if ( ptr_is_in_chunk(obj, cp) )
		       { if ( ptr_lt(obj, cp->bot) ) goto pbf;
		         else goto pnf;
		       }
		      else continue;
		    case -1:
		      /* Might be alloc'ed since the save, */
		      /* or might not be aligned. */
		      if ( ptr_lt(obj, ap->saved_cbot) ) goto pbf;
		    }
		 }
pnf:		/* Older save level or unaligned, not freeable. */
		alloc_print('-', '\\', obj, size);
		return;
pbf:		/* If we get here, OK to put the block on a free list. */
		ap->last_freed = cp;
pxf:		;
	   }
	else if ( obj >= ap->cbot )	/* not aligned object, punt */
	   {	alloc_print('-', '~', obj, size);
		return;
	   }
	/* Put on a free list if small enough */
	alloc_print('-', '#', obj, size);
	if ( block_size <= max_chain_size && block_size >= sizeof(char **) )
	   {	char **fptr = &ap->free[block_size >> log2_align_mod];
		*(char **)cobj = *fptr;
		*fptr = cobj;
	   }
#undef obj
}

/* Grow an object.  This may require allocating a new copy. */
/* Return 0 if not enough room. */
/****** Note: the object must have been allocated at
 ****** the current save level. */
byte *
alloc_grow(byte *obj, uint old_num, uint new_num, uint elt_size,
  const char *client_name)
{	register alloc_state_ptr ap = &as_current;
	uint old_size = old_num * elt_size;
	uint new_size = new_num * elt_size;
	byte *nobj;
	if ( new_size == old_size ) return obj;
	if ( new_size < ap->big_size ) /* try to grow in place */
	  { uint old_block_size;
	    uint new_block_size;
	    if ( obj == ap->ctop )
	      { /* Might be able to grow in place */
		uint diff = new_size - old_size;
		if ( diff <= ap->ctop - ap->cbot )
		  { alloc_print('>', '>', obj, new_size);
		    ap->ctop -= diff;
		    memcpy(ap->ctop, obj, old_size);
		    return ap->ctop;
		  }
	      }
	    old_block_size = align_round(old_size);
	    new_block_size = align_round(new_size);
	    if ( obj + old_block_size == ap->cbot )
	      { /* Might be able to grow in place */
		uint diff = new_block_size - old_block_size;
		if ( diff <= ap->ctop - ap->cbot )
		  { alloc_print('>', '<', obj, new_size);
		    ap->cbot += diff;
		    return obj;
		  }
	      }
	  }
	/* Can't grow in place.  Allocate a new object and copy. */
	nobj = (byte *)alloc(new_num, elt_size, client_name);
	if ( nobj == 0 )
		return 0;
	memcpy(nobj, obj, old_size);
	alloc_free((char *)obj, old_num, elt_size, client_name);
	alloc_print('>', '&', obj, new_size);
	return nobj;
}

/* Shrink an object. */
/****** Note: the object must have been allocated at
 ****** the current save level. */
byte *
alloc_shrink(byte *obj, uint old_num, uint new_num, uint elt_size,
  const char *client_name)
{	register alloc_state_ptr ap = &as_current;
	uint old_size = old_num * elt_size;
	uint new_size = new_num * elt_size;
	if ( new_size == old_size ) return obj;
	if ( old_size >= ap->big_size )
	  { /* Allocate a new block. */
	    byte *nobj = (byte *)alloc(new_num, elt_size, client_name);
	    if ( nobj == 0 ) return obj; /* can't shrink, leave as is */
	    memcpy(nobj, obj, new_size);
	    alloc_free((char *)obj, old_num, elt_size, client_name);
	    alloc_print('<', '&', obj, new_size);
	    return nobj;
	  }
	else if ( obj == ap->ctop )
	  { /* Move the object up in place. */
	    /* memcpy doesn't do this properly. */
	    register byte *from = obj + new_size;
	    register byte *to = obj + old_size;
	    while ( from > obj ) *--to = *--from;
	    obj = ap->ctop = to;
	  }
	else
	  { uint new_block_size = align_round(new_size);
	    alloc_free((char *)(obj + new_block_size),
		       1, align_round(old_size) - new_block_size,
		       "alloc_shrink");
	  }
	alloc_print('<', ' ', obj, new_size);
	return obj;
}

/* ------ Private routines ------ */

/* Allocate (with malloc) an object too large to be put in a chunk. */
/* Return NULL on failure. */
private char *
alloc_large(alloc_state_ptr ap, uint size, const char *client_name)
{	alloc_block *mblock = (alloc_block *)
		(*ap->palloc)(1, alloc_block_size + size, client_name);
	char *block;
	if ( mblock == NULL ) return NULL;
	block = (char *)mblock + alloc_block_size;
   	alloc_print_large('+', '*', block, size);
	mblock->next = ap->malloc_chain;
	mblock->size = size;
	mblock->save_level = ap->save_level;
	mblock->cap = ap;
	ap->malloc_chain = mblock;
	ap->used += size;
	ap->total += size;
	return block;
}

/* Allocate a new chunk.  Return true if successful. */
#define chunk_head_size align_round(sizeof(alloc_chunk))
private int
alloc_add_chunk(register alloc_state_ptr ap, uint csize)
{	char *space =
		(*ap->palloc)(1, chunk_head_size + csize, "alloc chunk");
	long discard;
	if ( space == NULL )
		return 0;
	ap->num_chunks++;
	/* Accumulate statistics */
	ap->total += csize;
	alloc_status(&ap->used, &discard);
	/* Stash the state of the old chunk */
	if ( ap->current_ptr != 0 )	/* check for very first chunk */
		*ap->current_ptr = ap->current;
	/* Initialize the new chunk */
	ap->cbase = ap->cbot = (byte *)space + chunk_head_size;
	ap->climit = ap->ctop = ap->cbot + csize;
	ap->current.next = ap->current_ptr;
	ap->current.save_level = ap->save_level;
	ap->current_ptr = (alloc_chunk *)space;
	return 1;
}
#undef chunk_head_size

/* Free a large object (allocated with malloc). */
private void
alloc_free_large(char *cobj, uint size, const char *client_name)
{	alloc_block **prev;
	alloc_block *mblock = (alloc_block *)(cobj - alloc_block_size);
	alloc_state_ptr ap = mblock->cap;
	if ( mblock->save_level == ap->save_level )
	 for ( prev = &ap->malloc_chain; *prev != 0; prev = &mblock->next )
	   {	mblock = *prev;
		if ( (char *)mblock + alloc_block_size == cobj )
		   {	*prev = mblock->next;
			ap->used -= size;
			ap->total -= size;
			(*ap->pfree)((char *)mblock,
				    1, size + alloc_block_size,
				    "large object");
			alloc_print_large('-', '*', cobj, size);
			return;
		   }
	   }
	alloc_print('-', '?', cobj, size);
}
