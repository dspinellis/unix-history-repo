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

/* isave.c */
/* Save/restore machinery for Ghostscript */
#include "ghost.h"
#include "memory_.h"
#include "alloc.h"
#include "astate.h"
#include "name.h"
#include "packed.h"
#include "save.h"
#include "store.h"			/* for ref_assign */

/* Imported restore routines */
extern void file_restore(P1(alloc_save *));
extern void font_restore(P1(alloc_save *));

/*
 * The logic for saving and restore the state is rather subtle.
 * Both the changes to individual objects, and the overall state
 * of the memory manager, must be saved and restored.
 */

/*
 * To save the state of the memory manager:
 *	Save the state of the current chunk in which we are allocating.
 *	Save the identity of the current chunk.
 *	Save and reset the malloc chain and the orphan block chains.
 * By doing this, we guarantee that no object older than the save
 * can be freed.
 *
 * To restore the state of the memory manager:
 *	Free all chunks newer than the save.
 *	Free all malloc'ed blocks newer than the save.
 *	Make current the chunk that was current at the time of the save.
 *	Free all objects allocated in the current chunk since the save.
 */

/*
 * For saving changes to individual objects, we add an "attribute" bit
 * (l_new) that logically belongs to the slot where the descriptor is stored,
 * not to the descriptor itself.  The bit means "the contents
 * of this slot have been changed since the last save."
 * To keep track of changes since the save, we associate a chain of
 * <slot, old_contents> pairs that remembers the old contents of slots.
 *
 * When creating an object, if the save level is non-zero:
 *	Set the bit in all slots.
 *
 * When storing into a slot, if the save level is non-zero:
 *	If the bit isn't set, save the address and contents of the slot
 *	  on the current contents chain.
 *	Set the bit after storing the new value.
 *
 * To do a save:
 *	If the save level is non-zero:
 *		Reset the bit in all slots on the contents chain, and in all
 *		  objects created since the previous save.
 *	Push the head of contents chain, and reset the chain to empty.
 *
 * To do a restore:
 *	Check all the stacks to make sure they don't contain references
 *	  to objects created since the save.
 *	Restore all the slots on the contents chain.
 *	Pop the contents chain head.
 *	If the save level is now non-zero:
 *		Scan the newly restored contents chain, and set the bit in all
 *		  the slots it references.
 *		Scan all objects created since the previous save, and set the
 *		  bit in all the slots of each object.
 */

/* Declare the mask for checking stores. */
/* This is -1 if we are not in a save, 0 if we are in a save. */
int alloc_save_test_mask;
/* Declare the mask for tagging new objects. */
/* This is 0 if we are not in a save, l_new if we are in a save. */
int alloc_save_new_mask;
#define set_in_save()\
  (alloc_save_test_mask = 0, alloc_save_new_mask = l_new)
#define set_not_in_save()\
  (alloc_save_test_mask = -1, alloc_save_new_mask = 0)

/* Structure for saved change chain for save/restore. */
/* If where = 0, contents is a t_array that refers to */
/* a newly allocated object; if contents.value.refs is 0, */
/* this is a free record (possible since we allocate them two at a time.) */
/* We merge adjacent objects to reduce */
/* the need to allocate alloc_change records. */
struct alloc_change_s {
	alloc_change *next;
	ref *where;
	ref contents;
};
#define alloc_change_is_free(cp)\
  ((cp)->where == 0 && (cp)->contents.value.refs == 0)

/*
 * Macro to allocate a pair of change records.
 * Must be used in the following form:
 *	if_not_alloc_change_pair(cp, ap, cname)
 *	 { ... failure code ...
 *	 }
 */
#define if_not_alloc_change_pair(cp, ap, cname)\
  cp = (alloc_change *)alloc(2, sizeof(alloc_change), cname);\
  if ( cp != 0 )\
   { cp->next = ap->changes;\
     cp[1].next = cp;\
     cp[1].where = 0;\
     cp[1].contents.value.refs = 0;\
     r_set_size(&cp[1].contents, 0);\
     ap->changes = cp + 1;\
   }\
  else

/* Saved state of allocator and other things as needed. */
struct alloc_save_s {
	alloc_state state;
	alloc_state_ptr cap;
	uint name_cnt;
};

/* Debugging printout */
#ifdef DEBUG
private void
alloc_save_print(alloc_change *cp)
{	dprintf1(" %lx:", (ulong)cp);
	if ( cp->where )
	  dprintf4(" %lx: %x %x %lx\n", (ulong)cp->where,
		   r_type_attrs(&cp->contents), r_size(&cp->contents),
		   (ulong)cp->contents.value.intval);
	else
	  dprintf2(" %lx(%u)\n", (ulong)cp->contents.value.refs,
		   r_size(&cp->contents));
}
#endif

/* Forward references */
private void save_set_new(P2(alloc_state_ptr, int));

/* Initialize the save/restore machinery. */
void
alloc_save_init()
{	set_not_in_save();
}

/* Save the state. */
alloc_save *
alloc_save_state()
{	register alloc_state_ptr ap = alloc_state_current;
	alloc_save *save =
		(alloc_save *)alloc(1, sizeof(alloc_save),
				    "alloc_save_state");
	if ( save == 0 ) return 0;
	save->state = *ap;
	save->cap = ap;
	save->name_cnt = name_count();
	/* Reset the l_new attribute in all slots.  The only slots that */
	/* can have the attribute set are the ones on the changes chain. */
	save_set_new(ap, 0);
	/* Clear the free chains, to prevent old objects from being freed. */
	memset(&ap->free[0], 0, num_free_chains * sizeof(char *));
	ap->malloc_chain = 0;
	ap->saved = save;
	ap->save_level++;
	ap->changes = 0;
	ap->saved_cbot = ap->cbot;
	ap->saved_ctop = ap->ctop;
	/* Clear the last_freed cache, because the cache pointer */
	/* must point to a chunk at the current save level. */
	ap->last_freed = 0;
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf3("[u]save at %lx: cbot=%lx ctop=%lx\n", (ulong)save,
		 (ulong)ap->cbot, (ulong)ap->ctop);
   }
#endif
	set_in_save();
	return save;
}

/* Allocate a ref-containing object and record it as new. */
ref *
alloc_refs(uint num_refs, const char *client_name)
{	register alloc_state_ptr ap = alloc_state_current;
	register alloc_change *cp;
	register ref *obj = (ref *)alloc(num_refs, sizeof(ref), client_name);
	if ( obj == 0 ) return 0;
	if ( ap->save_level == 0 ) /* no saving */
	  return obj;
	cp = ap->changes;
	if ( cp != 0 && cp->where == 0 && cp->contents.value.refs != 0 &&
	     obj == cp->contents.value.refs + r_size(&cp->contents) &&
	     /* Don't create a single block that is large enough to */
	     /* mislead the allocator into thinking it was allocated */
	     /* with a single malloc. */
	     r_size(&cp->contents) + num_refs < ap->big_size / sizeof(ref)
	   )
	   {	/* Merge adjacent allocations. */
		r_inc_size(&cp->contents, num_refs);
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf1("[u]alloc_refs %s merge", client_name);
	alloc_save_print(cp);
   }
#endif
	   }
	else
	   {	if ( cp == 0 || !alloc_change_is_free(cp) )
		   {	/* Allocate a pair of entries. */
			if_not_alloc_change_pair(cp, ap, "alloc_refs")
			   {
				alloc_free((char *)obj, num_refs, sizeof(ref),
					   client_name);
				return 0;
			   }
		   }
		cp->where = 0;
		r_set_size(&cp->contents, num_refs);
		cp->contents.value.refs = obj;
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf1("[u]alloc_refs %s", client_name);
	alloc_save_print(cp);
   }
#endif
	   }
	return obj;
}

/* Deallocate a ref-containing object.  This is a dummy for now. */
void
alloc_free_refs(ref *ptr, uint num_refs, const char *client_name)
{
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf3("[u]alloc_free_refs (%lx,%d) %s\n",
		 (ulong)ptr, num_refs, client_name);
   }
#endif
}


/* Record a state change that must be undone for restore, */
/* and mark it as having been saved. */
/* This can only be called if we are in a save. */
int
alloc_save_change(ref *where, const char *client_name)
{	register alloc_state_ptr ap = alloc_state_current;
	register alloc_change *cp;
	if ( ap->save_level == 0 ) return 0;	/* no saving */
	cp = ap->changes;
	if ( cp == 0 || !alloc_change_is_free(cp) )
	   {	/* Allocate a pair of entries. */
		if_not_alloc_change_pair(cp, ap, "alloc_save_change")
		   {	return -1;
		   }
	   }
	cp->where = where;
	ref_assign(&cp->contents, where);
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf1("[u]save %s", client_name);
	alloc_save_print(cp);
   }
#endif
	if ( !r_is_packed(where) ) r_set_attrs(where, l_new);
	return 0;
}

/* Return the current save level */
int
alloc_save_level()
{	return alloc_state_current->save_level;
}

/* Test whether a reference would be invalidated by a restore. */
int
alloc_is_since_save(char *ptr, alloc_save *save)
{
	/* A reference can postdate a save in one of three ways: */
	/*	- It is in the chunk that was current at the time */
	/*	    of the save, and allocated more recently. */
	/*	- It is in a chunk allocated since the save; */
	/*	- It was malloc'ed since the save; */

	register alloc_state_ptr ap = save->cap;

#ifdef DEBUG
if ( gs_debug['U'] )
	dprintf2("[U]is_since_save %lx, %lx:\n", (ulong)ptr, (ulong)save);
#endif

	/* Check against current chunk at the time of the save */
	if ( ptr_is_in_chunk(ptr, &save->state.current) )
	   {	/* In the chunk, check against allocation pointers */
		/* at the time of the save */
#ifdef DEBUG
if ( gs_debug['U'] )
		dprintf2("[U?]  current chunk %lx, %lx\n",
			 (ulong)save->state.cbot, (ulong)save->state.ctop);
#endif
		return ( (ptr_ord_t)ptr >= (ptr_ord_t)save->state.cbot &&
			 (ptr_ord_t)ptr < (ptr_ord_t)save->state.ctop );
	   }

	/* Check against chunks allocated since the save */
	   {	alloc_chunk *chunk = &ap->current;
		while ( chunk->save_level > save->state.save_level )
		   {	if ( ptr_is_in_chunk(ptr, chunk) )
			   {
#ifdef DEBUG
if ( gs_debug['U'] )
		dprintf3("[U+]  new chunk %lx: %lx, %lx\n", chunk,
			 (ulong)chunk->base, (ulong)chunk->limit);
#endif
				   return 1;
			   }
			chunk = chunk->next;
		   }
	   }

	/* Check the malloc chains since the save */
	   {	alloc_state *asp = ap;
		for ( ; asp != &save->state; asp = &asp->saved->state )
		   {	alloc_block *mblk = asp->malloc_chain;
			for ( ; mblk != 0; mblk = mblk->next )
			  if ( alloc_block_size + (char *)mblk == ptr )
			   {
#ifdef DEBUG
if ( gs_debug['U'] )
				dprintf("[U+]  malloc'ed\n");
#endif
				return 1;
			   }
		   }
	   }

	/* Not in any of those places, must be OK. */
	return 0;
}

/* Test whether a name would be invalidated by a restore. */
int
alloc_name_is_since_save(ref *pnref, alloc_save *save)
{	return name_is_since_count(pnref, save->name_cnt);
}

/* Validate a saved state pointer. */
int
alloc_restore_state_check(alloc_save *save)
{	alloc_save *sprev = save->cap->saved;
	while ( sprev != save )
	   {	if ( sprev == 0 ) return -1;	/* not on chain */
		sprev = sprev->state.saved;
	   }
	return 0;
}

/* Restore the state.  The client is responsible for calling */
/* alloc_restore_state_check first, and for ensuring that */
/* there are no surviving pointers for which alloc_is_since_save is true. */
void
alloc_restore_state(alloc_save *save)
{	register alloc_state_ptr ap = save->cap;
	alloc_save *sprev;

#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf1("[u]restore from %lx\n", (ulong)save);
   }
#endif

	/* Iteratively restore the state */
	do
	  { sprev = ap->saved;

	    /* Close inaccessible files. */
	    file_restore(save);

	    /* Remove entries from font and character caches. */
	    font_restore(save);

	    /* Adjust the name table. */
	    name_restore(sprev->name_cnt);

	    /* Undo changes since the save. */
	    { alloc_change *cp = ap->changes;
	      while ( cp )
		{
#ifdef DEBUG
if ( gs_debug['u'] )
   {	dprintf("[u]restore");
	alloc_save_print(cp);
   }
#endif
		  if ( cp->where )
		    ref_assign(cp->where, &cp->contents);
		  else if ( cp->contents.value.refs != 0 )	/* might be an unfilled save record */
		    { alloc_free((char *)cp->contents.value.refs,
				 r_size(&cp->contents), sizeof(ref),
				 "alloc_restore_state");
		    }
		  cp = cp->next;
		}
	    }

	    /* Free chunks allocated since the save. */
	    { alloc_chunk *cp = ap->current_ptr;
	      *cp = ap->current;	/* update in memory */
	    }
	    while ( ap->current.save_level == ap->save_level )
	      {	byte *cp = (byte *)ap->current_ptr;
		uint csize = ap->climit - cp;
		ap->current_ptr = ap->current.next;
		ap->current = *ap->current_ptr;
		(*ap->pfree)((char *)cp, 1, csize, "alloc_restore_state(chunk)");
	      }

	    /* Free blocks allocated with malloc since the save. */
	    /* Since we reset the chain when we did the save, */
	    /* we just free all the objects on the current chain. */
	    { while ( ap->malloc_chain != 0 )
		{ alloc_block *mblock = ap->malloc_chain;
		  ap->malloc_chain = mblock->next;
		  (*ap->pfree)((char *)mblock,
			       1, alloc_block_size + mblock->size,
			       "alloc_restore_state(malloc'ed)");
		}
	    }

	    /* Restore the allocator state. */
	    *ap = sprev->state;
	    alloc_free((char *)sprev, 1, sizeof(alloc_save),
		       "alloc_restore_state");

	  }
	while ( sprev != save );

	/* Clean up */
	if ( sprev != 0 )
	  ap->saved_cbot = sprev->state.cbot,
	  ap->saved_ctop = sprev->state.ctop;
	/* Clear the last_freed cache, because the cache pointer */
	/* must point to a chunk at the current save level. */
	ap->last_freed = 0;
	if ( ap->save_level == 0 )
	  set_not_in_save();
	/* Set the l_new attribute in all slots that have been saved. */
	save_set_new(ap, l_new);
}

/* ------ Internal routines ------ */

/* Set or reset the l_new attribute in every slot on the current */
/* change chain. */
private void
save_set_new(alloc_state_ptr ap, int new)		/* l_new or 0 */
{	register alloc_change *cp = ap->changes;
	while ( cp )
	   {	ref *rp = cp->where;
		if ( rp != 0 )
		   {	if ( !r_is_packed(rp) )
				rp->tas.type_attrs =
				  (rp->tas.type_attrs & ~l_new) + new;
		   }
		else
		   {	register ushort size = r_size(&cp->contents);
			register ref *ep = cp->contents.value.refs;
			while ( size-- )
			   {	if ( !r_is_packed(ep) )
				  ep->tas.type_attrs =
				    (ep->tas.type_attrs & ~l_new) + new,
				ep++;
			   }
		   }
		cp = cp->next;
	   }
}
