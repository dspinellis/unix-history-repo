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

/* iname.c */
/* Name lookup for Ghostscript interpreter */
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "name.h"
#include "store.h"

/* Definitions and structure for the name table. */
/* The first entry is left unused. */
/* 1-character names are the next nt_1char_size entries. */
#define nt_log2_sub_size 7
#define nt_sub_size (1 << nt_log2_sub_size)
#define nt_sub_index_mask (nt_sub_size - 1)
#define nt_hash_size 256		/* must be a power of 2 */
#define nt_1char_size 256		/* must cover a full byte */
typedef name name_sub_table[nt_sub_size];
typedef struct {
	ushort hash[nt_hash_size];
	name *table[1 << (16 - nt_log2_sub_size)];	/* name_sub_table */
	ref count;			/* t_integer */
#define nt_count(nt) (uint)((nt)->count.value.intval)
#define set_nt_count(nt,cnt) ((nt)->count.value.intval = (cnt))
} name_table;
#define name_index_ptr(nt, index)\
  ((nt)->table[(index) >> nt_log2_sub_size] + ((index) & nt_sub_index_mask))

/*
 * Scramble the assignment order within a sub-table, so that
 * dictionary lookup doesn't have to scramble the index.
 * The algorithm must have three properties:
 *	- It must map 0 to 0;
 *	- It must only scramble the sub-table index;
 *	- It must be a permutation on the sub-table index.
 * We do something very simple for now.
 */
#define name_count_to_index(cnt)\
  (((cnt) & (-nt_sub_size)) + (((cnt) * 59) & nt_sub_index_mask))
/* We also store the reverse permutation, for age checking in restore. */
#define name_index_to_count(idx)\
  ((idx) ^ name_sub_index_to_count[(idx) & nt_sub_index_mask])
#if nt_sub_size <= 256
private byte name_sub_index_to_count[nt_sub_size];
#else
private ushort name_sub_index_to_count[nt_sub_size];
#endif

/* The one and only name table (for now). */
private name_table *the_nt;

/* Forward references */
private int name_alloc_sub(P1(name_table *));

/* Make a t_name ref out of a name * */
#define make_name(pref, pnm) make_tv(pref, t_name, pname, pnm)

/* Initialize the name table */
void
name_init()
{	register uint i;
	/* Initialize the index_to_count map. */
	for ( i = 0; i < nt_sub_size; i++ )
	   {	uint idx = name_count_to_index(i);
		name_sub_index_to_count[idx] = idx ^ i;
	   }
	the_nt = (name_table *)alloc(1, sizeof(name_table), "name_init");
	memset(the_nt, 0, sizeof(name_table));
	make_int(&the_nt->count, 1);
	for ( i = 0; i <= nt_1char_size; i += nt_sub_size )
	   {	set_nt_count(the_nt, i + 1);
		name_alloc_sub(the_nt);
	   }
}

/* Look up or enter a name in the table. */
/* Return 0 or an error code. */
/* The return may overlap the characters of the string! */
/* See name.h for the meaning of enterflag. */
int
name_ref(const byte *ptr, uint isize, ref *pref, int enterflag)
{	register name *pname;
	const byte *cptr;
	ushort size = (ushort)isize;	/* see name.h */
	if ( size == 1 )
	   {	uint ccnt = *ptr + 1;
		uint nidx = name_count_to_index(ccnt);
		pname = name_index_ptr(the_nt, nidx);
		if ( pname->string_size != 0 )
		   {	make_name(pref, pname);
			return 0;
		   }
		if ( enterflag < 0 ) return e_undefined;
		pname->index = nidx;
		pname->next_index = 0;
		if_debug4('n', "[n]new name 0x%lx#%u, length=%u, count=%u\n",
			  (ulong)pname, nidx, isize, ccnt);
	   }
	else
	   {	ushort *phash =
		  the_nt->hash +
		    ((ushort)string_hash(ptr, size) & (nt_hash_size - 1));
		uint nidx = *phash;
		uint ncnt;
		while ( nidx != 0 )
		   {	pname = name_index_ptr(the_nt, nidx);
			if ( pname->string_size == size &&
			     !memcmp(ptr, pname->string_bytes, size)
			   )
			   {	make_name(pref, pname);
				return 0;
			   }
			nidx = pname->next_index;
		   }
		/* Not in table, allocate a new entry. */
		if ( enterflag < 0 ) return e_undefined;
		if ( !(nt_count(the_nt) & (nt_sub_size - 1)) )
		   {	int code = name_alloc_sub(the_nt);
			if ( code < 0 ) return code;
		   }
		ncnt = nt_count(the_nt);
		nidx = name_count_to_index(ncnt);
		pname = name_index_ptr(the_nt, nidx);
		pname->index = nidx;
		pname->next_index = *phash;
		*phash = nidx;
		if_debug4('n', "[n]new name 0x%lx#%u, length=%u, count=%u\n",
			  (ulong)pname, nidx, isize, ncnt);
		ref_save(&the_nt->count, "name_ref(count)");
		set_nt_count(the_nt, ncnt + 1);
	   }
	/* Name was not in the table.  Make a new entry. */
	if ( enterflag )
	   {	cptr = (const byte *)alloc(size, 1, "name_ref(string)");
		if ( cptr == 0 ) return e_VMerror;
		memcpy((byte *)cptr, ptr, size);
	   }
	else
		cptr = ptr;
	pname->string_size = size;
	pname->string_bytes = cptr;
	pname->pvalue = pv_no_defn;
	make_name(pref, pname);
	return 0;
}

/* Get the string for a name. */
void
name_string_ref(const ref *pnref /* t_name */,
  ref *psref /* result, t_string */)
{	name *pname = pnref->value.pname;
	make_tasv(psref, t_string, a_read+a_execute, pname->string_size,
		  bytes, (byte *)pname->string_bytes);
}

/* Convert a t_string object to a name. */
/* Copy the executable attribute. */
int
name_from_string(const ref *psref, ref *pnref)
{	int exec = r_has_attr(psref, a_executable);
	int code = name_ref(psref->value.bytes, r_size(psref), pnref, 1);
	if ( code < 0 ) return code;
	if ( exec ) r_set_attrs(pnref, a_executable);
	return code;
}

/* Enter a name during initialization. */
/* Fatal error if the entry fails. */
void
name_enter(const char *str, ref *pref)
{	if ( name_ref((const byte *)str, strlen(str), pref, 0) )
		lprintf1("name_enter failed - %s", str),
		gs_exit(1);
}

/* Get the name with a given index. */
void
name_index_ref(uint index, ref *pnref)
{	make_name(pnref, name_index_ptr(the_nt, index));
}

/* Get the current name count. */
uint
name_count()
{	return nt_count(the_nt);
}

/* Check whether a name was created since a given count. */
int
name_is_since_count(ref *pnref, uint cnt)
{	return name_index_to_count(name_index(pnref)) >= cnt;
}

/* Clean up the name table before a restore. */
/* The count will be reset, and added subtables will be freed. */
/* All we have to do is remove initial entries from the hash chains, */
/* since we know they are linked in decreasing index order */
/* (by sub-table, but not within each sub-table.) */
/* (There will be some spurious non-zero entries in the subtable table, */
/* but this doesn't matter since they will never be accessed.) */
void
name_restore(uint old_count)
{	ushort *phash = &the_nt->hash[0];
	uint old_sub = old_count & -nt_sub_size;
	register uint i;
	for ( i = 0; i < nt_hash_size; phash++, i++ )
	   {	register ushort *pnh = phash;
		while ( *pnh >= old_sub )
		   {	if ( name_index_to_count(*pnh) < old_count )
			   pnh = &name_index_ptr(the_nt, *pnh)->next_index;
			else
			   *pnh = name_index_ptr(the_nt, *pnh)->next_index;
		   }
	   }
}

/* ------ Internal procedures ------ */

/* Allocate the next sub-table. */
private int
name_alloc_sub(name_table *nt)
{	name *sub = (name *)alloc(1, sizeof(name_sub_table), "name_alloc_sub");
	if ( sub == 0 ) return e_VMerror;
	memset(sub, 0, sizeof(name_sub_table));
	nt->table[nt_count(nt) >> nt_log2_sub_size] = sub;
	return 0;
}
