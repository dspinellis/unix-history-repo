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

/* idict.c */
/* Dictionaries for Ghostscript */
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "name.h"
#include "packed.h"
#include "save.h"			/* for value cache in names */
#include "store.h"
#include "iutil.h"			/* for obj_eq */
#include "dict.h"			/* interface definition */
#include "dstack.h"			/* ditto */

/*
 * A dictionary is a structure of three elements (refs):
 *
 *	count - a t_integer whose value says how many entries are
 *	occupied (N), and whose size says how many elements the client
 *	thinks the dictionary can hold (C).  C may be less than M (see below).
 *
 *	keys - a t_shortarray or t_array of M+1 elements, containing
 *	the keys.
 *
 *	values - a t_array of M+1 elements, containing the values.
 *
 * C < M is possible because on 32-bit systems, we round up M so that
 * M is a power of 2; this allows us to use masking rather than division
 * for computing the initial hash probe.  However, C is always the
 * maxlength specified by the client, so clients get a consistent story.
 */
#define dict_round_size (!arch_ints_are_short)
#if dict_round_size
#  define hash_mod(hash, size) ((hash) & ((size) - 1))
#else
#  define hash_mod(hash, size) ((hash) % (size))
#endif
/*
 * In the packed form, unused or deleted entries contain packed_key_empty
 * or packed_key_deleted respectively; in the unpacked form, unused
 * or deleted entries contain a literal or executable null respectively.
 * The first entry is always marked as a deleted entry, to avoid a
 * special wrap-around check.
 *
 * Note that if the keys slot in the dictionary is new,
 * all the key slots are new (more recent than the last save).
 * We use this fact to avoid saving stores into packed keys
 * for newly created dictionaries.
 */
#define dict_is_packed(dct) r_has_type(&(dct)->keys, t_shortarray)
#define packed_key_empty (pt_tag(pt_integer) + 0)
#define packed_key_deleted (pt_tag(pt_integer) + 1)
#define packed_key_impossible pt_tag(pt_full_ref)	/* never matches */
#define packed_name_key(nidx)\
  ((nidx) <= packed_max_name_index ? pt_tag(pt_literal_name) + (nidx) :\
   packed_key_impossible)
/*
 * Using a special mark for deleted entries causes lookup time to degrade
 * as entries are inserted and deleted.  This is not a problem, because
 * entries are almost never deleted.
 */
#define d_maxlength(dct) r_size(&(dct)->count)
#define d_set_maxlength(dct,siz) r_set_size(&(dct)->count,siz)
#define nslots(dct) r_size(&(dct)->values)
#define npairs(dct) (nslots(dct) - 1)
#define d_length(dct) ((uint)((dct)->count.value.intval))

/* Define the size of the largest valid dictionary. */
/* This is limited by the size field of the keys and values refs, */
/* and by the enumeration interface, which requires the size to */
/* fit in an int. */
const uint dict_max_size = max_ushort / 2 - 2;

/* Define whether dictionaries expand automatically when full. */
int dict_auto_expand = 1;

/* Define the hashing function for names. */
/* We don't have to scramble the index, because */
/* indices are assigned in a scattered order (see name_ref in iname.c). */
#define dict_name_index_hash(nidx) (nidx)

/* Define whether dictionaries are packed by default. */
#define default_pack 1

/* Forward references */
private int dict_create_contents(P3(uint size, dict *pdict, int pack));

/* Create a dictionary. */
int
dict_create(uint size, ref *pref)
{	dict *pdict =
	  (dict *)alloc_refs(sizeof(dict) / sizeof(ref), "dict_create");
	int code;
	if ( pdict == 0 ) return e_VMerror;
	code = dict_create_contents(size, pdict, default_pack);
	if ( code < 0 ) return code;
	make_tav_new(pref, t_dictionary, a_all, pdict, pdict);
	return 0;
}
private int
dict_create_unpacked_keys(uint asize, dict *pdict)
{	ref *kp = alloc_refs(asize, "dict_create(keys)");
	ref *zp;
	register uint i;
	if ( kp == 0 ) return e_VMerror;
	make_tasv_new(&pdict->keys, t_array, a_all, asize,
		      refs, kp);
	for ( zp = kp, i = asize; i; zp++, i-- )
		make_null_new(zp);
	r_set_attrs(kp, a_executable);	/* wraparound entry */
	return 0;
}
private int
dict_create_contents(uint size, dict *pdict, int pack)
{	uint csize = (size == 0 ? 1 : size);	/* client-specified size */
	uint asize = csize;
	ref *vp;
	register uint i;
	ref *zp;
#if dict_round_size
	/* Round up the actual allocated size to the next higher */
	/* power of 2, so we can use & instead of %. */
	while ( asize & (asize - 1) ) asize = (asize | (asize >> 1)) + 1;
#endif
	asize++;		/* allow room for wraparound entry */
	vp = alloc_refs(asize, "dict_create(values)");
	if ( vp == 0 ) return e_VMerror;
	make_tasv_new(&pdict->values, t_array, a_all, asize, refs, vp);
	for ( zp = vp, i = asize; i; zp++, i-- )
		make_null_new(zp);
	if ( pack )
	   {	uint ksize = (asize + packed_per_ref - 1) / packed_per_ref;
		ref_packed *pkp = (ref_packed *)alloc_refs(ksize, "dict_create(packed keys)");
		ref_packed *pzp;
		make_tasv_new(&pdict->keys, t_shortarray, a_all, asize,
			      packed, pkp);
		for ( pzp = pkp, i = 0; i < asize || i % packed_per_ref; pzp++, i++ )
			*pzp = packed_key_empty;
		*pkp = packed_key_deleted;	/* wraparound entry */
	   }
	else				/* not packed */
	   {	int code = dict_create_unpacked_keys(asize, pdict);
		if ( code < 0 ) return code;
	   }
	make_tv_new(&pdict->count, t_integer, intval, 0);
	d_set_maxlength(pdict, csize);
	return 0;
}

/*
 * Define a macro for searching a packed dictionary.  Free variables:
 *	ref_packed kpack - holds the packed key.
 *	uint hash - holds the hash of the name.
 *	dict *pdict - points to the dictionary.
 *	uint size - holds npairs(pdict).
 *	int wrap - counts wraparounds, initialized to 0.
 * Note that the macro is *not* enclosed in {}, so that we can access
 * the values of kbot and kp after leaving the loop.
 */
#define packed_search(del,pre,post)\
   int wrap = 0;\
   ref_packed *kbot = pdict->keys.value.packed;\
   register ref_packed *kp;\
   for ( kp = kbot + hash_mod(hash, size) + 2; ; )\
    { if ( *--kp == kpack )\
       { pre (pdict->values.value.refs + (kp - kbot));\
	 post;\
       }\
      else if ( !packed_ref_is_name(kp) )\
       { /* Empty, deleted, or wraparound. Figure out which. */\
	 if ( *kp == packed_key_empty ) break;\
	 if ( kp == kbot )	/* wrap */\
	  { if ( wrap++ ) break;	/* 2 wraps */\
	    kp += size + 1;\
	  }\
	 else { del; }\
       }\
    }

/*
 * Look up in a stack of dictionaries.  Store a pointer to the value slot
 * where found, or to the (value) slot for inserting.
 * Return 1 if found, 0 if not and there is room for a new entry in
 * the top dictionary on the stack, or e_dictfull if the top dictionary
 * is full and the key is missing.
 * Note that pdbot <= pdtop, and the search starts at pdtop.
 */
int
dict_lookup(const ref *pdbot, const ref *pdtop, const ref *pkey,
  ref **ppvalue /* result is stored here */)
{	const ref *pdref = pdtop;
	name *kpname;
	uint nidx;
	ref_packed kpack;
	uint hash;
	int ktype;
	int full = 1;			/* gets set to 0 or e_dictfull */
	/* Compute hash.  The only types we bother with are strings, */
	/* names, and (unlikely, but worth checking for) integers. */
	switch ( r_type(pkey) )
	   {
	case t_name:
		kpname = pkey->value.pname;
nh:		nidx = kpname->index;
		hash = dict_name_index_hash(nidx);
		kpack = packed_name_key(nidx);
		ktype = t_name;
		break;
	case t_string:			/* convert to a name first */
	   {	ref nref;
		int code = name_ref(pkey->value.bytes,
				    r_size(pkey), &nref, 1);
		if ( code < 0 ) return code;
		kpname = nref.value.pname;
	   }	goto nh;
	case t_integer:
		hash = (uint)pkey->value.intval * 30503;
		kpack = packed_key_impossible;
		ktype = -1;
		break;
	default:
		hash = r_btype(pkey) * 99;	/* yech */
		kpack = packed_key_impossible;
		ktype = -1;
	   }
	do
	   {	dict *pdict = pdref->value.pdict;
		uint size = npairs(pdict);
		int wrap = 0;
		register int etype;
		/* Search the dictionary */
		if ( dict_is_packed(pdict) )
		   {	ref_packed *pslot = 0;
			packed_search(if ( pslot == 0 ) pslot = kp,
				      *ppvalue =, return 1);
			if ( full > 0 )	/* first dictionary */
			   {	if ( pslot == 0 )
				 { if ( wrap == 2 )
				     full = e_dictfull;
				   else
				     *ppvalue = pdict->values.value.refs +
				       (kp - kbot),
				     full = 0;
				 }
				else
				  *ppvalue = pdict->values.value.refs +
				    (pslot - kbot),
				  full = 0;
			   }
		   }
		else
		   {	ref *kbot = pdict->keys.value.refs;
			register ref *kp;
			ref *pslot = 0;
			for ( kp = kbot + hash_mod(hash, size) + 2; ; )
			   {	--kp;
				if ( (etype = r_type(kp)) == ktype )
				   {	/* Fast comparison if both keys are names */
					if ( kp->value.pname == kpname )
					   {	*ppvalue = pdict->values.value.refs + (kp - kbot);
						return 1;
					   }
				   }
				else if ( etype == t_null )
				   {	/* Empty, deleted, or wraparound. */
					/* Figure out which. */
					if ( kp == kbot )	/* wrap */
					   {	if ( wrap++ )	/* wrapped twice */
						   {	if ( full > 0 )
							   {	if ( pslot != 0 )
									break;
								full = e_dictfull;
							   }
							goto next_dict;
						   }
						kp += size + 1;
					   }
					else if ( r_has_attr(kp, a_executable) )
					   {	/* Deleted entry, save the slot. */
						if ( pslot == 0 ) pslot = kp;
					   }
					else	/* key not found */
						break;
				   }
				else
				   {	if ( obj_eq(kp, pkey) )
					   {	*ppvalue = pdict->values.value.refs + (kp - kbot);
						return 1;
					   }
				   }
			   }
			if ( full > 0 )
			   {	*ppvalue = pdict->values.value.refs +
				  ((pslot != 0 ? pslot : kp) - kbot);
				full = 0;
			   }
		   }
next_dict: ;
	   }
	while ( --pdref >= pdbot );
	return full;
}

/*
 * Look up a name on the dictionary stack.
 * Return the pointer to the value if found, 0 if not.
 * This is just an optimization of dict_lookup with a different interface.
 */
ref *
dict_find_name(ref *pname)
{	ds_ptr pdref = dsp;
	name *kpname = pname->value.pname;
	uint nidx = kpname->index;
	uint hash = dict_name_index_hash(nidx);
	ref_packed kpack = packed_name_key(nidx);
	int wrap = 0;
	do
	   {	dict *pdict = pdref->value.pdict;
		uint size = npairs(pdict);
		if ( dict_is_packed(pdict) )
		   {	packed_search(0, return, 0);
		   }
		else
		   {	ref *kbot = pdict->keys.value.refs;
			register ref *kp;
			/* Search the dictionary */
			for ( kp = kbot + hash_mod(hash, size) + 2; ; )
			   {	--kp;
				if ( r_has_type(kp, t_name) )
				   {	if ( kp->value.pname == kpname )
					  return pdict->values.value.refs +
					    (kp - kbot);
				   }
				else if ( r_has_type(kp, t_null) )
				   {	/* Empty, deleted, or wraparound. */
					/* Figure out which. */
					if ( !r_has_attr(kp, a_executable) )
						break;
					if ( kp == kbot )	/* wrap */
					   {	if ( wrap++ )
							break;	/* 2 wraps */
						kp += size + 1;
					   }
				   }
			   }
		   }
	   }
	while ( --pdref >= dstack );
	return (ref *)0;
}

/*
 * Enter a key-value pair in a dictionary.
 * The caller is responsible for ensuring key is not a null.
 * Return 0, e_dictfull, or e_VMerror if the key was a string
 * and a VMerror occurred when converting it to a name.
 */
int
dict_put(ref *pdref /* t_dictionary */, const ref *pkey, const ref *pvalue)
{	ref *pvslot;
top:	if ( dict_find(pdref, pkey, &pvslot) <= 0 )	/* not found */
	   {	/* Check for overflow */
		dict *pdict = pdref->value.pdict;
		ref kname;
		uint index = pvslot - pdict->values.value.refs;
		if ( d_length(pdict) == npairs(pdict) )
		   {	int code;
			ulong new_size;
			if ( !dict_auto_expand )
				return e_dictfull;
			new_size = (ulong)npairs(pdict) * 3 / 2 + 2;
			if ( new_size > dict_max_size )
			   {	if ( npairs(pdict) >= dict_max_size )
					return e_dictfull;
				new_size = dict_max_size;
			   }
			code = dict_resize(pdref, (uint)new_size);
			if ( code < 0 ) return code;
			goto top;	/* keep things simple */
		   }
		/* If the key is a string, convert it to a name. */
		if ( r_has_type(pkey, t_string) )
		   {	int code = name_from_string(pkey, &kname);
			if ( code < 0 ) return code;
			pkey = &kname;
		   }
		if ( dict_is_packed(pdict) )
		   {	ref_packed *kp;
			if ( !r_has_type(pkey, t_name) ||
			     name_index(pkey) > packed_max_name_index
			   )
			   {	/* Change to unpacked representation. */
				/* We can't just use dict_resize, */
				/* because the values slots mustn't move. */
				uint count = nslots(pdict);
				ref_packed *old_keys = pdict->keys.value.packed;
				int code;
				ref *nkp;
				if ( alloc_save_new_mask )
					alloc_save_change(&pdict->keys, "dict_unpack(keys)");
				code = dict_create_unpacked_keys(count, pdict);
				if ( code < 0 ) return code;
				for ( kp = old_keys, nkp = pdict->keys.value.refs; count--; kp++, nkp++ )
				  if ( packed_ref_is_name(kp) )
				    packed_get(kp, nkp);
				alloc_free_refs((ref *)old_keys,
						(count + packed_per_ref - 1) / packed_per_ref,
						"dict_unpack(old keys)");
				return dict_put(pdref, pkey, pvalue);
			   }
			kp = pdict->keys.value.packed + index;
			if ( alloc_save_new_mask &&
			     !r_has_attr(&pdict->keys, l_new)
			   )
			   {	/* See initial comment for why it is safe */
				/* not to save the change if the keys */
				/* array itself is new. */
				alloc_save_change(pdict->keys.value.refs + (index / packed_per_ref), "dict_put(key)");
			   }
			*kp = pt_tag(pt_literal_name) + name_index(pkey);
		   }
		else
		   {	ref *kp = pdict->keys.value.refs + index;
			if_debug2('d', "[d]%lx fill key %lx\n",
				  (ulong)pdict, (ulong)kp);
			ref_assign_old(kp, pkey, "dict_put(key)");	/* set key of pair */
		   }
		ref_save(&pdict->count, "dict_put(count)");
		pdict->count.value.intval++;
		/* If the key is a name, update its 1-element cache. */
		if ( r_has_type(pkey, t_name) )
		   {	name *pname = pkey->value.pname;
			if ( pname->pvalue == pv_no_defn &&
				(pdict == systemdict.value.pdict ||
				 pdict == userdict.value.pdict) &&
				/* Only set the cache if we aren't inside */
				/* a save.  This way, we never have to */
				/* undo setting the cache. */
				alloc_save_level() == 0
			   )
			   {	/* Set the cache */
				pname->pvalue = pvslot;
			   }
			else	/* The cache is worthless */
				pname->pvalue = pv_other;
		   }
	   }
	if_debug6('d', "[d]in %lx put %lx: %lx %lx -> %lx %lx\n",
		  (ulong)pdref->value.pdict, (ulong)pvslot,
		  ((ulong *)pvslot)[0], ((ulong *)pvslot)[1],
		  ((ulong *)pvalue)[0], ((ulong *)pvalue)[1]);
	ref_assign_old(pvslot, pvalue, "dict_put(value)");
	return 0;
}

/* Remove an element from a dictionary. */
int
dict_undef(ref *pdref, const ref *pkey)
{	ref *pvslot;
	dict *pdict;
	uint index;
	if ( dict_find(pdref, pkey, &pvslot) <= 0 )
		return e_undefined;
	/* Remove the entry from the dictionary. */
	pdict = pdref->value.pdict;
	index = pvslot - pdict->values.value.refs;
	if ( dict_is_packed(pdict) )
	   {	ref_packed *pkp = pdict->keys.value.packed + index;
		/* Since packed arrays don't have room for a saved bit, */
		/* always save the entire ref containing this key. */
		/* This wastes a little space, but undef is rare. */
		/* See the initial comment for why it is safe not to save */
		/* the change if the keys array itself is new. */
		if ( alloc_save_new_mask && !r_has_attr(&pdict->keys, l_new) )
			alloc_save_change(pdict->keys.value.refs + (index / packed_per_ref), "dict_undef(key)");
		/* Accumulating deleted entries slows down lookup. */
		/* Detect the easy case where we can use an empty entry */
		/* rather than a deleted one, namely, when the next entry */
		/* in the probe order is empty. */
		if ( pkp[-1] == packed_key_empty )
			*pkp = packed_key_empty;
		else
			*pkp = packed_key_deleted;
	   }
	else				/* not packed */
	   {	ref *kp = pdict->keys.value.refs + index;
		make_null_old(kp, "dict_undef(key)");
		/* Accumulating deleted entries slows down lookup. */
		/* Detect the easy case where we can use an empty entry */
		/* rather than a deleted one, namely, when the next entry */
		/* in the probe order is empty. */
		if ( !r_has_type(kp - 1, t_null) ||	/* full entry */
		     r_has_attr(kp - 1, a_executable)	/* deleted or wraparound */
		    )
			r_set_attrs(kp, a_executable);	/* mark as deleted */
	   }
	ref_save(&pdict->count, "dict_undef(count)");
	pdict->count.value.intval--;
	/* If the key is a name, update its 1-element cache. */
	if ( r_has_type(pkey, t_name) )
	   {	name *pname = pkey->value.pname;
		if ( pv_valid(pname->pvalue) &&
			(pdict == systemdict.value.pdict ||
			 pdict == userdict.value.pdict) )
		   {	/* Clear the cache */
			pname->pvalue = pv_no_defn;
		   }
	   }
	make_null_old(pvslot, "dict_undef(value)");
	return 0;
}

/* Return the number of elements in a dictionary. */
uint
dict_length(const ref *pdref /* t_dictionary */)
{	return d_length(pdref->value.pdict);
}

/* Return the capacity of a dictionary. */
uint
dict_maxlength(const ref *pdref /* t_dictionary */)
{	return d_maxlength(pdref->value.pdict);
}

/* Copy one dictionary into another. */
int
dict_copy(const ref *pdrfrom /* t_dictionary */, ref *pdrto /* t_dictionary */)
{	int index = dict_first(pdrfrom);
	ref elt[2];
	int code;
	while ( (index = dict_next(pdrfrom, index, elt)) >= 0 )
	  if ( (code = dict_put(pdrto, &elt[0], &elt[1])) < 0 )
	    return code;
	return 0;
}

/* Resize a dictionary. */
int
dict_resize(ref *pdrfrom, uint new_size)
{	dict *pdict = pdrfrom->value.pdict;
	uint count = nslots(pdict);
	dict dnew;
	ref drto;
	int code;
	if ( new_size < d_length(pdict) )
	   {	if ( !dict_auto_expand )
			return e_dictfull;
		new_size = d_length(pdict);
	   }
	if ( (code = dict_create_contents(new_size, &dnew, dict_is_packed(pdict))) < 0 )
		return code;
	make_tav_new(&drto, t_dictionary, a_all, pdict, &dnew);
	dict_copy(pdrfrom, &drto);	/* can't fail */
	/* Free the old dictionary */
	alloc_free_refs(pdict->values.value.refs, count,
			"dict_resize(old values)");
	alloc_free_refs(pdict->keys.value.refs,
			(dict_is_packed(pdict) ?
			 (count + packed_per_ref - 1) / packed_per_ref :
			 count),
			"dict_resize(old keys)");
	ref_assign_old(&pdict->keys, &dnew.keys, "dict_resize(keys)");
	ref_assign_old(&pdict->values, &dnew.values, "dict_resize(values)");
	ref_save(&pdict->count, "dict_resize(size)");
	d_set_maxlength(pdict, new_size);
	return 0;
}

/* Prepare to enumerate a dictionary. */
int
dict_first(const ref *pdref)
{	return (int)nslots(pdref->value.pdict);
}

/* Enumerate the next element of a dictionary. */
int
dict_next(const ref *pdref, int index, ref *eltp /* ref eltp[2] */)
{	dict *pdict = pdref->value.pdict;
	ref *vp = pdict->values.value.refs + index;
	while ( vp--, --index >= 0 )
	   {	array_get(&pdict->keys, (long)index, eltp);
		/* Make sure this is a valid entry. */
		if ( r_has_type(eltp, t_name) ||
		     (!dict_is_packed(pdict) && !r_has_type(eltp, t_null))
		   )
		   {	eltp[1] = *vp;
			return index;
		   }
	   }
	return -1;			/* no more elements */
}
