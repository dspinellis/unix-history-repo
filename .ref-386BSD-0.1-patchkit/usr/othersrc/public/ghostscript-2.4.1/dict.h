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

/* dict.h */
/* Interfaces for Ghostscript dictionary package */

/*
 * Contrary to our usual practice, we expose the (first-level)
 * representation of a dictionary in the interface file,
 * because it is so important that access checking go fast.
 * The access attributes for the dictionary are stored in
 * the contents ref.
 */
struct dict_s {
	ref count;		/* t_integer, # of occupied entries; */
				/* "size" is maxlength as seen by client. */
	ref keys;		/* t_shortarray or t_array, keys */
	ref values;		/* t_array, values */
};

/* Define the maximum size of a dictionary. */
extern const uint dict_max_size;

/* Define whether dictionaries expand automatically when full. */
/* Note that if dict_auto_expand is true, dict_put, dict_copy, and */
/* dict_resize cannot return e_dictfull; however, they can return */
/* e_VMerror.  (dict_lookup and dict_find can return e_dictfull */
/* even if dict_auto_expand is true.) */
extern int dict_auto_expand;

/* Create a dictionary. */
extern int dict_create(P2(uint maxlength, ref *pdref));

/* Return a pointer to a ref that holds the access attributes */
/* for a dictionary. */
#define dict_access_ref(pdref) (&(pdref)->value.pdict->values)
#define check_dict_read(dref) check_read(*dict_access_ref(&dref))
#define check_dict_write(dref) check_write(*dict_access_ref(&dref))

/* Look up in a stack of dictionaries.  Store a pointer to the value slot */
/* where found, or to the (value) slot for inserting. */
/* Return 1 if found, 0 if not and there is room for a new entry, */
/* or e_dictfull if the dictionary is full and the key is missing. */
/* The caller is responsible for ensuring key is not a null. */
/* Note that pdbot <= pdtop, and the search starts at pdtop. */
extern int dict_lookup(P4(const ref *pdbot, const ref *pdtop, const ref *key, ref **ppvalue));
/* Look up in just one dictionary. */
#define dict_find(dref,key,ppvalue) dict_lookup(dref,dref,key,ppvalue)

/* Enter a key-value pair in a dictionary. */
/* Return 0, e_dictfull, or e_VMerror if the key was a string */
/* and a VMerror occurred when converting it to a name. */
extern int dict_put(P3(ref *pdref, const ref *key, const ref *pvalue));

/* Remove a key-value pair from a dictionary. */
/* Return 0 or e_undefined. */
extern int dict_undef(P2(ref *pdref, const ref *key));

/* Return the number of elements in a dictionary. */
extern uint dict_length(P1(const ref *pdref));

/* Return the capacity of a dictionary. */
extern uint dict_maxlength(P1(const ref *pdref));

/* Copy one dictionary into another. */
/* Return 0 or e_dictfull. */
extern int dict_copy(P2(const ref *dfrom, ref *dto));

/* Grow or shrink a dictionary. */
/* Return 0, e_dictfull, or e_VMerror. */
extern int dict_resize(P2(ref *pdref, uint newmaxlength));

/* Prepare to enumerate a dictionary. */
/* Return an integer suitable for the first call to dict_next. */
extern int dict_first(P1(const ref *pdref));

/* Enumerate the next element of a dictionary. */
/* index is initially the result of a call on dict_first. */
/* Either store a key and value at eltp[0] and eltp[1] */
/* and return an updated index, or return -1 */
/* to signal that there are no more elements in the dictionary. */
extern int dict_next(P3(const ref *pdref, int index, ref *eltp));
