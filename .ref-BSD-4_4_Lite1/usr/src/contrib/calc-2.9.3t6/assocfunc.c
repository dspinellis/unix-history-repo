/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Association table routines.
 * An association table is a type of value which can be "indexed" by
 * one or more arbitrary values.  Each element in the table is thus an
 * association between a particular set of index values and a result value.
 * The elements in an association table are stored in a hash table for
 * quick access.
 */

#include "value.h"


#define	MINHASHSIZE	31	/* minimum size of hash tables */
#define	GROWHASHSIZE	50	/* approximate growth for hash tables */
#define	CHAINLENGTH	10	/* desired number of elements on a hash chain */
#define	ELEMSIZE(n)	(sizeof(ASSOCELEM) + (sizeof(VALUE) * ((n) - 1)))


static ASSOCELEM *elemindex MATH_PROTO((ASSOC *ap, long index));
static BOOL compareindices MATH_PROTO((VALUE *v1, VALUE *v2, long dim));
static void resize MATH_PROTO((ASSOC *ap, long newsize));
static void assoc_elemfree MATH_PROTO((ASSOCELEM *ep));
static long nextprime MATH_PROTO((long n));


/*
 * Return the address of the value specified by normal indexing of
 * an association.  The create flag is TRUE if a value is going to be
 * assigned into the specified indexing location.  If create is FALSE and
 * the index value doesn't exist, a pointer to a NULL value is returned.
 */
VALUE *
associndex(ap, create, dim, indices)
	ASSOC *ap;		/* association to index into */
	BOOL create;		/* whether to create the index value */
	long dim;		/* dimension of the indexing */
	VALUE *indices;		/* table of values being indexed by */
{
	ASSOCELEM **listhead;
	ASSOCELEM *ep;
	static VALUE val;
	HASH hash;
	int i;

	if (dim <= 0)
		math_error("No dimensions for indexing association");

	/*
	 * Calculate the hash value to use for this set of indices
	 * so that we can first select the correct hash chain, and
	 * also so we can quickly compare each element for a match.
	 */
	hash = 0;
	for (i = 0; i < dim; i++)
		/* ignore Saber-C warning about Over/underflow */
		hash = hash * 67319821 + hashvalue(&indices[i]);

	/*
	 * Search the correct hash chain for the specified set of indices.
	 * If found, return the address of the found element's value.
	 */
	listhead = &ap->a_table[hash % ap->a_size];
	for (ep = *listhead; ep; ep = ep->e_next) {
		if ((ep->e_hash != hash) || (ep->e_dim != dim))
			continue;
		if (compareindices(ep->e_indices, indices, dim))
			return &ep->e_value;
	}

	/*
	 * The set of indices was not found.
	 * Either return a pointer to a NULL value for a read reference,
	 * or allocate a new element in the list for a write reference.
	 */
	if (!create) {
		val.v_type = V_NULL;
		return &val;
	}

	ep = (ASSOCELEM *) malloc(ELEMSIZE(dim));
	if (ep == NULL)
		math_error("Cannot allocate association element");
	ep->e_dim = dim;
	ep->e_hash = hash;
	ep->e_value.v_type = V_NULL;
	for (i = 0; i < dim; i++)
		copyvalue(&indices[i], &ep->e_indices[i]);
	ep->e_next = *listhead;
	*listhead = ep;
	ap->a_count++;

	resize(ap, ap->a_count / CHAINLENGTH);

	return &ep->e_value;
}


/*
 * Search an association for the specified value starting at the
 * specified index.  Returns the element number (zero based) of the
 * found value, or -1 if the value was not found.
 */
long
assocsearch(ap, vp, index)
	ASSOC *ap;
	VALUE *vp;
	long index;
{
	ASSOCELEM *ep;

	if (index < 0)
		index = 0;
	while (TRUE) {
		ep = elemindex(ap, index);
		if (ep == NULL)
			return -1;
		if (!comparevalue(&ep->e_value, vp))
			return index;
		index++;
	}
}


/*
 * Search an association backwards for the specified value starting at the
 * specified index.  Returns the element number (zero based) of the
 * found value, or -1 if the value was not found.
 */
long
assocrsearch(ap, vp, index)
	ASSOC *ap;
	VALUE *vp;
	long index;
{
	ASSOCELEM *ep;

	if (index >= ap->a_count)
		index = ap->a_count - 1;
	while (TRUE) {
		ep = elemindex(ap, index);
		if (ep == NULL)
			return -1;
		if (!comparevalue(&ep->e_value, vp))
			return index;
		index--;
	}
}


/*
 * Return the address of an element of an association indexed by the
 * double-bracket operation.
 */
static ASSOCELEM *
elemindex(ap, index)
	ASSOC *ap;		/* association to index into */
	long index;		/* index of desired element */
{
	ASSOCELEM *ep;
	int i;

	if ((index < 0) || (index > ap->a_count))
		return NULL;

	/*
	 * This loop should be made more efficient by remembering
	 * previously requested locations within the association.
	 */
	for (i = 0; i < ap->a_size; i++) {
		for (ep = ap->a_table[i]; ep; ep = ep->e_next) {
			if (index-- == 0)
				return ep;
		}
	}
	return NULL;
}


/*
 * Return the address of the value specified by double-bracket indexing
 * of an association.  Returns NULL if there is no such element.
 */
VALUE *
assocfindex(ap, index)
	ASSOC *ap;		/* association to index into */
	long index;		/* index of desired element */
{
	ASSOCELEM *ep;

	ep = elemindex(ap, index);
	if (ep == NULL)
		return NULL;
	return &ep->e_value;
}


/*
 * Compare two associations to see if they are identical.
 * Returns TRUE if they are different.
 */
BOOL
assoccmp(ap1, ap2)
	ASSOC *ap1, *ap2;
{
	ASSOCELEM **table1;
	ASSOCELEM *ep1;
	ASSOCELEM *ep2;
	long size1;
	long size2;
	HASH hash;
	long dim;

	if (ap1 == ap2)
		return FALSE;
	if (ap1->a_count != ap2->a_count)
		return TRUE;

	table1 = ap1->a_table;
	size1 = ap1->a_size;
	size2 = ap2->a_size;
	while (size1-- > 0) {
		for (ep1 = *table1++; ep1; ep1 = ep1->e_next) {
			hash = ep1->e_hash;
			dim = ep1->e_dim;
			for (ep2 = ap2->a_table[hash % size2]; ;
				ep2 = ep2->e_next)
			{
				if (ep2 == NULL)
					return TRUE;
				if (ep2->e_hash != hash)
					continue;
				if (ep2->e_dim != dim)
					continue;
				if (compareindices(ep1->e_indices,
					ep2->e_indices, dim))
						break;
			}
			if (comparevalue(&ep1->e_value, &ep2->e_value))
				return TRUE;
		}
	}
	return FALSE;
}


/*
 * Copy an association value.
 */
ASSOC *
assoccopy(oldap)
	ASSOC *oldap;
{
	ASSOC *ap;
	ASSOCELEM *oldep;
	ASSOCELEM *ep;
	ASSOCELEM **listhead;
	int oldhi;
	int i;

	ap = assocalloc(oldap->a_count / CHAINLENGTH);
	ap->a_count = oldap->a_count;

	for (oldhi = 0; oldhi < oldap->a_size; oldhi++) {
		for (oldep = oldap->a_table[oldhi]; oldep;
			oldep = oldep->e_next)
		{
			ep = (ASSOCELEM *) malloc(ELEMSIZE(oldep->e_dim));
			if (ep == NULL)
				math_error("Cannot allocate association element");
			ep->e_dim = oldep->e_dim;
			ep->e_hash = oldep->e_hash;
			ep->e_value.v_type = V_NULL;
			for (i = 0; i < ep->e_dim; i++)
				copyvalue(&oldep->e_indices[i], &ep->e_indices[i]);
			copyvalue(&oldep->e_value, &ep->e_value);
			listhead = &ap->a_table[ep->e_hash % ap->a_size];
			ep->e_next = *listhead;
			*listhead = ep;
		}
	}
	return ap;
}


/*
 * Resize the hash table for an association to be the specified size.
 * This is only actually done if the growth from the previous size is
 * enough to make this worthwhile.
 */
static void
resize(ap, newsize)
	ASSOC *ap;
	long newsize;
{
	ASSOCELEM **oldtable;
	ASSOCELEM **newtable;
	ASSOCELEM **oldlist;
	ASSOCELEM **newlist;
	ASSOCELEM *ep;
	int i;

	if (newsize < ap->a_size + GROWHASHSIZE)
		return;

	newsize = nextprime(newsize);
	newtable = (ASSOCELEM **) malloc(sizeof(ASSOCELEM *) * newsize);
	if (newtable == NULL)
		math_error("No memory to grow association");
	for (i = 0; i < newsize; i++)
		newtable[i] = NULL;

	oldtable = ap->a_table;
	oldlist = oldtable;
	for (i = 0; i < ap->a_size; i++) {
		while (*oldlist) {
			ep = *oldlist;
			*oldlist = ep->e_next;
			newlist = &newtable[ep->e_hash % newsize];
			ep->e_next = *newlist;
			*newlist = ep;
		}
		oldlist++;
	}

	ap->a_table = newtable;
	ap->a_size = newsize;
	free((char *) oldtable);
}


/*
 * Free an association element, along with any contained values.
 */
static void
assoc_elemfree(ep)
	ASSOCELEM *ep;
{
	int i;

	for (i = 0; i < ep->e_dim; i++)
		freevalue(&ep->e_indices[i]);
	freevalue(&ep->e_value);
	ep->e_dim = 0;
	ep->e_next = NULL;
	free((char *) ep);
}


/*
 * Allocate a new association value with an initial hash table.
 * The hash table size is set at specified (but at least a minimum size).
 */
ASSOC *
assocalloc(initsize)
	long initsize;
{
	register ASSOC *ap;
	int i;

	if (initsize < MINHASHSIZE)
		initsize = MINHASHSIZE;
	ap = (ASSOC *) malloc(sizeof(ASSOC));
	if (ap == NULL)
		math_error("No memory for association");
	ap->a_count = 0;
	ap->a_size = initsize;
	ap->a_table = (ASSOCELEM **) malloc(sizeof(ASSOCELEM *) * initsize);
	if (ap->a_table == NULL) {
		free((char *) ap);
		math_error("No memory for association");
	}
	for (i = 0; i < initsize; i++)
		ap->a_table[i] = NULL;
	return ap;
}


/*
 * Free an association value, along with all of its elements.
 */
void
assocfree(ap)
	register ASSOC *ap;
{
	ASSOCELEM **listhead;
	ASSOCELEM *ep;
	ASSOCELEM *nextep;
	int i;

	listhead = ap->a_table;
	for (i = 0; i < ap->a_size; i++) {
		nextep = *listhead;
		*listhead = NULL;
		while (nextep) {
			ep = nextep;
			nextep = ep->e_next;
			assoc_elemfree(ep);
		}
		listhead++;
	}
	free((char *) ap->a_table);
	ap->a_table = NULL;
	free((char *) ap);
}


/*
 * Print out an association along with the specified number of
 * its elements.  The elements are printed out in shortened form.
 */
void
assocprint(ap, max_print)
	ASSOC *ap;
	long max_print;
{
	ASSOCELEM *ep;
	long index;
	long i;
	int savemode;

	if (max_print <= 0) {
		math_fmt("assoc (%ld element%s)", ap->a_count,
			((ap->a_count == 1) ? "" : "s"));
		return;
	}
	math_fmt("\n  assoc (%ld element%s):\n", ap->a_count,
		((ap->a_count == 1) ? "" : "s"));

	for (index = 0; ((index < max_print) && (index < ap->a_count));
		index++)
	{
		ep = elemindex(ap, index);
		if (ep == NULL)
			continue;
		math_str("  [");
		for (i = 0; i < ep->e_dim; i++) {
			if (i)
				math_chr(',');
			savemode = math_setmode(MODE_FRAC);
			printvalue(&ep->e_indices[i],
				(PRINT_SHORT | PRINT_UNAMBIG));
			math_setmode(savemode);
		}
		math_str("] = ");
		printvalue(&ep->e_value, PRINT_SHORT | PRINT_UNAMBIG);
		math_chr('\n');
	}
	if (max_print < ap->a_count)
		math_str("  ...\n");
}


/*
 * Return a trivial hash value for an association.
 */
HASH
assochash(ap)
	ASSOC *ap;
{
	return ap->a_count * 700001;
}


/*
 * Compare two lists of index values to see if they are identical.
 * Returns TRUE if they are the same.
 */
static BOOL
compareindices(v1, v2, dim)
	VALUE *v1;
	VALUE *v2;
	long dim;
{
	int i;

	for (i = 0; i < dim; i++)
		if (v1[i].v_type != v2[i].v_type)
			return FALSE;

	while (dim-- > 0)
		if (comparevalue(v1++, v2++))
			return FALSE;

	return TRUE;
}


/*
 * Return the next prime number up from the specified value.
 * This is used to pick a good hash table size.
 */
static long
nextprime(n)
	long n;
{
	long i;

	if ((n & 0x01) == 0)
		n++;
	while (TRUE) {
		for (i = 3; n % i; i += 2) {
			if (i * i > n)
				return n;
		}
		n += 2;
	}
}

/* END CODE */
