/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * List handling routines.
 * Lists can be composed of any types of values, mixed if desired.
 * Lists are doubly linked so that elements can be inserted or
 * deleted efficiently at any point in the list.  A pointer is
 * kept to the most recently indexed element so that sequential
 * accesses are fast.
 */

#include "value.h"


static LISTELEM *elemalloc MATH_PROTO((void));
static LISTELEM *listelement MATH_PROTO((LIST *lp, long index));
static void elemfree MATH_PROTO((LISTELEM *ep));
static void removelistelement MATH_PROTO((LIST *lp, LISTELEM *ep));


/*
 * Free lists for list headers and list elements.
 */
static FREELIST	headerfreelist = {
	sizeof(LIST),		/* size of list header */
	20			/* number of free headers to keep */
};

static FREELIST elementfreelist = {
	sizeof(LISTELEM),	/* size of list element */
	1000			/* number of free list elements to keep */
};


/*
 * Insert an element before the first element of a list.
 */
void
insertlistfirst(lp, vp)
	LIST *lp;		/* list to put element onto */
	VALUE *vp;		/* value to be inserted */
{
	LISTELEM *ep;		/* list element */

	ep = elemalloc();
	copyvalue(vp, &ep->e_value);
	if (lp->l_count == 0)
		lp->l_last = ep;
	else {
		lp->l_cacheindex++;
		lp->l_first->e_prev = ep;
		ep->e_next = lp->l_first;
	}
	lp->l_first = ep;
	lp->l_count++;
}


/*
 * Insert an element after the last element of a list.
 */
void
insertlistlast(lp, vp)
	LIST *lp;		/* list to put element onto */
	VALUE *vp;		/* value to be inserted */
{
	LISTELEM *ep;		/* list element */

	ep = elemalloc();
	copyvalue(vp, &ep->e_value);
	if (lp->l_count == 0)
		lp->l_first = ep;
	else {
		lp->l_last->e_next = ep;
		ep->e_prev = lp->l_last;
	}
	lp->l_last = ep;
	lp->l_count++;
}


/*
 * Insert an element into the middle of list at the given index (zero based).
 * The specified index will select the new element, so existing elements
 * at or beyond the index will be shifted down one position.  It is legal
 * to specify an index which is right at the end of the list, in which
 * case the element is appended to the list.
 */
void
insertlistmiddle(lp, index, vp)
	LIST *lp;		/* list to put element onto */
	long index;		/* element number to insert in front of */
	VALUE *vp;		/* value to be inserted */
{
	LISTELEM *ep;		/* list element */
	LISTELEM *oldep;	/* old list element at desired index */

	if (index == 0) {
		insertlistfirst(lp, vp);
		return;
	}
	if (index == lp->l_count) {
		insertlistlast(lp, vp);
		return;
	}
	oldep = NULL;
	if ((index >= 0) && (index < lp->l_count))
		oldep = listelement(lp, index);
	if (oldep == NULL)
		math_error("Index out of bounds for list insertion");
	ep = elemalloc();
	copyvalue(vp, &ep->e_value);
	ep->e_next = oldep;
	ep->e_prev = oldep->e_prev;
	ep->e_prev->e_next = ep;
	oldep->e_prev = ep;
	lp->l_cache = ep;
	lp->l_cacheindex = index;
	lp->l_count++;
}


/*
 * Remove the first element from a list, returning its value.
 * Returns the null value if no more elements exist.
 */
void
removelistfirst(lp, vp)
	LIST *lp;		/* list to have element removed */
	VALUE *vp;		/* location of the value */
{
	if (lp->l_count == 0) {
		vp->v_type = V_NULL;
		return;
	}
	*vp = lp->l_first->e_value;
	lp->l_first->e_value.v_type = V_NULL;
	removelistelement(lp, lp->l_first);
}


/*
 * Remove the last element from a list, returning its value.
 * Returns the null value if no more elements exist.
 */
void
removelistlast(lp, vp)
	LIST *lp;		/* list to have element removed */
	VALUE *vp;		/* location of the value */
{
	if (lp->l_count == 0) {
		vp->v_type = V_NULL;
		return;
	}
	*vp = lp->l_last->e_value;
	lp->l_last->e_value.v_type = V_NULL;
	removelistelement(lp, lp->l_last);
}


/*
 * Remove the element with the given index from a list, returning its value.
 */
void
removelistmiddle(lp, index, vp)
	LIST *lp;		/* list to have element removed */
	long index;		/* list element to be removed */
	VALUE *vp;		/* location of the value */
{
	LISTELEM *ep;		/* element being removed */

	ep = NULL;
	if ((index >= 0) && (index < lp->l_count))
		ep = listelement(lp, index);
	if (ep == NULL)
		math_error("Index out of bounds for list deletion");
	*vp = ep->e_value;
	ep->e_value.v_type = V_NULL;
	removelistelement(lp, ep);
}


/*
 * Remove an arbitrary element from a list.
 * The value contained in the element is freed.
 */
static void
removelistelement(lp, ep)
	register LIST *lp;		/* list header */
	register LISTELEM *ep;		/* list element to remove */
{
	if ((ep == lp->l_cache) || ((ep != lp->l_first) && (ep != lp->l_last)))
		lp->l_cache = NULL;
	if (ep->e_next)
		ep->e_next->e_prev = ep->e_prev;
	if (ep->e_prev)
		ep->e_prev->e_next = ep->e_next;
	if (ep == lp->l_first) {
		lp->l_first = ep->e_next;
		lp->l_cacheindex--;
	}
	if (ep == lp->l_last)
		lp->l_last = ep->e_prev;
	lp->l_count--;
	elemfree(ep);
}


/*
 * Search a list for the specified value starting at the specified index.
 * Returns the element number (zero based) of the found value, or -1 if
 * the value was not found.
 */
long
listsearch(lp, vp, index)
	LIST *lp;
	VALUE *vp;
	long index;
{
	register LISTELEM *ep;

	if (index < 0)
		index = 0;
	ep = listelement(lp, index);
	while (ep) {
		if (!comparevalue(&ep->e_value, vp)) {
			lp->l_cache = ep;
			lp->l_cacheindex = index;
			return index;
		}
		ep = ep->e_next;
		index++;
	}
	return -1;
}


/*
 * Search a list backwards for the specified value starting at the
 * specified index.  Returns the element number (zero based) of the
 * found value, or -1 if the value was not found.
 */
long
listrsearch(lp, vp, index)
	LIST *lp;
	VALUE *vp;
	long index;
{
	register LISTELEM *ep;

	if (index >= lp->l_count)
		index = lp->l_count - 1;
	ep = listelement(lp, index);
	while (ep) {
		if (!comparevalue(&ep->e_value, vp)) {
			lp->l_cache = ep;
			lp->l_cacheindex = index;
			return index;
		}
		ep = ep->e_prev;
		index--;
	}
	return -1;
}


/*
 * Index into a list and return the address for the value corresponding
 * to that index.  Returns NULL if the element does not exist.
 */
VALUE *
listfindex(lp, index)
	LIST *lp;		/* list to index into */
	long index;		/* index of desired element */
{
	LISTELEM *ep;

	ep = listelement(lp, index);
	if (ep == NULL)
		return NULL;
	return &ep->e_value;
}


/*
 * Return the element at a specified index number of a list.
 * The list is indexed starting at zero, and negative indices
 * indicate to index from the end of the list.  This routine finds
 * the element by chaining through the list from the closest one
 * of the first, last, and cached elements.  Returns NULL if the
 * element does not exist.
 */
static LISTELEM *
listelement(lp, index)
	register LIST *lp;	/* list to index into */
	long index;		/* index of desired element */
{
	register LISTELEM *ep;	/* current list element */
	long dist;		/* distance to element */
	long temp;		/* temporary distance */
	BOOL forward;		/* TRUE if need to walk forwards */

	if (index < 0)
		index += lp->l_count;
	if ((index < 0) || (index >= lp->l_count))
		return NULL;
	/*
	 * Check quick special cases first.
	 */
	if (index == 0)
		return lp->l_first;
	if (index == 1)
		return lp->l_first->e_next;
	if (index == lp->l_count - 1)
		return lp->l_last;
	if ((index == lp->l_cacheindex) && lp->l_cache)
		return lp->l_cache;
	/*
	 * Calculate whether it is better to go forwards from
	 * the first element or backwards from the last element.
	 */
	forward = ((index * 2) <= lp->l_count);
	if (forward) {
		dist = index;
		ep = lp->l_first;
	} else {
		dist = (lp->l_count - 1) - index;
		ep = lp->l_last;
	}
	/*
	 * Now see if we have a cached element and if so, whether or
	 * not the distance from it is better than the above distance.
	 */
	if (lp->l_cache) {
		temp = index - lp->l_cacheindex;
		if ((temp >= 0) && (temp < dist)) {
			dist = temp;
			ep = lp->l_cache;
			forward = TRUE;
		}
		if ((temp < 0) && (-temp < dist)) {
			dist = -temp;
			ep = lp->l_cache;
			forward = FALSE;
		}
	}
	/*
	 * Now walk forwards or backwards from the selected element
	 * until we reach the correct element.  Cache the location of
	 * the found element for future use.
	 */
	if (forward) {
		while (dist-- > 0)
			ep = ep->e_next;
	} else {
		while (dist-- > 0)
			ep = ep->e_prev;
	}
	lp->l_cache = ep;
	lp->l_cacheindex = index;
	return ep;
}


/*
 * Compare two lists to see if they are identical.
 * Returns TRUE if they are different.
 */
BOOL
listcmp(lp1, lp2)
	LIST *lp1, *lp2;
{
	LISTELEM *e1, *e2;
	long count;

	if (lp1 == lp2)
		return FALSE;
	if (lp1->l_count != lp2->l_count)
		return TRUE;
	e1 = lp1->l_first;
	e2 = lp2->l_first;
	count = lp1->l_count;
	while (count-- > 0) {
		if (comparevalue(&e1->e_value, &e2->e_value))
			return TRUE;
		e1 = e1->e_next;
		e2 = e2->e_next;
	}
	return FALSE;
}


/*
 * Copy a list
 */
LIST *
listcopy(oldlp)
	LIST *oldlp;
{
	LIST *lp;
	LISTELEM *oldep;

	lp = listalloc();
	oldep = oldlp->l_first;
	while (oldep) {
		insertlistlast(lp, &oldep->e_value);
		oldep = oldep->e_next;
	}
	return lp;
}


/*
 * Allocate an element for a list.
 */
static LISTELEM *
elemalloc()
{
	LISTELEM *ep;

	ep = (LISTELEM *) allocitem(&elementfreelist);
	if (ep == NULL)
		math_error("Cannot allocate list element");
	ep->e_next = NULL;
	ep->e_prev = NULL;
	ep->e_value.v_type = V_NULL;
	return ep;
}


/*
 * Free a list element, along with any contained value.
 */
static void
elemfree(ep)
	LISTELEM *ep;
{
	if (ep->e_value.v_type != V_NULL)
		freevalue(&ep->e_value);
	freeitem(&elementfreelist, (FREEITEM *) ep);
}


/*
 * Allocate a new list header.
 */
LIST *
listalloc()
{
	register LIST *lp;

	lp = (LIST *) allocitem(&headerfreelist);
	if (lp == NULL)
		math_error("Cannot allocate list header");
	lp->l_first = NULL;
	lp->l_last = NULL;
	lp->l_cache = NULL;
	lp->l_cacheindex = 0;
	lp->l_count = 0;
	return lp;
}


/*
 * Free a list header, along with all of its list elements.
 */
void
listfree(lp)
	register LIST *lp;
{
	register LISTELEM *ep;

	while (lp->l_count-- > 0) {
		ep = lp->l_first;
		lp->l_first = ep->e_next;
		elemfree(ep);
	}
	freeitem(&headerfreelist, (FREEITEM *) lp);
}


/*
 * Print out a list along with the specified number of its elements.
 * The elements are printed out in shortened form.
 */
void
listprint(lp, max_print)
	LIST *lp;
	long max_print;
{
	long count;
	long index;
	LISTELEM *ep;

	if (max_print > lp->l_count)
		max_print = lp->l_count;
	count = 0;
	ep = lp->l_first;
	index = lp->l_count;
	while (index-- > 0) {
		if ((ep->e_value.v_type != V_NUM) ||
			(!qiszero(ep->e_value.v_num)))
				count++;
		ep = ep->e_next;
	}
	if (max_print > 0)
		math_str("\n");
	math_fmt("list (%ld element%s, %ld nonzero)", lp->l_count,
		((lp->l_count == 1) ? "" : "s"), count);
	if (max_print <= 0)
		return;

	/*
	 * Walk through the first few list elements, printing their
	 * value in short and unambiguous format.
	 */
	math_str(":\n");
	ep = lp->l_first;
	for (index = 0; index < max_print; index++) {
		math_fmt("  [[%ld]] = ", index);
		printvalue(&ep->e_value, PRINT_SHORT | PRINT_UNAMBIG);
		math_str("\n");
		ep = ep->e_next;
	}
	if (max_print < lp->l_count)
		math_str("  ...\n");
}


/*
 * Return a trivial hash value for a list.
 */
HASH
listhash(lp)
	LIST *lp;
{
	HASH hash;

	hash = lp->l_count * 600011;
	if (lp->l_count > 0)
		hash = hash * 600043 + hashvalue(&lp->l_first->e_value);
	if (lp->l_count > 1)
		hash = hash * 600053 + hashvalue(&lp->l_last->e_value);
	return hash;
}

/* END CODE */
