/* list.c: routines for manipulating the List type */

#include "rc.h"

/*
   These list routines assign meta values of null to the resulting lists;
   it is impossible to glob with the value of a variable unless this value
   is rescanned with eval---therefore it is safe to throw away the meta-ness
   of the list.
*/

/* free a list from malloc space */

extern void listfree(List *p) {
	while (p != NULL) {
		List *n = p->n;
		efree(p->w);
		efree(p);
		p = n;
	}
}

/* Copy list into malloc space (for storing a variable) */

extern List *listcpy(List *s, void *(*alloc)(size_t)) {
	List *top, *r;
	for (top = r = NULL; s != NULL; s = s->n) {
		if (top == NULL)
			r = top = (*alloc)(sizeof (List));
		else
			r = r->n = (*alloc)(sizeof (List));
		r->w = (*alloc)(strlen(s->w) + 1);
		strcpy(r->w, s->w);
		r->m = NULL;
	}
	if (r != NULL)
		r->n = NULL;
	return top;
}

/* Length of list */

extern size_t listlen(List *s) {
	size_t size;
	for (size = 0; s != NULL; s = s->n)
		size += strlen(s->w) + 1;
	return size;
}

/* Number of elements in list */

extern int listnel(List *s) {
	int nel;
	for (nel = 0; s != NULL; s = s->n)
		nel++;
	return nel;
}
