/* nalloc.c: a simple single-arena allocator for command-line-lifetime allocation */
#include "rc.h"

static struct Block {
	size_t used, size;
	char *mem;
	Block *n;
} *fl, *ul;

/* alignto() works only with power of 2 blocks and assumes 2's complement arithmetic */
#define alignto(m, n)   ((m + n - 1) & ~(n - 1))
#define BLOCKSIZE ((size_t) 4096)

/* Allocate a block from the free list or malloc one if none in the fl fit */

static void getblock(size_t n) {
	Block *r, *p;
	for (r = fl, p = NULL; r != NULL; p = r, r = r->n)
		if (n <= r->size)
			break;	/* look for a block which fits the request */
	if (r != NULL) {	/* if one is found, take it off the free list */	
		if (p != NULL)
			p->n = r->n;
		else
			fl = r->n;
	} else {		/* else allocate a new block */
		r = enew(Block);
		r->mem = ealloc(r->size = alignto(n, BLOCKSIZE));
	}
	r->used = 0;
	r->n = ul;
	ul = r;
}

/*
   A fast single-arena allocator. Looks at the current block, and if
   there is not enough room, it goes to getblock() for more. "ul"
   stands for "used list", and the head of the list is the current
   block. "ulp" is a register cache for "ul"; this routine is hacked
   for speed. (sigh, optimizing RISC compilers still can't cache the
   address of a global in a register)
*/

extern void *nalloc(size_t n) {
	size_t base;
	Block *ulp;
        n = alignto(n, sizeof (ALIGN_T));
	ulp = ul;
	if (ulp != NULL && n + (base = ulp->used) < ulp->size) {
		ulp->used = base + n;
		return &ulp->mem[base];
	} else {
		getblock(n); /* assert(ul->used) == 0 */
		(ulp = ul)->used = n;
		return &ulp->mem[0];
	}
}

/*
   Frees memory from nalloc space by putting it on the free list.
   Returns free blocks to the system, retaining at least MAXMEM bytes
   worth of blocks for nalloc.
*/

#define MAXMEM 500000

extern void nfree() {
	size_t count;
	Block *r;
	if (ul == NULL)
		return;
	for (r = ul; r->n != NULL; r = r->n)
		;	/* get to end of used list */
	r->n = fl;	/* tack free list onto it */
	fl = ul;	/* and make it the free list */
	ul = NULL;	/* finally, zero out the used list */
	for (r = fl, count = r->size; r->n != NULL; r = r->n, count += r->size) {
		if (count >= MAXMEM) {
			Block *tmp = r;
			r = r->n;
			tmp->n = NULL;		/* terminate the free list */
			while (r != NULL) {	/* free memory off the tail of the free list */
				tmp = r->n;
				efree(r->mem);
				efree(r);
				r = tmp;
			}
		return;
		}
	}
}

/*
   "Allocates" a new arena by zeroing out the old one. Up to the
   calling routine to keep the old value of the block around.
*/

extern Block *newblock() {
	Block *old = ul;
	ul = NULL;
	return old;
}

/* "Restores" an arena to its saved value. */

extern void restoreblock(Block *old) {
	nfree();
	ul = old;
}

/* generic memory allocation functions */

extern void *ealloc(size_t n) {
	extern void *malloc(size_t);
	void *p = malloc(n);
	if (p == NULL) {
		uerror("malloc");
		rc_exit(1);
	}
	return p;
}

extern void *erealloc(void *p, size_t n) {
	extern void *realloc(void *, size_t);
	if ((p = realloc(p, n)) == NULL) {
		uerror("realloc");
		rc_exit(1);
	}
	return p;
}

extern void efree(void *p) {
	extern void free(void *);
	if (p != NULL)
		free(p);
}

