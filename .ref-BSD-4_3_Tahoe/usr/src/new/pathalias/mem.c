/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)mem.c	8.1 (down!honey) 86/01/19";
#endif

#include "def.h"

/* imported */
extern char *sbrk();

link	*
newlink()
{
	link	*rval;

	if ((rval = (link * ) calloc(1, sizeof(link))) == 0)
		nomem();
	return(rval);
}

node	*
newnode()
{
	node	*rval;

	if ((rval = (node * ) calloc(1, sizeof(node))) == 0)
		nomem();
	Ncount++;
	return(rval);
}

char	*
strsave(s)
char	*s;
{
	char *r;

	if ((r = malloc((unsigned int) strlen(s) + 1)) == 0)
		nomem();
	(void) strcpy(r, s);
	return(r);
}

#ifndef strclear
void
strclear(dst, len)
register char *dst;
register int len;
{
	while (--len >= 0)
		*dst++ = 0;
}
#endif /*strclear*/

node	**
newtable(size)
long	size;
{
	node	**rval;

	if ((rval = (node **) calloc(1, (unsigned int) (size * sizeof(*rval)))) == 0) 
		nomem();
	return(rval);
}

freetable(t, size)
node	**t;
long	size;
{
#ifdef MYMALLOC
	addtoheap((char *) t, (long) (size * sizeof(*t)));
#else
	free((char *) t);
#endif
}

nomem()
{
	fprintf(stderr, "%s: Out of memory (%ldk allocated)\n",
			ProgName, allocation());
	badmagic(1);
}

/* data space allocation -- main sets End very early */
allocation()
{
	static char	*dataspace;

	if (dataspace == 0) {	/* first time */
		dataspace = sbrk(0);		/* &end? */
		return(0);
	}
	return((sbrk(0) - dataspace)/1024);
}

#ifdef MYMALLOC

/* use c library malloc/calloc here, and here only */
#undef malloc
#undef calloc
extern char *malloc(), *calloc();

/* allocate in MBUFSIZ chunks.  4k works ok (less 16 for malloc quirks). */
#define MBUFSIZ (4 * 1024 - 16)

/* 
 * mess with ALIGN at your peril.  longword (== 0 mod 4)
 * alignment seems to work everywhere.
 */

#define ALIGN 2

typedef struct heap heap;
struct heap {
	heap *h_next;
	long h_size;
};

static heap *Mheap;	/* not to be confused with a priority queue */

addtoheap(p, size)
char *p;
long size;
{
	int adjustment;
	heap *pheap;

	/* p is aligned, but it doesn't hurt to check */
	adjustment = align(p);
	p += adjustment;
	size -= adjustment;

	if (size < 1024)
		return;		/* can't happen */
	pheap = (heap *) p;	/* pheap is shorthand */
	pheap->h_next = Mheap;
	pheap->h_size = size;
	Mheap = pheap;
}

/*
 * buffered malloc()
 *	returns space initialized to 0.  calloc isn't used, since
 *	strclear can be faster.
 *
 * free is ignored, except for very large objects,
 * which are returned to the heap with addtoheap(). 
 */

char	*
mymalloc(n)
register unsigned int	n;
{
	static long	size;		/* how much do we have on hand? */
	static char	*mstash;	/* where is it?  (kept aligned) */
	register char	*rval;

	n += align((char *) n);	/* keep everything aligned */
	if (n >= 1024) {		/* from hash table */
		rval = malloc(n);	/* aligned */
		strclear(rval, n);
		return(rval);
	}
	

	if (n > size) {
		/* look in the heap (already aligned) */
		if (Mheap) {
			mstash = (char *) Mheap;
			size = Mheap->h_size;
			Mheap = Mheap->h_next;
		} else {
			mstash = malloc(MBUFSIZ);	/* aligned */
			if (mstash == 0) {
				size = 0;
				return(0);
			}
			size = MBUFSIZ;
		}
		strclear(mstash, size);
	}
	rval = mstash;
	mstash += n;
	size -= n;
	return(rval);
}

/* what's the (mis-)alignment of n?  return the complement of (n mod 2^ALIGN) */
align(n)
char	*n;
{
	int	abits;	/* misalignment bits in n */

	abits = (int) n & ~(0xff << ALIGN) & 0xff;
	if (abits == 0)
		return(0);
	return((1 << ALIGN) - abits);
}

#endif /*MYMALLOC*/
