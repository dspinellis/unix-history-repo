/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)mem.c	9.6 92/08/25";
#endif

#include "def.h"

/* exports */
long Ncount;
extern void freelink(), wasted(), freetable();
extern long allocation();

/* imports */
extern char *Netchars;
extern int Vflag;
extern void die();
extern int strlen();
#ifdef DEBUG
extern char *sbrk();
#endif

/* privates */
STATIC void nomem();
static link *Lcache;
static unsigned int Memwaste;

link	*
newlink()
{	register link *rval;

	if (Lcache) {
	 	rval = Lcache;
		Lcache = Lcache->l_next;
		strclear((char *) rval, sizeof(link));
	} else if ((rval = (link * ) calloc(1, sizeof(link))) == 0)
		nomem();
	return rval;
}

/* caution: this destroys the contents of l_next */
void
freelink(l)
	link *l;
{
	l->l_next = Lcache;
	Lcache = l;
}

node	*
newnode()
{	register node *rval;

	if ((rval = (node * ) calloc(1, sizeof(node))) == 0)
		nomem();
	Ncount++;
	return rval;
}

dom *
newdom()
{       register dom *rval;

	if ((rval = (dom * ) calloc(1, sizeof(dom))) == 0)
		nomem();

	return rval;
}


char	*
strsave(s)
	char *s;
{	register char *r;

	if ((r = malloc((unsigned) strlen(s) + 1)) == 0)
		nomem();
	(void) strcpy(r, s);
	return r;
}

#ifndef strclear
void
strclear(str, len)
	register char *str;
	register long len;
{
	while (--len >= 0)
		*str++ = 0;
}
#endif /*strclear*/

node	**
newtable(size)
	long size;
{	register node **rval;

	if ((rval = (node **) calloc(1, (unsigned int) size * sizeof(node *))) == 0) 
		nomem();
	return rval;
}

void
freetable(t, size)
	node **t;
	long size;
{
#ifdef MYMALLOC
	STATIC void addtoheap();

	addtoheap((char *) t, size * sizeof(node *));
#else
	free((char *) t);
#endif
}

STATIC void
nomem()
{
#ifdef DEBUG
	static char epitaph[128];

	sprintf(epitaph, "out of memory (%ldk allocated)", allocation());
	die(epitaph);
#else
	die("out of memory");
#endif
}

/* data space allocation -- main sets `dataspace' very early */
long
allocation()
{
#ifdef DEBUG
	static char *dataspace;
	long rval;

	if (dataspace == 0) {		/* first time */
		dataspace = sbrk(0);
		return 0;
	}
	rval = (sbrk(0) - dataspace)/1024;
	if (rval < 0)			/* funny architecture? */
		rval = -rval;
	return rval;
#else
	return 0;
#endif
}

/* how much memory has been wasted? */
void
wasted()
{
	if (Memwaste == 0)
		return;
	vprintf(stderr, "memory allocator wasted %ld bytes\n", Memwaste);
}

#ifdef MYMALLOC

/* use c library malloc/calloc here, and here only */
#undef malloc
#undef calloc

/* imports */
extern char *malloc(), *calloc();

/* private */
STATIC int align();

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

STATIC void
addtoheap(p, size)
	char *p;
	long size;
{	int adjustment;
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
	register unsigned int n;
{	static unsigned int size; /* how much do we have on hand? */
	static char *mstash;	  /* where is it? */
	register char *rval;

	if (n >= 1024) {		/* for hash table */
		rval = malloc(n);	/* aligned */
		if (rval)
			strclear(rval, n);
		return rval;
	}

	n += align((char *) n);	/* keep everything aligned */

	if (n > size) {
		Memwaste += size;	/* toss the fragment */
		/* look in the heap */
		if (Mheap) {
			mstash = (char *) Mheap;	/* aligned */
			size = Mheap->h_size;
			Mheap = Mheap->h_next;
		} else {
			mstash = malloc(MBUFSIZ);	/* aligned */
			if (mstash == 0) {
				size = 0;
				return 0;
			}
			size = MBUFSIZ;
		}
		strclear(mstash, size);		/* what if size > 2^16? */
	}
	rval = mstash;
	mstash += n;
	size -= n;
	return rval;
}

/*
 * what's the (mis-)alignment of n?  return the complement of
 * n mod 2^ALIGN
 */
STATIC int
align(n)
	char *n;
{	register int abits;	/* misalignment bits in n */

	abits = (int) n & ~(0xff << ALIGN) & 0xff;
	if (abits == 0)
		return 0;
	return (1 << ALIGN) - abits;
}

#endif /*MYMALLOC*/
