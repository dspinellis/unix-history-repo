/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Description:
 *	This is a very fast storage allocator. It allocates blocks of a small
 *	number of different sizes, and keeps free lists of each size.  Blocks
 *	that don't exactly fit are passed up to the next larger size.  In this
 *	implementation, the available sizes are 2^n-4 (or 2^n-12) bytes long.
 *	This is designed for use in a program that uses vast quantities of
 *	memory, but bombs when it runs out.
 *
 * Abnormal Conditions
 *	This is a public domain storage allocator.
 *
 * Modifications:
 *	Date		Programmer		Description of modification
 *	27-FEB-90	Landon Curt Noll	most systems can ignore alloc.c
 *	2-OCT-89	David I. Bell		Add free list. Sbrk now optional
 *	30-JUN-87	Peter Miller		Made it work on Slimos.
 *	21-FEB-82	Chris Kingsley		Initial Coding
 *			kingsley@cit-20		Caltech
 */

#include <stdio.h>
#include "alloc.h"
#include "have_stdlib.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#if 0
#define DEBUG	1		/* defined if debugging code enabled */
#define MSTATS	1		/* defined if memory statistics kept */
#endif
#define	NO_SBRK	1		/* defined if cannot use sbrk */


#if defined(CALC_MALLOC)
/*
 * Make these functions really accessible here.
 */
#undef	malloc
#undef	realloc
#undef	free


#ifdef DEBUG
#define assert(x,v) if ((x)==0) assertfailed(v)
#else
#define assert(x,v)
#endif

typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef char * caddr_t;

#ifdef NO_SBRK
extern char * malloc();
extern char * realloc();
#else
extern char * sbrk();
#endif


/*
 * The overhead on a block is at least 4 bytes.  When free, this space
 * contains a pointer to the next free block, and the bottom two bits must
 * be zero.  When in use, the first byte is set to MAGIC, and the second
 * byte is the size index.  The remaining bytes are for alignment.
 * If range checking (RCHECK) is enabled and the size of the block fits
 * in two bytes, then the top two bytes hold the size of the requested block
 * plus the range checking words, and the header word MINUS ONE.
 */

union overhead
{
	union overhead * ov_next;	/* when free */
	struct
	{
		u_char ovu_magic;	/* magic number */
		u_char ovu_index;	/* bucket # */
#define ov_magic ovu.ovu_magic
#define ov_index ovu.ovu_index
#ifdef RCHECK
		u_short ovu_size;	/* actual block size */
		u_int ovu_rmagic;	/* range magic number */
#define ov_size ovu.ovu_size
#define ov_rmagic ovu.ovu_rmagic
#endif
	} ovu;
};

#define QUANTUM_NBITS	4
#define QUANTUM		(1<<QUANTUM_NBITS)

#define MAGIC	0xff		/* magic # on accounting info */
#define RMAGIC	0x55555555	/* magic # on range info */
#ifdef RCHECK
#define RSLOP	sizeof(u_int)
#else
#define RSLOP	0
#endif

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The overhead information
 * precedes the data area returned to the user.
 */

#define NBUCKETS	32	/* we can't run out on a 32 bit machine! */
static union overhead * nextf[NBUCKETS];
static union overhead *watchloc = 0;	/* location to be watched */

#ifdef MSTATS

/*
 * nmalloc[i] is the difference between the number of mallocs and frees
 * for a given block size.
 */

static u_int nmalloc[NBUCKETS];

#endif


/*
 * Watch some allocated memory to see if it gets blasted.
 */
allocwatch(cp)
	char *cp;
{
	if (cp == NULL) {
		watchloc = NULL;
		return;
	}
	watchloc = (union overhead *)cp - 1;
	assert(watchloc->ov_magic == MAGIC, 10);
}


alloccheck()
{
	assert((watchloc == NULL) || (watchloc->ov_magic == MAGIC), 11);
}


/*
 * NAME
 *	morecore - get more memory
 *
 * SYNOPSIS
 *	void
 *	morecore(bucket)
 *	int bucket;
 *
 * DESCRIPTION
 *	Morecore is used to allocate more memory to the indicated bucket.
 *
 * RETURNS
 *	void
 */
static void
morecore(bucket)
	register u_int	bucket;
{
	register union overhead * op;
	register u_int	rnu;	/* 2^rnu bytes will be requested */
	register u_int	nblks;	/* become nblks blocks of the desired size */
	register u_int	siz;

	assert(bucket >= QUANTUM_NBITS, 1);
	assert(bucket < NBUCKETS, 2);
	assert(!nextf[bucket], 3);
#ifndef NO_SBRK
	/*
	 * Insure memory is allocated on a page boundary.
	 * Should make getpageize() call?
	 */
#define PAGE_SIZE (1<<10)
	siz = (u_int)sbrk(0);
	if(siz & (PAGE_SIZE-1))
		sbrk(PAGE_SIZE - (siz & (PAGE_SIZE-1)));
#endif

	/* take 2k unless the block is bigger than that */
	rnu = (bucket <= 11) ? 11 : bucket;
	assert(rnu >= bucket, 4);
	nblks = 1L << (rnu - bucket); /* how many blocks to get */
	siz = 1L << rnu;

#ifndef NO_SBRK
	op = (union overhead *)sbrk(siz);
	/* no more room! */
	if ((int)op == -1)
		return;
	/*
	 * Round up to minimum allocation size boundary
	 * and deduct from block count to reflect.
	 */
	if((int)op & (QUANTUM-1))
	{
		op = (union overhead *)(((int)op + QUANTUM) &~ (QUANTUM-1));
		nblks--;
	}
#else
	op = (union overhead *)malloc(siz);
	/* no more room! */
	if (!op)
		return;
#endif
	/*
	 * Add new memory allocated to the
	 * free list for this hash bucket.
	 */
	nextf[bucket] = op;
	siz = 1L << bucket;
	while (--nblks)
	{
		op->ov_next = (union overhead *)((caddr_t)op + siz);
		op = op->ov_next;
	}
}


/*
 * NAME
 *	mem_alloc - memory allocator
 *
 * SYNOPSIS
 *	char *
 *	mem_alloc()
 *
 * DESCRIPTION
 *	Mem_alloc is used to allocate memory large enought to fit the requested
 *	size, and on a boundary suitable for placing any value.
 *
 * RETURNS
 *	char *, pointer to base of dynamic memory allocated
 *
 * CAVEAT
 *	Use mem_free() when you are finished with the space.
 */
char *
mem_alloc(nbytes)
	register unsigned long int nbytes;
{
	register union overhead *p;
	register int	bucket;
	register unsigned long int shiftr;

	if (nbytes > ((unsigned int) -1))
		return NULL;
	assert((watchloc == NULL) || (watchloc->ov_magic == MAGIC), 12);
	/*
	 * Convert amount of memory requested into
	 * closest block size stored in hash buckets
	 * which satisfies request.  Account for
	 * space used per block for accounting.
	 */
	nbytes = (nbytes + sizeof (union overhead) + RSLOP + (QUANTUM-1)) &~ (QUANTUM-1);
	shiftr = (nbytes - 1) >> QUANTUM_NBITS;
	/* apart from this loop, this is O(1) */
	bucket = QUANTUM_NBITS;
	while(shiftr)
	{
		shiftr >>= 1;
		bucket++;
	}

	/*
	 * If nothing in hash bucket right now,
	 * request more memory from the system.
	 */
	if (!nextf[bucket])
		morecore(bucket);
	if (!(p = nextf[bucket]))
		return (char*)0;
	/* remove from linked list */
	nextf[bucket] = p->ov_next;
	p->ov_magic = MAGIC;
	p->ov_index = bucket;
#ifdef MSTATS
	nmalloc[bucket]++;
#endif
#ifdef RCHECK
	/*
	 * Record allocated size of block and
	 * bound space with magic numbers
	 */
	if (nbytes <= (1L<<16))
		p->ov_size = nbytes - 1;
	p->ov_rmagic = RMAGIC;
	*((u_int *)((caddr_t)p + nbytes - RSLOP)) = RMAGIC;
#endif
	return ((char *)(p + 1));
}


/*
 * NAME
 *	mem_free - free memory
 *
 * SYNOPSIS
 *	int
 *	mem_free(cp)
 *	char * cp;
 *
 * DESCRIPTION
 *	Mem_free is used to release space allocated by mem_alloc
 *	or mem_realloc.
 *
 * RETURNS
 *	int
 *
 * CAVEAT
 *	do not pass mem_free() an argument that was returned by mem_alloc()
 *	or mem_realloc().
 */
int
mem_free(cp)
	char *	cp;
{
	register u_int	bucket;
	register union overhead *op;

	assert((watchloc == NULL) || (watchloc->ov_magic == MAGIC), 13);
	if (!cp)
		return;
	op = (union overhead *)cp - 1;
	assert(op->ov_magic == MAGIC, 5);	/* make sure it was in use */
	assert(op->ov_index < NBUCKETS, 6);
	assert(op->ov_index >= QUANTUM_NBITS, 7);
#ifdef RCHECK
	assert(op->ov_index > 16 || op->ov_size == (1L<<op->ov_index)-1, 8);
	assert(op->ov_rmagic == RMAGIC, 9);
	assert(op->ov_index > 16 || *(u_int *)((caddr_t)op + op->ov_size + 1 - RSLOP) == RMAGIC, 10);
#endif
#ifndef DEBUG
	if(op->ov_magic != MAGIC)
		return;		/* sanity */
#endif
	bucket = op->ov_index;
	op->ov_next = nextf[bucket];
	nextf[bucket] = op;
#ifdef MSTATS
	nmalloc[bucket]--;
#endif
}


/*
 * NAME
 *	findbucket - find a bucket
 *
 * SYNOPSIS
 *	int
 *	findbucket(freep, srchlen)
 *	union overhead * freep;
 *	int srchlen;
 *
 * DESCRIPTION
 *	Findbucket is used to find the bucket a free block is in.
 *	Search ``srchlen'' elements of each free list for a block whose
 *	header starts at ``freep''.  If srchlen is -1 search the whole list.
 *
 * RETURNS
 *	bucket number, or -1 if not found.
 */
static int
findbucket(freep, srchlen)
	union overhead *	freep;
	int	srchlen;
{
	register union overhead *p;
	register int	i, j;

	for (i = 0; i < NBUCKETS; i++)
	{
		j = 0;
		for (p = nextf[i]; p && j != srchlen; p = p->ov_next)
		{
			if (p == freep)
				return i;
			j++;
		}
	}
	return -1;
}


/*
 * When a program attempts "storage compaction" as mentioned in the
 * old malloc man page, it realloc's an already freed block.  Usually
 * this is the last block it freed; occasionally it might be farther
 * back.  We have to search all the free lists for the block in order
 * to determine its bucket: first we make one pass thru the lists
 * checking only the first block in each; if that fails we search
 * ``realloc_srchlen'' blocks in each list for a match (the variable
 * is extern so the caller can modify it).  If that fails we just copy
 * however many bytes was given to realloc() and hope it's not huge.
 */

static int realloc_srchlen = 4;	/* 4 should be plenty, -1 =>'s whole list */

/*
 * NAME
 *	mem_realloc - change size
 *
 * SYNOPSIS
 *	char
 *	mem_realloc(cp, nbytes)
 *	char * cp;
 *	u_int nbytes;
 *
 * DESCRIPTION
 *	Mem_realloc is used to enlarge a chunk of memory
 *	returned by mem_alloc() or mem_realloc().
 *
 * RETURNS
 *	char *, pointer to base of dynamic memory allocated
 *
 * CAVEAT
 *	Use mem_free() when you are finished with the space.
 */
char *
mem_realloc(cp, nbytes)
	char *cp;
	unsigned long	nbytes;
{
	register u_int	old_nbytes;
	register union overhead *op;
	char *	res;
	register u_int	old_bucket;
	short	was_alloced = 0;

	if (nbytes > ((unsigned int) -1))
		return NULL;
	assert((watchloc == NULL) || (watchloc->ov_magic == MAGIC), 14);
	if (!cp)
		return mem_alloc(nbytes);
	op = (union overhead *)((caddr_t)cp - sizeof (union overhead));
	if (op->ov_magic == MAGIC)
	{
		was_alloced++;
		old_bucket = op->ov_index;
	}
	else
	{
		/*
		 * Already free, doing "compaction".
		 *
		 * Search for the old block of memory on the
		 * free list. First, check the most common
		 * case (last element free'd), then (this failing)
		 * the last ``realloc_srchlen'' items free'd.
		 * If all lookups fail, then assume the size of
		 * the memory block being realloc'd is the
		 * smallest possible.
		 */
		if
		(
			(old_bucket = findbucket(op, 1)) == -1
		&&
			(old_bucket = findbucket(op, realloc_srchlen)) == -1
		)
			old_bucket = QUANTUM_NBITS;
	}
	old_nbytes = (1L << old_bucket) - sizeof(union overhead) - RSLOP;

	/*
	 * avoid the copy if same size block
	 */
	if
	(
		was_alloced
	&&
		nbytes <= old_nbytes
	&&
		nbytes > (old_nbytes >> 1) - sizeof(union overhead) - RSLOP
	)
		return cp;

	/*
	 * grab another chunk
	 */
	if(!(res = mem_alloc(nbytes)))
		return (char*)0;
	assert(cp != res, 11);
	memcpy(res, cp, (nbytes < old_nbytes) ? nbytes : old_nbytes);
	if(was_alloced)
		mem_free(cp);
	return res;
}

#else /*CALC_MALLOC*/

#undef MSTATS

#endif /*CALC_MALLOC*/



/*
 * Allocate a new item from the specified free list.
 * Returns NULL if no item can be allocated.
 */
ALLOCITEM *
allocitem(fp)
	FREELIST *fp;		/* free list header */
{
	FREEITEM *ip;		/* allocated item */

	if (fp->curfree > 0) {
		fp->curfree--;
		ip = fp->freelist;
		fp->freelist = ip->next;
		return (ALLOCITEM *) ip;
	}
	ip = (FREEITEM *) malloc(fp->itemsize);
	if (ip == NULL)
		return NULL;
	return (ALLOCITEM *) ip;
}


/*
 * Free an item by placing it back on a free list.
 * If too many items are on the list, it is really freed.
 */
void
freeitem(fp, ip)
	FREELIST *fp;		/* freelist header */
	FREEITEM *ip;		/* item to be freed */
{
	if (ip == NULL)
		return;
	if (fp->curfree >= fp->maxfree) {
		free((char *) ip);
		return;
	}
	ip->next = fp->freelist;
	fp->freelist = ip;
	fp->curfree++;
}


/*
 * NAME
 *	mem_stats - print memory statistics
 *
 * SYNOPSIS
 *	void
 *	mem_stats(s)
 *	char * s;
 *
 * DESCRIPTION
 *	Mem_stats is used to print out statistics about current memory usage.
 *	``s'' is the title string
 *
 *	Prints two lines of numbers, one showing the length of the free list
 *	for each size category, the second showing the number of mallocs -
 *	frees for each size category.
 *
 * RETURNS
 *	void
 */
/*ARGSUSED*/
void
mem_stats(s)
	char *	s;
{
#ifdef MSTATS
	register int	i, j;
	register union overhead *p;
	int	totfree = 0;
	int	totused = 0;

	fprintf(stderr, "Memory allocation statistics %s\n", s);
	fprintf(stderr, "%11s:%12s%12s%12s\n", "Bucket", "In Use", "Free", "Sum");
	for (i = 0; i < NBUCKETS; i++)
	{
		for (j = 0, p = nextf[i]; p; p = p->ov_next, j++)
			;
		if(!j && !nmalloc[i])
			continue;
		fprintf(stderr, "%11d:%12d%12d%12d\n", (1L<<i), nmalloc[i], j, j+nmalloc[i]);
		totfree += j * (1L << i);
		totused += nmalloc[i] * (1L << i);
	}
	fprintf(stderr, "%11s:%12d%12d%12d\n", "Totals", totused, totfree, totused+totfree);
#else
	fprintf(stderr, 
	    "Memory allocation stats were not compiled into calc\n");
#endif
}

#ifdef DEBUG
void
assertfailed(n)
{
	printf("Assertion %d failed\n", n);
	exit(1);
}
#endif

/* END CODE */
