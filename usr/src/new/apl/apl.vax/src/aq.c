static char Sccsid[] = "aq.c @(#)aq.c	1.1	10/1/82 Berkeley ";
/*
 *      malloc/free/afreset -- dynamic memory allocation
 *
 *
 * This source file is a slight modificattion of the alloc/free system
 * described by Kernighan and Ritchie in "The C Programming Language",
 * pp. 174-177.
 *
 * The modifications include allocation by a bestfit (rather than
 * firstfit) strategy, and ability to reset memory completely.
 */

/* NOTE: the "afnfree" and "afnused" values are not precise.  They
 * do not currently refect the allocation and release of headers.
 * This will be changed soon (I hope).
 * --J. Bruner
 */


#define NULL    0               /* null value */
#ifndef vax
#define NALLOC  128             /* # of units requested on each "sbrk" */
#else
#define	NALLOC	1024		/* lots of memory each time for vaxes */
#endif

typedef int     ALIGN;          /* force alignment on PDP-11 and VAX */

union header {                          /* free block header structure */
	struct {
		union header *ptr;      /* next free block */
		unsigned size;          /* size of this free block */
	} s;
	ALIGN x;                        /* force alignment of blocks */
};


typedef union header HEADER;


static HEADER base;                     /* empty list to get started */
static HEADER *allocp = NULL;           /* last allocated block */

int aftrace = 0;
int afnfree, afnused;


char *
malloc(nbytes)
unsigned nbytes;
{
	HEADER *morecore();
	register HEADER *p, *q;
	register HEADER *rp;
	HEADER *rq;
	register unsigned nunits;

	/* malloc is a general-purpose memory allocator.  It is called
	 * with the desired number of bytes, expressed as an unsigned
	 * integer, and it returns the address of the allocated memory.
	 * If "aftrace" is non-zero, a trace of the allocation will
	 * be printed.  If it is not possible to alloate what is
	 * requested, the error "workspace exceeded" will result.
	 *
	 * This routine was originally called "alloc" but was changed
	 * to "malloc" because an increasing number of library routines
	 * perform dynamic memory allocation.  A macro in "apl.h"
	 * converts calls on "alloc" to calls on "malloc".
	 */

	nunits = 1+(nbytes+sizeof(HEADER)-1)/sizeof(HEADER);
	if ((q=allocp) == NULL){
		base.s.ptr = allocp = q = &base;        /* start list */
		base.s.size = 0;
	}


	/* Search list for smallest free block */

	rp = NULL;
	for(p=q->s.ptr;;q=p, p=p->s.ptr){
		while(1){
			if (p->s.size >= nunits){
				if ((!rp) || p->s.size < rp->s.size){
					rp = p;
					rq = q;
				}
				if (p->s.size == nunits) break;
			}

			if (p == allocp)
				break;

			q = p;
			p = p->s.ptr;
		}

		if (rp) break;
		if ((p=morecore(nunits)) == NULL)
			error("workspace exceeded");
	}



	/* Allocate memory as needed */

	if (rp->s.size == nunits)
		rq->s.ptr = rp->s.ptr;
	else {
		rp->s.size -= nunits;
		rp += rp->s.size;
		rp->s.size = nunits;
	}
	allocp = rq;

	if (aftrace)
		printf("[alloc: %d at %o]\n",
			(int)nunits*sizeof(HEADER), rp);

	afnfree -= nunits;
	afnused += nunits;
	return((char *)(rp+1));
}


static HEADER *
morecore(nu)
unsigned nu;
{
	char *sbrk();
	register char *cp;
	register HEADER *up;
	register int rnu;

	/* Ask system for more memory.  Requests are made in blocks
	 * of NALLOC header units.  Returns NULL if request fails,
	 * "allocp" on success.
	 */

	rnu = NALLOC * ((nu+NALLOC-1) / NALLOC);
	cp = sbrk(rnu * sizeof(HEADER));
	if ((int)cp == -1)
		return(NULL);                   /* no more space */

	if (aftrace)
		printf("[morecore: %d at %o]\n", rnu*sizeof(HEADER),
			cp);

	up = (HEADER *)cp;
	up->s.size = rnu;
	free((char *)(up+1));
	return(allocp);
}


free(ap)
char *ap;
{
	register HEADER *p, *q;
	register unsigned fsize;


	/* Put block into free list.  Used by "morecore" to put a newly-
	 * allocated block of memory into the freelist -- also used to
	 * return a previously allocated block to the freelist.
	 */

	p = (HEADER *)ap - 1;           /* point to header */
	fsize = p->s.size;

	for (q=allocp; !(p > q && p < q->s.ptr); q=q->s.ptr)
		if (q >= q->s.ptr && (p > q || p < q->s.ptr))
			break;

	if (p+p->s.size == q->s.ptr){   /* join to upper nbr */
		p->s.size += q->s.ptr->s.size;
		p->s.ptr = q->s.ptr->s.ptr;
	} else
		p->s.ptr = q->s.ptr;

	if (q+q->s.size == p){          /* join to lower nbr */
		q->s.size += p->s.size;
		q->s.ptr = p->s.ptr;
	} else
		q->s.ptr = p;

	allocp = q;

	afnfree += fsize;
	afnused -= fsize;
	if (aftrace)
		printf("[free: %d at %o]\n", fsize*sizeof(HEADER),  ap);

}


afreset(){

	extern end;

	/* Zap all dynamic allocation by resettting the lists and
	 * releasing all additional memory.
	 */

	allocp = NULL;
	brk((char *)&end);
	afnfree = afnused = 0;
	if (aftrace)
		printf("[afreset: dynamic allocation released]\n");

}
