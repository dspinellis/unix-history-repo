/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "tune.h"

#ifdef MY_MALLOC

/*	avoid break bug */
#ifdef pdp11
# define GRANULE 64
#else
# define GRANULE 0
#endif

/*	C storage allocator
 *	circular first-fit strategy
 *	works with noncontiguous, but monotonically linked, arena
 *	each block is preceded by a ptr to the (pointer of) 
 *	the next following block
 *	blocks are exact number of words long 
 *	aligned to the data type requirements of ALIGN
 *	pointers to blocks must have BUSY bit 0
 *	bit in ptr is 1 for busy, 0 for idle
 *	gaps in arena are merely noted as busy blocks
 *	last block of arena (pointed to by alloct) is empty and
 *	has a pointer to first
 *	idle blocks are coalesced during space search
 *
 *	a different implementation may need to redefine
 *	ALIGN, NALIGN, BLOCK, BUSY, INT
 *	where INT is integer type to which a pointer can be cast
 */

#define INT		int
#define ALIGN		int
#define NALIGN		1
#define WORD		sizeof(union store)
#define BLOCK		1024	/* a multiple of WORD*/
#define BUSY		1
#define NULL		0
#define testbusy(p)	((INT)(p)&BUSY)
#define setbusy(p)	(union store *) ((INT) (p) | BUSY)
#define clearbusy(p)	(union store *) ((INT) (p) &~ BUSY)

union store {
	union store	*ptr;
	ALIGN	dummy[NALIGN];
	int	calloc;		/*calloc clears an array of integers*/
};

static union store	allocs[2],	/*initial arena*/
			*allocp,	/*search ptr*/
			*alloct,	/*arena top*/
			*allocx;	/*for benefit of realloc*/

char	*sbrk();

char *
malloc(nbytes)
unsigned int	nbytes;
{
	register union store	*p,
				*q;
	register int	nw;
	static int	temp;	/* coroutines assume no auto */

	if (allocs[0].ptr == 0) {	/* first time */
		allocs[0].ptr = setbusy(&allocs[1]);
		allocs[1].ptr = setbusy(&allocs[0]);
		alloct = &allocs[1];
		allocp = &allocs[0];
	}
	nw = (nbytes + WORD + WORD - 1) / WORD;
	for (p = allocp; ; ) {
		for (temp = 0; ; ) {
			if (!testbusy(p->ptr)) {
				while (!testbusy((q = p->ptr)->ptr))
					p->ptr = q->ptr;
				if(q >= p + nw && p + nw >= p)
					goto found;
			}
			q = p;
			p = clearbusy(p->ptr);
			if (p > q)
				;
			else if (q != alloct || p != allocs)
				return NULL;
			else if (++temp > 1)
				break;
		}
		temp = ((nw + BLOCK/WORD) / (BLOCK/WORD)) * (BLOCK/WORD);
		q = (union store *) sbrk(0);
		if (q + temp + GRANULE < q)
			return NULL;
		q = (union store *) sbrk(temp * WORD);
		if ((INT) q == -1)
			return NULL;
		alloct->ptr = q;
		if (q != alloct+1)
			alloct->ptr = setbusy(alloct->ptr);
		alloct = q->ptr = q + temp - 1;
		alloct->ptr = setbusy(allocs);
	}
found:
	allocp = p + nw;
	if (q > allocp) {
		allocx = allocp->ptr;
		allocp->ptr = p->ptr;
	}
	p->ptr = setbusy(allocp);
	return (char *) (p + 1);
}

/* freeing strategy tuned for LIFO allocation */

free(ap)
register char	*ap;
{
	register union store	*p = (union store *) ap;

	allocp = --p;
	p->ptr = clearbusy(p->ptr);
}

/*	realloc(p, nbytes) reallocates a block obtained from malloc()
 *	and freed since last call of malloc()
 *	to have new size nbytes, and old content
 *	returns new location, or 0 on failure
*/

char *
realloc(obj, nbytes)
char	*obj;
unsigned int	nbytes;
{
	register union store	*q,
				*p = (union store *) obj;
	union store	*s,
			*t;
	register unsigned int	nw;
	unsigned int	onw;

	if (testbusy(p[-1].ptr))
		free((char *) p);
	onw = p[-1].ptr - p;
	q = (union store *) malloc(nbytes);
	if(q == NULL || q == p)
		return((char *) q);
	s = p;
	t = q;
	nw = (nbytes + WORD - 1)/WORD;
	if (nw < onw)
		onw = nw;
	while (onw-- != 0)
		*t++ = *s++;
	if(q < p && q + nw >= p)
		(q + (q+nw-p))->ptr = allocx;
	return (char *) q;
}

#endif /* MY_MALLOC */
