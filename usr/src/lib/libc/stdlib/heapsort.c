/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ronnie Kon at Mindcraft Inc., Kevin Lew and Elmer Yglesias.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)heapsort.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>

/*
 * Swap two areas of size number of bytes.  Although qsort(3) permits random
 * blocks of memory to be sorted, sorting pointers is almost certainly the
 * common case (and, were it not, could easily be made so).  Regardless, it
 * isn't worth optimizing; the SWAP's get sped up by the cache, and pointer
 * arithmetic gets lost in the time required for comparison function calls.
 */
#define	SWAP(a, b) { \
	cnt = size; \
	do { \
		ch = *a; \
		*a++ = *b; \
		*b++ = ch; \
	} while (--cnt); \
}

/* Copy one block of size size to another. */
#define COPY(a, b) { \
	cnt = size; \
	t1 = a; \
	t2 = b; \
	do { \
		*t1++ = *t2++; \
	} while (--cnt); \
}

/*
 * Build the list into a heap, where a heap is defined such that for
 * the records K1 ... KN, Kj/2 >= Kj for 1 <= j/2 <= j <= N.
 *
 * There two cases.  If j == nmemb, select largest of Ki and Kj.  If
 * j < nmemb, select largest of Ki, Kj and Kj+1.
 */
#define CREATE(initval) { \
	for (i = initval; (j = i * 2) <= nmemb; i = j) { \
		p = (char *)bot + j * size; \
		if (j < nmemb && compar(p, p + size) < 0) { \
			p += size; \
			++j; \
		} \
		t = (char *)bot + i * size; \
		if (compar(p, t) <= 0) \
			break; \
		SWAP(t, p); \
	} \
}

/*
 * Select the top of the heap and 'heapify'.  Since by far the most expensive
 * action is the call to the compar function, a considerable optimization
 * in the average case can be achieved due to the fact that k, the displaced
 * elememt, is ususally quite small, so it would be preferable to first
 * heapify, always maintaining the invariant that the larger child is copied
 * over its parent's record.
 *
 * Then, starting from the *bottom* of the heap, finding k's correct place,
 * again maintianing the invariant.  As a result of the invariant no element
 * is 'lost' when k is assigned its correct place in the heap.
 *
 * The time savings from this optimization are on the order of 15-20% for the
 * average case. See Knuth, Vol. 3, page 158, problem 18.
 */
#define SELECT { \
	for (i = 1; (j = i * 2) <= nmemb; i = j) { \
		p = (char *)bot + j * size; \
		if (j < nmemb && compar(p, p + size) < 0) { \
			p += size; \
			++j; \
		} \
		t = (char *)bot + i * size; \
		COPY(t, p); \
	} \
	for (;;) { \
		j = i; \
		i = j / 2; \
		p = (char *)bot + j * size; \
		t = (char *)bot + i * size; \
		if (j == 1 || compar(k, t) < 0) { \
			COPY(p, k); \
			break; \
		} \
		COPY(p, t); \
	} \
}

/*
 * Heapsort -- Knuth, Vol. 3, page 145.  Runs in O (N lg N), both average
 * and worst.  While heapsort is faster than the worst case of quicksort,
 * the BSD quicksort does median selection so that the chance of finding
 * a data set that will trigger the worst case is nonexistent.  Heapsort's
 * only advantage over quicksort is that it requires little additional memory.
 */
int
heapsort(bot, nmemb, size, compar)
	void *bot;
	size_t nmemb, size;
	int (*compar) __P((const void *, const void *));
{
	register int cnt, i, j, l;
	register char ch, *t1, *t2;
	char *k, *p, *t;

	if (nmemb <= 1)
		return (0);

	if (!size) {
		errno = EINVAL;
		return (-1);
	}

	if ((k = malloc(size)) == NULL)
		return (-1);

	/*
	 * Items are numbered from 1 to nmemb, so offset from size bytes
	 * below the starting address.
	 */
	bot -= size;

	for (l = nmemb / 2 + 1; --l;)
		CREATE(l);

	/*
	 * For each element of the heap, save the largest element into its
	 * final slot, save the displaced element (k), then recreate the
	 * heap.
	 */
	while (nmemb > 1) {
		COPY(k, (char *)bot + nmemb * size);
		COPY((char *)bot + nmemb * size, (char *)bot + size);
		--nmemb;
		SELECT;
	}
	free(k);
	return (0);
}
