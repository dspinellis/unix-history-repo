/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)heapsort.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>

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

/*
 * Build the list into a heap, where a heap is defined such that for
 * the records K1 ... KN, Kj/2 >= Kj for 1 <= j/2 <= j <= N.
 *
 * There two cases.  If j == nmemb, select largest of Ki and Kj.  If
 * j < nmemb, select largest of Ki, Kj and Kj+1.
 *
 * The initial value depends on if we're building the initial heap or
 * reconstructing it after saving a value.
 */
#define	HEAP(initval) { \
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
 * Heapsort -- Knuth, Vol. 3, page 145.  Runs in O (N lg N), both average
 * and worst.  While heapsort is faster than the worst case of quicksort,
 * the BSD quicksort does median selection so that the chance of finding
 * a data set that will trigger the worst case is nonexistent.  Heapsort's
 * only advantage over quicksort is that it requires no additional memory.
 */
heapsort(bot, nmemb, size, compar)
	register void *bot;
	register size_t nmemb, size;
	int (*compar) __P((const void *, const void *));
{
	register char *p, *t, ch;
	register int cnt, i, j, l;

	if (nmemb <= 1)
		return (0);
	if (!size) {
		errno = EINVAL;
		return (-1);
	}
	/*
	 * Items are numbered from 1 to nmemb, so offset from size bytes
	 * below the starting address.
	 */
	bot -= size;

	for (l = nmemb / 2 + 1; --l;)
		HEAP(l);

	/*
	 * For each element of the heap, save the largest element into its
	 * final slot, then recreate the heap.
	 */
	while (nmemb > 1) {
		p = (char *)bot + size;
		t = (char *)bot + nmemb * size;
		SWAP(p, t);
		--nmemb;
		HEAP(1);
	}
	return (0);
}
