/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)radixsort.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/errno.h>
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>

#define	NCHARS	(UCHAR_MAX + 1)

/*
 * shellsort (diminishing increment sort) from Data Structures and
 * Algorithms, Aho, Hopcraft and Ullman, 1983 Edition, page 290;
 * see also Knuth Vol. 3, page 84.  The increments are selected from
 * formula (8), page 95.  Roughly O(N^3/2).
 *
 * __rspartition is the cutoff point for a further partitioning instead
 * of a shellsort.  If it changes check __rsshell_increments.  Both of
 * these are exported, as the best values are data dependent.  Unrolling
 * this loop has not proven worthwhile.
 */
#define	NPARTITION	40
int __rspartition = NPARTITION;
int __rsshell_increments[] = { 4, 1, 0, 0, 0, 0, 0, 0 };
#define SHELLSORT { \
	register u_char ch, *s1, *s2; \
	register int incr, *incrp; \
	for (incrp = __rsshell_increments; incr = *incrp++;) \
		for (t1 = incr; t1 < nmemb; ++t1) \
			for (t2 = t1 - incr; t2 >= 0;) { \
				s1 = p[t2] + indx; \
				s2 = p[t2 + incr] + indx; \
				while ((ch = tr[*s1++]) == tr[*s2] && ch) \
					++s2; \
				if (ch > tr[*s2]) { \
					s1 = p[t2]; \
					p[t2] = p[t2 + incr]; \
					p[t2 + incr] = s1; \
					t2 -= incr; \
				} else \
					break; \
			} \
}

/*
 * stack points to context structures.  Each structure defines a
 * scheduled partitioning.  Radixsort exits when the stack is empty.
 *
 * The stack size is data dependent, and guessing is probably not
 * worthwhile.  The initial stack fits in 1K with four bytes left over
 * for malloc.  The initial size is exported, as the best value is
 * data, and possibly, system, dependent.
 */
typedef struct _stack {
	u_char **bot;
	int indx, nmemb;
} CONTEXT;

int __radix_stacksize = (1024 - 4) / sizeof(CONTEXT);
#define	STACKPUSH { \
	if (stackp == estack) { \
		t1 = stackp - stack; \
		stackp = stack; \
		if (!(stack = (CONTEXT *)realloc((char *)stack, \
		    (__radix_stacksize *= 2) * sizeof(CONTEXT)))) { \
			t1 = errno; \
			free((char *)l2); \
			if (stackp) \
				free((char *)stackp); \
			errno = t1; \
			return(-1); \
		} \
		stackp = stack + t1; \
		estack = stack + __radix_stacksize; \
	} \
	stackp->bot = p; \
	stackp->nmemb = nmemb; \
	stackp->indx = indx; \
	++stackp; \
}

#define	STACKPOP { \
	if (stackp == stack) \
		break; \
	--stackp; \
	bot = stackp->bot; \
	nmemb = stackp->nmemb; \
	indx = stackp->indx; \
}

/*
 * A variant of MSD radix sorting; see Knuth Vol. 3, page 177, and 5.2.5,
 * Ex. 10 and 12.  Also, "Three Partition Refinement Algorithms, Paige and
 * Tarjan, SIAM J. Comput. Vol. 16, No. 6, December 1987.
 *
 * This uses a simple sort as soon as a bucket crosses a cutoff point, rather
 * than sorting the entire list after partitioning is finished.
 *
 * This is pure MSD instead of LSD of some number of MSD, switching to the
 * simple sort as soon as possible.  Takes linear time relative to the number
 * of bytes in the strings.
 */
radixsort(l1, nmemb, tab, endbyte)
	u_char **l1, *tab, endbyte;
	register int nmemb;
{
	register int i, indx, t1, t2;
	register u_char **l2, **p, **bot, *tr;
	CONTEXT *estack, *stack, *stackp;
	int c[NCHARS + 1];
	u_char ltab[NCHARS];

	if (nmemb <= 1)
		return(0);

	/*
	 * there are two arrays, one provided by the user (l1), and the
	 * temporary one (l2).  The data is sorted to the temporary stack,
	 * and then copied back.  The speedup of using index to determine
	 * which stack the data is on and simply swapping stacks back and
	 * forth, thus avoiding the copy every iteration, turns out to not
	 * be any faster than the current implementation.
	 */
	if (!(l2 = (u_char **)malloc(sizeof(u_char *) * nmemb)))
		return(-1);

	/* initialize stack */
	stack = stackp = estack = NULL;

	/*
	 * tr references a table of sort weights; multiple entries may
	 * map to the same weight; EOS char must have the lowest weight.
	 */
	if (tab)
		tr = tab;
	else {
		tr = ltab;
		for (t1 = 0, t2 = endbyte; t1 < t2; ++t1)
			tr[t1] = t1 + 1;
		tr[t2] = 0;
		for (t1 = endbyte + 1; t1 < NCHARS; ++t1)
			tr[t1] = t1;
	}

	/* first sort is entire stack */
	bot = l1;
	indx = 0;

	for (;;) {
		/* clear bucket count array */
		bzero((char *)c, sizeof(c));

		/*
		 * compute number of items that sort to the same bucket
		 * for this index.
		 */
		for (p = bot, i = nmemb; i--;)
			++c[tr[(*p++)[indx]]];

		/*
		 * sum the number of characters into c, dividing the temp
		 * stack into the right number of buckets for this bucket,
		 * this index.  C contains the cumulative total of keys
		 * before and included in this bucket, and will later be
		 * used as an index to the bucket.  c[NCHARS] contains
		 * the total number of elements, for determining how many
		 * elements the last bucket contains.
		 */
		for (i = 1; i <= NCHARS; ++i)
			c[i] += c[i - 1];

		/*
		 * partition the elements into buckets; c decrements
		 * through the bucket, and ends up pointing to the
		 * first element of the bucket.
		 */
		for (i = nmemb; i--;) {
			--p;
			l2[--c[tr[(*p)[indx]]]] = *p;
		}

		/* copy the partitioned elements back to user stack */
		bcopy(l2, bot, nmemb * sizeof(u_char *));

		++indx;
		/*
		 * sort buckets as necessary; don't sort c[0], it's the
		 * EOS character bucket, and nothing can follow EOS.
		 */
		for (i = NCHARS - 1; i; i--) {
			if ((nmemb = c[i + 1] - (t1 = c[i])) < 2)
				continue;
			p = bot + t1;
			if (nmemb > __rspartition)
				STACKPUSH
			else
				SHELLSORT
		}
		/* break out when stack is empty */
		STACKPOP
	}

	free((char *)l2);
	free((char *)stack);
#ifdef STATS
	(void)fprintf(stderr, "max stack %u.\n", __radix_stacksize);
#endif
	return(0);
}
