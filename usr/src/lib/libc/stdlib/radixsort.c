/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Dan Bernstein at New York University.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)radixsort.c	5.12 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>

static void shellsort __P((const u_char **, int, int, const u_char *, int));

/*
 * __rspartition is the cutoff point for a further partitioning instead
 * of a shellsort.  If it changes check __rsshell_increments.  Both of
 * these are exported, as the best values are data dependent.
 */
#define	NPARTITION	30

int __rspartition = NPARTITION;
int __rsshell_increments[] = { 4, 1, 0, 0, 0, 0, 0, 0 };

/*
 * Stackp points to context structures, where each structure schedules a
 * partitioning.  Radixsort exits when the stack is empty.
 *
 * If the buckets are placed on the stack randomly, the worst case is when
 * all the buckets but one contain (__rspartition + 1) elements and the bucket
 * pushed on the stack last contains the rest of the elements.  In this case,
 * stack growth is bounded by:
 *
 *	limit = (nelements / (__rspartitions + 1)) - 1;
 *
 * This is a very large number, 102,261,125 for the maximum 32-bit signed
 * integer if NPARTITION is 20.
 *
 * By forcing the largest bucket to be pushed on the stack first, the worst
 * case is when all but two buckets each contain (__rspartition + 1) elements,
 * with the remaining elements split equally between the first and last
 * buckets pushed on the stack.  In this case, stack growth is bounded by
 * the recurrence relation:
 *
 * nelements_max[1] = (NBINS-1) * (__rspartition + 1);
 * nelements_max[i] = (NBINS-3) * (__rspartition + 1) + 2 * npartitions[i-1];
 * which resolves to:
 * nelements_max[i] = ((NBINS-2) * (2^i - 1) + 1) * (__rspartitions + 1),
 *
 * which yields, for a given nelements,
 *
 * i = ceil(log2((((nelements / (__rspartition + 1)) - 1) / (NBINS - 2)) + 1));
 *
 * The bound is:
 *
 *	limit = i * (NBINS - 1);
 *
 * This is a much smaller number, 4845 for the maximum 32-bit signed int.
 */
#define	NBUCKETS	(UCHAR_MAX + 1)

typedef struct _stack {
	const u_char **bot;
	int indx, nmemb;
} CONTEXT;

#define	STACKPUSH { \
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
 * Ex. 10 and 12.  Also, "Three Partition Refinement Algorithms, Paige
 * and Tarjan, SIAM J. Comput. Vol. 16, No. 6, December 1987.
 *
 * This uses a simple sort as soon as a bucket crosses a cutoff point,
 * rather than sorting the entire list after partitioning is finished.
 * This should be an advantage.
 *
 * This is pure MSD instead of LSD of some number of MSD, switching to
 * the simple sort as soon as possible.  Takes linear time relative to
 * the number of bytes in the strings.
 */
int
radixsort(l1, nmemb, tab, endbyte)
	const u_char **l1;
	register int nmemb;
	const u_char *tab;
	u_int endbyte;
{
	register int *cpos, *first, *last, i, indx;
	register const u_char **bot, **l2, **p, *tr;
	CONTEXT *stack, *stackp;
	int c[NBUCKETS + 1], *max, *recd, t1, t2;
	u_char ltab[NBUCKETS];

	if (nmemb <= 1)
		return (0);

	/*
	 * T1 is the constant part of the equation, the number of elements
	 * represented on the stack between the top and bottom entries.
	 */
	if (nmemb > __rspartition)
		t1 = (nmemb / (__rspartition + 1) - 1) / (NBUCKETS - 2) + 1;
	else
		t1 = 0;
	for (i = 0; t1; ++i)
		t1 /= 2;
	if (i) {
		i *= NBUCKETS - 1;
		if ((stack = stackp = malloc(i * sizeof(CONTEXT))) == NULL)
			return (-1);
	}
	else
		stack = stackp = NULL;

	/*
	 * There are two arrays, l1 and l2.  The data is sorted into the temp
	 * stack, and then copied back.  The speedup of using the index to
	 * determine which stack the data is on and simply swapping stacks back
	 * and forth, thus avoiding the copy every iteration, turns out to not
	 * be any faster than the current implementation.
	 */
	if ((l2 = malloc(nmemb * sizeof(u_char **))) == NULL)
		return (-1);
	/*
	 * Tr references a table of sort weights; multiple entries may
	 * map to the same weight; EOS char must have the lowest weight.
	 */
	if (tab) {
		tr = tab;
		recd = c + tr[endbyte];
		if (recd != c && recd != c + NBUCKETS - 1) {
			errno = EINVAL;
			return (-1);
		}
	}
	else {
		for (t1 = 0, t2 = endbyte; t1 < t2; ++t1)
			ltab[t1] = t1 + 1;
		ltab[t2] = 0;
		for (t1 = endbyte + 1; t1 < NBUCKETS; ++t1)
			ltab[t1] = t1;
		tr = ltab;
		recd = c + tr[endbyte];
	}
	last = c + NBUCKETS;
	first = c;
	endbyte = tr[endbyte];

	/* First sort is entire stack. */
	bot = l1;
	indx = 0;

	for (;;) {
		/* Clear bucket count array */
		bzero(first, sizeof(c[0]) * (last - first + 1));
		*recd = 0;

		/*
		 * Compute number of items that sort to the same bucket
		 * for this index.
		 */
		first = c + NBUCKETS - 1;
		last = c;
		for (p = bot, i = nmemb; --i >= 0;) {
			++*(cpos = c + tr[(*p++)[indx]]);
			if (cpos > last && cpos != recd)
				last = cpos;
			if (cpos < first && cpos != recd)
				first = cpos;
		}
		++last;

		/*
		 * Sum the number of characters into c, dividing the temp
		 * stack into the right number of buckets for this bucket,
		 * this index.  C contains the cumulative total of keys
		 * before and included in this bucket, and will later be used
		 * as an index to the bucket.  c[NBUCKETS] (or *last) contains
		 * the total number of elements, for determining how many
		 * elements the last bucket contains.  At the same time
		 * find the largest bucket so it gets pushed first.
		 */
		t1 = (c == recd) ? c[0] : 0;
		t2 = __rspartition;
		for (i = last - (cpos = max = first); i-- >= 0;) {
			if (*cpos > t2) {
				t2 = *cpos;
				max = cpos;
			}
			t1 = *cpos++ += t1;
		}
		if (c != recd)
			*recd += t1;

		/*
		 * Partition the elements into buckets; c decrements through
		 * the bucket, and ends up pointing to the first element of
		 * the bucket.
		 */
		for (i = nmemb; --i >= 0;) {
			--p;
			l2[--c[tr[(*p)[indx]]]] = *p;
		}

		/* Copy the partitioned elements back to user stack */
		bcopy(l2, bot, nmemb * sizeof(u_char *));

		++indx;
		/*
		 * Sort buckets as necessary; don't sort the EOS character
		 * bucket c[endbyte] since it is already sorted.  *max is
		 * pushed first.
		 */
		for (i = last - (cpos = max); --i >= 0;) {
			if ((nmemb =  *(cpos+1) - (t1 = *cpos++)) < 2)
				continue;
			p = bot + t1;
			if (nmemb > __rspartition)
				STACKPUSH
			else
				shellsort(p, indx, nmemb, tr, endbyte);
		}
		for (i = max - (cpos = first); --i >= 0;) {
			if ((nmemb = *(cpos + 1) - (t1 = *cpos++)) < 2)
				continue;
			p = bot + t1;
			if (nmemb > __rspartition)
				STACKPUSH
			else
				shellsort(p, indx, nmemb, tr, endbyte);
		}
		/* Break out when stack is empty. */
		STACKPOP
	}
	free(stack);
	free(l2);
	return (0);
}

/*
 * Shellsort (diminishing increment sort) from Data Structures and
 * Algorithms, Aho, Hopcraft and Ullman, 1983 Edition, page 290;
 * see also Knuth Vol. 3, page 84.  The increments are selected from
 * formula (8), page 95.  Roughly O(N^3/2).
 */
static void
shellsort(p, indx, nmemb, tr, recd)
	register const u_char **p, *tr;
	register int indx, nmemb, recd;
{
	register const u_char *s1, *s2;
	register u_char ch;
	register int incr, *incrp, t1, t2;

	for (incrp = __rsshell_increments; incr = *incrp++;)
		for (t1 = incr; t1 < nmemb; ++t1)
			for (t2 = t1 - incr; t2 >= 0;) {
				s1 = p[t2] + indx;
				s2 = p[t2 + incr] + indx;
				while ((ch = tr[*s1++]) == tr[*s2] &&
				    (ch != recd))
					++s2;
				if (ch > tr[*s2]) {
					s1 = p[t2];
					p[t2] = p[t2 + incr];
					p[t2 + incr] = s1;
					t2 -= incr;
				} else
					break;
			}
}
