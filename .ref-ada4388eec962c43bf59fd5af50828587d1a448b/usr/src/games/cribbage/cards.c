/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cards.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "deck.h"
#include "cribbage.h"


/*
 * Initialize a deck of cards to contain one of each type.
 */
void
makedeck(d)
	CARD    d[];
{
	register int i, j, k;

	i = time(NULL);
	i = ((i & 0xff) << 8) | ((i >> 8) & 0xff) | 1;
	srand(i);
	k = 0;
	for (i = 0; i < RANKS; i++)
		for (j = 0; j < SUITS; j++) {
			d[k].suit = j;
			d[k++].rank = i;
		}
}

/*
 * Given a deck of cards, shuffle it -- i.e. randomize it
 * see Knuth, vol. 2, page 125.
 */
void
shuffle(d)
	CARD d[];
{
	register int j, k;
	CARD c;

	for (j = CARDS; j > 0; --j) {
		k = (rand() >> 4) % j;		/* random 0 <= k < j */
		c = d[j - 1];			/* exchange (j - 1) and k */
		d[j - 1] = d[k];
		d[k] = c;
	}
}

/*
 * return true if the two cards are equal...
 */
int
eq(a, b)
	CARD a, b;
{
	return ((a.rank == b.rank) && (a.suit == b.suit));
}

/*
 * isone returns TRUE if a is in the set of cards b
 */
int
isone(a, b, n)
	CARD a, b[];
	int n;
{
	register int i;

	for (i = 0; i < n; i++)
		if (eq(a, b[i]))
			return (TRUE);
	return (FALSE);
}

/*
 * remove the card a from the deck d of n cards
 */
void
cremove(a, d, n)
	CARD a, d[];
	int n;
{
	register int i, j;

	for (i = j = 0; i < n; i++)
		if (!eq(a, d[i]))
			d[j++] = d[i];
	if (j < n)
		d[j].suit = d[j].rank = EMPTY;
}

/*
 * sorthand:
 *	Sort a hand of n cards
 */
void
sorthand(h, n)
	register CARD h[];
	int n;
{
	register CARD *cp, *endp;
	CARD c;

	for (endp = &h[n]; h < endp - 1; h++)
		for (cp = h + 1; cp < endp; cp++)
			if ((cp->rank < h->rank) ||
			    (cp->rank == h->rank && cp->suit < h->suit)) {
				c = *h;
				*h = *cp;
				*cp = c;
			}
}
