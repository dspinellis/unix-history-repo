/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)deck.h	5.3 (Berkeley) 6/1/90
 */

# define	bool	char

# define	CC_D	deck[0]
# define	CH_D	deck[1]

struct dk_st {			/* deck description structure		*/
	int	num_cards;		/* number of cards in deck	*/
	int	last_card;		/* number of last card picked	*/
	bool	gojf_used;		/* set if gojf card out of deck	*/
	long	*offsets;		/* offests for start of cards	*/
};

typedef struct dk_st	DECK;
