/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)test.c	5.1 (Berkeley) 5/30/85";
#endif not lint

#include	<stdio.h>
#include	"deck.h"


CARD		known[ CARDS ];			/* a deck */
CARD		deck[ CARDS ];			/* a deck */
CARD		hand[ 4 ];			/* a hand */

int		knownum;


main( argc, argv )

    int		argc;
    char	*argv[];
{
	register  int		k, l, m;
	int			i, j, is, n, sum, sum2;
	CARD			ic, jc;
	CARD			d[ CARDS];
	extern char		expl[];

	printf( "Assuming cards are same suit\n" );
	if(  argc == 2  )  {
	    is = atoi( *++argv );
	    printf( "Starting at i = %d\n", is );
	}
	makedeck( deck );
# if 0
	for( i = is; i < RANKS; i++ )  {		/* first card */
	    ic.rank = i;
	    ic.suit = 0;
	    hand[0] = ic;
	    for( j = 0; j <= i; j++ )  {
		printf( "%d %d: sum  = %d\n", i, j, -10000000 );
		printf( "%d %d: sum2 = %d\n", i, j, -10000000 );
	    }
	    for( j = i + 1; j < RANKS; j++ )  {		/* second card */
		jc.rank = j;
		jc.suit = 0;
		hand[1] = jc;
		for( k = 0; k < CARDS; k++ )  d[k] = deck[k];
		n = CARDS;
		remove( ic, d, n-- );
		remove( jc, d, n-- );
		sum = 0;
		sum2 = 0;
		for( k = 0; k < n - 1; k++ )  {			/* 3rd card */
		    hand[2] = d[k];
		    for( l = k + 1; l < n; l++ )  {		/* 4th card */
			hand[3] = d[l];
			for( m = 0; m < n; m++ )  {		/* cut card */
			    if(  m != l  &&  m != k  )
					    sum += scorehand(hand, d[m], 4, FALSE, FALSE);
					    sum2 += scorehand(hand, d[m], 4, TRUE, FALSE);
			}
		    }
		}
		printf( "%d %d: sum  = %d\n", i, j, sum );
		printf( "%d %d: sum2 = %d\n", i, j, sum2 );
		fflush( stdout );
	    }
	}
	printf( "\nthe hand scores %d\n", i );
# else
	hand[0].rank = 0;
	hand[1].rank = 1;
	hand[2].rank = 2;
	hand[3].rank = 3;
	hand[4].rank = 4;
	hand[0].suit = 0;
	hand[1].suit = 0;
	hand[2].suit = 0;
	hand[3].suit = 0;
	hand[4].suit = 0;
	printf("scorehand of hand = %d\n", scorehand(hand, hand[4], CINHAND, FALSE, TRUE));
	printf("\t%s\n", expl);
	hand[0].rank = 0;
	hand[1].rank = 1;
	hand[2].rank = 2;
	hand[3].rank = 3;
	hand[4].rank = 4;
	hand[0].suit = 0;
	hand[1].suit = 0;
	hand[2].suit = 0;
	hand[3].suit = 0;
	hand[4].suit = 0;
	printf("scorehand of crib = %d\n", scorehand(hand, hand[4], CINHAND, TRUE, TRUE));
	printf("\t%s\n", expl);
	hand[0].rank = 0;
	hand[1].rank = 1;
	hand[2].rank = 2;
	hand[3].rank = 3;
	hand[4].rank = 4;
	hand[0].suit = 0;
	hand[1].suit = 0;
	hand[2].suit = 0;
	hand[3].suit = 0;
	hand[4].suit = 1;
	printf("scorehand of hand = %d\n", scorehand(hand, hand[4], CINHAND, FALSE, TRUE));
	printf("\t%s\n", expl);
	hand[0].rank = 0;
	hand[1].rank = 1;
	hand[2].rank = 2;
	hand[3].rank = 3;
	hand[4].rank = 4;
	hand[0].suit = 0;
	hand[1].suit = 0;
	hand[2].suit = 0;
	hand[3].suit = 0;
	hand[4].suit = 1;
	printf("scorehand of crib = %d\n", scorehand(hand, hand[4], CINHAND, TRUE, TRUE));
	printf("\t%s\n", expl);
# endif
}
