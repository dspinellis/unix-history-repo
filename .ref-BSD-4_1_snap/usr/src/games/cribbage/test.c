
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
	int			i, j, is, n, sum;
	CARD			ic, jc;
	CARD			d[ CARDS];

	printf( "Assuming cards are same suit\n" );
	if(  argc == 2  )  {
	    is = atoi( *++argv );
	    printf( "Starting at i = %d\n", is );
	}
	makedeck( deck );
	for( i = is; i < RANKS; i++ )  {		/* first card */
	    ic.rank = i;
	    ic.suit = 0;
	    hand[0] = ic;
	    for( j = 0; j <= i; j++ )  {
		printf( "%d %d: sum = %d\n", i, j, -10000000 );
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
		for( k = 0; k < n - 1; k++ )  {			/* 3rd card */
		    hand[2] = d[k];
		    for( l = k + 1; l < n; l++ )  {		/* 4th card */
			hand[3] = d[l];
			for( m = 0; m < n; m++ )  {		/* cut card */
			    if(  m != l  &&  m != k  )
					    sum += scorehand( hand, d[m], 4 );
			}
		    }
		}
		printf( "%d %d: sum = %d\n", i, j, sum );
		fflush( stdout );
	    }
	}
	printf( "\nthe hand scores %d\n", i );
}

