
#include	<stdio.h>
#include	"deck.h"


#define		LOGFILE		"/usr/games/lib/criblog"
#define		INSTRCMD	"ul /usr/games/lib/crib.instr | more -f"


CARD		deck[ CARDS ];			/* a deck */
CARD		phand[ FULLHAND ];		/* player's hand */
CARD		chand[ FULLHAND ];		/* computer's hand */
CARD		crib[ CINHAND ];		/* the crib */
CARD		turnover;			/* the starter */

CARD		known[ CARDS ];			/* cards we have seen */
int		knownum		= 0;		/* number of cards we know */

int		pscore		= 0;		/* player score in current game */
int		cscore		= 0;		/* comp score in current game */
int		pgames		= 0;		/* number games player won */
int		cgames		= 0;		/* number games comp won */
int		gamecount	= 0;		/* number games played */
int		glimit		= LGAME;	/* game playe to glimit */

BOOLEAN		iwon		= FALSE;	/* if comp won last game */
BOOLEAN		explain		= FALSE;	/* player mistakes explained */
BOOLEAN		rflag		= FALSE;	/* if all cuts random */
BOOLEAN		quiet		= FALSE;	/* if suppress random mess */

char		expl[ 128 ];			/* explanation */


main( argc, argv )

    int		argc;
    char	*argv[];
{
	FILE			*fopen();
	FILE			*f;
	char			*getline();
	register  char		*p;
	BOOLEAN			playing;
	char			*s;		/* for reading arguments */
	char			bust;		/* flag for arg reader */

	while( --argc > 0 )  {
	    if(  (*++argv)[0] != '-'  )  {
		fprintf( stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n" );
		exit( 1 );
	    }
	    bust = FALSE;
	    for( s = argv[0] + 1; *s != NULL; s++ ) {
		switch( *s )  {

		    case  'e':
			explain = TRUE;
			break;

		    case  'q':
			quiet = TRUE;
			break;

		    case  'r':
			rflag = TRUE;
			break;

		    default:
			fprintf( stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n" );
			exit( 2 );
			break;
		}
		if( bust )  break;
	    }
	}
	if( !quiet )  {
	    printf( "\nDo you need instructions for cribbage? " );
	    p = getline();
	    if(  *p == 'Y'  )  {
		system( INSTRCMD );
		printf( "\nFor the rules of this game, do 'man cribbage'\n" );
	    }
	}
	playing = TRUE;
	do  {
	    printf( quiet ? "\nL or S? " :
				"\nLong (to 121) or Short (to 61)? " );
	    p = getline();
	    glimit = ( *p == 'S' ? SGAME : LGAME );
	    game();
	    printf( "\nAnother game? " );
	    p = getline();
	    playing = (*p == 'Y');
	}  while( playing );
	if(  ( f = fopen(LOGFILE, "a") ) != NULL  )  {
	    fprintf( f, "Won %5.5d, Lost %5.5d\n",  cgames, pgames );
	    fclose( f );
	}
}



/*
 * play one game up to glimit points
 */

game()
{
	register  int		i, j;
	BOOLEAN			flag;
	BOOLEAN			compcrib;

	makedeck( deck );
	shuffle( deck );
	if( gamecount == 0 )  {
	    flag = TRUE;
	    do  {
		if( rflag )  {				/* player cuts deck */
		    i = ( rand() >>4 ) % CARDS;		/* random cut */
		}
		else  {
		    printf( quiet ? "\nCut for crib? " :
			"\nCut to see whose crib it is -- low card wins? " );
		    i = number( 0, CARDS - 1 );
		}
		do  {					/* comp cuts deck */
		    j = ( rand() >> 4 ) % CARDS;
		}  while( j == i );
		printf( quiet ? "You cut " : "You cut the " );
		printcard( deck[i], FALSE );
		printf( quiet ? ", I cut " : ",  I cut the " );
		printcard( deck[j], FALSE );
		printf( ".\n" );
		flag = ( deck[i].rank == deck[j].rank );
		if( flag )  {
		    printf( quiet ? "We tied...\n" :
				"We tied and have to try again...\n" );
		    shuffle( deck );
		    continue;
		}
		else  {
		    compcrib = deck[i].rank > deck[j].rank;
		}
	    }  while( flag );
	}
	else  {
	    printf( "Loser (%s) gets first crib.\n",  (iwon ? "you" : "me") );
	    compcrib = !iwon;
	}
	pscore = cscore = 0;
	flag = TRUE;
	do  {
	    shuffle( deck );
	    flag = !playhand( compcrib );
	    compcrib = !compcrib;
	    printf( "\nYou have %d points, I have %d.\n", pscore, cscore );
	}  while( flag );
	++gamecount;
	if(  cscore < pscore  )  {
	    if(  glimit - cscore > 30  )  {
		if(  glimit - cscore > 60  )  {
		    printf( "\nYOU DOUBLE SKUNKED ME!\n\n" );
		    pgames += 4;
		}
		else  {
		    printf( "\nYOU SKUNKED ME!\n\n" );
		    pgames += 2;
		}
	    }
	    else  {
		printf( "\nYOU WON!\n\n" );
		++pgames;
	    }
	    iwon = FALSE;
	}
	else  {
	    if(  glimit - pscore > 30  )  {
		if(  glimit - pscore > 60  )  {
		    printf( "\nI DOUBLE SKUNKED YOU!\n\n" );
		    cgames += 4;
		}
		else  {
		    printf( "\nI SKUNKED YOU!\n\n" );
		    cgames += 2;
		}
	    }
	    else  {
		printf( "\nI WON!\n\n" );
		++cgames;
	    }
	    iwon = TRUE;
	}
	printf( "\nI have won %d games, you have won %d\n", cgames, pgames );
}



/*
 * hand does up one hand of the game
 */

playhand( mycrib )

    BOOLEAN		mycrib;
{
	register  int			deckpos;

	knownum = 0;
	deckpos = deal( mycrib );
	sorthand( chand, FULLHAND );
	sorthand( phand, FULLHAND );
	makeknown( chand, FULLHAND );
	printf( "\nYour hand is: " );
	prhand( phand, FULLHAND, TRUE );
	printf( ".\n" );
	discard( mycrib );
	if(  cut( mycrib, deckpos )  )  return( TRUE );
	if(  peg( mycrib )  )  return( TRUE );
	if(  score( mycrib )  )  return( TRUE );
	return( FALSE );
}



/*
 * deal cards to both players from deck
 */

deal( mycrib )
{
	register  int		i, j;

	j = 0;
	for( i = 0; i < FULLHAND; i++ )  {
	    if( mycrib )  {
		phand[i] = deck[j++];
		chand[i] = deck[j++];
	    }
	    else  {
		chand[i] = deck[j++];
		phand[i] = deck[j++];
	    }
	}
	return( j );
}



/*
 * handle players discarding into the crib...
 * note: we call cdiscard() after prining first message so player doesn't wait
 */

discard( mycrib )

    BOOLEAN		mycrib;
{

	CARD			crd;

	printf( "It's %s crib...\n", (mycrib ? "my" : "your") );
	printf( quiet ? "Discard --> " : "Discard a card --> " );
	cdiscard( mycrib );			/* puts best discard at end */
	crd = phand[ infrom(phand, FULLHAND) ];
	remove( crd, phand, FULLHAND);
	crib[0] = crd;
    /* next four lines same as last four except for cdiscard() */
	printf( quiet ? "Discard --> " : "Discard a card --> " );
	crd = phand[ infrom(phand, FULLHAND - 1) ];
	remove( crd, phand, FULLHAND - 1 );
	crib[1] = crd;
	crib[2] = chand[4];
	crib[3] = chand[5];
	chand[4].rank = chand[4].suit = chand[5].rank = chand[5].suit = -1;
}



/*
 * cut the deck and set turnover
 */

cut( mycrib, pos )

    BOOLEAN		mycrib;
    int			pos;
{
	register  int		i;
	BOOLEAN			win = FALSE;

	if( mycrib )  {
	    if( rflag )  {			/* random cut */
		i = ( rand() >> 4 ) % (CARDS - pos);
	    }
	    else  {
		printf( quiet ? "Cut the deck? " :
			"How many cards down do you wish to cut the deck? " );
		i = number( 0, CARDS - pos - 1 );
	    }
	    turnover = deck[i + pos];
	    printf( quiet ? "You cut " : "You cut the " );
	    printcard( turnover, FALSE );
	    printf( ".\n" );
	    if(  turnover.rank == JACK  )  {
		printf( "I get two for his heels.\n" );
		win = chkscr( &cscore, 2 );
	    }
	}
	else  {
	    i = ( rand() >> 4 ) % (CARDS - pos)  +  pos;
	    turnover = deck[i];
	    printf( quiet ? "I cut " : "I cut the " );
	    printcard( turnover, FALSE );
	    printf( ".\n" );
	    if(  turnover.rank == JACK  )  {
		printf( "You get two for his heels.\n" );
		win = chkscr( &pscore, 2 );
	    }
	}
	makeknown( &turnover, 1 );
	return( win );
}



/*
 * handle all the pegging...
 */

peg( mycrib )

    BOOLEAN		mycrib;
{
	static  CARD		ch[ CINHAND ],  ph[ CINHAND ];
	static  CARD		table[ 14 ];
	CARD			crd;
	register  int		i, j, k;
	int			l;
	int			cnum, pnum, tcnt, sum;
	BOOLEAN			myturn, mego, ugo, last, played;

	cnum = pnum = CINHAND;
	for( i = 0; i < CINHAND; i++ )  {	/* make copies of hands */
	    ch[i] = chand[i];
	    ph[i] = phand[i];
	}
	tcnt = 0;			/* index to table of cards played */
	sum = 0;			/* sum of cards played */
	mego = ugo = FALSE;
	myturn = !mycrib;
	do  {
	    last = TRUE;				/* enable last flag */
	    if(  myturn  )  {				/* my tyrn to play */
		if(  !anymove( ch, cnum, sum )  )  {	/* if no card to play */
		    if(  !mego  &&  cnum  )  {		/* go for comp? */
			printf( "GO.\n" );
			mego = TRUE;
		    }
		    if(  anymove( ph, pnum, sum )  )  {	/* can player move? */
			myturn = !myturn;
		    }
		    else  {				/* give him his point */
			printf( quiet ? "You get one.\n" :
					"You get one point.\n" );
			if(  chkscr( &pscore, 1 )  )  return( TRUE );
			sum = 0;
			mego = ugo = FALSE;
			tcnt = 0;
		    }
		}
		else  {
		    played = TRUE;
		    j = -1;
		    k = 0;
		    for( i = 0; i < cnum; i++ )  {	/* maximize score */
			l = pegscore( ch[i], table, tcnt, sum );
			if(  l > k  )  {
			    k = l;
			    j = i;
			}
		    }
		    if(  j < 0  )  {			/* if nothing scores */
			j = cchose( ch, cnum, sum );
		    }
		    crd = ch[j];
		    printf( quiet ? "I play " : "I play the " );
		    printcard( crd, FALSE );
		    remove( crd, ch, cnum-- );
		    sum += VAL( crd.rank );
		    table[ tcnt++ ] = crd;
		    printf( ".  Total is %d", sum );
		    if( k > 0 )  {
			printf( quiet ? ".  I got %d" :
						".  I got %d points", k );
			if(  chkscr( &cscore, k )  )  return( TRUE );
		    }
		    printf( ".\n" );
		    myturn = !myturn;
		}
	    }
	    else  {
		if(  !anymove( ph, pnum, sum )  )  {	/* can player move? */
		    if(  !ugo  &&  pnum  )  {		/* go for player */
			printf( "You have a GO.\n" );
			ugo = TRUE;
		    }
		    if(  anymove( ch, cnum, sum )  )  {	/* can computer play? */
			myturn = !myturn;
		    }
		    else  {
			printf( quiet ? "I get one.\n" : "I get one point.\n" );
			if(  chkscr( &cscore, 1 )  )  return( TRUE );
			sum = 0;
			mego = ugo = FALSE;
			tcnt = 0;
		    }
		}
		else  {					/* player plays */
		    played = FALSE;
		    if(  pnum == 1  )  {
			crd = ph[0];
			printf( "You play your last card, the " );
			printcard( crd, TRUE );
			printf( ".  " );
		    }
		    else  {
			do  {
			    printf( "Your play ( " );
			    prhand( ph, pnum, TRUE );
			    printf( " ): " );
			    crd = ph[ infrom(ph, pnum) ];
			    if(  sum + VAL( crd.rank )  <=  31  )  {
				break;
			    }
			    else  {
				printf( "Total > 31 -- try again.\n" );
			    }
			}  while( TRUE );
		    }
		    makeknown( &crd, 1 );
		    remove( crd, ph, pnum-- );
		    i = pegscore( crd, table, tcnt, sum );
		    sum += VAL( crd.rank );
		    table[ tcnt++ ] = crd;
		    printf( "Total is %d", sum );
		    if( i > 0 )  {
			printf( quiet ? ".  You got %d" :
						".  You got %d points", i );
			if(  chkscr( &pscore, i )  )  return( TRUE );
		    }
		    printf( ".\n" );
		    myturn = !myturn;
		}
	    }
	    if(  sum >= 31  )  {
		sum = 0;
		mego = ugo = FALSE;
		tcnt = 0;
		last = FALSE;				/* disable last flag */
	    }
	    if(  !pnum  &&  !cnum  )  break;		/* both done */
	}  while( TRUE );
	if( last )  {
	    if( played )  {
		printf( quiet ? "I get one for last.\n" :
					"I get one point for last.\n" );
		if(  chkscr( &cscore, 1 )  )  return( TRUE );
	    }
	    else  {
		printf( quiet ? "You get one for last.\n" :
					"You get one point for last.\n" );
		if(  chkscr( &pscore, 1 )  )  return( TRUE );
	    }
	}
	return( FALSE );
}



/*
 * handle the scoring of the hands
 */

score( mycrib )

    BOOLEAN		mycrib;
{
	if(  mycrib  )  {
	    if(  plyrhand( phand, "hand" )  )  return( TRUE );
	    if(  comphand( chand, "hand" )  )  return( TRUE );
	    if(  comphand( crib, "crib" )  )  return( TRUE );
	}
	else  {
	    if(  comphand( chand, "hand" )  )  return( TRUE );
	    if(  plyrhand( phand, "hand" )  )  return( TRUE );
	    if(  plyrhand( crib, "crib" )  )  return( TRUE );
	}
	return( FALSE );
}

