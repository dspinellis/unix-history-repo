
#include	<stdio.h>
#include	"deck.h"

#define		LINESIZE		128

char		linebuf[ LINESIZE ];

char		*rankname[ RANKS ]	= { "ACE", "TWO", "THREE", "FOUR",
				            "FIVE", "SIX", "SEVEN", "EIGHT",
                                            "NINE", "TEN", "JACK", "QUEEN",
                                            "KING" };

char            *rankchar[ RANKS ]      = { "A", "2", "3", "4", "5", "6", "7",
                                            "8", "9", "T", "J", "Q", "K" };

char            *suitname[ SUITS ]      = { "SPADES", "HEARTS", "DIAMONDS",
                                            "CLUBS" };

char            *suitchar[ SUITS ]      = { "S", "H", "D", "C" };



/*
 * call prcard in one of two forms
 */

printcard( c, brief )

    CARD                c;
    BOOLEAN             brief;
{
        if( brief )  return(  prcard( c, TRUE, (char *) NULL, TRUE )  );
        else         return(  prcard( c, FALSE, " of ", FALSE )  );
}



/*
 * print the value of a card in ascii
 */

prcard( c, brfrank, mid, brfsuit )

    CARD                c;
    char                *mid;
    BOOLEAN             brfrank,  brfsuit;
{
        if(  c.rank == -1  ||  c.suit == -1  )  return( FALSE );
        if( brfrank )  {
            printf( "%1.1s",  rankchar[ c.rank ] );
        }
        else  {
            printf( "%s",  rankname[ c.rank ] );
        }
        if( mid != NULL )  printf( "%s", mid );
        if( brfsuit )  {
            printf( "%1.1s",  suitchar[ c.suit ] );
        }
        else  {
            printf( "%s",  suitname[ c.suit ] );
        }
        return( TRUE );
}



/*
 * prhand prints a hand of n cards
 */

prhand( h, n, brief )

    CARD                h[];
    int                 n;
    BOOLEAN             brief;
{
        register  int           i;

        --n;
        for( i = 0; i < n; i++ )  {
            if(  printcard( h[i], brief )  )  {
                if( brief )  printf( ", " );
                else     printf( "\n" );
            }
        }
        printcard( h[i], brief );
}



/*
 * infrom reads a card, supposedly in hand, accepting unambigous brief input
 * returns the index of the card found...
 */

infrom( hand, n )

    CARD                hand[];
    int                 n;
{
        register  int           i, j;
        CARD                    crd;

        if(  n < 1  )  {
            printf( "\nINFROM: %d = n < 1!!\n", n );
            exit( 74 );
        }
        do  {
            if(  incard( &crd )  )  {           /* if card is full card */
                if(  !isone( crd, hand, n )  )  {
                    printf( "That's not in your hand.  Play one of ( " );
                    prhand( hand, n, TRUE );
                    printf( " ): " );
                }
                else  {
                    for( i = 0; i < n; i++ )  {
                        if(  hand[i].rank == crd.rank  &&
                             hand[i].suit == crd.suit      )  break;
                    }
                    if(  i >= n  )  {
                        printf( "\nINFROM: isone or something messed up\n" );
                        exit( 77 );
                    }
                    return( i );
                }
            }
            else  {                             /* if not full card... */
                if(  crd.rank  !=  -1  )  {
                    for( i = 0; i < n; i++ )  {
                        if(  hand[i].rank == crd.rank  )  break;
                    }
                    if(  i >= n  )  {
                        printf( "No such rank in your hand.  Play one of ( " );
                        prhand( hand, n, TRUE );
                        printf( " ): " );
                    }
                    else  {
                        for( j = i + 1; j < n; j++ )  {
                            if(  hand[j].rank == crd.rank  )  break;
                        }
                        if(  j < n  )  {
                            printf( "Ambiguous rank.  Play one of ( " );
                            prhand( hand, n, TRUE );
                            printf( " ): " );
                        }
                        else  {
                            return( i );
                        }
                    }
                }
                else  {
                    printf( "Sorry, I missed that.  Play one of ( " );
                    prhand( hand, n, TRUE );
                    printf( " ): " );
                }
            }
        }  while( TRUE );
	return( 0 );   /*  Useless:  keeps lint quiet  */
}



/*
 * incard inputs a card in any format
 * it reads a line ending with a CR and then parses it
 */

incard( crd )

    CARD                *crd;
{
        char                    *getline();
        register  int           i;
        int                     rnk, sut;
        char                    *line, *p, *p1;
        BOOLEAN                 retval;

        retval = FALSE;
        rnk = sut = -1;
        if(  !( line = getline() )  )  goto  gotit;
        p = p1 = line;
        while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
        *p1++ = NULL;
        if(  *p == NULL  )  goto  gotit;
                        /* IMPORTANT: no real card has 2 char first name */
        if(  strlen(p) == 2  )  {               /* check for short form */
            rnk = -1;
            for( i = 0; i < RANKS; i++ )  {
                if(  *p == *rankchar[i]  )  {
                    rnk = i;
                    break;
                }
            }
            if(  rnk == -1  )  goto  gotit;     /* it's nothing... */
            ++p;                                /* advance to next char */
            sut = -1;
            for( i = 0; i < SUITS; i++ )  {
                if(  *p == *suitchar[i]  )  {
                    sut = i;
                    break;
                }
            }
            if(  sut != -1  )  retval = TRUE;
            goto  gotit;
        }
        rnk = -1;
        for( i = 0; i < RANKS; i++ )  {
            if(  !strcmp( p, rankname[i] )  ||  !strcmp( p, rankchar[i] )  )  {
                rnk = i;
                break;
            }
        }
        if(  rnk == -1  )  goto  gotit;
        p = p1;
        while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
        *p1++ = NULL;
        if(  *p == NULL  )  goto  gotit;
        if(  !strcmp( "OF", p )  )  {
            p = p1;
            while(  *p1 != ' '  &&  *p1 != NULL  )  ++p1;
            *p1++ = NULL;
            if(  *p == NULL  )  goto  gotit;
        }
        sut = -1;
        for( i = 0; i < SUITS; i++ )  {
            if(  !strcmp( p, suitname[i] )  ||  !strcmp( p, suitchar[i] )  )  {
                sut = i;
                break;
            }
        }
        if(  sut != -1  )  retval = TRUE;
gotit:
        (*crd).rank = rnk;
        (*crd).suit = sut;
        return( retval );
}



/*
 * getuchar reads and converts to upper case
 */

getuchar()
{
        register  int           c;

        c = getchar();
        if(  c < 'a'  ||  c > 'z'  )  return( c );
        else                          return(  c + ('A' - 'a')  );
}



/*
 * number reads in a decimal number and makes sure it is between
 * lo and hi inclusive
 * a cr counts as lo
 */

number( lo, hi )

    int         lo, hi;
{
        char                    *getline();
        register  char          *p;
        register  int            sum;

        sum = 0;
        do  {
            if(  !( p = getline() )  )  return( lo );   /* no line = lo */
            if(  *p == NULL  )  return( lo );
            sum = 0;
            while(  *p == ' '  ||  *p == '\t'  )  ++p;
            if(  *p < '0'  ||  *p > '9'  )  {
                sum = lo - 1;
            }
            else  {
                do  {
                    sum = 10*sum + (*p - '0');
                    ++p;
                }  while(  '0' <= *p  &&  *p <= '9'  );
            }
            if(  *p != ' '  &&  *p != '\t'  &&  *p != NULL  )  sum = lo - 1;
            if(  sum >= lo  &&  sum <= hi  )  break;
            if(  sum == lo - 1  )  {
                printf( "that doesn't look like a number, try again --> " );
            }
            else  {
                printf( "%d is not between %d and %d inclusive, try again --> ",
                                                                sum, lo, hi );
            }
        }  while( TRUE );
        return( sum );
}



/*
 * getline reads the next line up to '\n' or EOF
 * multiple spaces are compressed to one space
 * a space is inserted before a ','
 */

char  *getline()
{
        register  char                  c, *p;

        do  {
            c = getuchar();
            if(  c < 0  )  return( NULL );
        }  while(  c == ' '  ||  c == '\t'  );
        if(  c == '\n'  )  {
            linebuf[0] = NULL;
            return( linebuf );
        }
        p = linebuf;
        *p = c;                                         /* first non-blank */
        while(  ( c = getuchar() )  >= 0  )  {          /* read one line */
            if(  c == '\n'  )  break;
            if(  c == ','  )  if(  *p != ' '  )  *++p = ' ';
            if(  *p == ' '  )  {                        /* compress spaces */
                if(  c == ' '  ||  c == '\t'  )  continue;
                else  *++p = c;
            }
            else  {
                if(  c == ' '  ||  c == '\t'  )  *++p = ' ';
                else  *++p = c;
            }
            if(  p - linebuf  >=  (LINESIZE - 2)  )  {
                do  {
                    c = getuchar();
                    if(  c < 0  )  return( NULL );
                }  while(  c != '\n'  );
                break;
            }
        }
        if(  c < 0  )  return( NULL );
        *++p = NULL;
        *++p = NULL;
        return( linebuf );
}




