/*
 * program to to decrypt caesar(tm) cypher
 * (caesar is a trademark of the roman empire)
 *
 * to compile:
 *
 *	cc decrypt.c -lm -o decrypt.c
 *
 * usage:
 *
 *	decrypt [n] < file
 *
 * where n is an optional forced rotation.
 *
 * authors: Stan King, John Eldridge, based on algorithm suggested by
 *		Bob Morris
 * 29-Sep-82
 *
 */

#ifdef SCCSID
static char	*SccsId = "@(#)caesar.c	1.7	4/16/85";
#endif /* SCCSID */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
extern char *calloc();

main(argc, argv)
int argc;
char *argv[];
{
	/* letter frequencies (taken from some unix(tm) documentation) */
	/* (unix is a trademark of Bell Laboratories) */
	static double stdf[ 26 ] =
	{
		7.97, 1.35, 3.61, 4.78, 12.37, 2.01, 1.46, 4.49,
		6.39, 0.04, 0.42, 3.81, 2.69, 5.92, 6.96, 2.91,
		0.08, 6.63, 8.77, 9.68, 2.62, 0.81, 1.88, 0.23,
		2.07, 0.06,
	};
	int obs[26];
	int bufsize;
	int c, i, try;
	double dot, winnerdot;  /* .. */
	int winner, forced = 0;
	char *inbuf;

	bufsize = 0;
	if( argc > 1 )
		sscanf( argv[1], "%d", &forced );
	if( forced == 0 )
		forced = -1000;
		
	inbuf = calloc( BUFSIZ, 1 );

	/* adjust frequency table to weight low probs REAL low */
	for (i=0; i<26; i++)	{
		stdf[i] = log(stdf[i]) + log(26.0/100.0);
	}

	/* Decode each line separately */
	for (;;) {
		for (i=0; i<=25; obs[i++]=0)
			;

		/* get a sample of the text */
		for( i = 0; i < BUFSIZ; i++ ) {
			if( (c = getchar()) == EOF ) {
				exit(0);
			}	
			inbuf[i] = c;
			if (c == '\n') {
				bufsize = i+1; 
				break;
			}
			if (islower(c))
				obs[c-'a'] += 1;
			else if (isupper(c))
				obs[c-'A'] += 1;
		}

		/* now "dot" the freqs with the observed letter freqs */
		/*	and keep track of best fit */
		winner = 0;	
		for (try = 0; try<26; try+=13) {
			dot = 0;
			for ( i=0; i<26; i++ ) {
				dot += obs[i] * stdf[ (i+try) % 26 ];
				}
			/* initialize winning score */
			if( try == 0 )
				winnerdot = dot;
			if( dot > winnerdot ) {
				/* got a new winner! */
				winner = try;
				winnerdot = dot;
			}
		}

		if (forced != -1000)
			winner = forced;

		/* print out sample buffer */
		for( i = 0; i < bufsize; i++ )
			putchar( rotate( inbuf[i], winner ) );
	}
 }


static int
rotate( c, perm )
char c;
int perm;
{
	if (isupper(c))	{
		return 'A' + (c - 'A' + perm) % 26 ;
	}
	else if (islower(c)) {
		return 'a' + (c-'a'+perm) % 26 ;
	}
	else return c;
}
