#ifndef lint
static char sccsid[] = "@(#)code.c	4.2	(Berkeley)	7/21/83";
#endif not lint

/*
 * PURPOSE:	sorted list compressor (works with a modified 'find'
 *		to encode/decode a filename database)
 *
 * USAGE:	bigram < list > bigrams
 *		process bigrams (see updatedb) > common_bigrams
 *		code common_bigrams < list > squozen_list
 *
 * METHOD:	Uses 'front compression' (see ";login:", March 1983, p. 8 ).
 *		Output format is, per line, an offset differential count byte
 *		followed by a partially bigram-encoded ascii residue. 
 *
 *  	The codes are:
 *
 *	0-28	likeliest differential counts + offset to make nonnegative 
 *	30	escape code for out-of-range count to follow in next word
 *	128-255 bigram codes, (128 most common, as determined by 'updatedb')
 *	32-127  single character (printable) ascii residue
 *
 * SEE ALSO:	updatedb.csh, bigram.c, find.c
 * 
 * AUTHOR:	James A. Woods, Informatics General Corp.,
 *		NASA Ames Research Center, 10/82
 */

#include <stdio.h>

#define MAXPATH 1024		/* maximum pathname length */
#define	RESET	30		/* switch code */

char path[MAXPATH];
char oldpath[MAXPATH] = " ";	
char bigrams[257] = { 0 };

main ( argc, argv )
	int argc; char *argv[];
{
  	int count, oldcount, diffcount;
	int j, code;
	char bigram[3];
	FILE *fp;

	oldcount = 0;
	bigram[2] = NULL;

	if ((fp = fopen(argv[1], "r")) == NULL) {
		printf("Usage: code common_bigrams < list > coded_list\n");
		exit(1);
	}
	fgets ( bigrams, 257, fp );
	fwrite ( bigrams, 1, 256, stdout );

     	while ( gets ( path ) != NULL ) {
		/*
		   squelch unprintable chars so as not to botch decoding
		*/
		for ( j = 0; path[j] != NULL; j++ ) {	
			path[j] &= 0177;		
			if ( path[j] < 040 || path[j] == 0177 )
				path[j] = '?';
		}
		count = prefix_length ( oldpath, path );
		diffcount = count - oldcount;
		if ( (diffcount < -14) || (diffcount > 14) ) {
			putc ( RESET, stdout );
			putw ( diffcount + 14, stdout );
		}
		else
			putc ( diffcount + 14, stdout );	

		for ( j = count; path[j] != NULL; j += 2 ) {
			if ( path[j + 1] == NULL ) {
				putchar ( path[j] );
				break;
			}
			bigram[0] = path[j];
			bigram[1] = path[j + 1];
			/*
			    linear search for specific bigram in string table
			*/
			if ( (code = strindex ( bigrams, bigram )) % 2 == 0 )
				putchar ( (code / 2) | 0200 );	
			else 		
				fputs ( bigram, stdout );
		}
		strcpy ( oldpath, path );	
		oldcount = count;
	}
}

strindex ( string, pattern )	/* return location of pattern in string or -1 */
	char *string, *pattern;
{
	register char *s, *p, *q;

	for ( s = string; *s != NULL; s++ ) 
		if ( *s == *pattern ) {		/* fast first char check */
			for ( p = pattern + 1, q = s + 1; *p != NULL; p++, q++ )
				if ( *q != *p )
					break;
			if ( *p == NULL )	
				return ( q - strlen ( pattern ) - string );
		}
	return ( -1 );
}

prefix_length ( s1, s2 )	/* return length of longest common prefix */
	char *s1, *s2;		/* ... of strings s1 and s2 */
{
	register char *start;

    	for ( start = s1; *s1 == *s2; s1++, s2++ )	
		if ( *s1 == NULL )		
	    		break;
    	return ( s1 - start );
}
