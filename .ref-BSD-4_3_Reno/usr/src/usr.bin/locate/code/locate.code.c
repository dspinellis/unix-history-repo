/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James A. Woods.
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
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)locate.code.c	4.8 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * PURPOSE:	sorted list compressor (works with a modified 'find'
 *		to encode/decode a filename database)
 *
 * USAGE:	bigram < list > bigrams
 *		process bigrams (see updatedb) > common_bigrams
 *		code common_bigrams < list > squozen_list
 *
 * METHOD:	Uses 'front compression' (see ";login:", Volume 8, Number 1
 *		February/March 1983, p. 8 ).  Output format is, per line, an
 *		offset differential count byte followed by a partially bigram-
 *		encoded ascii residue.  A bigram is a two-character sequence,
 *		the first 128 most common of which are encoded in one byte.
 *
 * EXAMPLE:	For simple front compression with no bigram encoding,
 *		if the input is...		then the output is...
 *
 *		/usr/src			 0 /usr/src
 *		/usr/src/cmd/aardvark.c		 8 /cmd/aardvark.c
 *		/usr/src/cmd/armadillo.c	14 armadillo.c
 *		/usr/tmp/zoo			 5 tmp/zoo
 *
 *  	The codes are:
 *
 *	0-28	likeliest differential counts + offset to make nonnegative 
 *	30	switch code for out-of-range count to follow in next word
 *	128-255 bigram codes (128 most common, as determined by 'updatedb')
 *	32-127  single character (printable) ascii residue (ie, literal)
 *
 * SEE ALSO:	updatedb.csh, bigram.c, find.c
 * 
 * AUTHOR:	James A. Woods, Informatics General Corp.,
 *		NASA Ames Research Center, 10/82
 */

#include <sys/param.h>
#include <stdio.h>
#include "locate.h"

#define BGBUFSIZE	(NBG * 2)	/* size of bigram buffer */

char buf1[MAXPATHLEN] = " ";	
char buf2[MAXPATHLEN];
char bigrams[BGBUFSIZE + 1] = { 0 };

main ( argc, argv )
	int argc; char *argv[];
{
	register char *cp, *oldpath = buf1, *path = buf2;
  	int code, count, diffcount, oldcount = 0;
	FILE *fp;

	if ((fp = fopen(argv[1], "r")) == NULL) {
		printf("Usage: code common_bigrams < list > squozen_list\n");
		exit(1);
	}
	/* first copy bigram array to stdout */
	fgets ( bigrams, BGBUFSIZE + 1, fp );
	fwrite ( bigrams, 1, BGBUFSIZE, stdout );
	fclose( fp );

     	while ( fgets ( path, sizeof(buf2), stdin ) != NULL ) {
		/* truncate newline */
		cp = path + strlen(path) - 1;
		if (cp > path && *cp == '\n')
			*cp = '\0';
		/* squelch characters that would botch the decoding */
		for ( cp = path; *cp != NULL; cp++ ) {
			if ( (unsigned char)*cp >= PARITY )
				*cp &= PARITY-1;
			else if ( *cp <= SWITCH )
				*cp = '?';
		}
		/* skip longest common prefix */
		for ( cp = path; *cp == *oldpath; cp++, oldpath++ )
			if ( *oldpath == NULL )
				break;
		count = cp - path;
		diffcount = count - oldcount + OFFSET;
		oldcount = count;
		if ( diffcount < 0 || diffcount > 2*OFFSET ) {
			putc ( SWITCH, stdout );
			putw ( diffcount, stdout );
		}
		else
			putc ( diffcount, stdout );	

		while ( *cp != NULL ) {
			if ( *(cp + 1) == NULL ) {
				putchar ( *cp );
				break;
			}
			if ( (code = bgindex ( cp )) < 0 ) {
				putchar ( *cp++ );
				putchar ( *cp++ );
			}
			else {	/* found, so mark byte with parity bit */
				putchar ( (code / 2) | PARITY );
				cp += 2;
			}
		}
		if ( path == buf1 )		/* swap pointers */
			path = buf2, oldpath = buf1;
		else
			path = buf1, oldpath = buf2;
	}
}

bgindex ( bg )			/* return location of bg in bigrams or -1 */
	char *bg;
{
	register char *p;
	register char bg0 = bg[0], bg1 = bg[1];

	for ( p = bigrams; *p != NULL; p++ )
		if ( *p++ == bg0 && *p == bg1 )
			break;
	return ( *p == NULL ? -1 : --p - bigrams );
}
