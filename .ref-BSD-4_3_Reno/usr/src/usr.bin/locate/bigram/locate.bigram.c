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
static char sccsid[] = "@(#)locate.bigram.c	4.7 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 *  bigram < text > bigrams
 * 
 * List bigrams for 'updatedb' script.
 * Use 'code' to encode a file using this output.
 */

#include <stdio.h>
#include <sys/param.h>			/* for MAXPATHLEN */

char buf1[MAXPATHLEN] = " ";	
char buf2[MAXPATHLEN];

main ( )
{
  	register char *cp;
	register char *oldpath = buf1, *path = buf2;

     	while ( fgets ( path, sizeof(buf2), stdin ) != NULL ) {

		/* skip longest common prefix */
		for ( cp = path; *cp == *oldpath; cp++, oldpath++ )
			if ( *oldpath == NULL )
				break;
		/*
		 * output post-residue bigrams only
		 */
		while ( *cp != NULL && *(cp + 1) != NULL ) {
			putchar ( *cp++ );
			putchar ( *cp++ );
			putchar ( '\n' );
		}
		if ( path == buf1 )		/* swap pointers */
			path = buf2, oldpath = buf1;
		else
			path = buf1, oldpath = buf2;
   	}
}
