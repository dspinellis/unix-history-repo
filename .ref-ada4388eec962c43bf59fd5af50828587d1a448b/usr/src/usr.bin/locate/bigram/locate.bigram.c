/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James A. Woods.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)locate.bigram.c	8.2 (Berkeley) %G%";
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
			if ( *oldpath == '\0' )
				break;
		/*
		 * output post-residue bigrams only
		 */
		while ( *cp != '\0' && *(cp + 1) != '\0' ) {
			putchar ( *cp++ );
			putchar ( *cp++ );
			putchar ( '\n' );
		}
		if ( path == buf1 )		/* swap pointers */
			path = buf2, oldpath = buf1;
		else
			path = buf1, oldpath = buf2;
   	}
	return (0);
}
