#ifndef lint
static char sccsid[] = "@(#)locate.bigram.c	4.4	(Berkeley)	%G%";
#endif not lint

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

     	while ( gets ( path ) != NULL ) {

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
