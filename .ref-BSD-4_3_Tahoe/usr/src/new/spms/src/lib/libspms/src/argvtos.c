/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * argvtos() copies a list of arguments contained in an array of character
 * strings to a single dynamically allocated string. Each argument is
 * separated by one blank space. Returns a pointer to the string or null
 * if out of memory.
 */
#include "null.h"

#define SBUFINCR	1024
#define SBUFMAX		10240

char *
argvtos(argc, argv)
	char **argv;
	int  argc;
{
	register char *s;		/* string pointer */
	register int  i;		/* string buffer pointer */
	char *malloc();			/* memory allocator */
	char *realloc();		/* increase size of storage */
	char *sbuf;			/* string buffer */
	int nbytes;			/* bytes of memory required */
	int nu;				/* no. of SBUFINCR units required */
	int sbufsize;			/* current size of sbuf */
	int strlen();			/* string length */

	sbufsize = SBUFINCR;
	if ((sbuf = malloc((unsigned)sbufsize)) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	
	for (i = 0; argc-- > 0; ++argv)
		{
		if ((nbytes = (i+strlen(*argv)+1-sbufsize)) > 0)
			{
			nu = (nbytes+SBUFINCR-1)/SBUFINCR;
			sbufsize += nu * SBUFINCR;
			if (sbufsize > SBUFMAX)
				{
				warn("command string too long");
				return(NULL);
				}
			if ((sbuf = realloc(sbuf, (unsigned)sbufsize)) == NULL)
				{
				warn("out of memory");
				return(NULL);
				}
			}
		for (s = *argv; *s != '\0'; i++, s++)
			sbuf[i] = *s;
		sbuf[i++] = ' ';
		}
	sbuf[--i] = '\0';
	return(sbuf);
}
