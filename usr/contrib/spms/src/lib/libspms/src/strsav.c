/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * strsav() saves a string somewhere and returns a pointer to the somewhere.
 * Returns NULL on error.
 */
#include "null.h"

char *
strsav(s)
	char *s;
{
	char *sptr;			/* somewhere string pointer */
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */

	if ((sptr = malloc((unsigned)(strlen(s)+1))) == NULL)
		return(NULL);
	return(strcpy(sptr, s));
}
