/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * getproject() saves the contents of the PROJECT environment variable.
 * If the PROJECT variable is undefined or a null string, null is returned.
 */
#include "null.h"

char *_PROJECT = NULL;			/* project root directory pathname */

void
getproject()
{
	register char *pv;		/* ptr to start of PROJECT variable */
	char *getenv();			/* get environment variable */
	char *strsav();			/* save a string somewhere */

	if ((pv = getenv("PROJECT")) != NULL && *pv != '\0')
		_PROJECT = strsav(pv);
}
