/* $Header: getcwp.c,v 1.1 85/03/14 16:58:54 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * getcwp() returns the pathname of the current working project. If the
 * PROJECT environment variable is undefined or a null string, null is
 * returned.
 */
#include "null.h"

char *
getcwp()
{
	extern char *_PROJECT;		/* project root directory pathname */
	void getproject();		/* get PROJECT environment variable */

	if (_PROJECT == NULL)
		getproject();
	return(_PROJECT);
}
