/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathtail() removes head of pathname and returns a pointer to the first
 * character after the last separator.
 */
#include "path.h"

char *
pathtail(pathname)
	register char *pathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* pathname pointer */

	for (ls = p = pathname; *p != '\0'; p++)
		if (*p == _PSC)
			ls = p+1;
	return(ls);
}
