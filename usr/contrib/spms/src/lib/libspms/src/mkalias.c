/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * mkalias() constructs a project directory alias from a pathname.
 */
#include "path.h"

char *
mkalias(pathname)
	char *pathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* pathname pointer */

	for (ls = p = pathname; *p != '\0'; p++)
		if ((*p == _PSC || *p == _PPSC) && p[1] != '\0')
			ls = p+1;
	return(ls);
}
