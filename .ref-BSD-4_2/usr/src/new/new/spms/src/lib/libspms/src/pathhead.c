/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathhead() removes tail of pathname and returns pathname. The tail is
 * defined as that part of the pathname after the last separator.
 */
#include "path.h"

char *
pathhead(pathname)
	register char *pathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* pathname pointer */

	for (p = pathname; *p != '\0'; p++)
		if (*p == _PSC)
			ls = p;
	*ls = '\0';
	return(pathname);
}
