/* $Header: pathhead.c,v 1.2 85/03/08 17:21:50 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathhead() removes tail of pathname and returns pathname. The tail is
 * defined as that part of the pathname after the last separator.
 */
#include "null.h"
#include "path.h"

char *
pathhead(pathname)
	register char *pathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* pathname pointer */

	ls = NULL;
	for (p = pathname; *p != '\0'; p++)
		if (*p == _PSC)
			ls = p;
	if (ls != NULL) *ls = '\0';
	return(pathname);
}
