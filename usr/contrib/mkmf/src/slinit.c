/* $Header: slinit.c,v 1.2 85/03/18 13:18:52 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slinit() returns a pointer to the head block of a new list, or null
 * pointer if out of memory.
 */
#include <stdio.h>
#include "null.h"
#include "slist.h"

extern char *PGN;			/* program name */

SLIST *
slinit()
{
	char *malloc();			/* memory allocator */
	SLIST *slist;			/* pointer to list head block */

	if ((slist = (SLIST *) malloc(sizeof(SLIST))) == NULL)
		{
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "out of memory\n");
		return(NULL);
		}
	slist->nk = 0;
	slist->maxkey = 0;
	slist->head = slist->curblk = slist->tail = NULL;
	return(slist);
}
