#include <X/mit-copyright.h>

/* $Header: XCreateAssoc.c,v 10.7 86/12/16 17:12:11 tony Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

char *malloc();
char *calloc();
/*
 * XCreateAssocTable - Create an XAssocTable.  The size argument should be
 * a power of two for efficiency reasons.  Some size suggestions: use 32
 * buckets per 100 objects;  a reasonable maximum number of object per
 * buckets is 8.  If there is an error creating the XAssocTable, a NULL
 * pointer is returned.
 */
XAssocTable *XCreateAssocTable(size)
	register int size;		/* Desired size of the table. */
{
	register XAssocTable *table;	/* XAssocTable to be initialized. */
	register XAssoc *buckets;	/* Pointer to the first bucket in */
					/* the bucket array. */
	
	/* Malloc the XAssocTable. */
	if ((table = (XAssocTable *)malloc(sizeof(XAssocTable))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}
	
	/* Malloc the buckets (actually just their headers). */
	buckets = (XAssoc *)calloc(size, sizeof(XAssoc));
	if (buckets == NULL) {
		/* Calloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* Insert table data into the XAssocTable structure. */
	table->buckets = buckets;
	table->size = size;

	while (--size >= 0) {
		/* Initialize each bucket. */
		buckets->prev = buckets;
		buckets->next = buckets;
		buckets++;
	}

	return(table);
}
