#include <X/mit-copyright.h>

/* $Header: XCreateAssoc.c,v 10.5 86/02/01 15:31:04 tony Rel $ */
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
	register XAssoc *bucket;	/* Pointer to a bucket in the table. */
	
	/* Malloc the XAssocTable. */
	if ((table = (XAssocTable *)malloc(sizeof(XAssocTable))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}
	
	/* Malloc the bucket headers. */
	bucket = (XAssoc *)calloc(size, sizeof(XAssoc));
	if (bucket == NULL) {
		/* Calloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* Insert table data into the XAssocTable structure. */
	table->table = (XAssoc (*)[])bucket;
	table->size = size;

	while (--size >= 0) {
		/* Initialize each bucket. */
		bucket->prev = bucket;
		bucket->next = bucket;
		bucket++;
	}

	return(table);
}
