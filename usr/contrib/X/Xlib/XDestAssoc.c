#include <X/mit-copyright.h>

/* $Header: XDestAssoc.c,v 10.4 86/02/01 15:32:05 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XDestroyAssocTable - Destroy (free the memory associated with)
 * an XAssocTable.  
 */
XDestroyAssocTable(table)
	register XAssocTable *table;
{
	register int i;
	register XAssoc *bucket;
	register XAssoc *entry;

	/* Free the buckets. */
	for (i = 0; i < table->size; i++) {
		bucket = &(*table->table)[i];
		for (
	        	entry = bucket->next;
			entry != bucket;
			entry = entry->next
		) {
			free(entry);
		}
	}

	/* Free the bucket headers. */
	free(table->table);

	/* Free the table. */
	free(table);
}
