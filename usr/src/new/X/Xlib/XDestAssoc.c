#include <X/mit-copyright.h>

/* $Header: XDestAssoc.c,v 10.7 86/12/16 17:16:57 tony Exp $ */
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
	register XAssoc *entry, *entry_next;

	/* Free the buckets. */
	for (i = 0; i < table->size; i++) {
		bucket = &table->buckets[i];
		for (
	        	entry = bucket->next;
			entry != bucket;
			entry = entry_next
		) {
		        entry_next = entry->next;
			free((char *)entry);
		}
	}

	/* Free the bucket array. */
	free((char *)table->buckets);

	/* Free the table. */
	free((char *)table);
}
