#include <X/mit-copyright.h>

/* $Header: XLookUpAssoc.c,v 10.5 86/12/16 17:17:10 tony Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* 
 * XLookUpAssoc - Retrieve the data stored in an XAssocTable by its XId.
 * If an appropriately matching XId can be found in the table the routine will
 * return apointer to the data associated with it. If the XId can not be found
 * in the table the routine will return a NULL pointer.  All XId's are relative
 * to the currently active Display.
 */
caddr_t XLookUpAssoc(table, x_id)
	register XAssocTable *table;	/* XAssocTable to search in. */
	register XId x_id;			/* XId to search for. */
{
	int hash;
	register XAssoc *bucket;
	register XAssoc *entry;

	/* Hash the XId to get the bucket number. */
	hash = x_id & (table->size - 1);
	/* Look up the bucket to get the entries in that bucket. */
	bucket = &table->buckets[hash];
	/* Get the first entry in the bucket. */
	entry = bucket->next;

	/* Scan through the entries in the bucket for the right XId. */
	for (entry; entry != bucket; entry = entry->next) {
		if (entry->x_id == x_id) {
			/* We have the right XId. */
			if (entry->display == _XlibCurrentDisplay) {
				/* We have the right display. */
				/* We have the right entry! */
				return(entry->data);
			}
			/* Oops, identical XId's on different displays! */
			continue;
		}
		if (entry->x_id > x_id) {
			/* We have gone past where it should be. */
			/* It is apparently not in the table. */
			return(NULL);
		}
	}
	/* It is apparently not in the table. */
	return(NULL);
}
