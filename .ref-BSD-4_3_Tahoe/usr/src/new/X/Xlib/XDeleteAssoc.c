#include <X/mit-copyright.h>

/* $Header: XDeleteAssoc.c,v 10.7 86/12/16 17:16:46 tony Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XDeleteAssoc - Delete an association in an XAssocTable keyed on
 * an XId.  An association may be removed only once.  Redundant
 * deletes are meaningless (but cause no problems).
 */
XDeleteAssoc(table, x_id)
	register XAssocTable *table;
	register XId x_id;
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
				/* Remove it from the queue and */
				/* free the entry. */
				remque(entry);
				free((char *)entry);
				return;
			}
			/* Oops, identical XId's on different displays! */
			continue;
		}
		if (entry->x_id > x_id) {
			/* We have gone past where it should be. */
			/* It is apparently not in the table. */
			return;
		}
	}
	/* It is apparently not in the table. */
	return;
}
