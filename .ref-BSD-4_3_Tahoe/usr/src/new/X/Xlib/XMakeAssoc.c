#include <X/mit-copyright.h>

/* $Header: XMakeAssoc.c,v 10.5 86/12/16 17:17:20 tony Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XMakeAssoc - Insert data into an XAssocTable keyed on an XId.
 * Data is inserted into the table only once.  Redundant inserts are
 * meaningless (but cause no problems).  The queue in each association
 * bucket is sorted (lowest XId to highest XId).
 */
XMakeAssoc(table, x_id, data)
	register XAssocTable *table;
	register XId x_id;
	register caddr_t data;
{
	int hash;
	register XAssoc *bucket;
	register XAssoc *entry;
	register XAssoc *new_entry;
	
	/* Hash the XId to get the bucket number. */
	hash = x_id & (table->size - 1);
	/* Look up the bucket to get the entries in that bucket. */
	bucket = &table->buckets[hash];
	/* Get the first entry in the bucket. */
	entry = bucket->next;

	/* If (entry != bucket), the bucket is empty so make */
	/* the new entry the first entry in the bucket. */
	/* if (entry == bucket), the we have to search the */
	/* bucket. */
	if (entry != bucket) {
		/* The bucket isn't empty, begin searching. */
		/* If we leave the for loop then we have either passed */
		/* where the entry should be or hit the end of the bucket. */
		/* In either case we should then insert the new entry */
		/* before the current value of "entry". */
		for (entry; entry != bucket; entry = entry->next) {
			if (entry->x_id == x_id) {
				/* Entry has the same XId... */
				if (entry->display == _XlibCurrentDisplay) {
					/* Entry has the same Display... */
					/* Therefore there is already an */
					/* entry with this XId and Display, */
					/* reset its data value and return. */
					entry->data = data;
					return;
				}
				/* We found an association with the right */
				/* id but the wrong display! */
				continue;
			}
			/* If the current entry's XId is greater than the */
			/* XId of the entry to be inserted then we have */
			/* passed the location where the new XId should */
			/* be inserted. */
			if (entry->x_id > x_id) break;
		}
        }

	/* If we are here then the new entry should be inserted just */
	/* before the current value of "entry". */
	/* Create a new XAssoc and load it with new provided data. */
	new_entry = (XAssoc *)malloc(sizeof(XAssoc));
	if (new_entry == NULL) {
		/* Malloc failed! */
		errno = ENOMEM;
		_XIOError(_XlibCurrentDisplay);
	}
	new_entry->display = _XlibCurrentDisplay;
	new_entry->x_id = x_id;
	new_entry->data = data;

	/* Insert the new entry. */
	insque(new_entry, entry->prev);
}
