/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)updutils.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"

/*
 *  _BT_FIXSCAN -- Adjust a scan to cope with a change in tree structure.
 *
 *	If the user has an active scan on the database, and we delete an
 *	item from the page the cursor is pointing at, we need to figure
 *	out what to do about it.  Basically, the solution is to point
 *	"between" keys in the tree, using the CRSR_BEFORE flag.  The
 *	requirement is that the user should not miss any data in the
 *	tree during a scan, just because he happened to do some deletions
 *	or insertions while it was active.
 *
 *	In order to guarantee that the scan progresses properly, we need
 *	to save the key of any deleted item we were pointing at, so that
 *	we can check later insertions against it.
 *
 *	Parameters:
 *		t -- tree to adjust
 *		index -- index of item at which change was made
 *		newd -- new datum (for insertions only)
 *		op -- operation (DELETE or INSERT) causing change
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR (errno is set).
 *
 *	Side Effects:
 *		None.
 */

int
_bt_fixscan(t, index, newd, op)
	BTREE_P t;
	index_t index;
	DATUM *newd;
	int op;
{
	CURSOR *c;
	DATUM *d;

	c = &(t->bt_cursor);

	if (op == DELETE) {
		if (index < c->c_index)
			c->c_index--;
		else if (index == c->c_index) {
			if (!(c->c_flags & CRSR_BEFORE)) {
				if (_bt_crsrkey(t) == RET_ERROR)
					return (RET_ERROR);
				c->c_flags |= CRSR_BEFORE;
			}
		}
	} else {
		/*
		 *  If we previously deleted the object at this location,
		 *  and now we're inserting a new one, we need to do the
		 *  right thing -- the cursor should come either before
		 *  or after the new item, depending on the key that was
		 *  here, and the new one.
		 */

		if (c->c_flags & CRSR_BEFORE) {
			if (index <= c->c_index) {
				char *tmp;
				int itmp;
				pgno_t chain;
				int r;

				d = (DATUM *) GETDATUM(t->bt_curpage, index);
				if (d->d_flags & D_BIGKEY) {
					bcopy(&(newd->d_bytes[0]),
					      (char *) &chain,
					      sizeof(chain));
					if (_bt_getbig(t, chain, &tmp, &itmp)
					     == RET_ERROR)
						return (RET_ERROR);
				} else
					tmp = &(newd->d_bytes[0]);

				r = (*(t->bt_compare))(tmp, c->c_key);
				if (r < 0)
					c->c_index++;

				if (d->d_flags & D_BIGKEY)
					(void) free (tmp);
			}
		} else if (index <= c->c_index)
			c->c_index++;
	}
	return (RET_SUCCESS);
}

/*
 *  _BT_CRSRKEY -- Save a copy of the key of the record that the cursor
 *		   is pointing to.  The record is about to be deleted.
 *
 *	Parameters:
 *		t -- btree
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Allocates memory for the copy which should be released when
 *		it is no longer needed.
 */

int
_bt_crsrkey(t)
	BTREE_P t;
{
	CURSOR *c;
	DATUM *d;
	pgno_t pgno;
	int ignore;

	c = &(t->bt_cursor);
	d = (DATUM *) GETDATUM(t->bt_curpage, c->c_index);

	if (d->d_flags & D_BIGKEY) {
		bcopy(&(d->d_bytes[0]), (char *) &pgno, sizeof(pgno));
		return (_bt_getbig(t, pgno, &(c->c_key), &ignore));
	} else {
		if ((c->c_key = (char *) malloc(d->d_ksize)) == (char *) NULL)
			return (RET_ERROR);

		bcopy(&(d->d_bytes[0]), c->c_key, (size_t) (d->d_ksize));
	}
	return (RET_SUCCESS);
}
