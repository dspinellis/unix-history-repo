/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)seq.c	5.4 (Berkeley) 3/3/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <errno.h>
#include <db.h>
#include <stdlib.h>
#include "btree.h"

/*
 *  _BT_SEQINIT -- Initialize a sequential scan on the btree.
 *
 *	Sets the tree's notion of the current scan location correctly
 *	given a key and a direction.
 *
 *	Parameters:
 *		t -- tree in which to initialize scan
 *		key -- key for initial scan position
 *		flags -- R_NEXT, R_PREV
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if there's no data
 *		in the tree to scan.
 *
 *	Side Effects:
 *		Changes current scan position for the tree.  Almost certainly
 *		changes current page, as well.  Sets BTF_SEQINIT bit in tree
 *		flags, so that we know we've initialized a scan.
 */

int
_bt_seqinit(t, key, flags)
	BTREE_P t;
	DBT *key;
	int flags;
{
	BTITEM *item;
	BTHEADER *h;
	CURSOR *c;
	IDATUM *id;
	index_t last;

	/*
	 *  Figure out if we really have to search for the key that the
	 *  user supplied.  If key is null, then this is an unkeyed scan
	 *  and we can just start from an endpoint.
	 */

	c = &(t->bt_cursor);

	if (flags == R_CURSOR) {
		if (key->data != (u_char *) NULL) {

			/* key supplied, find first instance of it */
			item = _bt_first(t, key);
			c->c_index = item->bti_index;
			c->c_pgno = t->bt_curpage->h_pgno;
		} else {
			errno = EINVAL;
			return (RET_ERROR);
		}

	} else {

		/*
		 *  Unkeyed scan.  For backward scans, find the last item
		 *  in the tree; for forward scans, find the first item.
		 */

		if (_bt_getpage(t, (pgno_t) P_ROOT) == RET_ERROR)
			return (RET_ERROR);
		h = t->bt_curpage;
		if (flags == R_LAST || flags == R_PREV) {

			/* backward scan */
			while (!(h->h_flags & F_LEAF)) {
				last = NEXTINDEX(h) - 1;
				id = (IDATUM *) GETDATUM(h,last);
				if (_bt_getpage(t, id->i_pgno) == RET_ERROR)
					return (RET_ERROR);
				h = t->bt_curpage;
			}

			/* skip empty pages */
			while (NEXTINDEX(h) == 0 && h->h_prevpg != P_NONE) {
				if (_bt_getpage(t, h->h_prevpg) == RET_ERROR)
					return (RET_ERROR);
				h = t->bt_curpage;
			}

			c->c_pgno = h->h_pgno;
			if (NEXTINDEX(h) > 0)
				c->c_index = NEXTINDEX(h) - 1;
			else
				c->c_index = 0;
		} else if (flags == R_FIRST || flags == R_NEXT) {
			/* forward scan */
			while (!(h->h_flags & F_LEAF)) {
				id = (IDATUM *) GETDATUM(h,0);
				if (_bt_getpage(t, id->i_pgno) == RET_ERROR)
					return (RET_ERROR);
				h = t->bt_curpage;
			}

			/* skip empty pages */
			while (NEXTINDEX(h) == 0 && h->h_nextpg != P_NONE) {
				if (_bt_getpage(t, h->h_nextpg) == RET_ERROR)
					return (RET_ERROR);
				h = t->bt_curpage;
			}

			c->c_pgno = h->h_pgno;
			c->c_index = 0;
		} else {
			/* no flags passed in */
			errno = EINVAL;
			return (RET_ERROR);
		}
	}

	/* okay, scan is initialized */
	t->bt_flags |= BTF_SEQINIT;

	/* don't need the descent stack anymore */
	while (_bt_pop(t) != P_NONE)
		continue;

	if (c->c_index == NEXTINDEX(t->bt_curpage))
		return (RET_SPECIAL);

	return (RET_SUCCESS);
}

/*
 *  _BT_SEQADVANCE -- Advance the sequential scan on this tree.
 *
 *	Moves the current location pointer for the scan on this tree one
 *	spot in the requested direction.
 *
 *	Parameters:
 *		t -- btree being scanned
 *		flags -- for R_NEXT, R_PREV
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR, or RET_SPECIAL if there is no
 *		more data in the specified direction.
 *
 *	Side Effects:
 *		May change current page.
 */

int
_bt_seqadvance(t, flags)
	BTREE_P t;
	int flags;
{
	BTHEADER *h;
	CURSOR *c;
	index_t index;

	c = &(t->bt_cursor);
	index = c->c_index;

	if (_bt_getpage(t, c->c_pgno) == RET_ERROR)
		return (RET_ERROR);
	h = t->bt_curpage;

	/* by the time we get here, don't need the cursor key anymore */
	if (c->c_key != (char *) NULL)
		(void) free(c->c_key);

	if (flags == R_NEXT) {

		/*
		 *  This is a forward scan.  If the cursor is pointing
		 *  at a virtual record (that is, it was pointing at
		 *  a record that got deleted), then we should return
		 *  the record it's pointing at now.  Otherwise, we
		 *  should advance the scan.  In either case, we need
		 *  to be careful not to run off the end of the current
		 *  page.
		 */

		if (c->c_flags & CRSR_BEFORE) {

			if (index >= NEXTINDEX(h)) {
				/* out of items on this page, get next page */
				if (h->h_nextpg == P_NONE) {
					/* tell caller we're done... */
					c->c_index = NEXTINDEX(h);
					return (RET_SPECIAL);
				}

				/* skip empty pages */
				do {
					if (_bt_getpage(t, h->h_nextpg)
					    == RET_ERROR) {
						c->c_index = NEXTINDEX(h);
						return (RET_ERROR);
					}
					h = t->bt_curpage;
					c->c_pgno = h->h_pgno;
				} while (NEXTINDEX(h) == 0
					 && h->h_nextpg != P_NONE);

				if (NEXTINDEX(h) == 0) {
					/* tell caller we're done */
					c->c_index = NEXTINDEX(h);
					return (RET_SPECIAL);
				}
				index = 0;
			}
			c->c_flags &= ~CRSR_BEFORE;

		} else if (++index >= NEXTINDEX(h)) {

			/* out of items on this page, get next page */
			if (h->h_nextpg == P_NONE) {
				/* tell caller we're done... */
				c->c_index = NEXTINDEX(h);
				return (RET_SPECIAL);
			}

			/* skip empty pages */
			do {
				if (_bt_getpage(t, h->h_nextpg) == RET_ERROR) {
					c->c_index = NEXTINDEX(h);
					return (RET_ERROR);
				}
				h = t->bt_curpage;
				c->c_pgno = h->h_pgno;
			} while (NEXTINDEX(h) == 0 && h->h_nextpg != P_NONE);

			if (NEXTINDEX(h) == 0) {
				/* tell caller we're done */
				c->c_index = NEXTINDEX(h);
				return (RET_SPECIAL);
			}
			index = 0;
		}
	} else if (flags == R_PREV) {

		/* for backward scans, life is substantially easier */
		c->c_flags &= ~CRSR_BEFORE;
		if (c->c_key != (char *) NULL) {
			(void) free(c->c_key);
			c->c_key = (char *) NULL;
		}

		if (index == 0) {

			/* we may be done */
			c->c_index = 0;

			/* out of items on this page, get next page */
			if (h->h_prevpg == P_NONE)
				return (RET_SPECIAL);

			/* skip empty pages */
			do {
				if (_bt_getpage(t, h->h_prevpg) == RET_ERROR)
					return (RET_ERROR);
				h = t->bt_curpage;
				c->c_pgno = h->h_pgno;
			} while (NEXTINDEX(h) == 0 && h->h_prevpg != P_NONE);

			if (NEXTINDEX(h) == 0)
				return (RET_SPECIAL);

			index = NEXTINDEX(h) - 1;
		} else
			--index;
	} else {
		/* must specify a direction */
		errno = EINVAL;
		return (RET_ERROR);
	}

	c->c_index = index;
	return (RET_SUCCESS);
}
