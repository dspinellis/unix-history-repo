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
static char sccsid[] = "@(#)split.c	5.2 (Berkeley) 2/22/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"

/*
 *  _BT_SPLIT -- Split a page into two pages.
 *
 *	Splits are caused by insertions, and propogate up the tree in
 *	the usual way.  The root page is always page 1 in the file on
 *	disk, so root splits are handled specially.  On entry to this
 *	routine, t->bt_curpage is the page to be split.
 *
 *	Parameters:
 *		t -- btree in which to do split.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Changes the notion of the current page.
 */

int
_bt_split(t)
	BTREE_P t;
{
	BTHEADER *h;
	BTHEADER *left, *right;
	pgno_t nextpgno, parent;
	int nbytes, len;
	IDATUM *id;
	DATUM *d;
	char *src;
	IDATUM *new;
	pgno_t oldchain;
	u_char flags;

	h = (BTHEADER *) t->bt_curpage;

	/* split root page specially, since it must remain page 1 */
	if (h->h_pgno == P_ROOT) {
		return (_bt_splitroot(t));
	}

	/*
	 *  This is a little complicated.  We go to some trouble to
	 *  figure out which of the three possible cases -- in-memory tree,
	 *  disk tree (no cache), and disk tree (cache) -- we have, in order
	 *  to avoid unnecessary copying.  If we have a disk cache, then we
	 *  have to do some extra copying, though, since the cache code
	 *  manages buffers externally to this code.
	 */

	if (ISDISK(t) && ISCACHE(t)) {
		if ((left = (BTHEADER *) malloc((unsigned) t->bt_psize))
		    == (BTHEADER *) NULL)
			return (RET_ERROR);
		left->h_pgno = left->h_prevpg = left->h_nextpg = P_NONE;
		left->h_flags = t->bt_curpage->h_flags;
		left->h_lower = (index_t)
			  (((char *) &(left->h_linp[0])) - ((char *) left));
		left->h_upper = t->bt_psize;

	} else {
		if ((left = _bt_allocpg(t)) == (BTHEADER *) NULL)
			return (RET_ERROR);
	}
	left->h_pgno = h->h_pgno;

	if ((right = _bt_allocpg(t)) == (BTHEADER *) NULL)
		return (RET_ERROR);
	right->h_pgno = ++(t->bt_npages);

	/* now do the split */
	if (_bt_dopsplit(t, left, right) == RET_ERROR)
		return (RET_ERROR);

	right->h_prevpg = left->h_pgno;
	nextpgno = right->h_nextpg = h->h_nextpg;
	left->h_nextpg = right->h_pgno;
	left->h_prevpg = h->h_prevpg;

	/* okay, now use the left half of the page as the new page */
	if (ISDISK(t) && ISCACHE(t)) {
		(void) bcopy((char *) left, (char *) t->bt_curpage,
			     (int) t->bt_psize);
		(void) free ((char *) left);
		left = t->bt_curpage;
	} else {
		(void) free((char *) t->bt_curpage);
		t->bt_curpage = left;
	}

	/*
	 *  Write the new pages out.  We need them to stay where they are
	 *  until we're done updating the parent pages.
	 */

	if (_bt_write(t, left, NORELEASE) == RET_ERROR)
		return (RET_ERROR);
	if (_bt_write(t, right, NORELEASE) == RET_ERROR)
		return (RET_ERROR);

	/* update 'prev' pointer of old neighbor of left */
	if (nextpgno != P_NONE) {
		if (_bt_getpage(t, nextpgno) == RET_ERROR)
			return (RET_ERROR);
		h = t->bt_curpage;
		h->h_prevpg = right->h_pgno;
		h->h_flags |= F_DIRTY;
	}

	if ((parent = _bt_pop(t)) != P_NONE) {
		if (right->h_flags & F_LEAF) {
			d = (DATUM *) GETDATUM(right, 0);
			len = d->d_ksize;
			if (d->d_flags & D_BIGKEY) {
				bcopy(&(d->d_bytes[0]),
				      (char *) &oldchain,
				      sizeof(oldchain));
				if (_bt_markchain(t, oldchain) == RET_ERROR)
					return (RET_ERROR);
				src = (char *) &oldchain;
				flags = D_BIGKEY;
			} else {
				src = (char *) &(d->d_bytes[0]);
				flags = 0;
			}
		} else {
			id = (IDATUM *) GETDATUM(right, 0);
			len = id->i_size;
			flags = id->i_flags;
			src = (char *) &(id->i_bytes[0]);
		}
		nbytes = len + (sizeof(IDATUM) - sizeof(char));
		new = (IDATUM *) malloc((unsigned) nbytes);
		if (new == (IDATUM *) NULL)
			return (RET_ERROR);
		new->i_size = len;
		new->i_pgno = right->h_pgno;
		new->i_flags = flags;
		if (len > 0)
			(void) bcopy(src, (char *) &(new->i_bytes[0]), len);

		nbytes = LONGALIGN(nbytes) + sizeof(index_t);
		if (_bt_getpage(t, parent) == RET_ERROR)
			return (RET_ERROR);

		h = t->bt_curpage;

		/*
		 *  Split the parent if we need to, then reposition the
		 *  tree's current page pointer for the new datum.
		 */
		if ((h->h_upper - h->h_lower) < nbytes) {
			if (_bt_split(t) == RET_ERROR)
				return (RET_ERROR);
			if (_bt_reposition(t, new, parent, right->h_prevpg)
			      == RET_ERROR)
				return (RET_ERROR);
		}

		/* okay, now insert the new idatum */
		if (_bt_inserti(t, new, right->h_prevpg) == RET_ERROR)
			return (RET_ERROR);
	}

	/*
	 *  Okay, split is done; don't need right page stapled down anymore.
	 *  The page we call 'left' above is the new version of the old
	 *  (split) page, so we can't release it.
	 */

	if (_bt_release(t, right) == RET_ERROR)
		return (RET_ERROR);
	if (ISDISK(t) && !ISCACHE(t))
		(void) free((char *) right);

	return (RET_SUCCESS);
}

/*
 *  _BT_REPOSITION -- Reposition the current page pointer of a btree.
 *
 *	After splitting a node in the tree in order to make room for
 *	an insertion, we need to figure out which page after the split
 *	should get the item we want to insert.  This routine positions
 *	the tree's current page pointer appropriately.
 *
 *	Parameters:
 *		t -- tree to position
 *		new -- the item we want to insert
 *		parent -- parent of the node that we just split
 *		prev -- page number of item directly to the left of
 *			new's position in the tree.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		None.
 */

int
_bt_reposition(t, new, parent, prev)
	BTREE_P t;
	IDATUM *new;
	pgno_t parent;
	pgno_t prev;
{
	index_t i, next;
	IDATUM *idx;

	if (parent == P_ROOT) {

		/*
		 *  If we just split the root page, then there are guaranteed
		 *  to be exactly two IDATUMs on it.  Look at both of them
		 *  to see if they point to the page that we want.
		 */

		if (_bt_getpage(t, parent) == RET_ERROR)
			return (RET_ERROR);

		next = NEXTINDEX(t->bt_curpage);
		for (i = 0; i < next; i++) {
			idx = (IDATUM *) GETDATUM(t->bt_curpage, i);
			if (_bt_getpage(t, idx->i_pgno) == RET_ERROR)
				return (RET_ERROR);
			if (_bt_isonpage(t, new, prev) == RET_SUCCESS)
				return (RET_SUCCESS);
			if (_bt_getpage(t, parent) == RET_ERROR)
				return (RET_ERROR);
		}
	} else {

		/*
		 *  Get the parent page -- which is where the new item would
		 *  have gone -- and figure out whether the new item now goes
		 *  on the parent, or the page immediately to the right of
		 *  the parent.
		 */

		if (_bt_getpage(t, parent) == RET_ERROR)
			return (RET_ERROR);
		if (_bt_isonpage(t, new, prev) == RET_SUCCESS)
			return (RET_SUCCESS);
		if (_bt_getpage(t, t->bt_curpage->h_nextpg) == RET_ERROR)
			return (RET_ERROR);
		if (_bt_isonpage(t, new, prev) == RET_SUCCESS)
			return (RET_SUCCESS);
	}
	return (RET_ERROR);
}

/*
 *  _BT_ISONPAGE -- Is the IDATUM for a given page number on the current page?
 *
 *	This routine is used by _bt_reposition to decide whether the current
 *	page is the correct one on which to insert a new item.
 *
 *	Parameters:
 *		t -- tree to check
 *		new -- the item that will be inserted (used for binary search)
 *		prev -- page number of page whose IDATUM is immediately to
 *			the left of new's position in the tree.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR (corresponding to TRUE, FALSE).
 */

int
_bt_isonpage(t, new, prev)
	BTREE_P t;
	IDATUM *new;
	pgno_t prev;
{
	BTHEADER *h = (BTHEADER *) t->bt_curpage;
	index_t i, next;
	IDATUM *idx;

	i = _bt_binsrch(t, &(new->i_bytes[0]));
	while (i != 0 && _bt_cmp(t, &(new->i_bytes[0]), i) == 0)
		--i;
	next = NEXTINDEX(h);
	idx = (IDATUM *) GETDATUM(h, i);
	while (i < next && idx->i_pgno != prev) {
		i++;
		idx = (IDATUM *) GETDATUM(h, i);
	}
	if (idx->i_pgno == prev)
		return (RET_SUCCESS);
	else
		return (RET_ERROR);
}

/*
 *  _BT_SPLITROOT -- Split the root of a btree.
 *
 *	The root page for a btree is always page one.  This means that in
 *	order to split the root, we need to do extra work.
 *
 *	Parameters:
 *		t -- tree to split
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Splits root upward in the usual way, adding two new pages
 *		to the tree (rather than just one, as in usual splits).
 */

int
_bt_splitroot(t)
	BTREE_P t;
{
	BTHEADER *h = t->bt_curpage;
	BTHEADER *left, *right;
	IDATUM *id;
	BTHEADER *where_h;
	char *src, *dest;
	int len, nbytes;
	u_long was_leaf;
	pgno_t oldchain;
	u_char flags;

	/* get two new pages for the split */
	if ((left = _bt_allocpg(t)) == (BTHEADER *) NULL)
		return (RET_ERROR);
	left->h_pgno = ++(t->bt_npages);
	if ((right = _bt_allocpg(t)) == (BTHEADER *) NULL)
		return (RET_ERROR);
	right->h_pgno = ++(t->bt_npages);

	/* do the split */
	if (_bt_dopsplit(t, left, right) == RET_ERROR)
		return (RET_ERROR);

	/* connect the new pages correctly */
	right->h_prevpg = left->h_pgno;
	left->h_nextpg = right->h_pgno;

	/*
	 *  Write the child pages out now.  We need them to remain
	 *  where they are until we finish updating parent pages,
	 *  however.
	 */

	if (_bt_write(t, left, NORELEASE) == RET_ERROR)
		return (RET_ERROR);
	if (_bt_write(t, right, NORELEASE) == RET_ERROR)
		return (RET_ERROR);

	/* now change the root page into an internal page */
	was_leaf = (h->h_flags & F_LEAF);
	h->h_flags &= ~F_LEAF;
	h->h_lower = (index_t) (((char *) (&(h->h_linp[0]))) - ((char *) h));
	h->h_upper = (index_t) t->bt_psize;
	(void) bzero((char *) &(h->h_linp[0]), (int) (h->h_upper - h->h_lower));

	/* put two new keys on root page */
	where_h = left;
	while (where_h) {
		DATUM *data;
		IDATUM *idata;

		if (was_leaf) {
			data = (DATUM *) GETDATUM(where_h, 0);

			if (where_h == left) {
				len = 0;	/* first key in tree is null */
			} else {
				if (data->d_flags & D_BIGKEY) {
					bcopy(&(data->d_bytes[0]),
					      (char *) &oldchain,
					      sizeof(oldchain));
					if (_bt_markchain(t, oldchain) == RET_ERROR)
						return (RET_ERROR);
					src = (char *) &oldchain;
					flags = D_BIGKEY;
				} else {
					src = (char *) &(data->d_bytes[0]);
					flags = 0;
				}
				len = data->d_ksize;
			}
		} else {
			idata = (IDATUM *) GETDATUM(where_h, 0);
			len = idata->i_size;
			flags = idata->i_flags;
			src = &(idata->i_bytes[0]);
		}
		dest = ((char *) h) + h->h_upper;
		nbytes = len + (sizeof (IDATUM) - sizeof(char));
		dest -= LONGALIGN(nbytes);
		id = (IDATUM *) dest;
		id->i_size = len;
		id->i_pgno = where_h->h_pgno;
		id->i_flags = flags;
		if (len > 0)
			(void) bcopy((char *) src, (char *) &(id->i_bytes[0]), len);
		dest -= ((int) h);
		h->h_linp[NEXTINDEX(h)] = (index_t) dest;
		h->h_upper = (index_t) dest;
		h->h_lower += sizeof(index_t);

		/* next page */
		if (where_h == left)
			where_h = right;
		else
			where_h = (BTHEADER *) NULL;
	}

	if (_bt_release(t, left) == RET_ERROR)
		return (RET_ERROR);
	if (_bt_release(t, right) == RET_ERROR)
		return (RET_ERROR);

	/*
	 *  That's it, split is done.  If we're doing a non-cached disk
	 *  btree, we can free up the pages we allocated, as they're on
	 *  disk, now.
	 */

	if (ISDISK(t) && !ISCACHE(t)) {
		(void) free ((char *) left);
		(void) free ((char *) right);
	}

	h->h_flags |= F_DIRTY;

	return (RET_SUCCESS);
}

/*
 *  _BT_DOPSPLIT -- Do the work of splitting a page
 *
 *	This routine takes two page pointers and splits the data on the
 *	current page of the btree between them.
 *
 *	We do a lot of work here to handle duplicate keys on a page; we
 *	have to place these keys carefully to guarantee that later searches
 *	will find them correctly.  See comments in the code below for details.
 *
 *	Parameters:
 *		t -- tree to split
 *		left -- pointer to page to get left half of the data
 *		right -- pointer to page to get right half of the data
 *
 *	Returns:
 *		None.
 */

int
_bt_dopsplit(t, left, right)
	BTREE_P t;
	BTHEADER *left;
	BTHEADER *right;
{
	BTHEADER *h = t->bt_curpage;
	size_t psize;
	char *where;
	BTHEADER *where_h;
	index_t where_i;
	int nbytes, dsize, fixedsize, freespc;
	index_t i;
	index_t save_lower, save_upper, save_i;
	index_t switch_i;
	char *save_key;
	DATUM *d;
	CURSOR *c;
	index_t top;
	int free_save;
	pgno_t chain;
	int ignore;

	/*
	 *  Our strategy is to put half the bytes on each page.  We figure
	 *  out how many bytes we have total, and then move items until
	 *  the last item moved put at least 50% of the data on the left
	 *  page.
	 */
	save_key = (char *) NULL;
	psize = (int) t->bt_psize;
	where = ((char *) left) + psize;
	where_h = left;
	where_i = 0;
	nbytes = psize - (int) ((char *) &(h->h_linp[0]) - ((char *) h));
	freespc = nbytes;

	top = NEXTINDEX(h);
	if (h->h_flags & F_LEAF)
		fixedsize = (sizeof(DATUM) - sizeof(char));
	else
		fixedsize = (sizeof(IDATUM) - sizeof(char));

	save_key = (char *) NULL;

	/* move data */
	for (i = 0; i < top; i++) {

		/*
		 *  Internal and leaf pages have different layouts for
		 *  data items, but in both cases the first entry in the
		 *  data item is a size_t.
		 */
		d = (DATUM *) GETDATUM(h,i);
		if (h->h_flags & F_LEAF) {
			dsize = d->d_ksize + d->d_dsize + fixedsize;
		} else {
			dsize = d->d_ksize + fixedsize;
		}

		/*
		 *  If a page contains duplicate keys, we have to be
		 *  careful about splits.  A sequence of duplicate keys
		 *  may not begin in the middle of one page and end in
		 *  the middle of another; it must begin on a page boundary,
		 *  in order for searches on the internal nodes to work
		 *  correctly.
		 */
		if (where_h == left) {
			if (save_key == (char *) NULL) {
				if (h->h_flags & F_LEAF) {
					if (d->d_flags & D_BIGKEY) {
						free_save = TRUE;
						bcopy(&(d->d_bytes[0]),
						     (char *) &chain,
						     sizeof(chain));
						if (_bt_getbig(t, chain,
							       &save_key,
							       &ignore)
						    == RET_ERROR)
							return (RET_ERROR);
					} else {
						free_save = FALSE;
						save_key = (char *) &(d->d_bytes[0]);
					}
				} else {
					IDATUM *id = (IDATUM *) d;

					if (id->i_flags & D_BIGKEY) {
						free_save = TRUE;
						bcopy(&(id->i_bytes[0]),
						     (char *) &chain,
						     sizeof(chain));
						if (_bt_getbig(t, chain,
							       &save_key,
							       &ignore)
						    == RET_ERROR)
							return (RET_ERROR);
					} else {
						free_save = FALSE;
						save_key = (char *)
							&(id->i_bytes[0]);
					}
				}
				save_i = 0;
				save_lower = where_h->h_lower;
				save_upper = where_h->h_upper;
			} else {
				if (_bt_cmp(t, save_key, i) != 0) {
					save_lower = where_h->h_lower;
					save_upper = where_h->h_upper;
					save_i = i;
				}
				if (h->h_flags & F_LEAF) {
					if (free_save)
						(void) free(save_key);
					if (d->d_flags & D_BIGKEY) {
						free_save = TRUE;
						bcopy(&(d->d_bytes[0]),
						     (char *) &chain,
						     sizeof(chain));
						if (_bt_getbig(t, chain,
							       &save_key,
							       &ignore)
						    == RET_ERROR)
							return (RET_ERROR);
					} else {
						free_save = FALSE;
						save_key = (char *) &(d->d_bytes[0]);
					}
				} else {
					IDATUM *id = (IDATUM *) d;

					if (id->i_flags & D_BIGKEY) {
						free_save = TRUE;
						bcopy(&(id->i_bytes[0]),
						     (char *) &chain,
						     sizeof(chain));
						if (_bt_getbig(t, chain,
							       &save_key,
							       &ignore)
						    == RET_ERROR)
							return (RET_ERROR);
					} else {
						free_save = FALSE;
						save_key = (char *)
							&(id->i_bytes[0]);
					}
				}
			}
		}

		/* copy data and update page state */
		where -= LONGALIGN(dsize);
		(void) bcopy((char *) d, (char *) where, dsize);
		where_h->h_upper = where_h->h_linp[where_i] =
			(index_t) (where - (int) where_h);
		where_h->h_lower += sizeof(index_t);
		where_i++;

		/* if we've moved half, switch to the right-hand page */
		nbytes -= LONGALIGN(dsize) + sizeof(index_t);
		if ((freespc - nbytes) > nbytes) {
			nbytes = 2 * freespc;

			/* identical keys go on the same page */
			if (save_i > 0) {
				/* i gets incremented at loop bottom... */
				i = save_i - 1;
				where_h->h_lower = save_lower;
				where_h->h_upper = save_upper;
			}
			where = ((char *) right) + psize;
			where_h = right;
			switch_i = where_i;
			where_i = 0;
		}
	}

	/*
	 *  If there was an active scan on the database, and we just
	 *  split the page that the cursor was on, we may need to
	 *  adjust the cursor to point to the same entry as before the
	 *  split.
	 */

	c = &(t->bt_cursor);
	if ((t->bt_flags & BTF_SEQINIT)
	    && (c->c_pgno == h->h_pgno)
	    && (c->c_index >= switch_i)) {
		c->c_pgno = where_h->h_pgno;
		c->c_index -= where_i;
	}
	return (RET_SUCCESS);
}
