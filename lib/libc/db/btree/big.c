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
static char sccsid[] = "@(#)big.c	5.2 (Berkeley) 2/22/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"

/*
 *  _BT_GETBIG -- Get big data from indirect pages.
 *
 *	This routine chases indirect blocks for the big object at the 
 *	specified page to a buffer, and return the address of the buffer.
 *
 *	Parameters:
 *		t -- btree with the indirect blocks
 *		pgno -- page number that starts the chain
 *		p -- (char **) to get the address of the buffer containing
 *		     the key or datum.
 *		sz -- pointer to an int to get the size of the instantiated
 *		      object.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		None.
 */

int
_bt_getbig(t, pgno, p, sz)
	BTREE_P t;
	pgno_t pgno;
	char **p;
	int *sz;
{
	pgno_t save;
	size_t nbytes;
	size_t nhere;
	BTHEADER *h;
	char *top, *from, *where;

	save = t->bt_curpage->h_pgno;
	if (_bt_getpage(t, pgno) == RET_ERROR)
		return (RET_ERROR);

	h = t->bt_curpage;

	bcopy((char *) &(h->h_linp[0]),
	      (char *) &nbytes,
	      (size_t) sizeof(nbytes));

	if ((*p = (char *) malloc(nbytes)) == (char *) NULL)
		return (RET_ERROR);

	*sz = nbytes;
	from = ((char *) (&h->h_linp[0])) + sizeof(nbytes);
	top = ((char *) h) + t->bt_psize;

	/* need more space for data? */

	where = *p;

	while (nbytes > 0) {
		nhere = (int) (top - from);
		if (nhere > nbytes) {
			(void) bcopy(from, where, (size_t) nbytes);
			nbytes = 0;
		} else {
			(void) bcopy(from, where, nhere);
			where += nhere;
			nbytes -= nhere;
			if (_bt_getpage(t, h->h_nextpg) == RET_ERROR)
				return (RET_ERROR);
			h = t->bt_curpage;
			top = ((char *) h) + t->bt_psize;
			from = (char *) &(h->h_linp[0]);
		}
	}

	if (_bt_getpage(t, save) == RET_ERROR)
		return (RET_ERROR);

	return (RET_SUCCESS);
}

/*
 *  _BT_DELINDIR -- Delete a chain of indirect blocks from the btree.
 *
 *	When a large item is deleted from the tree, this routine puts the
 *	space that it occupied onto the free list for later reuse.  The
 *	bt_free entry in the btree structure points at the head of this list.
 *	This value is also stored on disk in the btree's metadata.
 *
 *	Parameters:
 *		t -- btree from which to delete pages
 *		chain -- page number that starts the chain.
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Invalidates the current on-disk version of the btree's
 *		metadata (if any), and updates the free list appropriately.
 */

int
_bt_delindir(t, chain)
	BTREE_P t;
	pgno_t chain;
{
	BTHEADER *h;
	pgno_t save;
	pgno_t oldfree;

	h = t->bt_curpage;
	save = h->h_pgno;
	if (_bt_getpage(t, chain) == RET_ERROR)
		return (RET_ERROR);

	/*
	 *  If some internal node is pointing at this chain, don't
	 *  delete it.
	 */

	if (t->bt_curpage->h_flags & F_PRESERVE) {
		if (_bt_getpage(t, save) == RET_ERROR)
			return (RET_ERROR);
		return (RET_SUCCESS);
	}

	/* if there's nothing on the free list, this is easy... */
	if (t->bt_free == P_NONE) {
		t->bt_free = chain;
	} else {
		oldfree = t->bt_free;

		/* find the end of the data chain for the deleted datum */
		t->bt_free = chain;
		do {
			if (_bt_getpage(t, chain) == RET_ERROR)
				return (RET_ERROR);
			h = t->bt_curpage;
			if (h->h_nextpg != P_NONE)
				chain = h->h_nextpg;
		} while (h->h_nextpg != P_NONE);

		/* link freed pages into free list */
		h->h_nextpg = oldfree;
		if (_bt_write(t, h, RELEASE) == RET_ERROR)
			return (RET_ERROR);
		if (_bt_getpage(t, oldfree) == RET_ERROR)
			return (RET_ERROR);
		h = t->bt_curpage;
		h->h_prevpg = chain;
		if (_bt_write(t, h, RELEASE) == RET_ERROR)
			return (RET_ERROR);
	}

	/* restore the tree's current page pointer */
	if (_bt_getpage(t, save) == RET_ERROR)
		return (RET_ERROR);

	/* we have trashed the tree metadata; rewrite it later */
	t->bt_flags &= ~BTF_METAOK;

	return (RET_SUCCESS);
}

/*
 *  _BT_INDIRECT -- Write a series of indirect pages for big objects.
 *
 *	A chain of indirect pages looks like
 *
 *	   +-------------------+   +---------------------+
 *	   |hdr|size|	       |   |hdr|		 |
 *	   +---+----+	       |   +---+		 |
 *	   |   ... data ...    |   |   ... data ...	 |    ...
 *	   |		       |   |			 |
 *	   +-------------------+   +---------------------+
 *
 *	where hdr is a standard btree page header (with the indirect bit
 *	set), size on the first page is the real size of the datum, and
 *	data are bytes of the datum, split across as many pages as necessary.
 *	Indirect pages are chained together with the h_prevpg and h_nextpg
 *	entries of the page header struct.
 *
 *	A single DBT is written per chain, so space on the last page is
 *	wasted.
 *
 *	We return the page number of the start of the chain.
 *
 *	When a big object is deleted from a tree, the space that it occupied
 *	is placed on a free list for later reuse.  This routine checks that
 *	free list before allocating new pages to the big datum being inserted.
 *
 *	Parameters:
 *		t -- btree in which to store indirect blocks
 *		data -- DBT with the big datum in it
 *		pgno -- place to put the starting page number of the chain
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		Current page is changed on return.
 */

int
_bt_indirect(t, data, pgno)
	BTREE_P t;
	DBT *data;
	pgno_t *pgno;
{
	pgno_t prev;
	char *top;
	char *where;
	char *from;
	size_t dsize;
	pgno_t nextchn;
	int ischain;
	BTHEADER *cur;

	/* get set for first page in chain */
	prev = P_NONE;
	dsize = data->size;
	from = (char *) data->data;

	/* if there are blocks on the free list, use them first */
	if ((*pgno = t->bt_free) == P_NONE) {
		if ((cur = _bt_allocpg(t)) == (BTHEADER *) NULL)
			return (RET_ERROR);

		ischain = 0;
		*pgno = cur->h_pgno = ++(t->bt_npages);
	} else {
		if (_bt_getpage(t, *pgno) == RET_ERROR)
			return (RET_ERROR);
		ischain = 1;
		cur = t->bt_curpage;
	}

	cur->h_flags = F_CONT|F_LEAF;
	(void) bcopy((char *) &dsize, (char *) &cur->h_linp[0], sizeof(size_t));
	where = ((char *) (&cur->h_linp[0])) + sizeof(size_t);

	/* fill and write pages in the chain */
	for (;;) {
		int nhere;

		top = ((char *) cur) + t->bt_psize;
		cur->h_prevpg = prev;
		nextchn = cur->h_nextpg;
		nhere = (int) (top - where);

		if (nhere >= dsize) {
			(void) bcopy(from, where, (int) dsize);
			cur->h_nextpg = P_NONE;
			dsize = 0;
		} else {
			(void) bcopy(from, where, (int) nhere);
			dsize -= nhere;
			from += nhere;
			if (nextchn == P_NONE)
				cur->h_nextpg = t->bt_npages + 1;
			prev = cur->h_pgno;
		}

		/* current page is ready to go; write it out */
		if (_bt_write(t, cur, RELEASE) == RET_ERROR)
			return (RET_ERROR);

		/* free the current page, if appropriate */
		if (ISDISK(t) && !ISCACHE(t) && !ischain) {
			(void) free ((char *) cur);
		}

		/* done? */
		if (dsize == 0)
			break;

		/* allocate another page */
		if (nextchn == P_NONE) {
			if ((cur = _bt_allocpg(t)) == (BTHEADER *) NULL)
				return (RET_ERROR);
			ischain = 0;
			cur->h_pgno = ++(t->bt_npages);
		} else {
			if (_bt_getpage(t, nextchn) == RET_ERROR)
				return (RET_ERROR);
			ischain = 1;
			cur = t->bt_curpage;
		}
		cur->h_flags = F_LEAF | F_CONT;

		where = (char *) (&cur->h_linp[0]);
	}

	/* if we used pages from the free list, record changes to it */
	if (*pgno == t->bt_free) {
		t->bt_free = nextchn;
		t->bt_flags &= ~BTF_METAOK;
	}

	return (RET_SUCCESS);
}

/*
 *  _BT_MARKCHAIN -- Mark a chain of pages as used by an internal node.
 *
 *	Chains of indirect blocks pointed to by leaf nodes get reclaimed
 *	when the item that points to them gets deleted.  Chains pointed
 *	to by internal nodes never get deleted.  This routine marks a
 *	chain as pointed to by an internal node.
 *
 *	Parameters:
 *		t -- tree in which to mark
 *		chain -- number of first page in chain
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		None.
 */

int
_bt_markchain(t, chain)
	BTREE_P t;
	pgno_t chain;
{
	pgno_t save;

	save = t->bt_curpage->h_pgno;

	if (_bt_getpage(t, chain) == RET_ERROR)
		return (RET_ERROR);

	t->bt_curpage->h_flags |= (F_DIRTY|F_PRESERVE);

	if (_bt_getpage(t, save) == RET_ERROR)
		return (RET_ERROR);

	return (RET_SUCCESS);
}
