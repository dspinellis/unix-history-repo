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
static char sccsid[] = "@(#)utils.c	5.3 (Berkeley) 3/3/91";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"

/*
 *  _BT_BUILDRET -- Build return key/data pair as a result of search or scan.
 *
 *	This routine manages the statically allocated buffers in which we
 *	return data to the user.
 *
 *	Parameters:
 *		t -- btree from which to return datum
 *		d -- DATUM to be returned to the user.
 *		data -- data argument supplied by user for return
 *		key -- key argument supplied by user for return
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 *
 *	Side Effects:
 *		May free and reallocate static buffers, if the data
 *		we want to return is bigger than the space we have to
 *		do so.
 */

int
_bt_buildret(t, d, data, key)
	BTREE_P t;
	DATUM *d;
	DBT *data;
	DBT *key;
{
	static int _data_s = 0;
	static int _key_s = 0;
	static char *_data = (char *) NULL;
	static char *_key = (char *) NULL;
	pgno_t chain;

	if (d->d_flags & D_BIGKEY) {
		if (_key != (char *) NULL)
			(void) free(_key);
		(void) bcopy((char *) &(d->d_bytes[0]),
		      	     (char *) &chain,
		      	     sizeof(chain));
		if (_bt_getbig(t, chain, &_key, &_key_s) == RET_ERROR)
			return (RET_ERROR);
		key->data = (u_char *)_key;
		key->size = _key_s;
	} else {
		/* need more space for key? */
		if (d->d_ksize > _key_s) {
			if (_key != (char *) NULL)
				(void) free (_key);
			if ((_key = (char *) malloc((unsigned) d->d_ksize))
			    == (char *) NULL)
				return (RET_ERROR);
			_key_s = d->d_ksize;
		}

		key->data = (u_char *)_key;
		if ((key->size = d->d_ksize) > 0)
			(void) bcopy(&(d->d_bytes[0]),
				     _key,
				     (int) d->d_ksize);
	}

	if (d->d_flags & D_BIGDATA) {
		if (_data != (char *) NULL)
			(void) free(_data);
		(void) bcopy(&(d->d_bytes[d->d_ksize]),
		      	     (char *) &chain,
		      	     sizeof(chain));
		if (_bt_getbig(t, chain, &_data, &_data_s) == RET_ERROR)
			return (RET_ERROR);
		data->data = (u_char *)_data;
		data->size = _data_s;
	} else {
		/* need more space for data? */
		if (d->d_dsize > _data_s) {
			if (_data != (char *) NULL)
				(void) free (_data);
			if ((_data = (char *) malloc((unsigned) d->d_dsize))
			    == (char *) NULL)
				return (RET_ERROR);
			_data_s = d->d_dsize;
		}

		data->data = (u_char *)_data;

		if ((data->size = d->d_dsize) > 0)
			(void) bcopy(&(d->d_bytes[d->d_ksize]),
				      _data,
				      (size_t) (d->d_dsize));
	}

	return (RET_SUCCESS);
}

/*
 *  _BT_CMP -- Compare a key to a given item on the current page.
 *
 *	This routine is a wrapper for the user's comparison function.
 *
 *	Parameters:
 *		t -- tree in which to do comparison
 *		p -- pointer to one argument for the comparison function
 *		n -- index of item to supply second arg to comparison function
 *
 *	Returns:
 *		< 0 if p is < item at n
 *		= 0 if p is = item at n
 *		> 0 if p is > item at n
 */

int
_bt_cmp(t, p, n)
	BTREE_P t;
	char *p;
	index_t n;
{
	BTHEADER *h;
	IDATUM *id;
	DATUM *d;
	char *arg;
	int ignore;
	int free_arg;
	pgno_t chain;
	int r;

	h = t->bt_curpage;

	/*
	 *  The left-most key at any level of the tree on internal pages
	 *  is guaranteed (artificially, by the code here) to be less than
	 *  any user key.  This saves us from having to update the leftmost
	 *  key when the user inserts a new key in the tree smaller than
	 *  anything we've seen yet.
	 */

	if (h->h_prevpg == P_NONE && !(h->h_flags & F_LEAF) && n == 0)
		return (1);

	if (h->h_flags & F_LEAF) {
		d = (DATUM *) GETDATUM(h,n);
		if (d->d_flags & D_BIGKEY) {
			free_arg = TRUE;
			bcopy(&(d->d_bytes[0]), (char *) &chain, sizeof(chain));
			if (_bt_getbig(t, chain, &arg, &ignore) == RET_ERROR)
				return (RET_ERROR);
		} else {
			free_arg = FALSE;
			arg = &(d->d_bytes[0]);
		}
	} else {
		id = (IDATUM *) GETDATUM(h,n);
		if (id->i_flags & D_BIGKEY) {
			free_arg = TRUE;
			bcopy(&(id->i_bytes[0]),
			      (char *) &chain,
			      sizeof(chain));
			if (_bt_getbig(t, chain, &arg, &ignore) == RET_ERROR)
				return (RET_ERROR);
		} else {
			free_arg = FALSE;
			arg = &(id->i_bytes[0]);
		}
	}
	r = (*(t->bt_compare))(p, arg);

	if (free_arg)
		(void) free(arg);

	return (r);
}

/*
 *  _BT_PUSH/_BT_POP -- Push/pop a parent page number on the parent stack.
 *
 *	When we descend the tree, we keep track of parent pages in order
 *	to handle splits on insertions.
 *
 *	Parameters:
 *		t -- tree for which to push parent
 *		pgno -- page number to push (_bt_push only)
 *
 *	Returns:
 *		RET_SUCCESS, RET_ERROR.
 */

int
_bt_push(t, pgno)
	BTREE_P t;
	pgno_t pgno;
{
	BTSTACK *new;

	if ((new = (BTSTACK *) malloc((unsigned) sizeof(BTSTACK)))
	    ==  (BTSTACK *) NULL)
		return (RET_ERROR);
	new->bts_pgno = pgno;
	new->bts_next = t->bt_stack;
	t->bt_stack = new;

	return (RET_SUCCESS);
}

pgno_t
_bt_pop(t)
	BTREE_P t;
{
	BTSTACK *s;
	pgno_t p = P_NONE;

	if ((s = t->bt_stack) != (BTSTACK *) NULL) {
		p = s->bts_pgno;
		t->bt_stack = s->bts_next;
		(void) free ((char *) s);
	}
	return (p);
}

#ifdef DEBUG
void
_btdump(tree)
	BTREE tree;
{
	BTREE_P t = (BTREE_P) tree;
	DATUM *d;
	IDATUM *id;
	BTHEADER *h;
	pgno_t npages;
	pgno_t i;
	index_t cur, top;

	npages = t->bt_npages;
	(void) printf("\"%s\" fd %d pgsz %d curpg %d @ 0x%lx",
		t->bt_fname, t->bt_s.bt_d.d_fd,
		t->bt_psize, t->bt_curpage);
	(void) printf("npg %d cmp 0x%lx flags=(", npages, t->bt_compare);
	if (t->bt_flags & BTF_SEQINIT)
		(void) printf("BTF_SEQINIT");
	(void) printf(")\n");

	for (i = P_ROOT; i <= npages; i++) {
		if (_bt_getpage(t, i) == RET_ERROR)
			_punt();
		h = t->bt_curpage;
		top = NEXTINDEX(h);
		(void) printf("    page %d:\n", i);
		(void) printf("\tpgno %d prev %d next %d\n",
			h->h_pgno, h->h_prevpg, h->h_nextpg);
		(void) printf("\tlower %d upper %d nextind %d flags (",
			h->h_lower, h->h_upper, top);
		if (h->h_flags & F_LEAF)
			(void) printf("F_LEAF");
		else
			(void) printf("<internal>");
		if (h->h_flags & F_DIRTY)
			(void) printf("|F_DIRTY");
		if (h->h_flags & F_PRESERVE)
			(void) printf("|F_PRESERVE");
		if (h->h_flags & F_CONT) {
			(void) printf("|F_CONT)");
			if (h->h_prevpg == P_NONE) {
				size_t longsz;
				(void) bcopy((char *) &(h->h_linp[0]),
					      (char *) &longsz,
					      sizeof(longsz));
				printf("\n\t\t(chain start, data length %ld)",
					longsz);
			}
			printf("\n");
			continue;
		}
		(void) printf(")\n");
		for (cur = 0; cur < top; cur++) {
			(void) printf("\t  [%d] off %d ", cur, h->h_linp[cur]);
			if (h->h_flags & F_LEAF) {
				d = (DATUM *) GETDATUM(h,cur);
				(void) printf("ksize %d", d->d_ksize);
				if (d->d_flags & D_BIGKEY)
					(void) printf(" (indirect)");
				(void) printf("; dsize %d", d->d_dsize);
				if (d->d_flags & D_BIGDATA)
					(void) printf(" (indirect)");
			} else {
				id = (IDATUM *) GETDATUM(h,cur);
				(void) printf("size %d pgno %d",
					id->i_size, id->i_pgno);
				if (id->i_flags & D_BIGKEY)
					(void) printf(" (indirect)");
			}
			(void) printf("\n");
		}
		(void) printf("\n");
	}
}
#endif /* DEBUG */

#ifdef DEBUG
_punt()
{
	int pid;

	pid = getpid();
	(void) kill(pid, SIGILL);
}
#endif /* DEBUG */
