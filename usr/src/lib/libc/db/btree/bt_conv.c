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
static char sccsid[] = "@(#)bt_conv.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <db.h>
#include <stdio.h>
#include "btree.h"

/*
 * __BT_BPGIN, __BT_BPGOUT --
 *	Convert host-specific number layout to/from the host-independent
 *	format stored on disk.
 *
 * Parameters:
 *	tree:	tree
 *	h:	page to convert
 *
 * Side Effects:
 *	Layout of tree metadata on the page is changed in place.
 *
 * Warnings:
 *	Everywhere else in the code, the types pgno_t and index_t are
 *	opaque.  These two routines know what they really are.
 */
void
__bt_pgin(t, pg, p)
	void *t;
	pgno_t pg;
	void *p;
{
	register BINTERNAL *bi;
	register BLEAF *bl;
	register int i, top;
	PAGE *h;

	if (((BTREE *)t)->bt_lorder == BYTE_ORDER)
		return;

	h = p;
	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BLSWAP(h->lower);
	BLSWAP(h->upper);

	top = NEXTINDEX(h);
	if (!(h->flags & (P_BLEAF | P_RLEAF)))
		for (i = 0; i < top; i++) {
			BLSWAP(h->linp[i]);
			bi = GETBINTERNAL(h, i);
			BLSWAP(bi->ksize);
			BLSWAP(bi->pgno);
			BLSWAP(bi->flags);
			if (bi->flags & P_BIGKEY)
				BLSWAP(*(long *)bi->bytes);
		}
	else if (!(h->flags & P_OVERFLOW))
		for (i = 0; i < top; i++) {
			BLSWAP(h->linp[i]);
			bl = GETBLEAF(h, i);
			BLSWAP(bl->dsize);
			BLSWAP(bl->ksize);
			BLSWAP(bl->flags);
			if (bl->flags & P_BIGKEY)
				BLSWAP(*(long *)bl->bytes);
			if (bl->flags & P_BIGDATA)
				BLSWAP(*(long *)(bl->bytes + bl->ksize));
		}
}

void
__bt_pgout(t, pg, p)
	void *t;
	pgno_t pg;
	void *p;
{
	register BINTERNAL *bi;
	register BLEAF *bl;
	register int i, top;
	PAGE *h;

	if (((BTREE *)t)->bt_lorder == BYTE_ORDER)
		return;

	h = p;
	top = NEXTINDEX(h);
	if (!(h->flags & (P_BLEAF | P_RLEAF)))
		for (i = 0; i < top; i++) {
			bi = GETBINTERNAL(h, i);
			BLSWAP(bi->ksize);
			BLSWAP(bi->pgno);
			if (bi->flags & P_BIGKEY)
				BLSWAP(*(long *)bi->bytes);
			BLSWAP(h->linp[i]);
		}
	else if (!(h->flags & P_OVERFLOW))
		for (i = 0; i < top; i++) {
			bl = GETBLEAF(h, i);
			BLSWAP(bl->ksize);
			BLSWAP(bl->dsize);
			if (bl->flags & P_BIGKEY)
				BLSWAP(*(long *)bl->bytes);
			if (bl->flags & P_BIGDATA)
				BLSWAP(*(long *)(bl->bytes + bl->ksize));
			BLSWAP(h->linp[i]);
		}
	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BLSWAP(h->lower);
	BLSWAP(h->upper);
}
