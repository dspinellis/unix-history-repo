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
static char sccsid[] = "@(#)bt_conv.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <db.h>
#include <stdio.h>

#include "btree.h"

static void kdswap __P((PAGE *));

/*
 * __BT_BPGIN, __BT_BPGOUT --
 *	Convert host-specific number layout to/from the host-independent
 *	format stored on disk.
 *
 * Parameters:
 *	t:	tree
 *	pg:	page number
 *	h:	page to convert
 */
void
__bt_pgin(t, pg, p)
	void *t;
	pgno_t pg;
	void *p;
{
	PAGE *h;

	if (((BTREE *)t)->bt_lorder == BYTE_ORDER)
		return;

	h = p;
	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BSSWAP(h->lower);
	BSSWAP(h->upper);
	kdswap(h);
}

void
__bt_pgout(t, pg, p)
	void *t;
	pgno_t pg;
	void *p;
{
	PAGE *h;

	if (((BTREE *)t)->bt_lorder == BYTE_ORDER)
		return;

	h = p;
	kdswap(h);
	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BSSWAP(h->lower);
	BSSWAP(h->upper);
}

/*
 * KDSWAP -- Actually swap the bytes on the page.
 *
 * Parameters:
 *	h:	page to convert
 *
 * Warnings:
 *	Everywhere else in the code, the pgno_t and index_t types are
 *	opaque.  These routines know what they really are.
 */
static void
kdswap(h)
	PAGE *h;
{
	register int i, top;
	register char *p;			/* Really void, thanks ANSI! */
	u_char flags;

	top = NEXTINDEX(h);
	switch (h->flags & P_TYPE) {
	case P_BINTERNAL:
		for (i = 0; i < top; i++) {
			BSSWAP(h->linp[i]);
			p = (char *)GETBINTERNAL(h, i);
			BLPSWAP(p);
			p += sizeof(size_t);
			BLPSWAP(p);
			p += sizeof(pgno_t);
			if (*(u_char *)p & P_BIGKEY) {
				p += sizeof(u_char);
				BLPSWAP(p);
				p += sizeof(pgno_t);
				BLPSWAP(p);
			}
		}
		break;
	case P_BLEAF:
		for (i = 0; i < top; i++) {
			BSSWAP(h->linp[i]);
			p = (char *)GETBLEAF(h, i);
			BLPSWAP(p);
			p += sizeof(size_t);
			BLPSWAP(p);
			p += sizeof(size_t);
			flags = *(u_char *)p;
			if (flags & (P_BIGKEY | P_BIGDATA)) {
				p += sizeof(u_char);
				if (flags & P_BIGKEY) {
					BLPSWAP(p);
					p += sizeof(pgno_t);
					BLPSWAP(p);
				}
				if (flags & P_BIGDATA) {
					p += sizeof(size_t);
					BLPSWAP(p);
					p += sizeof(pgno_t);
					BLPSWAP(p);
				}
			}
		}
		break;
	}
}
