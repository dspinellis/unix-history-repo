/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bt_conv.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <stdio.h>

#include <db.h>
#include "btree.h"

static void mswap __P((PAGE *));

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
__bt_pgin(t, pg, pp)
	void *t;
	pgno_t pg;
	void *pp;
{
	PAGE *h;
	int i, top;
	u_char flags;
	char *p;

	if (!ISSET(((BTREE *)t), B_NEEDSWAP))
		return;
	if (pg == P_META) {
		mswap(pp);
		return;
	}

	h = pp;
	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BSSWAP(h->lower);
	BSSWAP(h->upper);

	top = NEXTINDEX(h);
	if ((h->flags & P_TYPE) == P_BINTERNAL)
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
	else if ((h->flags & P_TYPE) == P_BLEAF)
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
}

void
__bt_pgout(t, pg, pp)
	void *t;
	pgno_t pg;
	void *pp;
{
	PAGE *h;
	int i, top;
	u_char flags;
	char *p;

	if (!ISSET(((BTREE *)t), B_NEEDSWAP))
		return;
	if (pg == P_META) {
		mswap(pp);
		return;
	}

	h = pp;
	top = NEXTINDEX(h);
	if ((h->flags & P_TYPE) == P_BINTERNAL)
		for (i = 0; i < top; i++) {
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
			BSSWAP(h->linp[i]);
		}
	else if ((h->flags & P_TYPE) == P_BLEAF)
		for (i = 0; i < top; i++) {
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
			BSSWAP(h->linp[i]);
		}

	BLSWAP(h->pgno);
	BLSWAP(h->prevpg);
	BLSWAP(h->nextpg);
	BLSWAP(h->flags);
	BSSWAP(h->lower);
	BSSWAP(h->upper);
}

/*
 * MSWAP -- Actually swap the bytes on the meta page.
 *
 * Parameters:
 *	p:	page to convert
 */
static void
mswap(pg)
	PAGE *pg;
{
	char *p;

	p = (char *)pg;
	BLPSWAP(p);		/* m_magic */
	p += sizeof(u_long);
	BLPSWAP(p);		/* m_version */
	p += sizeof(u_long);
	BLPSWAP(p);		/* m_psize */
	p += sizeof(u_long);
	BLPSWAP(p);		/* m_free */
	p += sizeof(u_long);
	BLPSWAP(p);		/* m_nrecs */
	p += sizeof(u_long);
	BLPSWAP(p);		/* m_flags */
	p += sizeof(u_long);
}
