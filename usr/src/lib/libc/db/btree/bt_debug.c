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
static char sccsid[] = "@(#)bt_debug.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>

#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "btree.h"

#ifdef DEBUG
/*
 * BT_DUMP -- Dump the tree
 *
 * Parameters:
 *	dbp:	pointer to the DB
 */
void
__bt_dump(dbp)
	DB *dbp;
{
	BTREE *t;
	PAGE *h;
	pgno_t i;
	char *sep;

	t = dbp->internal;
	(void)fprintf(stderr, "%s: pgsz %d",
	    ISSET(t, BTF_INMEM) ? "memory" : "disk", t->bt_psize);
	if (ISSET(t, BTF_RECNO))
		(void)fprintf(stderr, " keys %lu", t->bt_nrecs);
#undef X
#define	X(flag, name) \
	if (ISSET(t, flag)) { \
		(void)fprintf(stderr, "%s%s", sep, name); \
		sep = ", "; \
	}
	if (t->bt_flags) {
		sep = " flags (";
		X(BTF_DELCRSR,	"DELCRSR");
		X(BTF_FIXEDLEN,	"FIXEDLEN");
		X(BTF_INMEM,	"INMEM");
		X(BTF_NODUPS,	"NODUPS");
		X(BTF_RDONLY,	"RDONLY");
		X(BTF_RECNO,	"RECNO");
		X(BTF_SEQINIT,	"SEQINIT");
		X(BTF_METADIRTY,"METADIRTY");
		(void)fprintf(stderr, ")\n");
	}
#undef X

	for (i = P_ROOT; (h = mpool_get(t->bt_mp, i, 0)) != NULL; ++i) {
		__bt_dpage(h);
		(void)mpool_put(t->bt_mp, h, 0);
	}
}

/*
 * BT_DMPAGE -- Dump the meta page
 *
 * Parameters:
 *	h:	pointer to the PAGE
 */
void
__bt_dmpage(h)
	PAGE *h;
{
	BTMETA *m;
	char *sep;

	m = (BTMETA *)h;
	(void)fprintf(stderr, "magic %lx\n", m->m_magic);
	(void)fprintf(stderr, "version %lu\n", m->m_version);
	(void)fprintf(stderr, "psize %lu\n", m->m_psize);
	(void)fprintf(stderr, "free %lu\n", m->m_free);
	(void)fprintf(stderr, "nrecs %lu\n", m->m_nrecs);
	(void)fprintf(stderr, "flags %lu", m->m_flags);
#undef X
#define	X(flag, name) \
	if (m->m_flags & flag) { \
		(void)fprintf(stderr, "%s%s", sep, name); \
		sep = ", "; \
	}
	if (m->m_flags) {
		sep = " (";
		X(BTF_NODUPS,	"NODUPS");
		X(BTF_RECNO,	"RECNO");
		(void)fprintf(stderr, ")");
	}
	(void)fprintf(stderr, "\nlorder %lu\n", m->m_lorder);
}

/*
 * BT_DNPAGE -- Dump the page
 *
 * Parameters:
 *	n:	page number to dump.
 */
void
__bt_dnpage(dbp, pgno)
	DB *dbp;
	pgno_t pgno;
{
	BTREE *t;
	PAGE *h;

	t = dbp->internal;
	if ((h = mpool_get(t->bt_mp, pgno, 0)) != NULL) {
		__bt_dpage(h);
		(void)mpool_put(t->bt_mp, h, 0);
	}
}

/*
 * BT_DPAGE -- Dump the page
 *
 * Parameters:
 *	h:	pointer to the PAGE
 */
void
__bt_dpage(h)
	PAGE *h;
{
	BINTERNAL *bi;
	BLEAF *bl;
	RINTERNAL *ri;
	RLEAF *rl;
	index_t cur, top;
	char *sep;

	(void)fprintf(stderr, "    page %d: (", h->pgno);
#undef X
#define	X(flag, name) \
	if (h->flags & flag) { \
		(void)fprintf(stderr, "%s%s", sep, name); \
		sep = ", "; \
	}
	sep = "";
	X(P_BINTERNAL,	"BINTERNAL")		/* types */
	X(P_BLEAF,	"BLEAF")
	X(P_RINTERNAL,	"RINTERNAL")		/* types */
	X(P_RLEAF,	"RLEAF")
	X(P_OVERFLOW,	"OVERFLOW")
	X(P_PRESERVE,	"PRESERVE");
	(void)fprintf(stderr, ")\n");
#undef X

	(void)fprintf(stderr, "\tprev %2d next %2d", h->prevpg, h->nextpg);
	if (h->flags & P_OVERFLOW)
		return;

	top = NEXTINDEX(h);
	(void)fprintf(stderr, " lower %3d upper %3d nextind %d\n",
	    h->lower, h->upper, top);
	for (cur = 0; cur < top; cur++) {
		(void)fprintf(stderr, "\t[%03d] %4d ", cur, h->linp[cur]);
		switch(h->flags & P_TYPE) {
		case P_BINTERNAL:
			bi = GETBINTERNAL(h, cur);
			(void)fprintf(stderr,
			    "size %2d pgno %2d", bi->ksize, bi->pgno);
			if (bi->flags & P_BIGKEY)
				(void)fprintf(stderr, " (indirect)");
			else if (bi->ksize)
				(void)fprintf(stderr, " {%s}", bi->bytes);
			break;
		case P_RINTERNAL:
			ri = GETRINTERNAL(h, cur);
			(void)fprintf(stderr, "entries %2d pgno %2d",
				ri->nrecs, ri->pgno);
			break;
		case P_BLEAF:
			bl = GETBLEAF(h, cur);
			if (bl->flags & P_BIGKEY)
				(void)fprintf(stderr,
				    "big key page %lu size %u/",
				    *(pgno_t *)bl->bytes,
				    *(size_t *)(bl->bytes + sizeof(pgno_t)));
			else if (bl->ksize)
				(void)fprintf(stderr, "%s/", bl->bytes);
			if (bl->flags & P_BIGDATA)
				(void)fprintf(stderr,
				    "big data page %lu size %u",
				    *(pgno_t *)(bl->bytes + bl->ksize),
				    *(size_t *)(bl->bytes + bl->ksize +
				    sizeof(pgno_t)));
			else if (bl->dsize)
				(void)fprintf(stderr,
				    "%s", bl->bytes + bl->ksize);
			break;
		case P_RLEAF:
			rl = GETRLEAF(h, cur);
			if (rl->flags & P_BIGDATA)
				(void)fprintf(stderr,
				    "big data page %lu size %u",
				    *(pgno_t *)rl->bytes,
				    *(size_t *)(rl->bytes + sizeof(pgno_t)));
			else if (rl->dsize)
				(void)fprintf(stderr, "%s", rl->bytes);
			break;
		}
		(void)fprintf(stderr, "\n");
	}
}
#endif

#ifdef STATISTICS
/*
 * BT_STAT -- Gather/print the tree statistics
 *
 * Parameters:
 *	dbp:	pointer to the DB
 */
void
__bt_stat(dbp)
	DB *dbp;
{
	extern u_long bt_cache_hit, bt_cache_miss;
	extern u_long bt_rootsplit, bt_split, bt_sortsplit;
	extern u_long bt_pfxsaved;
	BTREE *t;
	PAGE *h;
	pgno_t i, pcont, pinternal, pleaf;
	u_long ifree, lfree, nkeys;
	int levels;

	t = dbp->internal;
	pcont = pinternal = pleaf = 0;
	nkeys = ifree = lfree = 0;
	for (i = P_ROOT; (h = mpool_get(t->bt_mp, i, 0)) != NULL; ++i) {
		switch(h->flags & P_TYPE) {
		case P_BINTERNAL:
		case P_RINTERNAL:
			++pinternal;
			ifree += h->upper - h->lower;
			break;
		case P_BLEAF:
		case P_RLEAF:
			++pleaf;
			lfree += h->upper - h->lower;
			nkeys += NEXTINDEX(h);
			break;
		case P_OVERFLOW:
			++pcont;
			break;
		}
		(void)mpool_put(t->bt_mp, h, 0);
	}

	/* Count the levels of the tree. */
	for (i = P_ROOT, levels = 0 ;; ++levels) {
		h = mpool_get(t->bt_mp, i, 0);
		if (h->flags & (P_BLEAF|P_RLEAF)) {
			if (levels == 0)
				levels = 1;
			(void)mpool_put(t->bt_mp, h, 0);
			break;
		}
		i = ISSET(t, BTF_RECNO) ?
		    GETRINTERNAL(h, 0)->pgno :
		    GETBINTERNAL(h, 0)->pgno;
		(void)mpool_put(t->bt_mp, h, 0);
	}

	(void)fprintf(stderr, "%d level%s with %ld keys",
	    levels, levels == 1 ? "" : "s", nkeys);
	if (ISSET(t, BTF_RECNO))
		(void)fprintf(stderr, " (%ld header count)", t->bt_nrecs);
	(void)fprintf(stderr,
	    "\n%lu pages (leaf %ld, internal %ld, overflow %ld)\n",
	    pinternal + pleaf + pcont, pleaf, pinternal, pcont);
	(void)fprintf(stderr, "%ld cache hits, %ld cache misses\n",
	    bt_cache_hit, bt_cache_miss);
	(void)fprintf(stderr, "%ld splits (%ld root splits, %ld sort splits)\n",
	    bt_split, bt_rootsplit, bt_sortsplit);
	pleaf *= t->bt_psize - BTDATAOFF;
	if (pleaf)
		(void)fprintf(stderr,
		    "%.0f%% leaf fill (%ld bytes used, %ld bytes free)\n",
		    ((double)(pleaf - lfree) / pleaf) * 100,
		    pleaf - lfree, lfree);
	pinternal *= t->bt_psize - BTDATAOFF;
	if (pinternal)
		(void)fprintf(stderr,
		    "%.0f%% internal fill (%ld bytes used, %ld bytes free\n",
		    ((double)(pinternal - ifree) / pinternal) * 100,
		    pinternal - ifree, ifree);
	if (bt_pfxsaved)
		(void)fprintf(stderr, "prefix checking removed %lu bytes.\n",
		    bt_pfxsaved);
}
#endif
