/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vfs_cache.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "vnode.h"
#include "namei.h"

/*
 * Name caching works as follows:
 *
 * Names found by directory scans are retained in a cache
 * for future reference.  It is managed LRU, so frequently
 * used names will hang around.  Cache is indexed by hash value
 * obtained from (ino,dev,name) where ino & dev refer to the
 * directory containing name.
 *
 * For simplicity (and economy of storage), names longer than
 * a maximum length of NCHNAMLEN are not cached; they occur
 * infrequently in any case, and are almost never of interest.
 *
 * Upon reaching the last segment of a path, if the reference
 * is for DELETE, or NOCACHE is set (rewrite), and the
 * name is located in the cache, it will be dropped.
 */

/*
 * Structures associated with name cacheing.
 */
#define	NCHHASH		128	/* size of hash table */

#if	((NCHHASH)&((NCHHASH)-1)) != 0
#define	NHASH(h)	(((unsigned)(h) >> 6) % (NCHHASH))
#else
#define	NHASH(h)	(((unsigned)(h) >> 6) & ((NCHHASH)-1))
#endif

union nchash {
	union	nchash *nch_head[2];
	struct	namecache *nch_chain[2];
} nchash[NCHHASH];
#define	nch_forw	nch_chain[0]
#define	nch_back	nch_chain[1]

struct	namecache *nchhead, **nchtail;	/* LRU chain pointers */
struct	nchstats nchstats;		/* cache effectiveness statistics */

/*
 * Look for a the name in the cache. We don't do this
 * if the segment name is long, simply so the cache can avoid
 * holding long names (which would either waste space, or
 * add greatly to the complexity).
 */
struct vnode *
cache_lookup(ndp)
	register struct nameidata *ndp;
{
	register struct vnode *dp;
	register struct namecache *ncp;
	union nchash *nhp;

	return (0);		/* XXX for now */
	if (ndp->ni_dent.d_namlen > NCHNAMLEN) {
		nchstats.ncs_long++;
		ndp->ni_makeentry = 0;
		return (0);
	}
	dp = ndp->ni_vp;
	nhp = &nchash[NHASH(dp)];
	for (ncp = nhp->nch_forw; ncp != (struct namecache *)nhp;
	    ncp = ncp->nc_forw) {
		if (ncp->nc_vp == dp &&
		    ncp->nc_nlen == ndp->ni_dent.d_namlen &&
		    !bcmp(ncp->nc_name, ndp->ni_dent.d_name,
			(unsigned)ncp->nc_nlen))
			break;
	}
	if (ncp == (struct namecache *)nhp) {
		nchstats.ncs_miss++;
		ncp = NULL;
		return (0);
	}
	if (ndp->ni_makeentry) {
		/*
		 * move this slot to end of LRU
		 * chain, if not already there
		 */
		if (ncp->nc_nxt) {
			/* remove from LRU chain */
			*ncp->nc_prev = ncp->nc_nxt;
			ncp->nc_nxt->nc_prev = ncp->nc_prev;

			/* and replace at end of it */
			ncp->nc_nxt = NULL;
			ncp->nc_prev = nchtail;
			*nchtail = ncp;
			nchtail = &ncp->nc_nxt;
		}
		/* ndp->ni_dent.d_ino = dp->i_number; */
		/* ni_dent.d_reclen is garbage ... */
		nchstats.ncs_goodhits++;
		return (ncp->nc_vp);
	}

	/*
	 * Last component and we are renaming or deleting,
	 * the cache entry is invalid, or otherwise don't
	 * want cache entry to exist.
	 */
	nchstats.ncs_badhits++;
	/* remove from LRU chain */
	*ncp->nc_prev = ncp->nc_nxt;
	if (ncp->nc_nxt)
		ncp->nc_nxt->nc_prev = ncp->nc_prev;
	else
		nchtail = ncp->nc_prev;
	remque(ncp);		/* remove from hash chain */
	/* insert at head of LRU list (first to grab) */
	ncp->nc_nxt = nchhead;
	ncp->nc_prev = &nchhead;
	nchhead->nc_prev = &ncp->nc_nxt;
	nchhead = ncp;
	/* and make a dummy hash chain */
	ncp->nc_forw = ncp;
	ncp->nc_back = ncp;
	ncp = NULL;
	return (0);
}

/*
 * Add an entry to the cache
 */
cache_enter(ndp)
	register struct nameidata *ndp;
{
	register struct namecache *ncp;
	union nchash *nhp;

	return;		/* XXX for now */
	/*
	 * Free the cache slot at head of lru chain.
	 */
	if (ncp = nchhead) {
		/* remove from lru chain */
		*ncp->nc_prev = ncp->nc_nxt;
		if (ncp->nc_nxt)
			ncp->nc_nxt->nc_prev = ncp->nc_prev;
		else
			nchtail = ncp->nc_prev;
		remque(ncp);		/* remove from old hash chain */
		/* grab the inode we just found */
		ncp->nc_vp = ndp->ni_vp;
		/* fill in cache info */
		ncp->nc_dp = ndp->ni_dvp;	/* parents vnode */
		ncp->nc_nlen = ndp->ni_dent.d_namlen;
		bcopy(ndp->ni_dent.d_name, ncp->nc_name,
		    (unsigned)ncp->nc_nlen);
		/* link at end of lru chain */
		ncp->nc_nxt = NULL;
		ncp->nc_prev = nchtail;
		*nchtail = ncp;
		nchtail = &ncp->nc_nxt;
		/* and insert on hash chain */
		nhp = &nchash[NHASH(ndp->ni_vp)];
		insque(ncp, nhp);
	}
}

/*
 * Name cache initialization, from main() when we are booting
 */
nchinit()
{
	register union nchash *nchp;
	register struct namecache *ncp;

	nchhead = 0;
	nchtail = &nchhead;
	for (ncp = namecache; ncp < &namecache[nchsize]; ncp++) {
		ncp->nc_forw = ncp;			/* hash chain */
		ncp->nc_back = ncp;
		ncp->nc_nxt = NULL;			/* lru chain */
		*nchtail = ncp;
		ncp->nc_prev = nchtail;
		nchtail = &ncp->nc_nxt;
		/* all else is zero already */
	}
	for (nchp = nchash; nchp < &nchash[NCHHASH]; nchp++) {
		nchp->nch_head[0] = nchp;
		nchp->nch_head[1] = nchp;
	}
}

/*
 * Cache flush, a particular vnode; called when a vnode is renamed to
 * remove entries that would now be invalid
 *
 * The line "nxtcp = nchhead" near the end is to avoid potential problems
 * if the cache lru chain is modified while we are dumping the
 * inode.  This makes the algorithm O(n^2), but do you think I care?
 */
cache_purge(vp)
	register struct vnode *vp;
{
	register struct namecache *ncp, *nxtcp;

	for (ncp = nchhead; ncp; ncp = nxtcp) {
		nxtcp = ncp->nc_nxt;
		if (ncp->nc_vp == NULL || ncp->nc_vp != vp)
			continue;
		/* free the resources we had */
		ncp->nc_vp = NULL;
		ncp->nc_dp = NULL;
		remque(ncp);		/* remove entry from its hash chain */
		ncp->nc_forw = ncp;	/* and make a dummy one */
		ncp->nc_back = ncp;
		/* delete this entry from LRU chain */
		*ncp->nc_prev = nxtcp;
		if (nxtcp)
			nxtcp->nc_prev = ncp->nc_prev;
		else
			nchtail = ncp->nc_prev;
		/* cause rescan of list, it may have altered */
		nxtcp = nchhead;
		/* put the now-free entry at head of LRU */
		ncp->nc_nxt = nxtcp;
		ncp->nc_prev = &nchhead;
		nxtcp->nc_prev = &ncp->nc_nxt;
		nchhead = ncp;
	}
}

/*
 * Cache flush, a whole filesystem; called when filesys is umounted to
 * remove entries that would now be invalid
 *
 * The line "nxtcp = nchhead" near the end is to avoid potential problems
 * if the cache lru chain is modified while we are dumping the
 * inode.  This makes the algorithm O(n^2), but do you think I care?
 */
cache_purgevfs(mp)
	register struct mount *mp;
{
	register struct namecache *ncp, *nxtcp;

	for (ncp = nchhead; ncp; ncp = nxtcp) {
		nxtcp = ncp->nc_nxt;
		if (ncp->nc_vp == NULL || ncp->nc_vp->v_mount != mp)
			continue;
		/* free the resources we had */
		ncp->nc_vp = NULL;
		ncp->nc_dp = NULL;
		remque(ncp);		/* remove entry from its hash chain */
		ncp->nc_forw = ncp;	/* and make a dummy one */
		ncp->nc_back = ncp;
		/* delete this entry from LRU chain */
		*ncp->nc_prev = nxtcp;
		if (nxtcp)
			nxtcp->nc_prev = ncp->nc_prev;
		else
			nchtail = ncp->nc_prev;
		/* cause rescan of list, it may have altered */
		nxtcp = nchhead;
		/* put the now-free entry at head of LRU */
		ncp->nc_nxt = nxtcp;
		ncp->nc_prev = &nchhead;
		nxtcp->nc_prev = &ncp->nc_nxt;
		nchhead = ncp;
	}
}
