/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_cache.c	7.15 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "mount.h"
#include "vnode.h"
#include "namei.h"
#include "errno.h"
#include "malloc.h"

/*
 * Name caching works as follows:
 *
 * Names found by directory scans are retained in a cache
 * for future reference.  It is managed LRU, so frequently
 * used names will hang around.  Cache is indexed by hash value
 * obtained from (vp, name) where vp refers to the directory
 * containing name.
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
struct namecache **nchashtbl;
u_long	nchash;				/* size of hash table - 1 */
long	numcache;			/* number of cache entries allocated */
struct	namecache *nchhead, **nchtail;	/* LRU chain pointers */
struct	nchstats nchstats;		/* cache effectiveness statistics */

int doingcache = 1;			/* 1 => enable the cache */

/*
 * Look for a the name in the cache. We don't do this
 * if the segment name is long, simply so the cache can avoid
 * holding long names (which would either waste space, or
 * add greatly to the complexity).
 *
 * Lookup is called with ni_dvp pointing to the directory to search,
 * ni_ptr pointing to the name of the entry being sought, ni_namelen
 * tells the length of the name, and ni_hash contains a hash of
 * the name. If the lookup succeeds, the vnode is returned in ni_vp
 * and a status of -1 is returned. If the lookup determines that
 * the name does not exist (negative cacheing), a status of ENOENT
 * is returned. If the lookup fails, a status of zero is returned.
 */
int
cache_lookup(dvp, vpp, cnp)
	struct vnode *dvp;
	struct vnode **vpp;
	struct componentname *cnp;
{
	register struct namecache *ncp, *ncq, **ncpp;

	if (!doingcache)
		return (0);
	if (cnp->cn_namelen > NCHNAMLEN) {
		nchstats.ncs_long++;
		cnp->cn_flags &= ~MAKEENTRY;
		return (0);
	}
	ncpp = &nchashtbl[cnp->cn_hash & nchash];
	for (ncp = *ncpp; ncp; ncp = ncp->nc_forw) {
		if (ncp->nc_dvp == dvp &&
		    ncp->nc_dvpid == dvp->v_id &&
		    ncp->nc_nlen == cnp->cn_namelen &&
		    !bcmp(ncp->nc_name, cnp->cn_nameptr, (u_int)ncp->nc_nlen))
			break;
	}
	if (ncp == NULL) {
		nchstats.ncs_miss++;
		return (0);
	}
	if (!(cnp->cn_flags & MAKEENTRY)) {
		nchstats.ncs_badhits++;
	} else if (ncp->nc_vp == NULL) {
		if (cnp->cn_nameiop != CREATE) {
			nchstats.ncs_neghits++;
			/*
			 * Move this slot to end of LRU chain,
			 * if not already there.
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
			return (ENOENT);
		}
	} else if (ncp->nc_vpid != ncp->nc_vp->v_id) {
		nchstats.ncs_falsehits++;
	} else {
		nchstats.ncs_goodhits++;
		/*
		 * move this slot to end of LRU chain, if not already there
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
		*vpp = ncp->nc_vp;
		return (-1);
	}

	/*
	 * Last component and we are renaming or deleting,
	 * the cache entry is invalid, or otherwise don't
	 * want cache entry to exist.
	 */
	/* remove from LRU chain */
	if (ncq = ncp->nc_nxt)
		ncq->nc_prev = ncp->nc_prev;
	else
		nchtail = ncp->nc_prev;
	*ncp->nc_prev = ncq;
	/* remove from hash chain */
	if (ncq = ncp->nc_forw)
		ncq->nc_back = ncp->nc_back;
	*ncp->nc_back = ncq;
	/* and make a dummy hash chain */
	ncp->nc_forw = NULL;
	ncp->nc_back = NULL;
	/* insert at head of LRU list (first to grab) */
	if (ncq = nchhead)
		ncq->nc_prev = &ncp->nc_nxt;
	else
		nchtail = &ncp->nc_nxt;
	nchhead = ncp;
	ncp->nc_nxt = ncq;
	ncp->nc_prev = &nchhead;
	return (0);
}

/*
 * Add an entry to the cache
 */
cache_enter(dvp, vp, cnp)
	struct vnode *dvp;
	struct vnode *vp;
	struct componentname *cnp;
{
	register struct namecache *ncp, *ncq, **ncpp;

#ifdef DIAGNOSTIC
	if (cnp->cn_namelen > NCHNAMLEN)
		panic("cache_enter: name too long");
#endif
	if (!doingcache)
		return;
	/*
	 * Free the cache slot at head of lru chain.
	 */
	if (numcache < desiredvnodes) {
		ncp = (struct namecache *)
			malloc((u_long)sizeof *ncp, M_CACHE, M_WAITOK);
		bzero((char *)ncp, sizeof *ncp);
		numcache++;
	} else if (ncp = nchhead) {
		/* remove from lru chain */
		if (ncq = ncp->nc_nxt)
			ncq->nc_prev = ncp->nc_prev;
		else
			nchtail = ncp->nc_prev;
		*ncp->nc_prev = ncq;
		/* remove from old hash chain, if on one */
		if (ncp->nc_back) {
			if (ncq = ncp->nc_forw)
				ncq->nc_back = ncp->nc_back;
			*ncp->nc_back = ncq;
			ncp->nc_forw = NULL;
			ncp->nc_back = NULL;
		}
	} else
		return;
	/* grab the vnode we just found */
	ncp->nc_vp = vp;
	if (vp)
		ncp->nc_vpid = vp->v_id;
	else
		ncp->nc_vpid = 0;
	/* fill in cache info */
	ncp->nc_dvp = dvp;
	ncp->nc_dvpid = dvp->v_id;
	ncp->nc_nlen = cnp->cn_namelen;
	bcopy(cnp->cn_nameptr, ncp->nc_name, (unsigned)ncp->nc_nlen);
	/* link at end of lru chain */
	ncp->nc_nxt = NULL;
	ncp->nc_prev = nchtail;
	*nchtail = ncp;
	nchtail = &ncp->nc_nxt;
	/* and insert on hash chain */
	ncpp = &nchashtbl[cnp->cn_hash & nchash];
	if (ncq = *ncpp)
		ncq->nc_back = &ncp->nc_forw;
	ncp->nc_forw = ncq;
	ncp->nc_back = ncpp;
	*ncpp = ncp;
}

/*
 * Name cache initialization, from vfs_init() when we are booting
 */
nchinit()
{

	nchtail = &nchhead;
	nchashtbl = hashinit(desiredvnodes, M_CACHE, &nchash);
}

/*
 * Cache flush, a particular vnode; called when a vnode is renamed to
 * hide entries that would now be invalid
 */
cache_purge(vp)
	struct vnode *vp;
{
	struct namecache *ncp, **ncpp;

	vp->v_id = ++nextvnodeid;
	if (nextvnodeid != 0)
		return;
	for (ncpp = &nchashtbl[nchash]; ncpp >= nchashtbl; ncpp--) {
		for (ncp = *ncpp; ncp; ncp = ncp->nc_forw) {
			ncp->nc_vpid = 0;
			ncp->nc_dvpid = 0;
		}
	}
	vp->v_id = ++nextvnodeid;
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
	struct mount *mp;
{
	register struct namecache *ncp, *nxtcp;

	for (ncp = nchhead; ncp; ncp = nxtcp) {
		if (ncp->nc_dvp == NULL || ncp->nc_dvp->v_mount != mp) {
			nxtcp = ncp->nc_nxt;
			continue;
		}
		/* free the resources we had */
		ncp->nc_vp = NULL;
		ncp->nc_dvp = NULL;
		/* remove from old hash chain, if on one */
		if (ncp->nc_back) {
			if (nxtcp = ncp->nc_forw)
				nxtcp->nc_back = ncp->nc_back;
			*ncp->nc_back = nxtcp;
			ncp->nc_forw = NULL;
			ncp->nc_back = NULL;
		}
		/* delete this entry from LRU chain */
		if (nxtcp = ncp->nc_nxt)
			nxtcp->nc_prev = ncp->nc_prev;
		else
			nchtail = ncp->nc_prev;
		*ncp->nc_prev = nxtcp;
		/* cause rescan of list, it may have altered */
		/* also put the now-free entry at head of LRU */
		if (nxtcp = nchhead)
			nxtcp->nc_prev = &ncp->nc_nxt;
		else
			nchtail = &ncp->nc_nxt;
		nchhead = ncp;
		ncp->nc_nxt = nxtcp;
		ncp->nc_prev = &nchhead;
	}
}
