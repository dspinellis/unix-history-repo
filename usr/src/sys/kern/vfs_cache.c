/*
 * Copyright (c) 1989, 1993, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Poul-Henning Kamp of the FreeBSD Project.
 *
 * %sccs.include.redist.c%
 *
 * from: vfs_cache.c,v 1.11 1995/03/12 02:01:20 phk Exp $
 *
 *	@(#)vfs_cache.c	8.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/namei.h>
#include <sys/errno.h>
#include <sys/malloc.h>

/*
 * Name caching works as follows:
 *
 * Names found by directory scans are retained in a cache
 * for future reference.  It is managed LRU, so frequently
 * used names will hang around.  Cache is indexed by hash value
 * obtained from (vp, name) where vp refers to the directory
 * containing name.
 *
 * If it is a "negative" entry, (i.e. for a name that is known NOT to
 * exist) the vnode pointer will be NULL.
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
#define NCHHASH(dvp, cnp) \
	(&nchashtbl[((dvp)->v_id + (cnp)->cn_hash) & nchash])
LIST_HEAD(nchashhead, namecache) *nchashtbl;	/* Hash Table */
u_long	nchash;				/* size of hash table - 1 */
long	numcache;			/* number of cache entries allocated */
TAILQ_HEAD(, namecache) nclruhead;	/* LRU chain */
struct	nchstats nchstats;		/* cache effectiveness statistics */

int doingcache = 1;			/* 1 => enable the cache */

/*
 * Delete an entry from its hash list and move it to the front
 * of the LRU list for immediate reuse.
 */
#define PURGE(ncp)  {						\
	LIST_REMOVE(ncp, nc_hash);				\
	ncp->nc_hash.le_prev = 0;				\
	TAILQ_REMOVE(&nclruhead, ncp, nc_lru);			\
	TAILQ_INSERT_HEAD(&nclruhead, ncp, nc_lru);		\
}

/*
 * Move an entry that has been used to the tail of the LRU list
 * so that it will be preserved for future use.
 */
#define TOUCH(ncp)  {						\
	if (ncp->nc_lru.tqe_next != 0) {			\
		TAILQ_REMOVE(&nclruhead, ncp, nc_lru);		\
		TAILQ_INSERT_TAIL(&nclruhead, ncp, nc_lru);	\
	}							\
}

/*
 * Lookup an entry in the cache 
 *
 * We don't do this if the segment name is long, simply so the cache 
 * can avoid holding long names (which would either waste space, or
 * add greatly to the complexity).
 *
 * Lookup is called with dvp pointing to the directory to search,
 * cnp pointing to the name of the entry being sought. If the lookup
 * succeeds, the vnode is returned in *vpp, and a status of -1 is
 * returned. If the lookup determines that the name does not exist
 * (negative cacheing), a status of ENOENT is returned. If the lookup
 * fails, a status of zero is returned.
 */

int
cache_lookup(dvp, vpp, cnp)
	struct vnode *dvp;
	struct vnode **vpp;
	struct componentname *cnp;
{
	register struct namecache *ncp, *nnp;
	register struct nchashhead *ncpp;

	if (!doingcache) {
		cnp->cn_flags &= ~MAKEENTRY;
		return (0);
	}
	if (cnp->cn_namelen > NCHNAMLEN) {
		nchstats.ncs_long++;
		cnp->cn_flags &= ~MAKEENTRY;
		return (0);
	}

	ncpp = NCHHASH(dvp, cnp);
	for (ncp = ncpp->lh_first; ncp != 0; ncp = nnp) {
		nnp = ncp->nc_hash.le_next;
		/* If one of the vp's went stale, don't bother anymore. */
		if ((ncp->nc_dvpid != ncp->nc_dvp->v_id) ||
		    (ncp->nc_vp && ncp->nc_vpid != ncp->nc_vp->v_id)) {
			nchstats.ncs_falsehits++;
			PURGE(ncp);
			continue;
		}
		/* Now that we know the vp's to be valid, is it ours ? */
		if (ncp->nc_dvp == dvp &&
		    ncp->nc_nlen == cnp->cn_namelen &&
		    !bcmp(ncp->nc_name, cnp->cn_nameptr, (u_int)ncp->nc_nlen))
			break;
	}

	/* We failed to find an entry */
	if (ncp == 0) {
		nchstats.ncs_miss++;
		return (0);
	}

	/* We don't want to have an entry, so dump it */
	if ((cnp->cn_flags & MAKEENTRY) == 0) {
		nchstats.ncs_badhits++;
		PURGE(ncp);
		return (0);
	} 

	/* We found a "positive" match, return the vnode */
        if (ncp->nc_vp) {
		nchstats.ncs_goodhits++;
		TOUCH(ncp);
		*vpp = ncp->nc_vp;
		return (-1);
	}

	/* We found a negative match, and want to create it, so purge */
	if (cnp->cn_nameiop == CREATE) {
		nchstats.ncs_badhits++;
		PURGE(ncp);
		return (0);
	}

	/* We found a "negative" match, ENOENT notifies client of this match */
	nchstats.ncs_neghits++;
	TOUCH(ncp);
	return (ENOENT);
}

/*
 * Add an entry to the cache.
 */
void
cache_enter(dvp, vp, cnp)
	struct vnode *dvp;
	struct vnode *vp;
	struct componentname *cnp;
{
	register struct namecache *ncp;
	register struct nchashhead *ncpp;

	if (!doingcache)
		return;

#ifdef DIAGNOSTIC
	if (cnp->cn_namelen > NCHNAMLEN)
		panic("cache_enter: name too long");
#endif

	/*
	 * We allocate a new entry if we are less than the maximum
	 * allowed and the one at the front of the LRU list is in use.
	 * Otherwise we use the one at the front of the LRU list.
	 */
	if (numcache < desiredvnodes &&
	    ((ncp = nclruhead.tqh_first) == NULL ||
	    ncp->nc_hash.le_prev != 0)) {
		/* Add one more entry */
		ncp = (struct namecache *)
			malloc((u_long)sizeof *ncp, M_CACHE, M_WAITOK);
		bzero((char *)ncp, sizeof *ncp);
		numcache++;
	} else if (ncp = nclruhead.tqh_first) {
		/* reuse an old entry */
		TAILQ_REMOVE(&nclruhead, ncp, nc_lru);
		if (ncp->nc_hash.le_prev != 0) {
			LIST_REMOVE(ncp, nc_hash);
			ncp->nc_hash.le_prev = 0;
		}
	} else {
		/* give up */
		return;
	}

	/* fill in cache info, if vp is NULL this is a "negative" cache entry */
	ncp->nc_vp = vp;
	if (vp)
		ncp->nc_vpid = vp->v_id;
	else
		ncp->nc_vpid = 0;
	ncp->nc_dvp = dvp;
	ncp->nc_dvpid = dvp->v_id;
	ncp->nc_nlen = cnp->cn_namelen;
	bcopy(cnp->cn_nameptr, ncp->nc_name, (unsigned)ncp->nc_nlen);
	TAILQ_INSERT_TAIL(&nclruhead, ncp, nc_lru);
	ncpp = NCHHASH(dvp, cnp);
	LIST_INSERT_HEAD(ncpp, ncp, nc_hash);
}

/*
 * Name cache initialization, from vfs_init() when we are booting
 */
void
nchinit()
{

	TAILQ_INIT(&nclruhead);
	nchashtbl = hashinit(desiredvnodes, M_CACHE, &nchash);
}

/*
 * Invalidate a all entries to particular vnode.
 * 
 * We actually just increment the v_id, that will do it. The entries will
 * be purged by lookup as they get found. If the v_id wraps around, we
 * need to ditch the entire cache, to avoid confusion. No valid vnode will
 * ever have (v_id == 0).
 */
void
cache_purge(vp)
	struct vnode *vp;
{
	struct namecache *ncp;
	struct nchashhead *ncpp;

	vp->v_id = ++nextvnodeid;
	if (nextvnodeid != 0)
		return;
	for (ncpp = &nchashtbl[nchash]; ncpp >= nchashtbl; ncpp--) {
		while (ncp = ncpp->lh_first)
			PURGE(ncp);
	}
	vp->v_id = ++nextvnodeid;
}

/*
 * Flush all entries referencing a particular filesystem.
 *
 * Since we need to check it anyway, we will flush all the invalid
 * entriess at the same time.
 */
void
cache_purgevfs(mp)
	struct mount *mp;
{
	struct nchashhead *ncpp;
	struct namecache *ncp, *nnp;

	/* Scan hash tables for applicable entries */
	for (ncpp = &nchashtbl[nchash]; ncpp >= nchashtbl; ncpp--) {
		for (ncp = ncpp->lh_first; ncp != 0; ncp = nnp) {
			nnp = ncp->nc_hash.le_next;
			if (ncp->nc_dvpid != ncp->nc_dvp->v_id ||
			    (ncp->nc_vp && ncp->nc_vpid != ncp->nc_vp->v_id) ||
			    ncp->nc_dvp->v_mount == mp) {
				PURGE(ncp);
			}
		}
	}
}
