/*
 * Copyright (c) 1982, 1986, 1989, 1991, 1993, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_ihash.c	8.7 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/vnode.h>
#include <sys/malloc.h>
#include <sys/proc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufs_extern.h>

/*
 * Structures associated with inode cacheing.
 */
LIST_HEAD(ihashhead, inode) *ihashtbl;
u_long	ihash;		/* size of hash table - 1 */
#define	INOHASH(device, inum)	(&ihashtbl[((device) + (inum)) & ihash])
struct simplelock ufs_ihash_slock;

/*
 * Initialize inode hash table.
 */
void
ufs_ihashinit()
{

	ihashtbl = hashinit(desiredvnodes, M_UFSMNT, &ihash);
	simple_lock_init(&ufs_ihash_slock);
}

/*
 * Use the device/inum pair to find the incore inode, and return a pointer
 * to it. If it is in core, return it, even if it is locked.
 */
struct vnode *
ufs_ihashlookup(dev, inum)
	dev_t dev;
	ino_t inum;
{
	struct inode *ip;

	simple_lock(&ufs_ihash_slock);
	for (ip = INOHASH(dev, inum)->lh_first; ip; ip = ip->i_hash.le_next)
		if (inum == ip->i_number && dev == ip->i_dev)
			break;
	simple_unlock(&ufs_ihash_slock);

	if (ip)
		return (ITOV(ip));
	return (NULLVP);
}

/*
 * Use the device/inum pair to find the incore inode, and return a pointer
 * to it. If it is in core, but locked, wait for it.
 */
struct vnode *
ufs_ihashget(dev, inum)
	dev_t dev;
	ino_t inum;
{
	struct proc *p = curproc;	/* XXX */
	struct inode *ip;
	struct vnode *vp;

loop:
	simple_lock(&ufs_ihash_slock);
	for (ip = INOHASH(dev, inum)->lh_first; ip; ip = ip->i_hash.le_next) {
		if (inum == ip->i_number && dev == ip->i_dev) {
			vp = ITOV(ip);
			simple_lock(&vp->v_interlock);
			simple_unlock(&ufs_ihash_slock);
			if (vget(vp, LK_EXCLUSIVE | LK_INTERLOCK, p))
				goto loop;
			return (vp);
		}
	}
	simple_unlock(&ufs_ihash_slock);
	return (NULL);
}

/*
* Insert the inode into the hash table, and return it locked.
 */
void
ufs_ihashins(ip)
	struct inode *ip;
{
	struct proc *p = curproc;		/* XXX */
	struct ihashhead *ipp;

	/* lock the inode, then put it on the appropriate hash list */
	lockmgr(&ip->i_lock, LK_EXCLUSIVE, (struct simplelock *)0, p);

	simple_lock(&ufs_ihash_slock);
	ipp = INOHASH(ip->i_dev, ip->i_number);
	LIST_INSERT_HEAD(ipp, ip, i_hash);
	simple_unlock(&ufs_ihash_slock);
}

/*
 * Remove the inode from the hash table.
 */
void
ufs_ihashrem(ip)
	struct inode *ip;
{
	struct inode *iq;

	simple_lock(&ufs_ihash_slock);
	LIST_REMOVE(ip, i_hash);
#ifdef DIAGNOSTIC
	ip->i_hash.le_next = NULL;
	ip->i_hash.le_prev = NULL;
#endif
	simple_unlock(&ufs_ihash_slock);
}
