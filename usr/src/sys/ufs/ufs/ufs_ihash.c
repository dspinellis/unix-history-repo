/*
 * Copyright (c) 1982, 1986, 1989, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_ihash.c	8.5 (Berkeley) %G%
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

/*
 * Initialize inode hash table.
 */
void
ufs_ihashinit()
{

	ihashtbl = hashinit(desiredvnodes, M_UFSMNT, &ihash);
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
	register struct inode *ip;

	for (ip = INOHASH(dev, inum)->lh_first; ip; ip = ip->i_hash.le_next) {
		if (inum == ip->i_number && dev == ip->i_dev)
			return (ITOV(ip));
	}
	return (NULL);
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
	register struct inode *ip;
	struct vnode *vp;

loop:
	for (ip = INOHASH(dev, inum)->lh_first; ip; ip = ip->i_hash.le_next) {
		if (inum == ip->i_number && dev == ip->i_dev) {
			if (ip->i_flag & IN_LOCKED) {
				ip->i_flag |= IN_WANTED;
				sleep(ip, PINOD);
				goto loop;
			}
			vp = ITOV(ip);
			if (vget(vp, 1))
				goto loop;
			return (vp);
		}
	}
	return (NULL);
}

/*
* Insert the inode into the hash table, and return it locked.
 */
void
ufs_ihashins(ip)
	struct inode *ip;
{
	struct ihashhead *ipp;

	ipp = INOHASH(ip->i_dev, ip->i_number);
	LIST_INSERT_HEAD(ipp, ip, i_hash);
	if (ip->i_flag & IN_LOCKED)
		panic("ufs_ihashins: already locked");
	if (curproc)
		ip->i_lockholder = curproc->p_pid;
	else
		ip->i_lockholder = -1;
	ip->i_flag |= IN_LOCKED;
}

/*
 * Remove the inode from the hash table.
 */
void
ufs_ihashrem(ip)
	register struct inode *ip;
{
	register struct inode *iq;

	LIST_REMOVE(ip, i_hash);
#ifdef DIAGNOSTIC
	ip->i_hash.le_next = NULL;
	ip->i_hash.le_prev = NULL;
#endif
}
