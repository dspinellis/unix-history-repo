/*
 * Copyright (c) 1982, 1986, 1989, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_ihash.c	8.4 (Berkeley) %G%
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
struct inode **ihashtbl;
u_long	ihash;		/* size of hash table - 1 */
#define	INOHASH(device, inum)	(((device) + (inum)) & ihash)

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
ufs_ihashlookup(device, inum)
	dev_t device;
	ino_t inum;
{
	register struct inode *ip;

	for (ip = ihashtbl[INOHASH(device, inum)];; ip = ip->i_next) {
		if (ip == NULL)
			return (NULL);
		if (inum == ip->i_number && device == ip->i_dev)
			return (ITOV(ip));
	}
	/* NOTREACHED */
}

/*
 * Use the device/inum pair to find the incore inode, and return a pointer
 * to it. If it is in core, but locked, wait for it.
 */
struct vnode *
ufs_ihashget(device, inum)
	dev_t device;
	ino_t inum;
{
	register struct inode *ip;
	struct vnode *vp;

	for (;;)
		for (ip = ihashtbl[INOHASH(device, inum)];; ip = ip->i_next) {
			if (ip == NULL)
				return (NULL);
			if (inum == ip->i_number && device == ip->i_dev) {
				if (ip->i_flag & IN_LOCKED) {
					ip->i_flag |= IN_WANTED;
					sleep(ip, PINOD);
					break;
				}
				vp = ITOV(ip);
				if (!vget(vp, 1))
					return (vp);
				break;
			}
		}
	/* NOTREACHED */
}

/*
 * Insert the inode into the hash table, and return it locked.
 */
void
ufs_ihashins(ip)
	struct inode *ip;
{
	struct inode **ipp, *iq;

	ipp = &ihashtbl[INOHASH(ip->i_dev, ip->i_number)];
	if (iq = *ipp)
		iq->i_prev = &ip->i_next;
	ip->i_next = iq;
	ip->i_prev = ipp;
	*ipp = ip;
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

	if (iq = ip->i_next)
		iq->i_prev = ip->i_prev;
	*ip->i_prev = iq;
#ifdef DIAGNOSTIC
	ip->i_next = NULL;
	ip->i_prev = NULL;
#endif
}
