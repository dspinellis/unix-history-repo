/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_ihash.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/malloc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufs_extern.h>

/*
 * Structures associated with inode cacheing.
 */
union ihash {
	union	ihash *ih_head[2];
	struct	inode *ih_chain[2];
} *ihashtbl;
#define	ih_forw	ih_chain[0]
#define	ih_back	ih_chain[1]

u_long	ihash;			/* size of hash table - 1 */
#define	INOHASH(dev, ino)	(((dev) + (ino)) & ihash)

/*
 * Initialize inode hash table.
 */
void
ufs_ihashinit()
{
	register union ihash *ihp;
	long ihashsize;

	ihashsize = roundup((desiredvnodes + 1) * sizeof *ihp / 2,
		NBPG * CLSIZE);
	ihashtbl = (union ihash *)malloc((u_long)ihashsize,
	    M_UFSMNT, M_WAITOK);
	for (ihash = 1; ihash <= ihashsize / sizeof *ihp; ihash <<= 1)
		continue;
	ihash = (ihash >> 1) - 1;
	for (ihp = &ihashtbl[ihash]; ihp >= ihashtbl; ihp--) {
		ihp->ih_head[0] = ihp;
		ihp->ih_head[1] = ihp;
	}
}

/*
 * Use the dev/ino pair to find the incore inode, and return a pointer to it.
 * If it is in core, but locked, wait for it.
 */
struct vnode *
ufs_ihashget(dev, ino)
	/* dev_t */ int dev;
	ino_t ino;
{
	register union ihash *ihp;
	register struct inode *ip;
	struct vnode *vp;

	ihp = &ihashtbl[INOHASH(dev, ino)];
loop:
	for (ip = ihp->ih_forw; ip != (struct inode *)ihp; ip = ip->i_forw) {
		if (ino != ip->i_number || dev != ip->i_dev)
			continue;
		if ((ip->i_flag & ILOCKED) != 0) {
			ip->i_flag |= IWANT;
			sleep((caddr_t)ip, PINOD);
			goto loop;
		}
		vp = ITOV(ip);
		if (vget(vp))
			goto loop;
		return (vp);
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
	insque(ip, &ihashtbl[INOHASH(ip->i_dev, ip->i_number)]);
	ILOCK(ip);
}
