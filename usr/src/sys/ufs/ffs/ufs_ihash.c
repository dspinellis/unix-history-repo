/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_ihash.c	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/vnode.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufs_extern.h>

#define	INOHSZ	512
#if	((INOHSZ & (INOHSZ - 1)) == 0)
#define	INOHASH(dev, ino)	(((dev) + (ino)) & (INOHSZ - 1))
#else
#define	INOHASH(dev, ino)	(((unsigned int)((dev) + (ino))) % INOHSZ)
#endif

static union ihead {
	union  ihead *ih_head[2];
	struct inode *ih_chain[2];
} ihead[INOHSZ];


/*
 * Initialize inode hash table.
 */
void
ufs_ihashinit()
{
	register union ihead *ih;
	register int i;

	for (ih = ihead, i = INOHSZ; --i >= 0; ++ih)
		ih->ih_head[0] = ih->ih_head[1] = ih;
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
	register union ihead *ih;
	register struct inode *ip;
	struct vnode *vp;

	ih = &ihead[INOHASH(dev, ino)];
loop:
	for (ip = ih->ih_chain[0]; ip != (struct inode *)ih; ip = ip->i_forw) {
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
	insque(ip, &ihead[INOHASH(ip->i_dev, ip->i_number)]);
	ILOCK(ip);
}
