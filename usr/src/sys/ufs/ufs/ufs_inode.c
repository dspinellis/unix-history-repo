/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_inode.c	7.43 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/kernel.h>
#include <sys/malloc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

u_long	nextgennumber;		/* Next generation number to assign. */
int	prtactive = 0;		/* 1 => print out reclaim of active vnodes */

int
ufs_init()
{
	static int first = 1;

	if (!first)
		return (0);
	first = 0;

#ifdef DIAGNOSTIC
	if ((sizeof(struct inode) - 1) & sizeof(struct inode))
		printf("ufs_init: bad size %d\n", sizeof(struct inode));
#endif
	ufs_ihashinit();
	dqinit();
	return (0);
}

/*
 * Unlock and decrement the reference count of an inode structure.
 */
void
ufs_iput(ip)
	register struct inode *ip;
{

	if ((ip->i_flag & ILOCKED) == 0)
		panic("iput");
	IUNLOCK(ip);
	vrele(ITOV(ip));
}

/*
 * Reclaim an inode so that it can be used for other purposes.
 */
int
ufs_reclaim(vp)
	register struct vnode *vp;
{
	register struct inode *ip;
	int i, type;

	if (prtactive && vp->v_usecount != 0)
		vprint("ufs_reclaim: pushing active", vp);
	/*
	 * Remove the inode from its hash chain.
	 */
	ip = VTOI(vp);
	remque(ip);
	/*
	 * Purge old data structures associated with the inode.
	 */
	cache_purge(vp);
	if (ip->i_devvp) {
		vrele(ip->i_devvp);
		ip->i_devvp = 0;
	}
#ifdef QUOTA
	for (i = 0; i < MAXQUOTAS; i++) {
		if (ip->i_dquot[i] != NODQUOT) {
			dqrele(vp, ip->i_dquot[i]);
			ip->i_dquot[i] = NODQUOT;
		}
	}
#endif
	switch (vp->v_mount->mnt_stat.f_type) {
	case MOUNT_UFS:
		type = M_FFSNODE;
		break;
	case MOUNT_MFS:
		type = M_MFSNODE;
		break;
	case MOUNT_LFS:
		type = M_LFSNODE;
		break;
	default:
		panic("ufs_reclaim: not ufs file");
	}
	FREE(vp->v_data, type);
	vp->v_data = NULL;
	return (0);
}

/*
 * Lock an inode. If its already locked, set the WANT bit and sleep.
 */
void
ufs_ilock(ip)
	register struct inode *ip;
{

	while (ip->i_flag & ILOCKED) {
		ip->i_flag |= IWANT;
		if (ip->i_lockholder == curproc->p_pid)
			panic("locking against myself");
		ip->i_lockwaiter = curproc->p_pid;
		(void) sleep((caddr_t)ip, PINOD);
	}
	ip->i_lockwaiter = 0;
	ip->i_lockholder = curproc->p_pid;
	ip->i_flag |= ILOCKED;
	curproc->p_spare[2]++;
}

/*
 * Unlock an inode.  If WANT bit is on, wakeup.
 */
void
ufs_iunlock(ip)
	register struct inode *ip;
{

	if ((ip->i_flag & ILOCKED) == 0)
		vprint("ufs_iunlock: unlocked inode", ITOV(ip));
	ip->i_lockholder = 0;
	ip->i_flag &= ~ILOCKED;
	curproc->p_spare[2]--;
	if (ip->i_flag&IWANT) {
		ip->i_flag &= ~IWANT;
		wakeup((caddr_t)ip);
	}
}
