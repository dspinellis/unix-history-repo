/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_vfsops.c	7.57 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include "ioctl.h"
#include "disklabel.h"
#include "stat.h"

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

/*
 * Flag to permit forcible unmounting.
 */
int doforce = 1;

/*
 * Make a filesystem operational.
 * Nothing to do at the moment.
 */
/* ARGSUSED */
int
ufs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

/*
	error = closei(dev, IFBLK, fs->fs_ronly? FREAD : FREAD|FWRITE);
	irele(ip);
	return (error);
}

/*
 * Return root of a filesystem
 */
int
ufs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	register struct inode *ip;
	struct ufsmount *ump;
	struct inode *nip;
	struct vnode tvp;
	int error;

	ump = VFSTOUFS(mp);
	tvp.v_mount = mp;
	ip = VTOI(&tvp);
	ip->i_vnode = &tvp;
	ip->i_dev = ump->um_dev;
	if (error = (ump->um_iget)(ip, (ino_t)ROOTINO, &nip))
		return (error);
	*vpp = ITOV(nip);
	return (0);
}

/*
 * Do operations associated with quotas
 */
int
ufs_quotactl(mp, cmds, uid, arg, p)
	struct mount *mp;
	int cmds;
	u_int uid;
	caddr_t arg;
	struct proc *p;
{
	struct ufsmount *ump = VFSTOUFS(mp);
	int cmd, type, error;

#ifndef QUOTA
	return (EOPNOTSUPP);
#else
	if (uid == -1)
		uid = p->p_cred->p_ruid;
	cmd = cmds >> SUBCMDSHIFT;

	switch (cmd) {
	case Q_GETQUOTA:
	case Q_SYNC:
		if (uid == p->p_cred->p_ruid)
			break;
		/* fall through */
	default:
		if (error = suser(p->p_ucred, &p->p_acflag))
			return (error);
	}

	type = cmd & SUBCMDMASK;
	if ((u_int)type >= MAXQUOTAS)
		return (EINVAL);

	switch (cmd) {

	case Q_QUOTAON:
		return (quotaon(p, mp, type, arg));

	case Q_QUOTAOFF:
		if (vfs_busy(mp))
			return (0);
		error = quotaoff(p, mp, type);
		vfs_unbusy(mp);
		return (error);

	case Q_SETQUOTA:
		return (setquota(mp, uid, type, arg));

	case Q_SETUSE:
		return (setuse(mp, uid, type, arg));

	case Q_GETQUOTA:
		return (getquota(mp, uid, type, arg));

	case Q_SYNC:
		if (vfs_busy(mp))
			return (0);
		error = qsync(mp);
		vfs_unbusy(mp);
		return (error);

	default:
		return (EINVAL);
	}
	/* NOTREACHED */
#endif
}

int syncprt = 0;

/*
 * Print out statistics on the current allocation of the buffer pool.
 * Can be enabled to print out on every ``sync'' by setting "syncprt"
 * above.
 */
void
ufs_bufstats()
{
	int s, i, j, count;
	register struct buf *bp, *dp;
	int counts[MAXBSIZE/CLBYTES+1];
	static char *bname[BQUEUES] = { "LOCKED", "LRU", "AGE", "EMPTY" };

	for (bp = bfreelist, i = 0; bp < &bfreelist[BQUEUES]; bp++, i++) {
		count = 0;
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			counts[j] = 0;
		s = splbio();
		for (dp = bp->av_forw; dp != bp; dp = dp->av_forw) {
			counts[dp->b_bufsize/CLBYTES]++;
			count++;
		}
		splx(s);
		printf("%s: total-%d", bname[i], count);
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			if (counts[j] != 0)
				printf(", %d-%d", j * CLBYTES, counts[j]);
		printf("\n");
	}
}

/*
 * File handle to vnode
 *
 * Have to be really careful about stale file handles:
 * - check that the inode number is valid
 * - call iget() to get the locked inode
 * - check for an unallocated inode (i_mode == 0)
 * - check that the generation number matches
 */
int
ufs_fhtovp(mp, fhp, vpp)
	register struct mount *mp;
	struct fid *fhp;
	struct vnode **vpp;
{
	register struct inode *ip;
	register struct ufid *ufhp;
	struct ufsmount *ump;
	struct inode *nip;
	struct vnode tvp;
	int error;

	ufhp = (struct ufid *)fhp;
	ump = VFSTOUFS(mp);
	tvp.v_mount = mp;
	ip = VTOI(&tvp);
	ip->i_vnode = &tvp;
	ip->i_dev = ump->um_dev;
	if (error = (ump->um_iget)(ip, ufhp->ufid_ino, &nip)) {
		*vpp = NULLVP;
		return (error);
	}
	ip = nip;
	if (ip->i_mode == 0) {
		ufs_iput(ip);
		*vpp = NULLVP;
		return (EINVAL);
	}
	if (ip->i_gen != ufhp->ufid_gen) {
		ufs_iput(ip);
		*vpp = NULLVP;
		return (EINVAL);
	}
	*vpp = ITOV(ip);
	return (0);
}

/*
 * Vnode pointer to File handle
 */
/* ARGSUSED */
ufs_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{
	register struct inode *ip;
	register struct ufid *ufhp;

	ip = VTOI(vp);
	ufhp = (struct ufid *)fhp;
	ufhp->ufid_len = sizeof(struct ufid);
	ufhp->ufid_ino = ip->i_number;
	ufhp->ufid_gen = ip->i_gen;
	return (0);
}
