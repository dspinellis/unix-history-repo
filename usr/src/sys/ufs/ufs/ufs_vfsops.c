/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_vfsops.c	7.64 (Berkeley) %G%
 */

#include <sys/param.h>
#include <net/radix.h>
#include <sys/domain.h>
#include <sys/socket.h>
#include <sys/mbuf.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/malloc.h>

#include <miscfs/specfs/specdev.h>
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
 * Do operations associated with quotas
 */
int
ufs_quotactl(mp, cmds, uid, arg, p)
	struct mount *mp;
	int cmds;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{
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

/*
 * Build hash lists of net addresses and hang them off the mount point.
 * Called by ufs_mount() to set up the lists of export addresses.
 */
ufs_hang_addrlist(mp, argp)
	struct mount *mp;
	struct ufs_args *argp;
{
	register struct netcred *np;
	register struct radix_node_head *rnh;
	register int i;
	struct radix_node *rn;
	struct ufsmount *ump;
	struct sockaddr *saddr, *smask = 0;
	struct domain *dom;
	int error;

	if (argp->slen == 0) {
		if (mp->mnt_flag & MNT_DEFEXPORTED)
			return (EPERM);
		np = &ump->um_defexported;
		np->netc_exflags = argp->exflags;
		np->netc_anon = argp->anon;
		np->netc_anon.cr_ref = 1;
		mp->mnt_flag |= MNT_DEFEXPORTED;
		return (0);
	}
	i = sizeof(struct netcred) + argp->slen + argp->msklen;
	np = (struct netcred *)malloc(i, M_NETADDR, M_WAITOK);
	bzero((caddr_t)np, i);
	saddr = (struct sockaddr *)(np + 1);
	if (error = copyin(argp->saddr, (caddr_t)saddr, argp->slen))
		goto out;
	if (saddr->sa_len > argp->slen)
		saddr->sa_len = argp->slen;
	if (argp->msklen) {
		smask = (struct sockaddr *)((caddr_t)saddr + argp->slen);
		if (error = copyin(argp->saddr, (caddr_t)smask, argp->msklen))
			goto out;
		if (smask->sa_len > argp->msklen)
			smask->sa_len = argp->msklen;
	}
	ump = VFSTOUFS(mp);
	i = saddr->sa_family;
	if ((rnh = ump->um_rtable[i]) == 0) {
		/*
		 * Seems silly to initialize every AF when most are not
		 * used, do so on demand here
		 */
		for (dom = domains; dom; dom = dom->dom_next)
			if (dom->dom_family == i && dom->dom_rtattach) {
				dom->dom_rtattach((void **)&ump->um_rtable[i],
					dom->dom_rtoffset);
				break;
			}
		if ((rnh = ump->um_rtable[i]) == 0) {
			error = ENOBUFS;
			goto out;
		}
	}
	rn = (*rnh->rnh_add)((caddr_t)saddr, (caddr_t)smask, rnh->rnh_treetop,
	    np->netc_rnodes);
	if (rn == 0 || np != (struct netcred *)rn) { /* already exists */
		error = EPERM;
		goto out;
	}
	np->netc_exflags = argp->exflags;
	np->netc_anon = argp->anon;
	np->netc_anon.cr_ref = 1;
	return (0);
out:
	free(np, M_NETADDR);
	return (error);
}

/* ARGSUSED */
static int
ufs_free_netcred(rn, w)
	struct radix_node *rn;
	caddr_t w;
{
	free((caddr_t)rn, M_NETADDR);
}
	

/*
 * Free the net address hash lists that are hanging off the mount points.
 */
void
ufs_free_addrlist(ump)
	struct ufsmount *ump;
{
	register int i;
	register struct radix_node_head *rnh;

	for (i = 0; i <= AF_MAX; i++)
		if (rnh = ump->um_rtable[i]) {
			(*rnh->rnh_walk)(rnh->rnh_treetop,
				ufs_free_netcred, (caddr_t)0);
			free((caddr_t)rnh, M_RTABLE);
			ump->um_rtable[i] = 0;
		}
}

/*
 * This is the generic part of fhtovp called after the underlying
 * filesystem has validated the file handle.
 *
 * Verify that a host should have access to a filesystem, and if so
 * return a vnode for the presented file handle.
 */
int
ufs_check_export(mp, ufhp, nam, vpp, exflagsp, credanonp)
	register struct mount *mp;
	struct ufid *ufhp;
	struct mbuf *nam;
	struct vnode **vpp;
	int *exflagsp;
	struct ucred **credanonp;
{
	register struct inode *ip;
	register struct netcred *np;
	register struct ufsmount *ump = VFSTOUFS(mp);
	register struct radix_node_head *rnh;
	struct vnode *nvp;
	struct sockaddr *saddr;
	int error;

	/*
	 * Get the export permission structure for this <mp, client> tuple.
	 */
	if ((mp->mnt_flag & MNT_EXPORTED) == 0)
		return (EACCES);
	if (nam == NULL) {
		np = NULL;
	} else {
		saddr = mtod(nam, struct sockaddr *);
		rnh = ump->um_rtable[saddr->sa_family];
		if (rnh == NULL) {
			np = NULL;
		} else {
			np = (struct netcred *)
			    (*rnh->rnh_match)((caddr_t)saddr, rnh->rnh_treetop);
			if (np->netc_rnodes->rn_flags & RNF_ROOT)
				np = NULL;
		}
	}
	if (np == NULL) {
		/*
		 * If no address match, use the default if it exists.
		 */
		if ((mp->mnt_flag & MNT_DEFEXPORTED) == 0)
			return (EACCES);
		np = &ump->um_defexported;
	}
	if (error = VFS_VGET(mp, ufhp->ufid_ino, &nvp)) {
		*vpp = NULLVP;
		return (error);
	}
	ip = VTOI(nvp);
	if (ip->i_mode == 0 || ip->i_gen != ufhp->ufid_gen) {
		ufs_iput(ip);
		*vpp = NULLVP;
		return (ESTALE);
	}
	*vpp = nvp;
	*exflagsp = np->netc_exflags;
	*credanonp = &np->netc_anon;
	return (0);
}
