/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_vfsops.c	7.60 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/socket.h>
#include <sys/malloc.h>
#include <sys/mbuf.h>
#include <netinet/in.h>
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
 * Build hash lists of net addresses and hang them off the mount point.
 * Called by ufs_mount() to set up the lists of export addresses.
 */
ufs_hang_addrlist(mp, argp)
	struct mount *mp;
	struct ufs_args *argp;
{
	register struct netaddrhash *np, **hnp;
	register int i;
	struct ufsmount *ump;
	struct sockaddr *saddr;
	struct mbuf *nam, *msk = (struct mbuf *)0;
	union nethostaddr netmsk;
	int error;

	if (error = sockargs(&nam, (caddr_t)argp->saddr, argp->slen, MT_SONAME))
		return (error);
	saddr = mtod(nam, struct sockaddr *);
	ump = VFSTOUFS(mp);
	if (saddr->sa_family == AF_INET &&
		((struct sockaddr_in *)saddr)->sin_addr.s_addr == INADDR_ANY) {
		m_freem(nam);
		if (mp->mnt_flag & MNT_DEFEXPORTED)
			return (EPERM);
		np = &ump->um_defexported;
		np->neth_exflags = argp->exflags;
		np->neth_anon = argp->anon;
		np->neth_anon.cr_ref = 1;
		mp->mnt_flag |= MNT_DEFEXPORTED;
		return (0);
	}
	if (argp->msklen > 0) {
		if (error = sockargs(&msk, (caddr_t)argp->smask, argp->msklen,
		    MT_SONAME)) {
			m_freem(nam);
			return (error);
		}

		/*
		 * Scan all the hash lists to check against duplications.
		 * For the net list, try both masks to catch a subnet
		 * of another network.
		 */
		hnp = &ump->um_netaddr[NETMASK_HASH];
		np = *hnp;
		if (saddr->sa_family == AF_INET)
			netmsk.had_inetaddr =
			    mtod(msk, struct sockaddr_in *)->sin_addr.s_addr;
		else
			netmsk.had_nam = msk;
		while (np) {
			if (netaddr_match(np->neth_family, &np->neth_haddr,
			    &np->neth_hmask, nam) ||
			    netaddr_match(np->neth_family, &np->neth_haddr,
			    &netmsk, nam)) {
				m_freem(nam);
				m_freem(msk);
				return (EPERM);
			}
			np = np->neth_next;
		}
		for (i = 0; i < NETHASHSZ; i++) {
			np = ump->um_netaddr[i];
			while (np) {
				if (netaddr_match(np->neth_family,
				    &np->neth_haddr, &netmsk, nam)) {
					m_freem(nam);
					m_freem(msk);
					return (EPERM);
				}
				np = np->neth_next;
			}
		}
	} else {
		hnp = &ump->um_netaddr[NETADDRHASH(saddr)];
		np = ump->um_netaddr[NETMASK_HASH];
		while (np) {
			if (netaddr_match(np->neth_family, &np->neth_haddr,
			    &np->neth_hmask, nam)) {
				m_freem(nam);
				return (EPERM);
			}
			np = np->neth_next;
		}
		np = *hnp;
		while (np) {
			if (netaddr_match(np->neth_family, &np->neth_haddr,
			    (union nethostaddr *)0, nam)) {
				m_freem(nam);
				return (EPERM);
			}
			np = np->neth_next;
		}
	}
	np = (struct netaddrhash *) malloc(sizeof(struct netaddrhash),
	    M_NETADDR, M_WAITOK);
	np->neth_family = saddr->sa_family;
	if (saddr->sa_family == AF_INET) {
		np->neth_inetaddr =
		    ((struct sockaddr_in *)saddr)->sin_addr.s_addr;
		m_freem(nam);
		if (msk) {
			np->neth_inetmask = netmsk.had_inetaddr;
			m_freem(msk);
			if (np->neth_inetaddr &~ np->neth_inetmask)
				return (EPERM);
		} else
			np->neth_inetmask = 0xffffffff;
	} else {
		np->neth_nam = nam;
		np->neth_msk = msk;
	}
	np->neth_exflags = argp->exflags;
	np->neth_anon = argp->anon;
	np->neth_anon.cr_ref = 1;
	np->neth_next = *hnp;
	*hnp = np;
	return (0);
}

/*
 * Free the net address hash lists that are hanging off the mount points.
 */
void
ufs_free_addrlist(ump)
	struct ufsmount *ump;
{
	register struct netaddrhash *np, *onp;
	register int i;

	for (i = 0; i <= NETHASHSZ; i++) {
		np = ump->um_netaddr[i];
		ump->um_netaddr[i] = (struct netaddrhash *)0;
		while (np) {
			onp = np;
			np = np->neth_next;
			if (onp->neth_family != AF_INET) {
				m_freem(onp->neth_nam);
				m_freem(onp->neth_msk);
			}
			free((caddr_t)onp, M_NETADDR);
		}
	}
}
