/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfs_node.c	7.8 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "mount.h"
#include "vnode.h"
#include "../ufs/dir.h"
#include "namei.h"
#include "errno.h"
#include "nfsv2.h"
#include "nfs.h"
#include "nfsnode.h"
#include "nfsmount.h"
#include "kernel.h"
#include "malloc.h"

/* The request list head */
extern struct nfsreq nfsreqh;

#define	NFSNOHSZ	512
#if	((NFSNOHSZ&(NFSNOHSZ-1)) == 0)
#define	NFSNOHASH(fhsum)	((fhsum)&(NFSNOHSZ-1))
#else
#define	NFSNOHASH(fhsum)	(((unsigned)(fhsum))%NFSNOHSZ)
#endif

union nhead {
	union  nhead *nh_head[2];
	struct nfsnode *nh_chain[2];
} nhead[NFSNOHSZ];

#define TRUE	1
#define	FALSE	0

/*
 * Initialize hash links for nfsnodes
 * and build nfsnode free list.
 */
nfs_nhinit()
{
	register int i;
	register union  nhead *nh = nhead;

	if (VN_MAXPRIVATE < sizeof(struct nfsnode))
		panic("nfs_nhinit: too small");
	for (i = NFSNOHSZ; --i >= 0; nh++) {
		nh->nh_head[0] = nh;
		nh->nh_head[1] = nh;
	}
}

/*
 * Look up a vnode/nfsnode by file handle.
 * Callers must check for mount points!!
 * In all cases, a pointer to a
 * nfsnode structure is returned.
 */
nfs_nget(mntp, fhp, npp)
	struct mount *mntp;
	register nfsv2fh_t *fhp;
	struct nfsnode **npp;
{
	register struct nfsnode *np;
	register struct vnode *vp;
	register u_char *fhpp;
	register u_long fhsum;
	struct vnode *nvp;
	union nhead *nh;
	int i, error;

	fhpp = &fhp->fh_bytes[0];
	fhsum = 0;
	for (i = 0; i < NFSX_FH; i++)
		fhsum += *fhpp++;
loop:
	nh = &nhead[NFSNOHASH(fhsum)];
	for (np = nh->nh_chain[0]; np != (struct nfsnode *)nh; np = np->n_forw) {
		if (mntp != NFSTOV(np)->v_mount ||
		    bcmp((caddr_t)fhp, (caddr_t)&np->n_fh, NFSX_FH))
			continue;
		/*
		 * Following is essentially an inline expanded
		 * copy of ngrab(), expanded inline for speed,
		 * and so that the test for a mounted on nfsnode
		 * can be deferred until after we are sure that
		 * the nfsnode isn't busy.
		 */
		if ((np->n_flag & NLOCKED) != 0) {
			np->n_flag |= NWANT;
			sleep((caddr_t)np, PINOD);
			goto loop;
		}
		vp = NFSTOV(np);
		if (vp->v_count == 0)	/* nfsnode on free list */
			vget(vp);
		else
			VREF(vp);
		np->n_flag |= NLOCKED;
		*npp = np;
		return(0);
	}
	if (error = getnewvnode(VT_NFS, mntp, &nfsv2_vnodeops, &nvp)) {
		*npp = 0;
		return (error);
	}
	vp = nvp;
	np = VTONFS(vp);
	np->n_vnode = vp;
	/*
	 * Insert the nfsnode in the hash queue for its new file handle
	 */
	np->n_flag = NLOCKED;
	insque(np, nh);
	bcopy((caddr_t)fhp, (caddr_t)&np->n_fh, NFSX_FH);
	np->n_attrstamp = 0;
	np->n_sillyrename = (struct sillyrename *)0;
	np->n_size = 0;
	np->n_mtime = 0;
	/*
	 * Initialize the associated vnode
	 */
	*npp = np;
	return (0);
}

/*
 * Convert a pointer to an nfsnode into a reference to an nfsnode.
 *
 * This is basically the internal piece of nget (after the
 * nfsnode pointer is located) but without the test for mounted
 * filesystems.  It is caller's responsibility to check that
 * the nfsnode pointer is valid.
 */
nfs_ngrab(np)
	register struct nfsnode *np;
{
	register struct vnode *vp = NFSTOV(np);

	while ((np->n_flag & NLOCKED) != 0) {
		np->n_flag |= NWANT;
		sleep((caddr_t)np, PINOD);
	}
	if (vp->v_count == 0)		/* ino on free list */
		vget(vp);
	else
		VREF(vp);
	np->n_flag |= NLOCKED;
}

nfs_inactive(vp)
	struct vnode *vp;
{
	register struct nfsnode *np;
	register struct nameidata *ndp;
	register struct sillyrename *sp;
	struct nfsnode *dnp;

	if (vp == NULL || vp->v_count != 0)
		panic("nfs_inactive: NULL or active vp");
	np = VTONFS(vp);
	nfs_lock(vp);
	sp = np->n_sillyrename;
	np->n_sillyrename = (struct sillyrename *)0;
	if (sp) {
		/*
		 * Remove the silly file that was rename'd earlier
		 */
		ndp = &sp->s_namei;
		if (!nfs_nget(vp->v_mount, &sp->s_fh, &dnp)) {
			ndp->ni_dvp = NFSTOV(dnp);
			nfs_removeit(ndp);
			nfs_nput(ndp->ni_dvp);
		}
		crfree(ndp->ni_cred);
		free((caddr_t)sp, M_TEMP);
	}
	nfs_unlock(vp);
	np->n_flag &= NBUFFERED;
#ifdef notdef
	/*
	 * Scan the request list for any requests left hanging about
	 */
	s = splnet();
	rep = nfsreqh.r_next;
	while (rep && rep != &nfsreqh) {
		if (rep->r_vp == vp) {
			rep->r_prev->r_next = rep2 = rep->r_next;
			rep->r_next->r_prev = rep->r_prev;
			m_freem(rep->r_mreq);
			if (rep->r_mrep != NULL)
				m_freem(rep->r_mrep);
			free((caddr_t)rep, M_NFSREQ);
			rep = rep2;
		} else
			rep = rep->r_next;
	}
	splx(s);
#endif
	return (0);
}

/*
 * Reclaim an nfsnode so that it can be used for other purposes.
 */
nfs_reclaim(vp)
	register struct vnode *vp;
{
	register struct nfsnode *np = VTONFS(vp);

	if (vp->v_count != 0)
		panic("nfs_reclaim: active inode");
	/*
	 * Remove the nfsnode from its hash chain.
	 */
	remque(np);
	np->n_forw = np;
	np->n_back = np;
	cache_purge(vp);
	/*
	 * Flush out any associated bio buffers that might be lying about
	 */
	if (vp->v_type == VREG && (np->n_flag & NBUFFERED)) {
		np->n_flag |= NLOCKED;
		nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE);
	}
	vp->v_type = VNON;
	return (0);
}

/*
 * Remove any nfsnodes in the nfsnode cache belonging to mount.
 *
 * There should not be any active ones, return error if any are found
 * (nb: this is a user error, not a system err).
 */
nfs_nflush(mntp)
	struct mount *mntp;
{
	register struct vnode *vp;
	int busy = 0;

	for (vp = mntp->m_mounth; vp; vp = vp->v_mountf) {
		if (vp->v_count) {
			busy++;
			continue;
		}
		/*
		 * As v_count == 0, the nfsnode was on the free list already,
		 * so it will fall off the bottom eventually.
		 * We could perhaps move it to the head of the free list,
		 * but as umounts are done so infrequently, we would gain
		 * very little, while making the code bigger.
		 */
		nfs_reclaim(vp);
	}
	if (busy)
		return (EBUSY);
	return (0);
}

/*
 * Lock an nfsnode
 */
nfs_lock(vp)
	struct vnode *vp;
{
	register struct nfsnode *np = VTONFS(vp);

	while (np->n_flag & NLOCKED) {
		np->n_flag |= NWANT;
		sleep((caddr_t)np, PINOD);
	}
	np->n_flag |= NLOCKED;
}

/*
 * Unlock an nfsnode
 */
nfs_unlock(vp)
	struct vnode *vp;
{
	register struct nfsnode *np = VTONFS(vp);

	np->n_flag &= ~NLOCKED;
	if (np->n_flag & NWANT) {
		np->n_flag &= ~NWANT;
		wakeup((caddr_t)np);
	}
}

/*
 * Unlock and vrele()
 * since I can't decide if dirs. should be locked, I will check for
 * the lock and be flexible
 */
nfs_nput(vp)
	struct vnode *vp;
{
	register struct nfsnode *np = VTONFS(vp);

	if (np->n_flag & NLOCKED)
		nfs_unlock(vp);
	vrele(vp);
}

nfs_abortop(ndp)
	register struct nameidata *ndp;
{
	register struct nfsnode *np;

	if (ndp->ni_vp != NULL) {
		np = VTONFS(ndp->ni_vp);
		if (np->n_flag & NLOCKED)
			nfs_unlock(ndp->ni_vp);
		vrele(ndp->ni_vp);
	}
	if (ndp->ni_dvp != NULL) {
		np = VTONFS(ndp->ni_dvp);
		if (np->n_flag & NLOCKED)
			nfs_unlock(ndp->ni_dvp);
		vrele(ndp->ni_dvp);
	}
}

/*
 * This is silly, but if you use a macro and try and use it in a file
 * that has mbuf.h included, m_data --> m_hdr.mh_data and this is not
 * a good thing
 */
struct nfsmount *vfs_to_nfs(mp)
	struct mount *mp;
{
	return ((struct nfsmount *)mp->m_data);
}
