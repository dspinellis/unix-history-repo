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
 *	@(#)nfs_node.c	7.5 (Berkeley) %G%
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

union nhead {				/* inode LRU cache, Chris Maltby */
	union  nhead *nh_head[2];
	struct nfsnode *nh_chain[2];
} nhead[NFSNOHSZ];

struct nfsnode *nfreeh, **nfreet;

#define TRUE	1
#define	FALSE	0

/*
 * Initialize hash links for nfsnodes
 * and build nfsnode free list.
 */
nfs_nhinit()
{
	register int i;
	register struct nfsnode *np = nfsnode;
	register union  nhead *nh = nhead;

	for (i = NFSNOHSZ; --i >= 0; nh++) {
		nh->nh_head[0] = nh;
		nh->nh_head[1] = nh;
	}
	nfreeh = np;
	nfreet = &np->n_freef;
	np->n_freeb = &nfreeh;
	np->n_forw = np;
	np->n_back = np;
	NFSTOV(np)->v_data = (qaddr_t)np;
	NFSTOV(np)->v_type = VNON;
	for (i = nnfsnode; --i > 0; ) {
		++np;
		np->n_forw = np;
		np->n_back = np;
		NFSTOV(np)->v_data = (qaddr_t)np;
		NFSTOV(np)->v_type = VNON;
		*nfreet = np;
		np->n_freeb = nfreet;
		nfreet = &np->n_freef;
	}
	np->n_freef = NULL;
}

/*
 * Look up an vnode/nfsnode by file handle.
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
	register struct nfsnode *nq;
	register u_char *fhpp;
	register u_long fhsum;
	register int i;
	union  nhead *nh;
	int error;

	fhpp = &fhp->fh_bytes[0];
	fhsum = 0;
	for (i = 0; i < NFSX_FH; i++)
		fhsum += *fhpp++;
loop:
	nh = &nhead[NFSNOHASH(fhsum)];
	for (np = nh->nh_chain[0]; np != (struct nfsnode *)nh; np = np->n_forw) {
		if (mntp == NFSTOV(np)->v_mount &&
		    !bcmp((caddr_t)fhp, (caddr_t)&np->n_fh, NFSX_FH)) {
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
			if (vp->v_count == 0) {		/* nfsno on free list */
				if (nq = np->n_freef)
					nq->n_freeb = np->n_freeb;
				else
					nfreet = np->n_freeb;
				*np->n_freeb = nq;
				np->n_freef = NULL;
				np->n_freeb = NULL;
			}
			np->n_flag |= NLOCKED;
			VREF(vp);
			*npp = np;
			return(0);
		}

	}
	if ((np = nfreeh) == NULL) {
		tablefull("nfsnode");
		*npp = 0;
		return(ENFILE);
	}
	vp = NFSTOV(np);
	if (vp->v_count)
		panic("free nfsnode isn't");
	if (nq = np->n_freef)
		nq->n_freeb = &nfreeh;
	nfreeh = nq;
	np->n_freef = NULL;
	np->n_freeb = NULL;
	/*
	 * Now to take nfsnode off the hash chain it was on
	 * (initially, or after an nflush, it is on a "hash chain"
	 * consisting entirely of itself, and pointed to by no-one,
	 * but that doesn't matter)
	 */
	remque(np);
	/*
	 * Flush out any associated bio buffers that might be lying about
	 */
	if (vp->v_type == VREG && (np->n_flag & NBUFFERED)) {
		np->n_flag |= NLOCKED;
		nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE);
	}
	/*
	 * Insert the nfsnode in the hash queue for its new file handle
	 */
	np->n_flag = NLOCKED;
	insque(np, nh);
	bcopy((caddr_t)fhp, (caddr_t)&np->n_fh, NFSX_FH);
#ifndef notyet
	cache_purge(vp);
#endif
	np->n_attrstamp = 0;
	np->n_sillyrename = (struct sillyrename *)0;
	np->n_id = ++nextnfsnodeid;
	np->n_size = 0;
	np->n_mtime = 0;
	/*
	 * Initialize the associated vnode
	 */
	vinit(vp, mntp, VNON, &nfsv2_vnodeops);
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
	if (vp->v_count == 0) {		/* ino on free list */
		register struct nfsnode *nq;

		if (nq = np->n_freef)
			nq->n_freeb = np->n_freeb;
		else
			nfreet = np->n_freeb;
		*np->n_freeb = nq;
		np->n_freef = NULL;
		np->n_freeb = NULL;
	}
	VREF(vp);
	np->n_flag |= NLOCKED;
}

nfs_inactive(vp)
	struct vnode *vp;
{
	register struct nfsnode *np;
	register struct nameidata *ndp;
	register struct sillyrename *sp;
	register struct nfsreq *rep;
	struct nfsreq *rep2;
	struct nfsnode *dnp;
	int s;

	if (vp == NULL)
		panic("nfs_inactive NULL vp");
	if (vp->v_count == 0) {
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
		np->n_flag = 0;
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
		/*
		 * Put the nfsnode on the end of the free list.
		 */
		if (nfreeh) {
			*nfreet = np;
			np->n_freeb = nfreet;
		} else {
			nfreeh = np;
			np->n_freeb = &nfreeh;
		}
		np->n_freef = NULL;
		nfreet = &np->n_freef;
	}
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
	register struct nfsnode *np;
	register struct vnode *vp;

	for (np = nfsnode; np < nfsnodeNNFSNODE; np++) {
		vp = NFSTOV(np);
		if (vp->v_mount == mntp)
			if (vp->v_count)
				return (EBUSY);
			else {
				remque(np);
				np->n_forw = np;
				np->n_back = np;
				/*
				 * as v_count == 0, the inode was on the free
				 * list already, just leave it there, it will
				 * fall off the bottom eventually. We could
				 * perhaps move it to the head of the free
				 * list, but as umounts are done so
				 * infrequently, we would gain very little,
				 * while making the code bigger.
				 */
			}
	}
	return (0);
}

/*
 * Lock an nfsnode
 */
nfs_lock(vp)
	struct vnode *vp;
{
	register struct nfsnode *np = VTONFS(vp);

	if (np->n_flag & NLOCKED)
		printf("pid %d hit locked nfsnode=0x%x\n",
		    u.u_procp->p_pid, np);
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

	if ((np->n_flag & NLOCKED) == 0) {
		printf("pid %d unlocking unlocked nfsnode=0x%x ",
		    u.u_procp->p_pid, np);
		printf("fh0=0x%x fh1=0x%x fh2=0x%x fh3=0x%x fh4=0x%x fh5=0x%x fh6=0x%x fh7=0x%x\n",
			np->n_fh.fh_bytes[0],np->n_fh.fh_bytes[1],
			np->n_fh.fh_bytes[2],np->n_fh.fh_bytes[3],
			np->n_fh.fh_bytes[4],np->n_fh.fh_bytes[5],
			np->n_fh.fh_bytes[6],np->n_fh.fh_bytes[7]);
	}
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
