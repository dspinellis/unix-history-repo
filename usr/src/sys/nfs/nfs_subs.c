/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfs_subs.c	7.48 (Berkeley) %G%
 */

/*
 * These functions support the macros and help fiddle mbuf chains for
 * the nfs op functions. They do things like create the rpc header and
 * copy data between mbuf chains and uio lists.
 */
#include "param.h"
#include "proc.h"
#include "filedesc.h"
#include "systm.h"
#include "kernel.h"
#include "mount.h"
#include "file.h"
#include "vnode.h"
#include "namei.h"
#include "mbuf.h"
#include "map.h"
#include "socket.h"

#include "ufs/ufs/quota.h"
#include "ufs/ufs/inode.h"
#include "ufs/ufs/ufsmount.h"

#include "rpcv2.h"
#include "nfsv2.h"
#include "nfsnode.h"
#include "nfs.h"
#include "xdr_subs.h"
#include "nfsm_subs.h"
#include "nfsmount.h"
#include "nqnfs.h"
#include "nfsrtt.h"

#define TRUE	1
#define	FALSE	0

/*
 * Data items converted to xdr at startup, since they are constant
 * This is kinda hokey, but may save a little time doing byte swaps
 */
u_long nfs_procids[NFS_NPROCS];
u_long nfs_xdrneg1;
u_long rpc_call, rpc_vers, rpc_reply, rpc_msgdenied, rpc_autherr,
	rpc_mismatch, rpc_auth_unix, rpc_msgaccepted, rpc_rejectedcred,
	rpc_auth_kerb;
u_long nfs_vers, nfs_prog, nfs_true, nfs_false;

/* And other global data */
static u_long nfs_xid = 0;
enum vtype ntov_type[7] = { VNON, VREG, VDIR, VBLK, VCHR, VLNK, VNON };
extern struct proc *nfs_iodwant[NFS_MAXASYNCDAEMON];
extern struct nfsreq nfsreqh;
extern int nqnfs_piggy[NFS_NPROCS];
extern struct nfsrtt nfsrtt;
extern union nqsrvthead nqthead;
extern union nqsrvthead nqfhead[NQLCHSZ];
extern time_t nqnfsstarttime;
extern u_long nqnfs_prog, nqnfs_vers;
extern int nqsrv_clockskew;
extern int nqsrv_writeslack;
extern int nqsrv_maxlease;

/*
 * Create the header for an rpc request packet
 * The hsiz is the size of the rest of the nfs request header.
 * (just used to decide if a cluster is a good idea)
 */
struct mbuf *
nfsm_reqh(vp, procid, hsiz, bposp)
	struct vnode *vp;
	u_long procid;
	int hsiz;
	caddr_t *bposp;
{
	register struct mbuf *mb;
	register u_long *tl;
	register caddr_t bpos;
	struct mbuf *mb2;
	struct nfsmount *nmp;
	int nqflag;

	MGET(mb, M_WAIT, MT_DATA);
	if (hsiz >= MINCLSIZE)
		MCLGET(mb, M_WAIT);
	mb->m_len = 0;
	bpos = mtod(mb, caddr_t);
	
	/*
	 * For NQNFS, add lease request.
	 */
	if (vp) {
		nmp = VFSTONFS(vp->v_mount);
		if (nmp->nm_flag & NFSMNT_NQNFS) {
			nqflag = NQNFS_NEEDLEASE(vp, procid);
			if (nqflag) {
				nfsm_build(tl, u_long *, 2*NFSX_UNSIGNED);
				*tl++ = txdr_unsigned(nqflag);
				*tl = txdr_unsigned(nmp->nm_leaseterm);
			} else {
				nfsm_build(tl, u_long *, NFSX_UNSIGNED);
				*tl = 0;
			}
		}
	}
	/* Finally, return values */
	*bposp = bpos;
	return (mb);
}

/*
 * Build the RPC header and fill in the authorization info.
 * The authorization string argument is only used when the credentials
 * come from outside of the kernel.
 * Returns the head of the mbuf list.
 */
struct mbuf *
nfsm_rpchead(cr, nqnfs, procid, auth_type, auth_len, auth_str, mrest,
	mrest_len, mbp, xidp)
	register struct ucred *cr;
	int nqnfs;
	int procid;
	int auth_type;
	int auth_len;
	char *auth_str;
	struct mbuf *mrest;
	int mrest_len;
	struct mbuf **mbp;
	u_long *xidp;
{
	register struct mbuf *mb;
	register u_long *tl;
	register caddr_t bpos;
	register int i;
	struct mbuf *mreq, *mb2;
	int siz, grpsiz, authsiz;

	authsiz = nfsm_rndup(auth_len);
	if (auth_type == RPCAUTH_NQNFS)
		authsiz += 2 * NFSX_UNSIGNED;
	MGETHDR(mb, M_WAIT, MT_DATA);
	if ((authsiz + 10*NFSX_UNSIGNED) >= MINCLSIZE) {
		MCLGET(mb, M_WAIT);
	} else if ((authsiz + 10*NFSX_UNSIGNED) < MHLEN) {
		MH_ALIGN(mb, authsiz + 10*NFSX_UNSIGNED);
	} else {
		MH_ALIGN(mb, 8*NFSX_UNSIGNED);
	}
	mb->m_len = 0;
	mreq = mb;
	bpos = mtod(mb, caddr_t);

	/*
	 * First the RPC header.
	 */
	nfsm_build(tl, u_long *, 8*NFSX_UNSIGNED);
	if (++nfs_xid == 0)
		nfs_xid++;
	*tl++ = *xidp = txdr_unsigned(nfs_xid);
	*tl++ = rpc_call;
	*tl++ = rpc_vers;
	if (nqnfs) {
		*tl++ = txdr_unsigned(NQNFS_PROG);
		*tl++ = txdr_unsigned(NQNFS_VER1);
	} else {
		*tl++ = txdr_unsigned(NFS_PROG);
		*tl++ = txdr_unsigned(NFS_VER2);
	}
	*tl++ = txdr_unsigned(procid);

	/*
	 * And then the authorization cred.
	 */
	*tl++ = txdr_unsigned(auth_type);
	*tl = txdr_unsigned(authsiz);
	switch (auth_type) {
	case RPCAUTH_UNIX:
		nfsm_build(tl, u_long *, auth_len);
		*tl++ = 0;		/* stamp ?? */
		*tl++ = 0;		/* NULL hostname */
		*tl++ = txdr_unsigned(cr->cr_uid);
		*tl++ = txdr_unsigned(cr->cr_groups[0]);
		grpsiz = (auth_len >> 2) - 5;
		*tl++ = txdr_unsigned(grpsiz);
		for (i = 1; i <= grpsiz; i++)
			*tl++ = txdr_unsigned(cr->cr_groups[i]);
		break;
	case RPCAUTH_NQNFS:
		nfsm_build(tl, u_long *, 2*NFSX_UNSIGNED);
		*tl++ = txdr_unsigned(cr->cr_uid);
		*tl = txdr_unsigned(auth_len);
		siz = auth_len;
		while (siz > 0) {
			if (M_TRAILINGSPACE(mb) == 0) {
				MGET(mb2, M_WAIT, MT_DATA);
				if (siz >= MINCLSIZE)
					MCLGET(mb2, M_WAIT);
				mb->m_next = mb2;
				mb = mb2;
				mb->m_len = 0;
				bpos = mtod(mb, caddr_t);
			}
			i = MIN(siz, M_TRAILINGSPACE(mb));
			bcopy(auth_str, bpos, i);
			mb->m_len += i;
			auth_str += i;
			bpos += i;
			siz -= i;
		}
		if ((siz = nfsm_rndup(auth_len) - auth_len) > 0) {
			for (i = 0; i < siz; i++)
				*bpos++ = '\0';
			mb->m_len += siz;
		}
		break;
	};
	nfsm_build(tl, u_long *, 2*NFSX_UNSIGNED);
	*tl++ = txdr_unsigned(RPCAUTH_NULL);
	*tl = 0;
	mb->m_next = mrest;
	mreq->m_pkthdr.len = authsiz + 10*NFSX_UNSIGNED + mrest_len;
	mreq->m_pkthdr.rcvif = (struct ifnet *)0;
	*mbp = mb;
	return (mreq);
}

/*
 * copies mbuf chain to the uio scatter/gather list
 */
nfsm_mbuftouio(mrep, uiop, siz, dpos)
	struct mbuf **mrep;
	register struct uio *uiop;
	int siz;
	caddr_t *dpos;
{
	register char *mbufcp, *uiocp;
	register int xfer, left, len;
	register struct mbuf *mp;
	long uiosiz, rem;
	int error = 0;

	mp = *mrep;
	mbufcp = *dpos;
	len = mtod(mp, caddr_t)+mp->m_len-mbufcp;
	rem = nfsm_rndup(siz)-siz;
	while (siz > 0) {
		if (uiop->uio_iovcnt <= 0 || uiop->uio_iov == NULL)
			return (EFBIG);
		left = uiop->uio_iov->iov_len;
		uiocp = uiop->uio_iov->iov_base;
		if (left > siz)
			left = siz;
		uiosiz = left;
		while (left > 0) {
			while (len == 0) {
				mp = mp->m_next;
				if (mp == NULL)
					return (EBADRPC);
				mbufcp = mtod(mp, caddr_t);
				len = mp->m_len;
			}
			xfer = (left > len) ? len : left;
#ifdef notdef
			/* Not Yet.. */
			if (uiop->uio_iov->iov_op != NULL)
				(*(uiop->uio_iov->iov_op))
				(mbufcp, uiocp, xfer);
			else
#endif
			if (uiop->uio_segflg == UIO_SYSSPACE)
				bcopy(mbufcp, uiocp, xfer);
			else
				copyout(mbufcp, uiocp, xfer);
			left -= xfer;
			len -= xfer;
			mbufcp += xfer;
			uiocp += xfer;
			uiop->uio_offset += xfer;
			uiop->uio_resid -= xfer;
		}
		if (uiop->uio_iov->iov_len <= siz) {
			uiop->uio_iovcnt--;
			uiop->uio_iov++;
		} else {
			uiop->uio_iov->iov_base += uiosiz;
			uiop->uio_iov->iov_len -= uiosiz;
		}
		siz -= uiosiz;
	}
	*dpos = mbufcp;
	*mrep = mp;
	if (rem > 0) {
		if (len < rem)
			error = nfs_adv(mrep, dpos, rem, len);
		else
			*dpos += rem;
	}
	return (error);
}

/*
 * copies a uio scatter/gather list to an mbuf chain...
 */
nfsm_uiotombuf(uiop, mq, siz, bpos)
	register struct uio *uiop;
	struct mbuf **mq;
	int siz;
	caddr_t *bpos;
{
	register char *uiocp;
	register struct mbuf *mp, *mp2;
	register int xfer, left, mlen;
	int uiosiz, clflg, rem;
	char *cp;

	if (siz > MLEN)		/* or should it >= MCLBYTES ?? */
		clflg = 1;
	else
		clflg = 0;
	rem = nfsm_rndup(siz)-siz;
	mp = mp2 = *mq;
	while (siz > 0) {
		if (uiop->uio_iovcnt <= 0 || uiop->uio_iov == NULL)
			return (EINVAL);
		left = uiop->uio_iov->iov_len;
		uiocp = uiop->uio_iov->iov_base;
		if (left > siz)
			left = siz;
		uiosiz = left;
		while (left > 0) {
			mlen = M_TRAILINGSPACE(mp);
			if (mlen == 0) {
				MGET(mp, M_WAIT, MT_DATA);
				if (clflg)
					MCLGET(mp, M_WAIT);
				mp->m_len = 0;
				mp2->m_next = mp;
				mp2 = mp;
				mlen = M_TRAILINGSPACE(mp);
			}
			xfer = (left > mlen) ? mlen : left;
#ifdef notdef
			/* Not Yet.. */
			if (uiop->uio_iov->iov_op != NULL)
				(*(uiop->uio_iov->iov_op))
				(uiocp, mtod(mp, caddr_t)+mp->m_len, xfer);
			else
#endif
			if (uiop->uio_segflg == UIO_SYSSPACE)
				bcopy(uiocp, mtod(mp, caddr_t)+mp->m_len, xfer);
			else
				copyin(uiocp, mtod(mp, caddr_t)+mp->m_len, xfer);
			mp->m_len += xfer;
			left -= xfer;
			uiocp += xfer;
			uiop->uio_offset += xfer;
			uiop->uio_resid -= xfer;
		}
		if (uiop->uio_iov->iov_len <= siz) {
			uiop->uio_iovcnt--;
			uiop->uio_iov++;
		} else {
			uiop->uio_iov->iov_base += uiosiz;
			uiop->uio_iov->iov_len -= uiosiz;
		}
		siz -= uiosiz;
	}
	if (rem > 0) {
		if (rem > M_TRAILINGSPACE(mp)) {
			MGET(mp, M_WAIT, MT_DATA);
			mp->m_len = 0;
			mp2->m_next = mp;
		}
		cp = mtod(mp, caddr_t)+mp->m_len;
		for (left = 0; left < rem; left++)
			*cp++ = '\0';
		mp->m_len += rem;
		*bpos = cp;
	} else
		*bpos = mtod(mp, caddr_t)+mp->m_len;
	*mq = mp;
	return (0);
}

/*
 * Help break down an mbuf chain by setting the first siz bytes contiguous
 * pointed to by returned val.
 * If Updateflg == True we can overwrite the first part of the mbuf data
 * (in this case it can never sleep, so it can be called from interrupt level)
 * it may however block when Updateflg == False
 * This is used by the macros nfsm_dissect and nfsm_dissecton for tough
 * cases. (The macros use the vars. dpos and dpos2)
 */
nfsm_disct(mdp, dposp, siz, left, updateflg, cp2)
	struct mbuf **mdp;
	caddr_t *dposp;
	int siz;
	int left;
	int updateflg;
	caddr_t *cp2;
{
	register struct mbuf *mp, *mp2;
	register int siz2, xfer;
	register caddr_t p;

	mp = *mdp;
	while (left == 0) {
		*mdp = mp = mp->m_next;
		if (mp == NULL)
			return (EBADRPC);
		left = mp->m_len;
		*dposp = mtod(mp, caddr_t);
	}
	if (left >= siz) {
		*cp2 = *dposp;
		*dposp += siz;
	} else if (mp->m_next == NULL) {
		return (EBADRPC);
	} else if (siz > MHLEN) {
		panic("nfs S too big");
	} else {
		/* Iff update, you can overwrite, else must alloc new mbuf */
		if (updateflg) {
			NFSMINOFF(mp);
		} else {
			MGET(mp2, M_WAIT, MT_DATA);
			mp2->m_next = mp->m_next;
			mp->m_next = mp2;
			mp->m_len -= left;
			mp = mp2;
		}
		*cp2 = p = mtod(mp, caddr_t);
		bcopy(*dposp, p, left);		/* Copy what was left */
		siz2 = siz-left;
		p += left;
		mp2 = mp->m_next;
		/* Loop around copying up the siz2 bytes */
		while (siz2 > 0) {
			if (mp2 == NULL)
				return (EBADRPC);
			xfer = (siz2 > mp2->m_len) ? mp2->m_len : siz2;
			if (xfer > 0) {
				bcopy(mtod(mp2, caddr_t), p, xfer);
				NFSMADV(mp2, xfer);
				mp2->m_len -= xfer;
				p += xfer;
				siz2 -= xfer;
			}
			if (siz2 > 0)
				mp2 = mp2->m_next;
		}
		mp->m_len = siz;
		*mdp = mp2;
		*dposp = mtod(mp2, caddr_t);
	}
	return (0);
}

/*
 * Advance the position in the mbuf chain.
 */
nfs_adv(mdp, dposp, offs, left)
	struct mbuf **mdp;
	caddr_t *dposp;
	int offs;
	int left;
{
	register struct mbuf *m;
	register int s;

	m = *mdp;
	s = left;
	while (s < offs) {
		offs -= s;
		m = m->m_next;
		if (m == NULL)
			return (EBADRPC);
		s = m->m_len;
	}
	*mdp = m;
	*dposp = mtod(m, caddr_t)+offs;
	return (0);
}

/*
 * Copy a string into mbufs for the hard cases...
 */
nfsm_strtmbuf(mb, bpos, cp, siz)
	struct mbuf **mb;
	char **bpos;
	char *cp;
	long siz;
{
	register struct mbuf *m1, *m2;
	long left, xfer, len, tlen;
	u_long *tl;
	int putsize;

	putsize = 1;
	m2 = *mb;
	left = M_TRAILINGSPACE(m2);
	if (left > 0) {
		tl = ((u_long *)(*bpos));
		*tl++ = txdr_unsigned(siz);
		putsize = 0;
		left -= NFSX_UNSIGNED;
		m2->m_len += NFSX_UNSIGNED;
		if (left > 0) {
			bcopy(cp, (caddr_t) tl, left);
			siz -= left;
			cp += left;
			m2->m_len += left;
			left = 0;
		}
	}
	/* Loop around adding mbufs */
	while (siz > 0) {
		MGET(m1, M_WAIT, MT_DATA);
		if (siz > MLEN)
			MCLGET(m1, M_WAIT);
		m1->m_len = NFSMSIZ(m1);
		m2->m_next = m1;
		m2 = m1;
		tl = mtod(m1, u_long *);
		tlen = 0;
		if (putsize) {
			*tl++ = txdr_unsigned(siz);
			m1->m_len -= NFSX_UNSIGNED;
			tlen = NFSX_UNSIGNED;
			putsize = 0;
		}
		if (siz < m1->m_len) {
			len = nfsm_rndup(siz);
			xfer = siz;
			if (xfer < len)
				*(tl+(xfer>>2)) = 0;
		} else {
			xfer = len = m1->m_len;
		}
		bcopy(cp, (caddr_t) tl, xfer);
		m1->m_len = len+tlen;
		siz -= xfer;
		cp += xfer;
	}
	*mb = m1;
	*bpos = mtod(m1, caddr_t)+m1->m_len;
	return (0);
}

/*
 * Called once to initialize data structures...
 */
nfs_init()
{
	register int i;
	union nqsrvthead *lhp;

	nfsrtt.pos = 0;
	rpc_vers = txdr_unsigned(RPC_VER2);
	rpc_call = txdr_unsigned(RPC_CALL);
	rpc_reply = txdr_unsigned(RPC_REPLY);
	rpc_msgdenied = txdr_unsigned(RPC_MSGDENIED);
	rpc_msgaccepted = txdr_unsigned(RPC_MSGACCEPTED);
	rpc_mismatch = txdr_unsigned(RPC_MISMATCH);
	rpc_autherr = txdr_unsigned(RPC_AUTHERR);
	rpc_rejectedcred = txdr_unsigned(AUTH_REJECTCRED);
	rpc_auth_unix = txdr_unsigned(RPCAUTH_UNIX);
	rpc_auth_kerb = txdr_unsigned(RPCAUTH_NQNFS);
	nfs_vers = txdr_unsigned(NFS_VER2);
	nfs_prog = txdr_unsigned(NFS_PROG);
	nfs_true = txdr_unsigned(TRUE);
	nfs_false = txdr_unsigned(FALSE);
	/* Loop thru nfs procids */
	for (i = 0; i < NFS_NPROCS; i++)
		nfs_procids[i] = txdr_unsigned(i);
	/* Ensure async daemons disabled */
	for (i = 0; i < NFS_MAXASYNCDAEMON; i++)
		nfs_iodwant[i] = (struct proc *)0;
	nfs_xdrneg1 = txdr_unsigned(-1);
	nfs_nhinit();			/* Init the nfsnode table */
	nfsrv_init(0);			/* Init server data structures */
	nfsrv_initcache();		/* Init the server request cache */

	/*
	 * Initialize the nqnfs server stuff.
	 */
	if (nqnfsstarttime == 0) {
		nqnfsstarttime = boottime.tv_sec + nqsrv_maxlease
			+ nqsrv_clockskew + nqsrv_writeslack;
		NQLOADNOVRAM(nqnfsstarttime);
		nqnfs_prog = txdr_unsigned(NQNFS_PROG);
		nqnfs_vers = txdr_unsigned(NQNFS_VER1);
		nqthead.th_head[0] = &nqthead;
		nqthead.th_head[1] = &nqthead;
		for (i = 0; i < NQLCHSZ; i++) {
			lhp = &nqfhead[i];
			lhp->th_head[0] = lhp;
			lhp->th_head[1] = lhp;
		}
	}

	/*
	 * Initialize reply list and start timer
	 */
	nfsreqh.r_prev = nfsreqh.r_next = &nfsreqh;
	nfs_timer();
}

/*
 * Attribute cache routines.
 * nfs_loadattrcache() - loads or updates the cache contents from attributes
 *	that are on the mbuf list
 * nfs_getattrcache() - returns valid attributes if found in cache, returns
 *	error otherwise
 */

/*
 * Load the attribute cache (that lives in the nfsnode entry) with
 * the values on the mbuf list and
 * Iff vap not NULL
 *    copy the attributes to *vaper
 */
nfs_loadattrcache(vpp, mdp, dposp, vaper)
	struct vnode **vpp;
	struct mbuf **mdp;
	caddr_t *dposp;
	struct vattr *vaper;
{
	register struct vnode *vp = *vpp;
	register struct vattr *vap;
	register struct nfsv2_fattr *fp;
	extern struct vnodeops spec_nfsv2nodeops, spec_vnodeops;
	register struct nfsnode *np;
	register long t1;
	caddr_t dpos, cp2;
	int error = 0;
	struct mbuf *md;
	enum vtype vtyp;
	u_short vmode;
	long rdev;
	struct timeval mtime;
	struct vnode *nvp;

	md = *mdp;
	dpos = *dposp;
	t1 = (mtod(md, caddr_t) + md->m_len) - dpos;
	if (error = nfsm_disct(&md, &dpos, NFSX_FATTR, t1, TRUE, &cp2))
		return (error);
	fp = (struct nfsv2_fattr *)cp2;
	vtyp = nfstov_type(fp->fa_type);
	vmode = fxdr_unsigned(u_short, fp->fa_mode);
	if (vtyp == VNON)
		vtyp = IFTOVT(vmode);
	rdev = fxdr_unsigned(long, fp->fa_rdev);
	fxdr_time(&fp->fa_mtime, &mtime);
	/*
	 * If v_type == VNON it is a new node, so fill in the v_type,
	 * n_mtime fields. Check to see if it represents a special 
	 * device, and if so, check for a possible alias. Once the
	 * correct vnode has been obtained, fill in the rest of the
	 * information.
	 */
	np = VTONFS(vp);
	if (vp->v_type == VNON) {
		if (vtyp == VCHR && rdev == 0xffffffff)
			vp->v_type = vtyp = VFIFO;
		else
			vp->v_type = vtyp;
		if (vp->v_type == VFIFO) {
#ifdef FIFO
			extern struct vnodeops fifo_nfsv2nodeops;
			vp->v_op = &fifo_nfsv2nodeops;
#else
			return (EOPNOTSUPP);
#endif /* FIFO */
		}
		if (vp->v_type == VCHR || vp->v_type == VBLK) {
			vp->v_op = &spec_nfsv2nodeops;
			if (nvp = checkalias(vp, (dev_t)rdev, vp->v_mount)) {
				/*
				 * Discard unneeded vnode, but save its nfsnode.
				 */
				remque(np);
				nvp->v_data = vp->v_data;
				vp->v_data = NULL;
				vp->v_op = &spec_vnodeops;
				vrele(vp);
				vgone(vp);
				/*
				 * Reinitialize aliased node.
				 */
				np->n_vnode = nvp;
				insque(np, nfs_hash(&np->n_fh));
				*vpp = vp = nvp;
			}
		}
		if ((VFSTONFS(vp->v_mount)->nm_flag & NFSMNT_NQNFS) == 0)
			np->n_mtime = mtime.tv_sec;
	}
	vap = &np->n_vattr;
	vap->va_type = vtyp;
	vap->va_mode = (vmode & 07777);
	vap->va_nlink = fxdr_unsigned(u_short, fp->fa_nlink);
	vap->va_uid = fxdr_unsigned(uid_t, fp->fa_uid);
	vap->va_gid = fxdr_unsigned(gid_t, fp->fa_gid);
	vap->va_size = fxdr_unsigned(u_long, fp->fa_size);
	if ((np->n_flag & NMODIFIED) == 0 || vap->va_size > np->n_size) {
		np->n_size = vap->va_size;
		vnode_pager_setsize(vp, np->n_size);
	}
	vap->va_blocksize = fxdr_unsigned(long, fp->fa_blocksize);
	vap->va_rdev = (dev_t)rdev;
	vap->va_bytes = fxdr_unsigned(long, fp->fa_blocks) * NFS_FABLKSIZE;
	vap->va_fsid = vp->v_mount->mnt_stat.f_fsid.val[0];
	vap->va_fileid = fxdr_unsigned(long, fp->fa_fileid);
	vap->va_atime.tv_sec = fxdr_unsigned(long, fp->fa_atime.tv_sec);
	vap->va_atime.tv_usec = 0;
	vap->va_flags = fxdr_unsigned(u_long, fp->fa_atime.tv_usec);
	vap->va_mtime = mtime;
	vap->va_ctime.tv_sec = fxdr_unsigned(long, fp->fa_ctime.tv_sec);
	vap->va_ctime.tv_usec = 0;
	vap->va_gen = fxdr_unsigned(u_long, fp->fa_ctime.tv_usec);
#ifdef _NOQUAD
	vap->va_size_rsv = 0;
	vap->va_bytes_rsv = 0;
#endif
	np->n_attrstamp = time.tv_sec;
	*dposp = dpos;
	*mdp = md;
	if (vaper != NULL) {
		bcopy((caddr_t)vap, (caddr_t)vaper, sizeof(*vap));
		if ((np->n_flag & NMODIFIED) && (np->n_size > vap->va_size))
			vaper->va_size = np->n_size;
	}
	return (0);
}

/*
 * Check the time stamp
 * If the cache is valid, copy contents to *vap and return 0
 * otherwise return an error
 */
nfs_getattrcache(vp, vap)
	register struct vnode *vp;
	struct vattr *vap;
{
	register struct nfsnode *np;

	np = VTONFS(vp);
	if (VFSTONFS(vp->v_mount)->nm_flag & NFSMNT_NQNFS) {
		if (!NQNFS_CKCACHABLE(vp, NQL_READ) || np->n_attrstamp == 0) {
			nfsstats.attrcache_misses++;
			return (ENOENT);
		}
	} else if ((time.tv_sec - np->n_attrstamp) >= NFS_ATTRTIMEO) {
		nfsstats.attrcache_misses++;
		return (ENOENT);
	}
	nfsstats.attrcache_hits++;
	bcopy((caddr_t)&np->n_vattr,(caddr_t)vap,sizeof(struct vattr));
	if ((np->n_flag & NMODIFIED) == 0) {
		np->n_size = vap->va_size;
		vnode_pager_setsize(vp, np->n_size);
	} else if (np->n_size > vap->va_size)
		vap->va_size = np->n_size;
	return (0);
}

/*
 * Set up nameidata for a lookup() call and do it
 */
nfs_namei(ndp, fhp, len, slp, nam, mdp, dposp, p)
	register struct nameidata *ndp;
	fhandle_t *fhp;
	int len;
	struct nfssvc_sock *slp;
	struct mbuf *nam;
	struct mbuf **mdp;
	caddr_t *dposp;
	struct proc *p;
{
	register int i, rem;
	register struct mbuf *md;
	register char *fromcp, *tocp;
	struct vnode *dp;
	int error, rdonly;
	struct componentname *cnp = &ndp->ni_cnd;

	MALLOC(cnp->cn_pnbuf, char *, len + 1, M_NAMEI, M_WAITOK);
	/*
	 * Copy the name from the mbuf list to ndp->ni_pnbuf
	 * and set the various ndp fields appropriately.
	 */
	fromcp = *dposp;
	tocp = cnp->cn_pnbuf;
	md = *mdp;
	rem = mtod(md, caddr_t) + md->m_len - fromcp;
	cnp->cn_hash = 0;
	for (i = 0; i < len; i++) {
		while (rem == 0) {
			md = md->m_next;
			if (md == NULL) {
				error = EBADRPC;
				goto out;
			}
			fromcp = mtod(md, caddr_t);
			rem = md->m_len;
		}
		if (*fromcp == '\0' || *fromcp == '/') {
			error = EINVAL;
			goto out;
		}
		if (*fromcp & 0200)
			if ((*fromcp&0377) == ('/'|0200) || cnp->cn_nameiop != DELETE) {
				error = EINVAL;
				goto out;
			}
		cnp->cn_hash += (unsigned char)*fromcp;
		*tocp++ = *fromcp++;
		rem--;
	}
	*tocp = '\0';
	*mdp = md;
	*dposp = fromcp;
	len = nfsm_rndup(len)-len;
	if (len > 0) {
		if (rem >= len)
			*dposp += len;
		else if (error = nfs_adv(mdp, dposp, len, rem))
			goto out;
	}
	ndp->ni_pathlen = tocp - cnp->cn_pnbuf;
	cnp->cn_nameptr = cnp->cn_pnbuf;
	/*
	 * Extract and set starting directory.
	 */
	if (error = nfsrv_fhtovp(fhp, FALSE, &dp, ndp->ni_cnd.cn_cred, slp,
	    nam, &rdonly))
		goto out;
	if (dp->v_type != VDIR) {
		vrele(dp);
		error = ENOTDIR;
		goto out;
	}
	ndp->ni_startdir = dp;
	if (rdonly)
		cnp->cn_flags |= (NOCROSSMOUNT | RDONLY);
	else
		cnp->cn_flags |= NOCROSSMOUNT;
	/*
	 * And call lookup() to do the real work
	 */
	cnp->cn_proc = p;
	if (error = lookup(ndp))
		goto out;
	/*
	 * Check for encountering a symbolic link
	 */
	if (cnp->cn_flags & ISSYMLINK) {
		if ((cnp->cn_flags & LOCKPARENT) && ndp->ni_pathlen == 1)
			vput(ndp->ni_dvp);
		else
			vrele(ndp->ni_dvp);
		vput(ndp->ni_vp);
		ndp->ni_vp = NULL;
		error = EINVAL;
		goto out;
	}
	/*
	 * Check for saved name request
	 */
	if (cnp->cn_flags & (SAVENAME | SAVESTART)) {
		cnp->cn_flags |= HASBUF;
		return (0);
	}
out:
	FREE(cnp->cn_pnbuf, M_NAMEI);
	return (error);
}

/*
 * A fiddled version of m_adj() that ensures null fill to a long
 * boundary and only trims off the back end
 */
void
nfsm_adj(mp, len, nul)
	struct mbuf *mp;
	register int len;
	int nul;
{
	register struct mbuf *m;
	register int count, i;
	register char *cp;

	/*
	 * Trim from tail.  Scan the mbuf chain,
	 * calculating its length and finding the last mbuf.
	 * If the adjustment only affects this mbuf, then just
	 * adjust and return.  Otherwise, rescan and truncate
	 * after the remaining size.
	 */
	count = 0;
	m = mp;
	for (;;) {
		count += m->m_len;
		if (m->m_next == (struct mbuf *)0)
			break;
		m = m->m_next;
	}
	if (m->m_len > len) {
		m->m_len -= len;
		if (nul > 0) {
			cp = mtod(m, caddr_t)+m->m_len-nul;
			for (i = 0; i < nul; i++)
				*cp++ = '\0';
		}
		return;
	}
	count -= len;
	if (count < 0)
		count = 0;
	/*
	 * Correct length for chain is "count".
	 * Find the mbuf with last data, adjust its length,
	 * and toss data from remaining mbufs on chain.
	 */
	for (m = mp; m; m = m->m_next) {
		if (m->m_len >= count) {
			m->m_len = count;
			if (nul > 0) {
				cp = mtod(m, caddr_t)+m->m_len-nul;
				for (i = 0; i < nul; i++)
					*cp++ = '\0';
			}
			break;
		}
		count -= m->m_len;
	}
	while (m = m->m_next)
		m->m_len = 0;
}

/*
 * nfsrv_fhtovp() - convert a fh to a vnode ptr (optionally locked)
 * 	- look up fsid in mount list (if not found ret error)
 *	- check that it is exported
 *	- get vp by calling VFS_FHTOVP() macro
 *	- if not lockflag unlock it with VOP_UNLOCK()
 *	- if cred->cr_uid == 0 or MNT_EXPORTANON set it to neth_anon
 */
nfsrv_fhtovp(fhp, lockflag, vpp, cred, slp, nam, rdonlyp)
	fhandle_t *fhp;
	int lockflag;
	struct vnode **vpp;
	struct ucred *cred;
	struct nfssvc_sock *slp;
	struct mbuf *nam;
	int *rdonlyp;
{
	register struct mount *mp;
	register struct netaddrhash *np;
	register struct ufsmount *ump;
	register struct nfsuid *uidp;
	struct sockaddr *saddr;
	int error;

	*vpp = (struct vnode *)0;
	if ((mp = getvfs(&fhp->fh_fsid)) == NULL)
		return (ESTALE);
	if ((mp->mnt_flag & MNT_EXPORTED) == 0)
		return (EACCES);

	/*
	 * Get the export permission structure for this <mp, client> tuple.
	 */
	ump = VFSTOUFS(mp);
	if (nam) {

		/*
		 * First search for a network match.
		 */
		np = ump->um_netaddr[NETMASK_HASH];
		while (np) {
		    if (nfs_netaddr_match(np->neth_family, &np->neth_haddr,
			&np->neth_hmask, nam))
			break;
			np = np->neth_next;
		}

		/*
		 * If not found, try for an address match.
		 */
		if (np == (struct netaddrhash *)0) {
		    saddr = mtod(nam, struct sockaddr *);
		    np = ump->um_netaddr[NETADDRHASH(saddr)];
		    while (np) {
			if (nfs_netaddr_match(np->neth_family, &np->neth_haddr,
			    (struct netaddrhash *)0, nam))
			    break;
			np = np->neth_next;
		    }
		}
	} else
		np = (struct netaddrhash *)0;
	if (np == (struct netaddrhash *)0) {

		/*
		 * If no address match, use the default if it exists.
		 */
		if ((mp->mnt_flag & MNT_DEFEXPORTED) == 0)
			return (EACCES);
		np = &ump->um_defexported;
	}

	/*
	 * Check/setup credentials.
	 */
	if (np->neth_exflags & MNT_EXKERB) {
		uidp = slp->ns_uidh[NUIDHASH(cred->cr_uid)];
		while (uidp) {
			if (uidp->nu_uid == cred->cr_uid)
				break;
			uidp = uidp->nu_hnext;
		}
		if (uidp) {
			if (cred->cr_ref != 1)
				panic("nsrv fhtovp");
			*cred = uidp->nu_cr;
		} else
			return (NQNFS_AUTHERR);
	} else if (cred->cr_uid == 0 || (np->neth_exflags & MNT_EXPORTANON))
		*cred = np->neth_anon;
	if (error = VFS_FHTOVP(mp, &fhp->fh_fid, 0, vpp))
		return (ESTALE);
	if (np->neth_exflags & MNT_EXRDONLY)
		*rdonlyp = 1;
	else
		*rdonlyp = 0;
	if (!lockflag)
		VOP_UNLOCK(*vpp);
	return (0);
}
