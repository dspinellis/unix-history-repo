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
 *	@(#)nfs_socket.c	7.3 (Berkeley) %G%
 */

/*
 * Socket operations for use by nfs (similar to uipc_socket.c, but never
 * with copies to/from a uio vector)
 * NB: For now, they only work for UDP datagram sockets.
 * (Use on stream sockets would require some record boundary mark in the
 *  stream such as Sun's RM (Section 3.2 of the Sun RPC Message Protocol
 *  manual, in Networking on the Sun Workstation, Part #800-1324-03
 *  and different versions of send, receive and reply that do not assume
 *  an atomic protocol
 */

#include "types.h"
#include "param.h"
#include "uio.h"
#include "user.h"
#include "mount.h"
#include "kernel.h"
#include "malloc.h"
#include "mbuf.h"
#include "vnode.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "netinet/in.h"
#include "rpcv2.h"
#include "nfsv2.h"
#include "nfs.h"
#include "xdr_subs.h"
#include "nfsm_subs.h"
#include "nfsmount.h"

#define	TRUE	1

/* set lock on sockbuf sb, sleep at neg prio */
#define nfs_sblock(sb) { \
	while ((sb)->sb_flags & SB_LOCK) { \
		(sb)->sb_flags |= SB_WANT; \
		sleep((caddr_t)&(sb)->sb_flags, PZERO-1); \
	} \
	(sb)->sb_flags |= SB_LOCK; \
}

/*
 * External data, mostly RPC constants in XDR form
 */
extern u_long rpc_reply, rpc_msgdenied, rpc_mismatch, rpc_vers, rpc_auth_unix,
	rpc_msgaccepted, rpc_call;
extern u_long nfs_prog, nfs_vers;
int	nfsrv_null(),
	nfsrv_getattr(),
	nfsrv_setattr(),
	nfsrv_lookup(),
	nfsrv_readlink(),
	nfsrv_read(),
	nfsrv_write(),
	nfsrv_create(),
	nfsrv_remove(),
	nfsrv_rename(),
	nfsrv_link(),
	nfsrv_symlink(),
	nfsrv_mkdir(),
	nfsrv_rmdir(),
	nfsrv_readdir(),
	nfsrv_statfs(),
	nfsrv_noop();

int (*nfsrv_procs[NFS_NPROCS])() = {
	nfsrv_null,
	nfsrv_getattr,
	nfsrv_setattr,
	nfsrv_noop,
	nfsrv_lookup,
	nfsrv_readlink,
	nfsrv_read,
	nfsrv_noop,
	nfsrv_write,
	nfsrv_create,
	nfsrv_remove,
	nfsrv_rename,
	nfsrv_link,
	nfsrv_symlink,
	nfsrv_mkdir,
	nfsrv_rmdir,
	nfsrv_readdir,
	nfsrv_statfs,
};


/*
 * This is a stripped down version of sosend() specific to
 * udp/ip and uses the mbuf list provdied
 */
nfs_udpsend(so, nam, top, flags, siz)
	register struct socket *so;
	struct mbuf *nam;
	struct mbuf *top;
	int flags;
	int siz;
{
	register int space;
	int error = 0, s, dontroute, first = 1;

	dontroute =
	    (flags & MSG_DONTROUTE) && (so->so_options & SO_DONTROUTE) == 0 &&
	    (so->so_proto->pr_flags & PR_ATOMIC);
#define	snderr(errno)	{ error = errno; splx(s); goto release; }

#ifdef MGETHDR
	top->m_pkthdr.len = siz;
#endif
restart:
	nfs_sblock(&so->so_snd);
	s = splnet();
	if (so->so_state & SS_CANTSENDMORE)
		snderr(EPIPE);
	if (so->so_error)
		snderr(so->so_error);
	space = sbspace(&so->so_snd);
	if (space < siz) {
		sbunlock(&so->so_snd);
		nfs_sbwait(&so->so_snd);
		splx(s);
		goto restart;
	}
	splx(s);
	if (dontroute)
		so->so_options |= SO_DONTROUTE;
	s = splnet();					/* XXX */
	error = (*so->so_proto->pr_usrreq)(so,
	    PRU_SEND,
	    top, (caddr_t)nam, (struct mbuf *)0, (struct mbuf *)0);
	splx(s);
	if (dontroute)
		so->so_options &= ~SO_DONTROUTE;
	top = (struct mbuf *)0;

release:
	sbunlock(&so->so_snd);
	if (top)
		m_freem(top);
	return (error);
}

/*
 * This is a stripped down udp specific version of soreceive()
 */
nfs_udpreceive(so, aname, mp)
	register struct socket *so;
	struct mbuf **aname;
	struct mbuf **mp;
{
	register struct mbuf *m;
	int s, error = 0;
	struct protosw *pr = so->so_proto;
	struct mbuf *nextrecord;

	if (aname)
		*aname = 0;

restart:
	sblock(&so->so_rcv);
	s = splnet();

	if (so->so_rcv.sb_cc == 0) {
		if (so->so_error) {
			error = so->so_error;
			so->so_error = 0;
			goto release;
		}
		if (so->so_state & SS_CANTRCVMORE)
			goto release;
		sbunlock(&so->so_rcv);
		sbwait(&so->so_rcv);
		splx(s);
		goto restart;
	}
	m = so->so_rcv.sb_mb;
	if (m == 0)
		panic("nfs_receive 1");
	nextrecord = m->m_nextpkt;
	if (m->m_type != MT_SONAME)
		panic("nfs_receive 1a");
	sbfree(&so->so_rcv, m);
	if (aname) {
		*aname = m;
		so->so_rcv.sb_mb = m->m_next;
		m->m_next = 0;
		m = so->so_rcv.sb_mb;
	} else {
		MFREE(m, so->so_rcv.sb_mb);
		m = so->so_rcv.sb_mb;
	}
	if (m && m->m_type == MT_RIGHTS)
		panic("nfs_receive 2");
	if (m && m->m_type == MT_CONTROL) {
		sbfree(&so->so_rcv, m);
		MFREE(m, so->so_rcv.sb_mb);
		m = so->so_rcv.sb_mb;
	}
	*mp = m;
	while (m) {
		if (m->m_type != MT_DATA && m->m_type != MT_HEADER)
			panic("nfs_receive 3");
		sbfree(&so->so_rcv, m);
		m = so->so_rcv.sb_mb = m->m_next;
	}
	so->so_rcv.sb_mb = nextrecord;
	so->so_state &= ~SS_RCVATMARK;	/* Necessary ?? */
release:
	sbunlock(&so->so_rcv);
	splx(s);
	return (error);
}

struct nfsreq nfsreqh = {
	(struct nfsreq *)0,
	(struct nfsreq *)0,
	(struct mbuf *)0,
	(struct mbuf *)0,
	(struct nfsmount *)0,
	0, 0, 0, 0, 0,
};

struct rpc_replyhead {
	u_long	r_xid;
	u_long	r_rep;
};

/*
 * Implement receipt of reply on a socket.
 * We depend on the way that records are added to the sockbuf
 * by sbappend*.  In particular, each record (mbufs linked through m_next)
 * must begin with an address, followed by optional MT_CONTROL mbuf
 * and then zero or more mbufs of data.
 * Although the sockbuf is locked, new data may still be appended,
 * and thus we must maintain consistency of the sockbuf during that time.
 * We must search through the list of received datagrams matching them
 * with outstanding requests using the xid, until ours is found.
 */
nfs_udpreply(so, mntp, myrep)
	register struct socket *so;
	struct nfsmount *mntp;
	struct nfsreq *myrep;
{
	register struct mbuf *m;
	register struct nfsreq *rep;
	register int error = 0, s;
	struct protosw *pr = so->so_proto;
	struct mbuf *nextrecord;
	struct sockaddr_in *saddr;
	u_long inaddr;
	struct rpc_replyhead replyh;
	struct mbuf *mp;
	char *cp;
	int cnt, xfer;
	int found;

restart:
	nfs_sblock(&so->so_rcv);
	/* Already received, bye bye */
	if (myrep->r_mrep != NULL) {
		sbunlock(&so->so_rcv);
		return (0);
	}
	/* If a soft mount and we have run out of retries */
	if (myrep->r_retry == 0 && myrep->r_timer == 0) {
		sbunlock(&so->so_rcv);
		return (ETIMEDOUT);
	}
	s = splnet();

	m = so->so_rcv.sb_mb;
	if (m == 0) {
		if (so->so_rcv.sb_cc)
			panic("nfs_soreply 1");
		if (so->so_error) {
			error = so->so_error;
			so->so_error = 0;
			goto release;
		}
		if (so->so_state & SS_CANTRCVMORE)
			goto release;
		sbunlock(&so->so_rcv);
		nfs_sbwait(&so->so_rcv);
		splx(s);
		goto restart;
	}
	nextrecord = m->m_nextpkt;

	/*
	 * Take off the address, check for rights and ditch any control
	 * mbufs.
	 */
	if (m->m_type != MT_SONAME)
		panic("nfs reply SONAME");
	saddr = mtod(m, struct sockaddr_in *);
	inaddr = saddr->sin_addr.s_addr;
	sbfree(&so->so_rcv, m);
	MFREE(m, so->so_rcv.sb_mb);
	m = so->so_rcv.sb_mb;
	if (m && m->m_type == MT_RIGHTS)
		panic("nfs reply RIGHTS");
	if (m && m->m_type == MT_CONTROL) {
		sbfree(&so->so_rcv, m);
		MFREE(m, so->so_rcv.sb_mb);
		m = so->so_rcv.sb_mb;
	}
	if (m) {
		m->m_nextpkt = nextrecord;
	} else {
		so->so_rcv.sb_mb = nextrecord;
		sbunlock(&so->so_rcv);
		splx(s);
		goto restart;
	}

	/*
	 * Get the xid and check that it is an rpc reply
	 */
	mp = m;
	if (m->m_len >= 2*NFSX_UNSIGNED)
		bcopy(mtod(m, caddr_t), (caddr_t)&replyh, 2*NFSX_UNSIGNED);
	else {
		cnt = 2*NFSX_UNSIGNED;
		cp = (caddr_t)&replyh;
		while (mp && cnt > 0) {
			if (mp->m_len > 0) {
				xfer = (mp->m_len >= cnt) ? cnt : mp->m_len;
				bcopy(mtod(mp, caddr_t), cp, xfer);
				cnt -= xfer;
				cp += xfer;
			}
			if (cnt > 0)
				mp = mp->m_next;
		}
	}
	found = 0;
	if (replyh.r_rep != rpc_reply || mp == NULL)
		goto dropit;
	/*
	 * Loop through the request list to match up the reply
	 * Iff no match, just drop the datagram
	 */
	rep = nfsreqh.r_next;
	while (!found && rep != &nfsreqh) {
		if (rep->r_mrep == NULL && replyh.r_xid == rep->r_xid &&
		    inaddr == rep->r_inaddr) {
			/* Found it.. */
			rep->r_mrep = m;
			while (m) {
				if (m->m_type != MT_DATA && m->m_type != MT_HEADER)
					panic("nfs_soreply 3");
				sbfree(&so->so_rcv, m);
				m = so->so_rcv.sb_mb = m->m_next;
			}
			so->so_rcv.sb_mb = nextrecord;
			if (rep == myrep)
				goto release;
			found++;
		}
		rep = rep->r_next;
	}
	/* Iff not matched to request, drop it */
dropit:
	if (!found) {
		sbdroprecord(&so->so_rcv);
	} else if (so->so_rcv.sb_flags & SB_WAIT) {
		so->so_rcv.sb_flags &= ~SB_WAIT;
		wakeup((caddr_t)&so->so_rcv.sb_cc);
	}
	sbunlock(&so->so_rcv);
	splx(s);
	goto restart;
release:
	sbunlock(&so->so_rcv);
	splx(s);
	return (error);
}

/*
 * nfs_request - goes something like this
 *	- fill in request struct
 *	- links it into list
 *	- calls nfs_sosend() for first transmit
 *	- calls nfs_soreceive() to get reply
 *	- break down rpc header and return with nfs reply pointed to
 *	  by mrep or error
 * nb: always frees up mreq mbuf list
 */
nfs_request(vp, mreq, xid, mp, mrp, mdp, dposp)
	struct vnode *vp;
	struct mbuf *mreq;
	u_long xid;
	struct mount *mp;
	struct mbuf **mrp;
	struct mbuf **mdp;
	caddr_t *dposp;
{
	register struct mbuf *m, *mrep;
	register struct nfsreq *rep;
	register u_long *p;
	register int len;
	struct nfsmount *mntp;
	struct mbuf *md;
	struct sockaddr_in *saddr;
	struct nfsreq *reph;
	caddr_t dpos;
	char *cp2;
	int t1;
	int s;
	int error;

	mntp = vfs_to_nfs(mp);
	m = mreq;
	MALLOC(rep, struct nfsreq *, sizeof(struct nfsreq), M_NFSREQ, M_WAITOK);
	rep->r_xid = xid;
	rep->r_mntp = mntp;
	saddr = mtod(mntp->nm_sockaddr, struct sockaddr_in *);
	rep->r_inaddr = saddr->sin_addr.s_addr;
	rep->r_vp = vp;
	if (mntp->nm_flag & NFSMNT_SOFT)
		rep->r_retry = mntp->nm_retrans;
	else
		rep->r_retry = VNOVAL;
	rep->r_mrep = NULL;
	rep->r_mreq = m;
	rep->r_timer = rep->r_timeout = mntp->nm_timeo;
	len = 0;
	while (m) {
		len += m->m_len;
		m = m->m_next;
	}
	rep->r_msiz = len;
	m = NFSMCOPY(mreq, 0, M_COPYALL, M_WAIT);

	/* Chain it into list of outstanding requests */
	reph = &nfsreqh;
	s = splnet();
	if (reph->r_prev == NULL) {
		reph->r_next = rep;
		rep->r_prev = reph;
	} else {
		reph->r_prev->r_next = rep;
		rep->r_prev = reph->r_prev;
	}
	reph->r_prev = rep;
	rep->r_next = reph;
	splx(s);

	/*
	 * Iff the NFSMCOPY above succeeded, send it off...
	 * otherwise the timer will retransmit later
	 */
	if (m != NULL)
		error = nfs_udpsend(mntp->nm_so, (struct mbuf *)0, m, 0, len);
	error = nfs_udpreply(mntp->nm_so, mntp, rep);

	s = splnet();
	rep->r_prev->r_next = rep->r_next;
	rep->r_next->r_prev = rep->r_prev;
	splx(s);
	m_freem(rep->r_mreq);
	mrep = md = rep->r_mrep;
	FREE((caddr_t)rep, M_NFSREQ);
	if (error)
		return (error);

	/*
	 * break down the rpc header and check if ok
	 */
	dpos = mtod(md, caddr_t);
	nfsm_disect(p, u_long *, 5*NFSX_UNSIGNED);
	p += 2;
	if (*p++ == rpc_msgdenied) {
		if (*p == rpc_mismatch)
			error = EOPNOTSUPP;
		else
			error = EACCES;
		m_freem(mrep);
		return (error);
	}
	/*
	 * skip over the auth_verf, someday we may want to cache auth_short's
	 * for nfs_reqhead(), but for now just dump it
	 */
	if (*++p != 0) {
		len = nfsm_rndup(fxdr_unsigned(long, *p));
		nfsm_adv(len);
	}
	nfsm_disect(p, u_long *, NFSX_UNSIGNED);
	/* 0 == ok */
	if (*p == 0) {
		nfsm_disect(p, u_long *, NFSX_UNSIGNED);
		if (*p != 0) {
			error = fxdr_unsigned(int, *p);
			m_freem(mrep);
			return (error);
		}
		*mrp = mrep;
		*mdp = md;
		*dposp = dpos;
		return (0);
	}
	m_freem(mrep);
	return (EPROTONOSUPPORT);
nfsmout:
	return (error);
}

/*
 * Get a request for the server main loop
 * - receive a request via. nfs_soreceive()
 * - verify it
 * - fill in the cred struct.
 */
nfs_getreq(so, prog, vers, maxproc, nam, mrp, mdp, dposp, retxid, proc, cr)
	struct socket *so;
	u_long prog;
	u_long vers;
	int maxproc;
	struct mbuf **nam;
	struct mbuf **mrp;
	struct mbuf **mdp;
	caddr_t *dposp;
	u_long *retxid;
	u_long *proc;
	register struct ucred *cr;
{
	register int i;
	register struct mbuf *m;
	nfsm_vars;
	int len, len2;

	if (error = nfs_udpreceive(so, nam, &mrep))
		return (error);
	md = mrep;
	dpos = mtod(mrep, caddr_t);
	nfsm_disect(p, u_long *, 10*NFSX_UNSIGNED);
	*retxid = *p++;
	if (*p++ != rpc_call) {
		m_freem(mrep);
		return (ERPCMISMATCH);
	}
	if (*p++ != rpc_vers) {
		m_freem(mrep);
		return (ERPCMISMATCH);
	}
	if (*p++ != prog) {
		m_freem(mrep);
		return (EPROGUNAVAIL);
	}
	if (*p++ != vers) {
		m_freem(mrep);
		return (EPROGMISMATCH);
	}
	*proc = fxdr_unsigned(u_long, *p++);
	if (*proc == NFSPROC_NULL) {
		*mrp = mrep;
		return (0);
	}
	if (*proc > maxproc || *p++ != rpc_auth_unix) {
		m_freem(mrep);
		return (EPROCUNAVAIL);
	}
	len = fxdr_unsigned(int, *p++);
	len2 = fxdr_unsigned(int, *++p);
	nfsm_adv(nfsm_rndup(len2));
	nfsm_disect(p, u_long *, 3*NFSX_UNSIGNED);
	cr->cr_uid = fxdr_unsigned(uid_t, *p++);
	cr->cr_gid = fxdr_unsigned(gid_t, *p++);
	len2 = fxdr_unsigned(int, *p);
	if (len2 > 10) {
		m_freem(mrep);
		return (EBADRPC);
	}
	nfsm_disect(p, u_long *, (len2+2)*NFSX_UNSIGNED);
	for (i = 1; i <= len2; i++)
		cr->cr_groups[i] = fxdr_unsigned(gid_t, *p++);
	cr->cr_ngroups = len2+1;
	/*
	 * Do we have any use for the verifier.
	 * According to the "Remote Procedure Call Protocol Spec." it
	 * should be AUTH_NULL, but some clients make it AUTH_UNIX?
	 * For now, just skip over it
	 */
	len2 = fxdr_unsigned(int, *++p);
	if (len2 > 0)
		nfsm_adv(nfsm_rndup(len2));
	*mrp = mrep;
	*mdp = md;
	*dposp = dpos;
	return (0);
nfsmout:
	return (error);
}

/*
 * Generate the rpc reply header
 * siz arg. is used to decide if adding a cluster is worthwhile
 */
nfs_rephead(siz, retxid, err, mrq, mbp, bposp)
	int siz;
	u_long retxid;
	int err;
	struct mbuf **mrq;
	struct mbuf **mbp;
	caddr_t *bposp;
{
	nfsm_vars;

	NFSMGETHDR(mreq);
	mb = mreq;
	if ((siz+RPC_REPLYSIZ) > MHLEN)
		NFSMCLGET(mreq, M_WAIT);
	p = mtod(mreq, u_long *);
	mreq->m_len = 6*NFSX_UNSIGNED;
	bpos = ((caddr_t)p)+mreq->m_len;
	*p++ = retxid;
	*p++ = rpc_reply;
	if (err == ERPCMISMATCH) {
		*p++ = rpc_msgdenied;
		*p++ = rpc_mismatch;
		*p++ = txdr_unsigned(2);
		*p = txdr_unsigned(2);
	} else {
		*p++ = rpc_msgaccepted;
		*p++ = 0;
		*p++ = 0;
		switch (err) {
		case EPROGUNAVAIL:
			*p = txdr_unsigned(RPC_PROGUNAVAIL);
			break;
		case EPROGMISMATCH:
			*p = txdr_unsigned(RPC_PROGMISMATCH);
			nfsm_build(p, u_long *, 2*NFSX_UNSIGNED);
			*p++ = txdr_unsigned(2);
			*p = txdr_unsigned(2);	/* someday 3 */
			break;
		case EPROCUNAVAIL:
			*p = txdr_unsigned(RPC_PROCUNAVAIL);
			break;
		default:
			*p = 0;
			if (err != VNOVAL) {
				nfsm_build(p, u_long *, NFSX_UNSIGNED);
				*p = txdr_unsigned(err);
			}
			break;
		};
	}
	*mrq = mreq;
	*mbp = mb;
	*bposp = bpos;
	if (err != 0 && err != VNOVAL)
		nfsstats.srvrpc_errs++;
	return (0);
}

/*
 * Nfs timer routine
 * Scan the nfsreq list and retranmit any requests that have timed out
 * To avoid retransmission attempts on STREAM sockets (in the future) make
 * sure to set the r_retry field to 0.
 */
nfs_timer()
{
	register struct nfsreq *rep;
	register struct mbuf *m;
	register struct socket *so;
	int s, len;

	s = splnet();
	rep = nfsreqh.r_next;
	while (rep && rep != &nfsreqh) {
		if (rep->r_timer > 0)
			rep->r_timer--;
		else if (rep->r_mrep == NULL && rep->r_retry > 0) {
			so = rep->r_mntp->nm_so;
			if ((so->so_state & SS_CANTSENDMORE) == 0 &&
			    !so->so_error &&
			    sbspace(&so->so_snd) >= rep->r_msiz) {
				m = NFSMCOPY(rep->r_mreq, 0, M_COPYALL, M_DONTWAIT);
				if (m != NULL) {
					nfsstats.rpcretries++;
					rep->r_timeout <<= 2; /* x4 backoff */
					if (rep->r_timeout > NFS_MAXTIMEO)
						rep->r_timeout = NFS_MAXTIMEO;
					rep->r_timer = rep->r_timeout;
					if (rep->r_retry != VNOVAL)
						rep->r_retry--;
#ifdef MGETHDR
					m->m_pkthdr.len = rep->r_msiz;
#endif
					(*so->so_proto->pr_usrreq)(so, PRU_SEND,
						m, (caddr_t)0, (struct mbuf *)0,
						(struct mbuf *)0);
				}
			}
		}
		rep = rep->r_next;
	}
	splx(s);
	timeout(nfs_timer, (caddr_t)0, hz/10);
}

/*
 * nfs_sbwait() is simply sbwait() but at a negative priority so that it
 * can not be interrupted by a signal.
 */
nfs_sbwait(sb)
	struct sockbuf *sb;
{
	sb->sb_flags |= SB_WAIT;
	sleep((caddr_t)&sb->sb_cc, PZERO-2);
}
