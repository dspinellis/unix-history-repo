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
 *	@(#)nfs_socket.c	7.7 (Berkeley) %G%
 */

/*
 * Socket operations for use by nfs (similar to uipc_socket.c, but never
 * with copies to/from a uio vector)
 * NB: For now, they only work for datagram sockets.
 * (Use on stream sockets would require some record boundary mark in the
 *  stream as defined by "RPC: Remote Procedure Call Protocol
 *  Specification" RFC1057 Section 10)
 *  and different versions of send, receive and reply that do not assume
 *  an atomic protocol
 */

#include "types.h"
#include "param.h"
#include "uio.h"
#include "user.h"
#include "proc.h"
#include "signal.h"
#include "mount.h"
#include "kernel.h"
#include "malloc.h"
#include "mbuf.h"
#include "vnode.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "rpcv2.h"
#include "nfsv2.h"
#include "nfs.h"
#include "xdr_subs.h"
#include "nfsm_subs.h"
#include "nfsmount.h"

#include "syslog.h"
#define nfs_log(message, host)	log(LOG_ERR, message, host)

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
 * nfs_sbwait() is simply sbwait() but at a negative priority so that it
 * can not be interrupted by a signal.
 */
nfs_sbwait(sb)
	struct sockbuf *sb;
{
	sb->sb_flags |= SB_WAIT;
	sleep((caddr_t)&sb->sb_cc, PZERO-2);
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

struct nfshost *nfshosth;
struct nfsreq nfsreqh;
int nfsrexmtthresh = NFS_FISHY;

/*
 * Initialize sockets and per-host congestion for a new NFS connection.
 * We do not free the sockaddr if error.
 */
nfs_connect(nmp, saddr)
	register struct nfsmount *nmp;
	struct mbuf *saddr;
{
	int s, error, srvaddrlen;
	struct mbuf *m;
	register struct nfshost *nfshp;

	nmp->nm_so = 0;
	if (error = socreate(mtod(saddr, struct sockaddr *)->sa_family,
				&nmp->nm_so, SOCK_DGRAM, 0))
		goto bad;

	/* Unix sockets do not provide a local bind for server reply */
	if (mtod(saddr, struct sockaddr *)->sa_family == AF_UNIX) {
		struct sockaddr *sa;
		static char client[] = "/tmp/.nfs/nfsclient##";
		static int serial;
		int firstserial;
		m = m_getclr(M_WAIT, MT_SONAME);
		if (m == NULL) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_len = sizeof (client) + 2;
		sa = mtod(m, struct sockaddr *);
		sa->sa_family = AF_UNIX;
#ifdef	MSG_TRUNC	/* Have sa_len to set? */
		sa->sa_len = m->m_len;
#endif
		bcopy(client, sa->sa_data, sizeof(client));
		firstserial = serial;
		do {
			if (++serial >= 100) serial = 0;
			sa->sa_data[19] = (serial / 10) + '0';
			sa->sa_data[20] = (serial % 10) + '0';
			error = sobind(nmp->nm_so, m);
			if (firstserial == serial) break;
		} while (error == EADDRINUSE);
		m_freem(m);
		if (error)
			goto bad;
	}

	if (error = soconnect(nmp->nm_so, saddr))
		goto bad;
	error = soreserve(nmp->nm_so,	/* get space ! */
				nmp->nm_wsize + 1024,		/* one out */
				(nmp->nm_rsize + 1024) * 4);	/* four in */
	if (error)
		goto bad;

	/*
	 * Search mount list for existing server entry.
	 *
	 * Note, even though we have a sockaddr, it is not quite reliable
	 * enough to bcmp against. For instance, a sockaddr_in has a 
	 * sin_zero field which is not reliably zeroed by user code (e.g.
	 * mount). So what we do as an attempt at transport independence
	 * is to get the peeraddr of our connected socket into a zeroed
	 * sockaddr. Then we cache that and compare against it. This is
	 * not exactly perfect. However it is not critical that it be, if
	 * we cannot match the sockaddr we will simply allocate a new nfshp
	 * per mount, which will disable the per-host congestion but
	 * everything else will work as normal.
	 */
	m = m_getclr(M_WAIT, MT_SONAME);
	if (m && (*(nmp->nm_so->so_proto->pr_usrreq))(nmp->nm_so, PRU_PEERADDR,
				(struct mbuf *)0, m, (struct mbuf *)0) == 0) {
		m_freem(saddr);
		saddr = m;
	} else
		m_freem(m);
	srvaddrlen = saddr->m_len;

	s = splnet();

	for (nfshp = nfshosth; nfshp; nfshp = nfshp->nh_next) {
		if (srvaddrlen != nfshp->nh_salen)
			continue;
		if (!bcmp(mtod(saddr,caddr_t),mtod(nfshp->nh_sockaddr,caddr_t),
				srvaddrlen))
			break;
	}
	if (nfshp)		/* Have an existing mount host */
		m_freem(saddr);
	else {
		MALLOC(nfshp,struct nfshost *,sizeof *nfshp,M_NFSMNT,M_WAITOK);
		bzero((caddr_t)nfshp, sizeof *nfshp);
		nfshp->nh_sockaddr = saddr;
		nfshp->nh_salen = srvaddrlen;
		/* Initialize other non-zero congestion variables */
		nfshp->nh_currto = NFS_TIMEO;
		nfshp->nh_window = 1;		    /* Initial send window */
		nfshp->nh_ssthresh = NFS_MAXWINDOW; /* Slowstart threshold */
		if (nfshosth) nfshosth->nh_prev = nfshp;	/* Chain in */
		nfshp->nh_next = nfshosth;
		nfshosth = nfshp;
	}
	nfshp->nh_refcnt++;
	splx(s);
	nmp->nm_hostinfo = nfshp;
	if (nmp->nm_rto == NFS_TIMEO) {
		nmp->nm_rto = nfshp->nh_currto;
		nmp->nm_rttvar = nmp->nm_rto << 1;
	}
	return (0);

bad:
	if (nmp->nm_so) (void) soclose(nmp->nm_so);
	nmp->nm_so = 0;
	return (error);
}

/*
 * NFS disconnect. Clean up and unlink.
 */
nfs_disconnect(nmp)
	register struct nfsmount *nmp;
{
	register struct nfshost *nfshp;

	if (nmp->nm_so)
		soclose(nmp->nm_so);
	nmp->nm_so = 0;
	if (nfshp = nmp->nm_hostinfo) {
		int s = splnet();
		if (--nfshp->nh_refcnt <= 0) {
			if (nfshp->nh_next)
				nfshp->nh_next->nh_prev = nfshp->nh_prev;
			if (nfshp->nh_prev)
				nfshp->nh_prev->nh_next = nfshp->nh_next;
			else
				nfshosth = nfshp->nh_next;
			/* If unix family, remove the nfsclient from /tmp */
			if (mtod(nfshp->nh_sockaddr,
				struct sockaddr *)->sa_family == AF_UNIX) {
					/* Lookup sa_data, do VOP_REMOVE... */
			}
			m_freem(nfshp->nh_sockaddr);
			FREE(nfshp, M_NFSMNT);
		}
		nmp->nm_hostinfo = 0;
		splx(s);
	}
}

/*
 * This is a stripped down non-interruptible version of sosend().
 */
nfs_send(so, nam, top, flags, siz)
	register struct socket *so;
	struct mbuf *nam;
	struct mbuf *top;
	int flags;
	int siz;
{
	int error, s;

#ifdef MGETHDR
	top->m_pkthdr.len = siz;
#endif
	for (;;) {
		nfs_sblock(&so->so_snd);
		s = splnet();
		if (error = nfs_sockerr(so, 1)) {
			splx(s);
			m_freem(top);
			break;
		}
		if (sbspace(&so->so_snd) < siz) {
			sbunlock(&so->so_snd);
			nfs_sbwait(&so->so_snd);
			splx(s);
			continue;
		}
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND, top,
			(struct mbuf *)nam, (struct mbuf *)0);
		splx(s);
		break;
	}
	sbunlock(&so->so_snd);
	return (error);
}

/*
 * This is a stripped down datagram specific version of soreceive()
 */
nfs_dgreceive(so, msk, mtch, aname, mp)
	register struct socket *so;
	u_long msk;
	u_long mtch;
	struct mbuf **aname;
	struct mbuf **mp;
{
	register struct mbuf *m;
	int s, error = 0;
	struct mbuf *nextrecord;

	if (aname)
		*aname = 0;

	for (;;) {
		sblock(&so->so_rcv);
		s = splnet();

		if (so->so_rcv.sb_cc == 0) {
			if (error = nfs_sockerr(so, 0)) {
				so->so_error = 0;
				break;
			}
			sbunlock(&so->so_rcv);
			sbwait(&so->so_rcv);
			splx(s);
			continue;
		}
		m = so->so_rcv.sb_mb;
		if (m == 0)
			panic("nfs_dgreceive 1");
		nextrecord = m->m_nextpkt;
		/* Save sender's address */
		if (m->m_type != MT_SONAME)
			panic("nfs_dgreceive 1a");
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
		/* Drop control mbuf's */
		if (m && m->m_type == MT_RIGHTS)
			panic("nfs_dgreceive 2");
		if (m && m->m_type == MT_CONTROL) {
			sbfree(&so->so_rcv, m);
			MFREE(m, so->so_rcv.sb_mb);
			m = so->so_rcv.sb_mb;
		}
		/* Dequeue packet from sockbuf */
		*mp = m;
		while (m) {
			if (m->m_type != MT_DATA && m->m_type != MT_HEADER)
				panic("nfs_dgreceive 3");
			sbfree(&so->so_rcv, m);
			m = so->so_rcv.sb_mb = m->m_next;
		}
		so->so_rcv.sb_mb = nextrecord;
		/* Return */
		break;
	}
	sbunlock(&so->so_rcv);
	splx(s);
	return (error);
}

struct rpc_replyhead {
	u_long	r_xid;
	u_long	r_rep;
};

/*
 * Implement NFS client side datagram receive.
 * We depend on the way that records are added to the sockbuf
 * by sbappend*.  In particular, each record (mbufs linked through m_next)
 * must begin with an address, followed by optional MT_CONTROL mbuf
 * and then zero or more mbufs of data.
 * We must search through the list of received datagrams matching them
 * with outstanding requests using the xid, until ours is found.
 */
nfs_dgreply(so, mntp, myrep)
	register struct socket *so;
	struct nfsmount *mntp;
	struct nfsreq *myrep;
{
	register struct mbuf *m;
	register struct nfsreq *rep;
	register int error = 0, s;
	int logged = 0;
	struct mbuf *nextrecord;
	struct rpc_replyhead replyh;

restart:
	nfs_sblock(&so->so_rcv);
	s = splnet();
	/* Already received and queued for us, bye bye */
	if (myrep->r_mrep != NULL) {
		error = 0;
		goto release;
	}
	/* If we have run out of retries (hard mounts have bogus count) */
	if (myrep->r_rexmit > myrep->r_retry) {
		error = ETIMEDOUT;
		nfsstats.rpctimeouts++;
giveup:
		if (myrep->r_flags & R_TIMING) {
			myrep->r_flags &= ~R_TIMING;
			mntp->nm_rtt = -1;
		}
		if (myrep->r_flags & R_SENT) {
			myrep->r_flags &= ~R_SENT;
			--mntp->nm_hostinfo->nh_sent;
			/* If count now 0, want to initiate new req */
		}
		goto release;
	}

	m = so->so_rcv.sb_mb;
	if (m == 0) {
		if (so->so_rcv.sb_cc)
			panic("nfs_soreply 1");
		if (error = nfs_sockerr(so, 0)) {
			so->so_error = 0;
			goto giveup;
		}
		/* Allow signals to interrupt request? (nfs_timer wakes up) */
		if ((mntp->nm_flag & NFSMNT_INT) &&
		    u.u_procp->p_sig & ~u.u_procp->p_sigmask) {
			error = EINTR;
			goto giveup;
		}
		if (mntp->nm_rexmit >= nfsrexmtthresh && logged++ == 0)
			uprintf("NFS server %s not responding, retrying\n",
				mntp->nm_host);
		sbunlock(&so->so_rcv);
		nfs_sbwait(&so->so_rcv);
		splx(s);
		goto restart;
	}

	/*
	 * Take off the address, check for rights and ditch any control
	 * mbufs.
	 */
	nextrecord = m->m_nextpkt;
	if (m->m_type != MT_SONAME)
		panic("nfs reply SONAME");
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
	if (m->m_len >= sizeof replyh)
		bcopy(mtod(m, caddr_t), (caddr_t)&replyh, sizeof replyh);
	else {
		struct mbuf *mp = m;
		caddr_t cp = (caddr_t)&replyh;
		int cnt = sizeof replyh;
		do {
			if (mp->m_len > 0) {
				int xfer = (mp->m_len >= cnt) ? cnt : mp->m_len;
				bcopy(mtod(mp, caddr_t), cp, xfer);
				cnt -= xfer;
				cp += xfer;
			}
			if (cnt > 0)
				mp = mp->m_next;
		} while (mp && cnt > 0);
		if (mp == NULL) {		/* Insufficient length */
			nfsstats.rpcinvalid++;
			goto dropit;
		}
	}
	if (replyh.r_rep != rpc_reply) {	/* Not a reply */
		nfsstats.rpcinvalid++;
		goto dropit;
	}
	/*
	 * Loop through the request list to match up the reply
	 * If no match, just drop the datagram
	 */
	if (rep = nfsreqh.r_next) {
	    while (rep != &nfsreqh) {
		/* The socket, being connected, will only queue matches */
		if (replyh.r_xid == rep->r_xid && so == rep->r_mntp->nm_so) {
			/* Found it.. */
			if (rep->r_mrep)	/* Already there - duplicate */
				break;
			rep->r_mrep = m;
			while (m) {
				if (m->m_type != MT_DATA && m->m_type != MT_HEADER)
					panic("nfs_soreply 3");
				sbfree(&so->so_rcv, m);
				m = so->so_rcv.sb_mb = m->m_next;
			}
			so->so_rcv.sb_mb = nextrecord;
			if (rep->r_flags & R_TIMING) {
				nfs_updatetimer(mntp);
				rep->r_flags &= ~R_TIMING;
				mntp->nm_rtt = -1;	/* re-arm timer */
			}
			if (rep->r_flags & R_SENT) {
				rep->r_flags &= ~R_SENT;
				--mntp->nm_hostinfo->nh_sent;
				/* If count now 0, want to initiate new req */
			}
			if (rep == myrep) {		/* This is success */
				if (logged)
					uprintf("NFS server %s responded\n",
						mntp->nm_host);
				goto release;
			}
			/* Else wake up other sleeper and wait for next */
			sbunlock(&so->so_rcv);
			sorwakeup(so);
			splx(s);
			goto restart;
		}
		rep = rep->r_next;
	    }
	}
	/* If not matched to request, drop it */
	nfsstats.rpcunexpected++;
dropit:
	sbdroprecord(&so->so_rcv);
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
nfs_request(vp, mreq, xid, idem, mp, mrp, mdp, dposp)
	struct vnode *vp;
	struct mbuf *mreq;
	u_long xid;
	int idem;
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
	rep->r_vp = vp;
	if (mntp->nm_flag & NFSMNT_SOFT)
		rep->r_retry = mntp->nm_retry;
	else
		rep->r_retry = NFS_MAXREXMIT + 1;	/* past clip limit */
	rep->r_flags = rep->r_rexmit = 0;
	/* Idempotency: add N * MINTIMEO to requests if not, else use 0 */
	rep->r_timer = rep->r_timerinit = -(idem * NFS_MINTIMEO);
	rep->r_mrep = NULL;
	rep->r_mreq = m;
	len = 0;
	while (m) {
		len += m->m_len;
		m = m->m_next;
	}
	rep->r_msiz = len;

	/*
	 * Do the client side RPC.
	 */
	nfsstats.rpcrequests++;
	s = splnet();
	/* Chain request into list of outstanding requests. Be sure
	 * to put it LAST so timer finds oldest requests first. */
	reph = &nfsreqh;
	if (reph->r_prev == NULL) {
		reph->r_next = rep;
		rep->r_prev = reph;
	} else {
		reph->r_prev->r_next = rep;
		rep->r_prev = reph->r_prev;
	}
	reph->r_prev = rep;
	rep->r_next = reph;
	/*
	 * If backing off another request or avoiding congestion, don't
	 * send this one now but let timer do it. If not timing a request,
	 * do it now.
	 */
	if (mntp->nm_hostinfo->nh_sent > 0 &&
	    (mntp->nm_hostinfo->nh_currexmit != 0 ||
	     mntp->nm_hostinfo->nh_sent >= mntp->nm_hostinfo->nh_window)) {
		splx(s);
		goto skipsend;
	}
	++mntp->nm_hostinfo->nh_sent;	/* Inconsistent if can't NFSMCOPY */
	rep->r_flags |= R_SENT;		/* But not a catastrophe */
	if (mntp->nm_rtt == -1) {
		mntp->nm_rtt = 0;
		rep->r_flags |= R_TIMING;
	}
	splx(s);

	/*
	 * If we can get a packet to send, send it off...
	 * otherwise the timer will retransmit later
	 */
	m = NFSMCOPY(mreq, 0, M_COPYALL, M_WAIT);
	if (m != NULL)
		(void) nfs_send(mntp->nm_so, (struct mbuf *)0, m, 0, len);
	/*
	 * Wait for the reply from our send or the timer's.
	 */
skipsend:
	error = nfs_dgreply(mntp->nm_so, mntp, rep);

	/*
	 * RPC done, unlink the request.
	 */
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
nfs_getreq(so, prog, vers, maxproc, nam, mrp, mdp, dposp, retxid, proc, cr,
	   msk, mtch)
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
	u_long msk;
	u_long mtch;
{
	register int i;
	register u_long *p;
	register long t1;
	caddr_t dpos, cp2;
	int error = 0;
	struct mbuf *mrep, *md;
	int len;

	if (error = nfs_dgreceive(so, msk, mtch, nam, &mrep))
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
	(void) fxdr_unsigned(int, *p++);
	len = fxdr_unsigned(int, *++p);
	nfsm_adv(nfsm_rndup(len));
	nfsm_disect(p, u_long *, 3*NFSX_UNSIGNED);
	cr->cr_uid = fxdr_unsigned(uid_t, *p++);
	cr->cr_gid = fxdr_unsigned(gid_t, *p++);
	len = fxdr_unsigned(int, *p);
	if (len > 10) {
		m_freem(mrep);
		return (EBADRPC);
	}
	nfsm_disect(p, u_long *, (len + 2)*NFSX_UNSIGNED);
	for (i = 1; i <= len; i++)
		cr->cr_groups[i] = fxdr_unsigned(gid_t, *p++);
	cr->cr_ngroups = len + 1;
	/*
	 * Do we have any use for the verifier.
	 * According to the "Remote Procedure Call Protocol Spec." it
	 * should be AUTH_NULL, but some clients make it AUTH_UNIX?
	 * For now, just skip over it
	 */
	len = fxdr_unsigned(int, *++p);
	if (len > 0)
		nfsm_adv(nfsm_rndup(len));
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
	register u_long *p;
	register long t1;
	caddr_t bpos;
	struct mbuf *mreq, *mb, *mb2;

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
 * sure to set the r_retry field to 0 (implies nm_retry == 0).
 */
nfs_timer()
{
	register struct nfsreq *rep;
	register struct mbuf *m;
	register struct socket *so;
	register struct nfsmount *mntp;
	int s, error;

	s = splnet();
	rep = nfsreqh.r_next;
	if (rep) for ( ; rep != &nfsreqh; rep = rep->r_next) {
		mntp = rep->r_mntp;
		if (rep->r_flags & R_TIMING)	/* update rtt in mount */
			mntp->nm_rtt++;
		/* If not timed out or reply already received, skip */
		if (++rep->r_timer < mntp->nm_rto || rep->r_mrep)
			continue;
		/* Do backoff and save new timeout in mount */
		if (rep->r_flags & R_TIMING) {
			nfs_backofftimer(mntp);
			rep->r_flags &= ~R_TIMING;
			mntp->nm_rtt = -1;
		}
		if (rep->r_flags & R_SENT) {
			rep->r_flags &= ~R_SENT;
			--mntp->nm_hostinfo->nh_sent;
		}
		/* Check state of socket, cf nfs_send */
		so = mntp->nm_so;
		if (error = nfs_sockerr(so, 1))
			goto wakeup;
		if (sbspace(&so->so_snd) < rep->r_msiz)
			goto wakeup;
		/* Check for too many retries, cf nfs_dgreply */
		if (++rep->r_rexmit > NFS_MAXREXMIT)	/* clip */
			rep->r_rexmit = NFS_MAXREXMIT;
		if (rep->r_rexmit > rep->r_retry)	/* too many */
			goto wakeup;
		/* Check for congestion control, cf nfs_request */
		if (mntp->nm_hostinfo->nh_sent >= mntp->nm_hostinfo->nh_window)
			goto wakeup;
		/* Send it! */
		m = NFSMCOPY(rep->r_mreq, 0, M_COPYALL, M_DONTWAIT);
		if (m == NULL)
			goto wakeup;
		nfsstats.rpcretries++;
#ifdef MGETHDR
		m->m_pkthdr.len = rep->r_msiz;
#endif
		(void)(*so->so_proto->pr_usrreq)(so, PRU_SEND, m,
			(struct mbuf *)0, (struct mbuf *)0);

		/* We need to time the request even though we're
		 * retransmitting, in order to maintain backoff. */
		mntp->nm_rtt = 0;
		++mntp->nm_hostinfo->nh_sent;
		rep->r_flags |= (R_SENT|R_TIMING);
		rep->r_timer = rep->r_timerinit;
wakeup:
		/* If error or interruptible mount, give user a look */
		if (error || (mntp->nm_flag & NFSMNT_INT))
			sorwakeup(so);
	}
	splx(s);
	timeout(nfs_timer, (caddr_t)0, hz/NFS_HZ);
}

/*
 * NFS timer update and backoff. The "Jacobson/Karels/Karn" scheme is
 * used here. The timer state is held in the nfsmount structure and
 * a single request is used to clock the response. When successful
 * the rtt smoothing in nfs_updatetimer is used, when failed the backoff
 * is done by nfs_backofftimer. We also log failure messages in these
 * routines.
 *
 * Congestion variables are held in the nfshost structure which
 * is referenced by nfsmounts and shared per-server. This separation
 * makes it possible to do per-mount timing which allows varying disk
 * access times to be dealt with, while preserving a network oriented
 * congestion control scheme.
 *
 * The windowing implements the Jacobson/Karels slowstart algorithm
 * with adjusted scaling factors. We start with one request, then send
 * 4 more after each success until the ssthresh limit is reached, then
 * we increment at a rate proportional to the window. On failure, we
 * remember 3/4 the current window and clamp the send limit to 1. Note
 * ICMP source quench is not reflected in so->so_error so we ignore that
 * for now.
 *
 * NFS behaves much more like a transport protocol with these changes,
 * shedding the teenage pedal-to-the-metal tendencies of "other"
 * implementations.
 *
 * Timers and congestion avoidance by Tom Talpey, Open Software Foundation.
 */

/*
 * The TCP algorithm was not forgiving enough. Because the NFS server
 * responds only after performing lookups/diskio/etc, we have to be
 * more prepared to accept a spiky variance. The TCP algorithm is:
 * TCP_RTO(mntp) ((((mntp)->nm_srtt >> 2) + (mntp)->nm_rttvar) >> 1)
 */
#define NFS_RTO(mntp)	(((mntp)->nm_srtt >> 3) + (mntp)->nm_rttvar)

nfs_updatetimer(mntp)
	register struct nfsmount *mntp;
{
	register struct nfshost *nfshp = mntp->nm_hostinfo;

	/* If retransmitted, clear and return */
	if (mntp->nm_rexmit || nfshp->nh_currexmit) {
		if (nfshp->nh_currexmit >= nfsrexmtthresh)
			nfs_log("NFS server %s OK\n", mntp->nm_host);
		mntp->nm_rexmit = nfshp->nh_currexmit = 0;
		return;
	}
	/* If have a measurement, do smoothing */
	if (mntp->nm_srtt) {
		register short delta;
		delta = mntp->nm_rtt - (mntp->nm_srtt >> 3);
		if ((mntp->nm_srtt += delta) <= 0)
			mntp->nm_srtt = 1;
		if (delta < 0)
			delta = -delta;
		delta -= (mntp->nm_rttvar >> 2);
		if ((mntp->nm_rttvar += delta) <= 0)
			mntp->nm_rttvar = 1;
	/* Else initialize */
	} else {
		mntp->nm_rttvar = mntp->nm_rtt << 1;
		if (mntp->nm_rttvar == 0) mntp->nm_rttvar = 2;
		mntp->nm_srtt = mntp->nm_rttvar << 2;
	}
	/* Compute new Retransmission TimeOut and clip */
	mntp->nm_rto = NFS_RTO(mntp);
	if (mntp->nm_rto < NFS_MINTIMEO)
		mntp->nm_rto = NFS_MINTIMEO;
	else if (mntp->nm_rto > NFS_MAXTIMEO)
		mntp->nm_rto = NFS_MAXTIMEO;
	nfshp->nh_currto = mntp->nm_rto;

	/* Update window estimate */
	if (nfshp->nh_window < nfshp->nh_ssthresh)	/* quickly */
		nfshp->nh_window += 4;
	else {						/* slowly */
		register long incr = ++nfshp->nh_winext;
		incr = (incr * incr) / nfshp->nh_window;
		if (incr > 0) {
			nfshp->nh_winext = 0;
			++nfshp->nh_window;
		}
	}
	if (nfshp->nh_window > NFS_MAXWINDOW)
		nfshp->nh_window = NFS_MAXWINDOW;
}

nfs_backofftimer(mntp)
	register struct nfsmount *mntp;
{
	register struct nfshost *nfshp = mntp->nm_hostinfo;
	register unsigned long newrto;

	/* Clip shift count */
	if (++mntp->nm_rexmit > 8 * sizeof mntp->nm_rto)
		mntp->nm_rexmit = 8 * sizeof mntp->nm_rto;
	/* Back off RTO exponentially */
	newrto = NFS_RTO(mntp);
	newrto <<= (mntp->nm_rexmit - 1);
	if (newrto == 0 || newrto > NFS_MAXTIMEO)
		newrto = NFS_MAXTIMEO;
	mntp->nm_rto = nfshp->nh_currto = newrto;

	/* If too many retries, message, assume a bogus RTT and re-measure */
	if (nfshp->nh_currexmit < mntp->nm_rexmit) {
		nfshp->nh_currexmit = mntp->nm_rexmit;
		if (nfshp->nh_currexmit >= nfsrexmtthresh) {
			if (nfshp->nh_currexmit == nfsrexmtthresh) {
				nfs_log("NFS server %s not responding\n",
								mntp->nm_host);
				mntp->nm_rttvar += (mntp->nm_srtt >> 2);
				mntp->nm_srtt = 0;
			}
			/* The routing invalidation should be a usrreq PRU */
			if (mtod(nfshp->nh_sockaddr,
				struct sockaddr *)->sa_family == AF_INET)
				in_losing(mntp->nm_so->so_pcb);
		}
	}
	/* Close down window but remember this point (3/4 current) for later */
	nfshp->nh_ssthresh = ((nfshp->nh_window << 1) + nfshp->nh_window) >> 2;
	nfshp->nh_window = 1;
	nfshp->nh_winext = 0;
}

/*
 * Not all errors are fatal. The closed checks deal
 * with errors a little strangely.
 */

nfs_sockerr(so, sending)
	struct socket *so;
	int sending;
{
	if (sending && (so->so_state & SS_CANTSENDMORE)) {
		so->so_error = EPIPE;
		return (EPIPE);
	}

	switch (so->so_error) {			/* inhibit certain errors */
	case ENETDOWN:
	case ENETUNREACH:
	case EHOSTDOWN:
	case EHOSTUNREACH:
		so->so_error = 0;
	case 0:
		break;
	default:				/* return all others */
		printf("nfs_sockerr: error %d on %s\n", so->so_error,
			sending?"send":"receive");
		return (so->so_error);
	}

	if (!sending && (so->so_state & SS_CANTRCVMORE)) {
		so->so_error = 0;		/* (no error) */
		return (EPIPE);
	}
	return (so->so_error);
}
