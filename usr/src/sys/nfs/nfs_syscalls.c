/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfs_syscalls.c	7.33 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "vnode.h"
#include "mount.h"
#include "proc.h"
#include "uio.h"
#include "malloc.h"
#include "buf.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "domain.h"
#include "protosw.h"
#include "namei.h"
#include "netinet/in.h"
#include "netinet/tcp.h"
#ifdef ISO
#include "netiso/iso.h"
#endif
#include "machine/endian.h"
#include "rpcv2.h"
#include "nfsv2.h"
#include "nfs.h"
#include "nfsrvcache.h"
#include "nfsmount.h"
#include "nqnfs.h"

/* Global defs. */
extern u_long nfs_prog, nfs_vers;
extern int (*nfsrv_procs[NFS_NPROCS])();
extern struct buf nfs_bqueue;
extern struct proc *nfs_iodwant[NFS_MAXASYNCDAEMON];
extern int nfs_numasync;
extern time_t nqnfsstarttime;
extern struct nfsrv_req nsrvq_head;
extern struct nfsd nfsd_head;
extern int nqsrv_writeslack;
struct nfssvc_sock *nfs_udpsock, *nfs_cltpsock;
int nuidhash_max = NFS_MAXUIDHASH;
static int nfs_numnfsd = 0;
int nfsd_waiting = 0;
static int notstarted = 1;
static int modify_flag = 0;
void nfsrv_cleancache(), nfsrv_rcv(), nfsrv_wakenfsd(), nfs_sndunlock();
void nfsrv_slpderef(), nfsrv_init();

#define	TRUE	1
#define	FALSE	0

static int nfs_asyncdaemon[NFS_MAXASYNCDAEMON];
/*
 * NFS server system calls
 * getfh() lives here too, but maybe should move to kern/vfs_syscalls.c
 */

/*
 * Get file handle system call
 */
struct getfh_args {
	char	*fname;
	fhandle_t *fhp;
};
getfh(p, uap, retval)
	struct proc *p;
	register struct getfh_args *uap;
	int *retval;
{
	register struct vnode *vp;
	fhandle_t fh;
	int error;
	struct nameidata nd;

	/*
	 * Must be super user
	 */
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	NDINIT(&nd, LOOKUP, FOLLOW | LOCKLEAF, UIO_USERSPACE, uap->fname, p);
	if (error = namei(&nd))
		return (error);
	vp = nd.ni_vp;
	bzero((caddr_t)&fh, sizeof(fh));
	fh.fh_fsid = vp->v_mount->mnt_stat.f_fsid;
	error = VFS_VPTOFH(vp, &fh.fh_fid);
	vput(vp);
	if (error)
		return (error);
	error = copyout((caddr_t)&fh, (caddr_t)uap->fhp, sizeof (fh));
	return (error);
}

static struct nfssvc_sock nfssvc_sockhead;

#define SLP_DEREFFREE	0x100
#define SLP_CLRFREE	0x200

/*
 * Nfs server psuedo system call for the nfsd's
 * Based on the flag value it either:
 * - adds a socket to the selection list
 * - remains in the kernel as an nfsd
 * - remains in the kernel as an nfsiod
 */
struct nfssvc_args {
	int flag;
	caddr_t argp;
};
nfssvc(p, uap, retval)
	struct proc *p;
	register struct nfssvc_args *uap;
	int *retval;
{
	struct nameidata nd;
	struct file *fp;
	struct mbuf *nam;
	struct nfsd_args nfsdarg;
	struct nfsd_srvargs nfsd_srvargs, *nsd = &nfsd_srvargs;
	struct nfsd_cargs ncd;
	struct nfsd *nfsd;
	struct nfssvc_sock *slp;
	struct nfsuid *nuidp, **nuh;
	struct nfsmount *nmp;
	int error;

	/*
	 * Must be super user
	 */
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	while (nfssvc_sockhead.ns_flag & SLP_INIT) {
		nfssvc_sockhead.ns_flag |= SLP_WANTINIT;
		(void) tsleep((caddr_t)&nfssvc_sockhead, PSOCK, "nfsd init", 0);
	}
	if (uap->flag & NFSSVC_BIOD)
		error = nfssvc_iod(p);
	else if (uap->flag & NFSSVC_MNTD) {
		if (error = copyin(uap->argp, (caddr_t)&ncd, sizeof (ncd)))
			return (error);
		NDINIT(&nd, LOOKUP, FOLLOW | LOCKLEAF, UIO_USERSPACE,
			ncd.ncd_dirp, p);
		if (error = namei(&nd))
			return (error);
		if ((nd.ni_vp->v_flag & VROOT) == 0)
			error = EINVAL;
		nmp = VFSTONFS(nd.ni_vp->v_mount);
		vput(nd.ni_vp);
		if (error)
			return (error);
		else if (nmp->nm_flag & NFSMNT_MNTD)
			return (0);
		nmp->nm_flag |= NFSMNT_MNTD;
		error = nqnfs_clientd(nmp, p->p_ucred, &ncd, uap->flag,
			uap->argp, p);
	} else if (uap->flag & NFSSVC_ADDSOCK) {
		if (error = copyin(uap->argp, (caddr_t)&nfsdarg,
		    sizeof(nfsdarg)))
			return (error);
		if (error = getsock(p->p_fd, nfsdarg.sock, &fp))
			return (error);
		/*
		 * Get the client address for connected sockets.
		 */
		if (nfsdarg.name == NULL || nfsdarg.namelen == 0)
			nam = (struct mbuf *)0;
		else if (error = sockargs(&nam, nfsdarg.name, nfsdarg.namelen,
			MT_SONAME))
			return (error);
		error = nfssvc_addsock(fp, nam);
	} else {
		if (error = copyin(uap->argp, (caddr_t)nsd, sizeof (*nsd)))
			return (error);
		if ((uap->flag & NFSSVC_AUTHIN) && (nfsd = nsd->nsd_nfsd) &&
			(nfsd->nd_slp->ns_flag & SLP_VALID)) {
			slp = nfsd->nd_slp;
			if (slp->ns_numuids < nuidhash_max) {
				slp->ns_numuids++;
				nuidp = (struct nfsuid *)
				   malloc(sizeof (struct nfsuid), M_NFSUID, M_WAITOK);
			} else
				nuidp = (struct nfsuid *)0;
			if ((slp->ns_flag & SLP_VALID) == 0) {
			    if (nuidp)
				free((caddr_t)nuidp, M_NFSUID);
			} else {
			    if (nuidp == (struct nfsuid *)0) {
				nuidp = slp->ns_lruprev;
				remque(nuidp);
				if (nuidp->nu_hprev)
					nuidp->nu_hprev->nu_hnext = nuidp->nu_hnext;
				if (nuidp->nu_hnext)
					nuidp->nu_hnext->nu_hprev = nuidp->nu_hprev;
			    }
			    nuidp->nu_cr = nsd->nsd_cr;
			    nuidp->nu_cr.cr_ref = 1;
			    nuidp->nu_uid = nsd->nsd_uid;
			    insque(nuidp, (struct nfsuid *)slp);
			    nuh = &slp->ns_uidh[NUIDHASH(nsd->nsd_uid)];
			    if (nuidp->nu_hnext = *nuh)
				nuidp->nu_hnext->nu_hprev = nuidp;
			    nuidp->nu_hprev = (struct nfsuid *)0;
			    *nuh = nuidp;
			}
		}
		if ((uap->flag & NFSSVC_AUTHINFAIL) && (nfsd = nsd->nsd_nfsd))
			nfsd->nd_flag |= NFSD_AUTHFAIL;
		error = nfssvc_nfsd(nsd, uap->argp, p);
	}
	if (error == EINTR || error == ERESTART)
		error = 0;
	return (error);
}

/*
 * Adds a socket to the list for servicing by nfsds.
 */
nfssvc_addsock(fp, mynam)
	struct file *fp;
	struct mbuf *mynam;
{
	register struct mbuf *m;
	register int siz;
	register struct nfssvc_sock *slp;
	register struct socket *so;
	struct nfssvc_sock *tslp;
	int error, s;

	so = (struct socket *)fp->f_data;
	tslp = (struct nfssvc_sock *)0;
	/*
	 * Add it to the list, as required.
	 */
	if (so->so_proto->pr_protocol == IPPROTO_UDP) {
		tslp = nfs_udpsock;
		if (tslp->ns_flag & SLP_VALID) {
			m_freem(mynam);
			return (EPERM);
		}
#ifdef ISO
	} else if (so->so_proto->pr_protocol == ISOPROTO_CLTP) {
		tslp = nfs_cltpsock;
		if (tslp->ns_flag & SLP_VALID) {
			m_freem(mynam);
			return (EPERM);
		}
#endif /* ISO */
	}
	if (so->so_type == SOCK_STREAM)
		siz = NFS_MAXPACKET + sizeof (u_long);
	else
		siz = NFS_MAXPACKET;
	if (error = soreserve(so, siz, siz)) {
		m_freem(mynam);
		return (error);
	}

	/*
	 * Set protocol specific options { for now TCP only } and
	 * reserve some space. For datagram sockets, this can get called
	 * repeatedly for the same socket, but that isn't harmful.
	 */
	if (so->so_type == SOCK_STREAM) {
		MGET(m, M_WAIT, MT_SOOPTS);
		*mtod(m, int *) = 1;
		m->m_len = sizeof(int);
		sosetopt(so, SOL_SOCKET, SO_KEEPALIVE, m);
	}
	if (so->so_proto->pr_domain->dom_family == AF_INET &&
	    so->so_proto->pr_protocol == IPPROTO_TCP) {
		MGET(m, M_WAIT, MT_SOOPTS);
		*mtod(m, int *) = 1;
		m->m_len = sizeof(int);
		sosetopt(so, IPPROTO_TCP, TCP_NODELAY, m);
	}
	so->so_rcv.sb_flags &= ~SB_NOINTR;
	so->so_rcv.sb_timeo = 0;
	so->so_snd.sb_flags &= ~SB_NOINTR;
	so->so_snd.sb_timeo = 0;
	if (tslp)
		slp = tslp;
	else {
		slp = (struct nfssvc_sock *)
			malloc(sizeof (struct nfssvc_sock), M_NFSSVC, M_WAITOK);
		printf("Alloc nfssvc_sock 0x%x\n", slp);
		bzero((caddr_t)slp, sizeof (struct nfssvc_sock));
		slp->ns_prev = nfssvc_sockhead.ns_prev;
		slp->ns_prev->ns_next = slp;
		slp->ns_next = &nfssvc_sockhead;
		nfssvc_sockhead.ns_prev = slp;
		slp->ns_lrunext = slp->ns_lruprev = (struct nfsuid *)slp;
	}
	slp->ns_so = so;
	slp->ns_nam = mynam;
	fp->f_count++;
	slp->ns_fp = fp;
	s = splnet();
	so->so_upcallarg = (caddr_t)slp;
	so->so_upcall = nfsrv_rcv;
	slp->ns_flag = (SLP_VALID | SLP_NEEDQ);
	nfsrv_wakenfsd(slp);
	splx(s);
	return (0);
}

/*
 * Called by nfssvc() for nfsds. Just loops around servicing rpc requests
 * until it is killed by a signal.
 */
nfssvc_nfsd(nsd, argp, p)
	struct nfsd_srvargs *nsd;
	caddr_t argp;
	struct proc *p;
{
	register struct mbuf *m, *nam2;
	register int siz;
	register struct nfssvc_sock *slp;
	register struct socket *so;
	register int *solockp;
	struct nfssvc_sock *oslp;
	struct nfsd *nd = nsd->nsd_nfsd;
	struct mbuf *mreq, *nam;
	int error, cacherep, s;
	int sotype;

	s = splnet();
	if (nd == (struct nfsd *)0) {
		nsd->nsd_nfsd = nd = (struct nfsd *)
			malloc(sizeof (struct nfsd), M_NFSD, M_WAITOK);
		bzero((caddr_t)nd, sizeof (struct nfsd));
		nd->nd_procp = p;
		nd->nd_cr.cr_ref = 1;
		insque(nd, &nfsd_head);
		nfs_numnfsd++;
	}
	/*
	 * Loop getting rpc requests until SIGKILL.
	 */
	for (;;) {
		if ((nd->nd_flag & NFSD_REQINPROG) == 0) {
			while (nd->nd_slp == (struct nfssvc_sock *)0 &&
				 (nfsd_head.nd_flag & NFSD_CHECKSLP) == 0) {
				nd->nd_flag |= NFSD_WAITING;
				nfsd_waiting++;
				error = tsleep((caddr_t)nd, PSOCK | PCATCH, "nfsd", 0);
				nfsd_waiting--;
				if (error)
					goto done;
			}
			if (nd->nd_slp == (struct nfssvc_sock *)0 &&
				(nfsd_head.nd_flag & NFSD_CHECKSLP)) {
				slp = nfssvc_sockhead.ns_next;
				while (slp != &nfssvc_sockhead) {
				    if ((slp->ns_flag & (SLP_VALID | SLP_DOREC))
					== (SLP_VALID | SLP_DOREC)) {
					    slp->ns_flag &= ~SLP_DOREC;
					    slp->ns_sref++;
					    nd->nd_slp = slp;
					    break;
				    }
				    slp = slp->ns_next;
				}
				if (slp == &nfssvc_sockhead)
					nfsd_head.nd_flag &= ~NFSD_CHECKSLP;
			}
			if ((slp = nd->nd_slp) == (struct nfssvc_sock *)0)
				continue;
			if (slp->ns_flag & SLP_VALID) {
				if (slp->ns_flag & SLP_DISCONN)
					nfsrv_zapsock(slp);
				else if (slp->ns_flag & SLP_NEEDQ) {
					slp->ns_flag &= ~SLP_NEEDQ;
					(void) nfs_sndlock(&slp->ns_solock,
						(struct nfsreq *)0);
					nfsrv_rcv(slp->ns_so, (caddr_t)slp,
						M_WAIT);
					nfs_sndunlock(&slp->ns_solock);
				}
				error = nfsrv_dorec(slp, nd);
				nd->nd_flag |= NFSD_REQINPROG;
			}
		} else {
			error = 0;
			slp = nd->nd_slp;
		}
		if (error || (slp->ns_flag & SLP_VALID) == 0) {
			nd->nd_slp = (struct nfssvc_sock *)0;
			nd->nd_flag &= ~NFSD_REQINPROG;
			nfsrv_slpderef(slp);
			continue;
		}
		splx(s);
		so = slp->ns_so;
		sotype = so->so_type;

		/*
		 * Check to see if authorization is needed.
		 */
		if (nd->nd_flag & NFSD_NEEDAUTH) {
			nd->nd_flag &= ~NFSD_NEEDAUTH;
			nsd->nsd_uid = nd->nd_cr.cr_uid;
			nsd->nsd_haddr =
			    mtod(slp->ns_nam, struct sockaddr_in *)->sin_addr.s_addr;
			nsd->nsd_authlen = nd->nd_authlen;
			(void) copyout(nd->nd_authstr, nsd->nsd_authstr,
				 nd->nd_authlen);
			(void) copyout((caddr_t)nsd, argp, sizeof (*nsd));
			return (ENEEDAUTH);
		}
		if (so->so_proto->pr_flags & PR_CONNREQUIRED)
			solockp = &slp->ns_solock;
		else
			solockp = (int *)0;
		/*
		 * nam == nam2 for connectionless protocols such as UDP
		 * nam2 == NULL for connection based protocols to disable
		 *    recent request caching.
		 */
		nam2 = nd->nd_nam;

		if (nam2) {
			nam = nam2;
			cacherep = nfsrv_getcache(nam2, nd, &mreq);
		} else {
			nam = slp->ns_nam;
			cacherep = RC_DOIT;
		}

		/*
		 * Check for just starting up for NQNFS and send
		 * fake "try again later" replies to the NQNFS clients.
		 */
		if (notstarted && nqnfsstarttime <= time.tv_sec) {
			if (modify_flag) {
				nqnfsstarttime = time.tv_sec + nqsrv_writeslack;
				modify_flag = 0;
			} else
				notstarted = 0;
		}
		if (notstarted) {
			if (nd->nd_nqlflag == NQL_NOVAL)
				cacherep = RC_DROPIT;
			else if (nd->nd_procnum != NFSPROC_WRITE) {
				nd->nd_procnum = NFSPROC_NOOP;
				nd->nd_repstat = NQNFS_TRYLATER;
				cacherep = RC_DOIT;
			} else
				modify_flag = 1;
		} else if (nd->nd_flag & NFSD_AUTHFAIL) {
			nd->nd_flag &= ~NFSD_AUTHFAIL;
			nd->nd_procnum = NFSPROC_NOOP;
			nd->nd_repstat = NQNFS_AUTHERR;
			cacherep = RC_DOIT;
		}

		switch (cacherep) {
		case RC_DOIT:
			error = (*(nfsrv_procs[nd->nd_procnum]))(nd,
				nd->nd_mrep, nd->nd_md, nd->nd_dpos, &nd->nd_cr,
				nam, &mreq);
			if (nd->nd_cr.cr_ref != 1) {
				printf("nfssvc cref=%d\n", nd->nd_cr.cr_ref);
				panic("nfssvc cref");
			}
			if (error) {
				if (nd->nd_procnum != NQNFSPROC_VACATED)
					nfsstats.srv_errs++;
				if (nam2) {
					nfsrv_updatecache(nam2, nd, FALSE, mreq);
					m_freem(nam2);
				}
				break;
			}
			nfsstats.srvrpccnt[nd->nd_procnum]++;
			if (nam2)
				nfsrv_updatecache(nam2, nd, TRUE, mreq);
			nd->nd_mrep = (struct mbuf *)0;
		case RC_REPLY:
			m = mreq;
			siz = 0;
			while (m) {
				siz += m->m_len;
				m = m->m_next;
			}
			if (siz <= 0 || siz > NFS_MAXPACKET) {
				printf("mbuf siz=%d\n",siz);
				panic("Bad nfs svc reply");
			}
			m = mreq;
			m->m_pkthdr.len = siz;
			m->m_pkthdr.rcvif = (struct ifnet *)0;
			/*
			 * For stream protocols, prepend a Sun RPC
			 * Record Mark.
			 */
			if (sotype == SOCK_STREAM) {
				M_PREPEND(m, NFSX_UNSIGNED, M_WAIT);
				*mtod(m, u_long *) = htonl(0x80000000 | siz);
			}
			if (solockp)
				(void) nfs_sndlock(solockp, (struct nfsreq *)0);
			if (slp->ns_flag & SLP_VALID)
			    error = nfs_send(so, nam2, m, (struct nfsreq *)0);
			else {
			    error = EPIPE;
			    m_freem(m);
			}
			if (nam2)
				MFREE(nam2, m);
			if (nd->nd_mrep)
				m_freem(nd->nd_mrep);
			if (error == EPIPE)
				nfsrv_zapsock(slp);
			if (solockp)
				nfs_sndunlock(solockp);
			if (error == EINTR || error == ERESTART) {
				nfsrv_slpderef(slp);
				s = splnet();
				goto done;
			}
			break;
		case RC_DROPIT:
			m_freem(nd->nd_mrep);
			m_freem(nam2);
			break;
		};
		s = splnet();
		if (nfsrv_dorec(slp, nd)) {
			nd->nd_flag &= ~NFSD_REQINPROG;
			nd->nd_slp = (struct nfssvc_sock *)0;
			nfsrv_slpderef(slp);
		}
#ifdef DIAGNOSTIC
		if (p->p_spare[0])
			panic("nfssvc: M_NAMEI");
		if (p->p_spare[1])
			panic("nfssvc: STARTSAVE");
#endif
	}
done:
	remque(nd);
	splx(s);
	free((caddr_t)nd, M_NFSD);
	nsd->nsd_nfsd = (struct nfsd *)0;
	if (--nfs_numnfsd == 0)
		nfsrv_init(TRUE);	/* Reinitialize everything */
	return (error);
}

/*
 * Asynchronous I/O daemons for client nfs.
 * These babies just pretend to be disk interrupt service routines.
 * They are mainly here for read ahead/write behind.
 * Never returns unless it fails or gets killed.
 */
nfssvc_iod(p)
	struct proc *p;
{
	register struct buf *bp, *dp;
	register int i, myiod;
	int error = 0;

	/*
	 * Assign my position or return error if too many already running
	 */
	myiod = -1;
	for (i = 0; i < NFS_MAXASYNCDAEMON; i++)
		if (nfs_asyncdaemon[i] == 0) {
			nfs_asyncdaemon[i]++;
			myiod = i;
			break;
		}
	if (myiod == -1)
		return (EBUSY);
	nfs_numasync++;
	dp = &nfs_bqueue;
	/*
	 * Just loop around doin our stuff until SIGKILL
	 */
	for (;;) {
		while (dp->b_actf == NULL && error == 0) {
			nfs_iodwant[myiod] = p;
			error = tsleep((caddr_t)&nfs_iodwant[myiod],
				PWAIT | PCATCH, "nfsidl", 0);
			nfs_iodwant[myiod] = (struct proc *)0;
		}
		while (dp->b_actf != NULL) {
			/* Take one off the end of the list */
			bp = dp->b_actl;
			if (bp->b_actl == dp) {
				dp->b_actf = dp->b_actl = (struct buf *)0;
			} else {
				dp->b_actl = bp->b_actl;
				bp->b_actl->b_actf = dp;
			}
			(void) nfs_doio(bp, (struct proc *)0);
		}
		if (error) {
			nfs_asyncdaemon[myiod] = 0;
			nfs_numasync--;
			return (error);
		}
	}
}

/*
 * Shut down a socket associated with an nfssvc_sock structure.
 * Should be called with the send lock set, if required.
 * The trick here is to increment the sref at the start, so that the nfsds
 * will stop using it and clear ns_flag at the end so that it will not be
 * reassigned during cleanup.
 */
nfsrv_zapsock(slp)
	register struct nfssvc_sock *slp;
{
	register struct nfsuid *nuidp, *onuidp;
	register int i;
	struct socket *so;
	struct file *fp;
	struct mbuf *m;

	slp->ns_flag &= ~SLP_ALLFLAGS;
	if (fp = slp->ns_fp) {
		slp->ns_fp = (struct file *)0;
		so = slp->ns_so;
		so->so_upcall = NULL;
		soshutdown(so, 2);
		closef(fp, (struct proc *)0);
		if (slp->ns_nam)
			MFREE(slp->ns_nam, m);
		m_freem(slp->ns_raw);
		m_freem(slp->ns_rec);
		nuidp = slp->ns_lrunext;
		while (nuidp != (struct nfsuid *)slp) {
			onuidp = nuidp;
			nuidp = nuidp->nu_lrunext;
			free((caddr_t)onuidp, M_NFSUID);
		}
		slp->ns_lrunext = slp->ns_lruprev = (struct nfsuid *)slp;
		for (i = 0; i < NUIDHASHSIZ; i++)
			slp->ns_uidh[i] = (struct nfsuid *)0;
	}
}

/*
 * Get an authorization string for the uid by having the mount_nfs sitting
 * on this mount point porpous out of the kernel and do it.
 */
nfs_getauth(nmp, rep, cred, auth_type, auth_str, auth_len)
	register struct nfsmount *nmp;
	struct nfsreq *rep;
	struct ucred *cred;
	int *auth_type;
	char **auth_str;
	int *auth_len;
{
	int error = 0;

	while ((nmp->nm_flag & NFSMNT_WAITAUTH) == 0) {
		nmp->nm_flag |= NFSMNT_WANTAUTH;
		(void) tsleep((caddr_t)&nmp->nm_authtype, PSOCK,
			"nfsauth1", 2 * hz);
		if (error = nfs_sigintr(nmp, rep, rep->r_procp)) {
			nmp->nm_flag &= ~NFSMNT_WANTAUTH;
			return (error);
		}
	}
	nmp->nm_flag &= ~(NFSMNT_WAITAUTH | NFSMNT_WANTAUTH);
	nmp->nm_authstr = *auth_str = (char *)malloc(RPCAUTH_MAXSIZ, M_TEMP, M_WAITOK);
	nmp->nm_authuid = cred->cr_uid;
	wakeup((caddr_t)&nmp->nm_authstr);

	/*
	 * And wait for mount_nfs to do its stuff.
	 */
	while ((nmp->nm_flag & NFSMNT_HASAUTH) == 0 && error == 0) {
		(void) tsleep((caddr_t)&nmp->nm_authlen, PSOCK,
			"nfsauth2", 2 * hz);
		error = nfs_sigintr(nmp, rep, rep->r_procp);
	}
	if (nmp->nm_flag & NFSMNT_AUTHERR) {
		nmp->nm_flag &= ~NFSMNT_AUTHERR;
		error = EAUTH;
	}
	if (error)
		free((caddr_t)*auth_str, M_TEMP);
	else {
		*auth_type = nmp->nm_authtype;
		*auth_len = nmp->nm_authlen;
	}
	nmp->nm_flag &= ~NFSMNT_HASAUTH;
	nmp->nm_flag |= NFSMNT_WAITAUTH;
	if (nmp->nm_flag & NFSMNT_WANTAUTH) {
		nmp->nm_flag &= ~NFSMNT_WANTAUTH;
		wakeup((caddr_t)&nmp->nm_authtype);
	}
	return (error);
}

/*
 * Derefence a server socket structure. If it has no more references and
 * is no longer valid, you can throw it away.
 */
void
nfsrv_slpderef(slp)
	register struct nfssvc_sock *slp;
{
	if (--(slp->ns_sref) == 0 && (slp->ns_flag & SLP_VALID) == 0) {
#ifdef NOTYET
		slp->ns_prev->ns_next = slp->ns_next;
		slp->ns_next->ns_prev = slp->ns_prev;
		free((caddr_t)slp, M_NFSSVC);
#else
		if (slp->ns_flag & SLP_DEREFFREE)
			panic("deref dup free 0x%x of deref free\n", slp);
		else {
			slp->ns_prev->ns_next = slp->ns_next;
			slp->ns_next->ns_prev = slp->ns_prev;
		}
		if (slp->ns_flag & SLP_CLRFREE)
			panic("deref dup free 0x%x of clrall free\n", slp);
		slp->ns_flag |= SLP_DEREFFREE;
		printf("Free deref sock 0x%x\n", slp);
#endif
	}
}

/*
 * Initialize the data structures for the server.
 * Handshake with any new nfsds starting up to avoid any chance of
 * corruption.
 */
void
nfsrv_init(terminating)
	int terminating;
{
	register struct nfssvc_sock *slp;
	struct nfssvc_sock *oslp;

	if (nfssvc_sockhead.ns_flag & SLP_INIT)
		panic("nfsd init");
	nfssvc_sockhead.ns_flag |= SLP_INIT;
	if (terminating) {
		slp = nfssvc_sockhead.ns_next;
		while (slp != &nfssvc_sockhead) {
			if (slp->ns_flag & SLP_VALID)
				nfsrv_zapsock(slp);
			slp->ns_next->ns_prev = slp->ns_prev;
			slp->ns_prev->ns_next = slp->ns_next;
			oslp = slp;
			slp = slp->ns_next;
#ifdef NOTYET
			free((caddr_t)oslp, M_NFSSVC);
#else
			if (oslp->ns_flag & SLP_DEREFFREE)
				panic("clrall dup free 0x%x of deref free\n",
					oslp);
			if (oslp->ns_flag & SLP_CLRFREE)
				panic("clrall dup free 0x%x of clrall free\n",
					oslp);
			oslp->ns_flag |= SLP_CLRFREE;
			printf("Free all socks 0x%x\n", oslp);
#endif
		}
		nfsrv_cleancache();	/* And clear out server cache */
	}
	nfs_udpsock = (struct nfssvc_sock *)
	    malloc(sizeof (struct nfssvc_sock), M_NFSSVC, M_WAITOK);
	printf("Alloc nfs_udpsock 0x%x\n", nfs_udpsock);
	bzero((caddr_t)nfs_udpsock, sizeof (struct nfssvc_sock));
	nfs_cltpsock = (struct nfssvc_sock *)
	    malloc(sizeof (struct nfssvc_sock), M_NFSSVC, M_WAITOK);
	printf("Alloc nfs_cltpsock 0x%x\n", nfs_cltpsock);
	bzero((caddr_t)nfs_cltpsock, sizeof (struct nfssvc_sock));
	nfssvc_sockhead.ns_next = nfs_udpsock;
	nfs_udpsock->ns_next = nfs_cltpsock;
	nfs_cltpsock->ns_next = &nfssvc_sockhead;
	nfssvc_sockhead.ns_prev = nfs_cltpsock;
	nfs_cltpsock->ns_prev = nfs_udpsock;
	nfs_udpsock->ns_prev = &nfssvc_sockhead;
	nfs_udpsock->ns_lrunext = nfs_udpsock->ns_lruprev =
		(struct nfsuid *)nfs_udpsock;
	nfs_cltpsock->ns_lrunext = nfs_cltpsock->ns_lruprev =
		(struct nfsuid *)nfs_cltpsock;
	nfsd_head.nd_next = nfsd_head.nd_prev = &nfsd_head;
	nfsd_head.nd_flag = 0;
	nfssvc_sockhead.ns_flag &= ~SLP_INIT;
	if (nfssvc_sockhead.ns_flag & SLP_WANTINIT) {
		nfssvc_sockhead.ns_flag &= ~SLP_WANTINIT;
		wakeup((caddr_t)&nfssvc_sockhead);
	}
}
