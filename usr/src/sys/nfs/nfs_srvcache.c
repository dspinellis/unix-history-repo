/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfs_srvcache.c	7.13 (Berkeley) %G%
 */

/*
 * Reference: Chet Juszczak, "Improving the Performance and Correctness
 *		of an NFS Server", in Proc. Winter 1989 USENIX Conference,
 *		pages 53-63. San Diego, February 1989.
 */
#include "param.h"
#include "vnode.h"
#include "mount.h"
#include "kernel.h"
#include "systm.h"
#include "proc.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "netinet/in.h"
#ifdef ISO
#include "netiso/iso.h"
#endif
#include "nfsm_subs.h"
#include "rpcv2.h"
#include "nfsv2.h"
#include "nfsrvcache.h"
#include "nfs.h"
#include "nqnfs.h"

#if	((NFSRCHSZ&(NFSRCHSZ-1)) == 0)
#define	NFSRCHASH(xid)		(((xid)+((xid)>>16))&(NFSRCHSZ-1))
#else
#define	NFSRCHASH(xid)		(((unsigned)((xid)+((xid)>>16)))%NFSRCHSZ)
#endif

union rhead {
	union  rhead *rh_head[2];
	struct nfsrvcache *rh_chain[2];
} rhead[NFSRCHSZ];

static struct nfsrvcache nfsrvcachehead;
static struct nfsrvcache nfsrvcache[NFSRVCACHESIZ];

#define TRUE	1
#define	FALSE	0

#define	NETFAMILY(rp) \
		(((rp)->rc_flag & RC_INETADDR) ? AF_INET : AF_ISO)

/*
 * Static array that defines which nfs rpc's are nonidempotent
 */
int nonidempotent[NFS_NPROCS] = {
	FALSE,
	FALSE,
	TRUE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
};

/* True iff the rpc reply is an nfs status ONLY! */
static int repliesstatus[NFS_NPROCS] = {
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	TRUE,
	TRUE,
	TRUE,
	TRUE,
	FALSE,
	TRUE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
};

/*
 * Initialize the server request cache list
 */
nfsrv_initcache()
{
	register int i;
	register struct nfsrvcache *rp = nfsrvcache;
	register struct nfsrvcache *hp = &nfsrvcachehead;
	register union  rhead *rh = rhead;

	for (i = NFSRCHSZ; --i >= 0; rh++) {
		rh->rh_head[0] = rh;
		rh->rh_head[1] = rh;
	}
	hp->rc_next = hp->rc_prev = hp;
	for (i = NFSRVCACHESIZ; i-- > 0; ) {
		rp->rc_state = RC_UNUSED;
		rp->rc_flag = 0;
		rp->rc_forw = rp;
		rp->rc_back = rp;
		rp->rc_next = hp->rc_next;
		hp->rc_next->rc_prev = rp;
		rp->rc_prev = hp;
		hp->rc_next = rp;
		rp++;
	}
}

/*
 * Look for the request in the cache
 * If found then
 *    return action and optionally reply
 * else
 *    insert it in the cache
 *
 * The rules are as follows:
 * - if in progress, return DROP request
 * - if completed within DELAY of the current time, return DROP it
 * - if completed a longer time ago return REPLY if the reply was cached or
 *   return DOIT
 * Update/add new request at end of lru list
 */
nfsrv_getcache(nam, nd, repp)
	struct mbuf *nam;
	register struct nfsd *nd;
	struct mbuf **repp;
{
	register struct nfsrvcache *rp;
	register union  rhead *rh;
	struct mbuf *mb;
	struct sockaddr_in *saddr;
	caddr_t bpos;
	int ret;

	if (nd->nd_nqlflag != NQL_NOVAL)
		return (RC_DOIT);
	rh = &rhead[NFSRCHASH(nd->nd_retxid)];
loop:
	for (rp = rh->rh_chain[0]; rp != (struct nfsrvcache *)rh; rp = rp->rc_forw) {
	    if (nd->nd_retxid == rp->rc_xid && nd->nd_procnum == rp->rc_proc &&
		nfs_netaddr_match(NETFAMILY(rp), &rp->rc_haddr, (union nethostaddr *)0, nam)) {
			if ((rp->rc_flag & RC_LOCKED) != 0) {
				rp->rc_flag |= RC_WANTED;
				(void) tsleep((caddr_t)rp, PZERO-1, "nfsrc", 0);
				goto loop;
			}
			rp->rc_flag |= RC_LOCKED;
			if (rp->rc_prev != &nfsrvcachehead) {
				rp->rc_next->rc_prev = rp->rc_prev;
				rp->rc_prev->rc_next = rp->rc_next;
				rp->rc_next = nfsrvcachehead.rc_next;
				nfsrvcachehead.rc_next = rp;
				rp->rc_prev = &nfsrvcachehead;
				rp->rc_next->rc_prev = rp;
			}
			if (rp->rc_state == RC_UNUSED)
				panic("nfsrv cache");
			if (rp->rc_state == RC_INPROG ||
			   (time.tv_sec - rp->rc_timestamp) < RC_DELAY) {
				nfsstats.srvcache_inproghits++;
				ret = RC_DROPIT;
			} else if (rp->rc_flag & RC_REPSTATUS) {
				nfsstats.srvcache_idemdonehits++;
				nfs_rephead(0, nd, rp->rc_status,
				   0, (u_quad_t *)0, repp, &mb, &bpos);
				rp->rc_timestamp = time.tv_sec;
				ret = RC_REPLY;
			} else if (rp->rc_flag & RC_REPMBUF) {
				nfsstats.srvcache_idemdonehits++;
				*repp = m_copym(rp->rc_reply, 0, M_COPYALL,
						M_WAIT);
				rp->rc_timestamp = time.tv_sec;
				ret = RC_REPLY;
			} else {
				nfsstats.srvcache_nonidemdonehits++;
				rp->rc_state = RC_INPROG;
				ret = RC_DOIT;
			}
			rp->rc_flag &= ~RC_LOCKED;
			if (rp->rc_flag & RC_WANTED) {
				rp->rc_flag &= ~RC_WANTED;
				wakeup((caddr_t)rp);
			}
			return (ret);
		}
	}
	nfsstats.srvcache_misses++;
	rp = nfsrvcachehead.rc_prev;
	while ((rp->rc_flag & RC_LOCKED) != 0) {
		rp->rc_flag |= RC_WANTED;
		(void) tsleep((caddr_t)rp, PZERO-1, "nfsrc", 0);
		rp = nfsrvcachehead.rc_prev;
	}
	rp->rc_flag |= RC_LOCKED;
	remque(rp);
	if (rp->rc_prev != &nfsrvcachehead) {
		rp->rc_next->rc_prev = rp->rc_prev;
		rp->rc_prev->rc_next = rp->rc_next;
		rp->rc_next = nfsrvcachehead.rc_next;
		nfsrvcachehead.rc_next = rp;
		rp->rc_prev = &nfsrvcachehead;
		rp->rc_next->rc_prev = rp;
	}
	if (rp->rc_flag & RC_REPMBUF)
		m_freem(rp->rc_reply);
	if (rp->rc_flag & RC_NAM)
		MFREE(rp->rc_nam, mb);
	rp->rc_flag &= (RC_LOCKED | RC_WANTED);
	rp->rc_state = RC_INPROG;
	rp->rc_xid = nd->nd_retxid;
	saddr = mtod(nam, struct sockaddr_in *);
	switch (saddr->sin_family) {
	case AF_INET:
		rp->rc_flag |= RC_INETADDR;
		rp->rc_inetaddr = saddr->sin_addr.s_addr;
		break;
	case AF_ISO:
	default:
		rp->rc_flag |= RC_NAM;
		rp->rc_nam = m_copym(nam, 0, M_COPYALL, M_WAIT);
		break;
	};
	rp->rc_proc = nd->nd_procnum;
	insque(rp, rh);
	rp->rc_flag &= ~RC_LOCKED;
	if (rp->rc_flag & RC_WANTED) {
		rp->rc_flag &= ~RC_WANTED;
		wakeup((caddr_t)rp);
	}
	return (RC_DOIT);
}

/*
 * Update a request cache entry after the rpc has been done
 */
void
nfsrv_updatecache(nam, nd, repvalid, repmbuf)
	struct mbuf *nam;
	register struct nfsd *nd;
	int repvalid;
	struct mbuf *repmbuf;
{
	register struct nfsrvcache *rp;
	register union	rhead *rh;

	if (nd->nd_nqlflag != NQL_NOVAL)
		return;
	rh = &rhead[NFSRCHASH(nd->nd_retxid)];
loop:
	for (rp = rh->rh_chain[0]; rp != (struct nfsrvcache *)rh; rp = rp->rc_forw) {
	    if (nd->nd_retxid == rp->rc_xid && nd->nd_procnum == rp->rc_proc &&
		nfs_netaddr_match(NETFAMILY(rp), &rp->rc_haddr, (union nethostaddr *)0, nam)) {
			if ((rp->rc_flag & RC_LOCKED) != 0) {
				rp->rc_flag |= RC_WANTED;
				(void) tsleep((caddr_t)rp, PZERO-1, "nfsrc", 0);
				goto loop;
			}
			rp->rc_flag |= RC_LOCKED;
			rp->rc_state = RC_DONE;
			/*
			 * If we have a valid reply update status and save
			 * the reply for non-idempotent rpc's.
			 * Otherwise invalidate entry by setting the timestamp
			 * to nil.
			 */
			if (repvalid) {
				rp->rc_timestamp = time.tv_sec;
				if (nonidempotent[nd->nd_procnum]) {
					if (repliesstatus[nd->nd_procnum]) {
						rp->rc_status = nd->nd_repstat;
						rp->rc_flag |= RC_REPSTATUS;
					} else {
						rp->rc_reply = m_copym(repmbuf,
							0, M_COPYALL, M_WAIT);
						rp->rc_flag |= RC_REPMBUF;
					}
				}
			} else {
				rp->rc_timestamp = 0;
			}
			rp->rc_flag &= ~RC_LOCKED;
			if (rp->rc_flag & RC_WANTED) {
				rp->rc_flag &= ~RC_WANTED;
				wakeup((caddr_t)rp);
			}
			return;
		}
	}
}

/*
 * Clean out the cache. Called when the last nfsd terminates.
 */
void
nfsrv_cleancache()
{
	register int i;
	register struct nfsrvcache *rp = nfsrvcache;
	register struct nfsrvcache *hp = &nfsrvcachehead;
	register union  rhead *rh = rhead;

	for (i = NFSRCHSZ; --i >= 0; rh++) {
		rh->rh_head[0] = rh;
		rh->rh_head[1] = rh;
	}
	hp->rc_next = hp->rc_prev = hp;
	for (i = NFSRVCACHESIZ; i-- > 0; ) {
		if (rp->rc_flag & RC_REPMBUF)
			m_freem(rp->rc_reply);
		if (rp->rc_flag & RC_NAM)
			m_freem(rp->rc_nam);
		rp->rc_state = RC_UNUSED;
		rp->rc_flag = 0;
		rp->rc_forw = rp;
		rp->rc_back = rp;
		rp->rc_next = hp->rc_next;
		hp->rc_next->rc_prev = rp;
		rp->rc_prev = hp;
		hp->rc_next = rp;
		rp++;
	}
}
