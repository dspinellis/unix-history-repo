/*	quota_sys.c	Melb 4.8	82/12/28	*/

/*
 *	various restrictions on the freedom usually offered by unix
 *	are imposed here
 */

#ifdef QUOTA		/* but naturally only if you want it all */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/quota.h"
#include "../h/mount.h"
#include "../h/fs.h"
#include "../h/share.h"
#include "../h/uio.h"
#include "../h/nami.h"

/*
 * quota cache - hash chain headers
 */
#define	NQHASH		29		/* a small prime */
#define	QHASH(uid)	((unsigned)(uid) % NQHASH)

struct	qhash	{
	struct	qhash	*qh_forw;	/* MUST be first */
	struct	qhash	*qh_back;	/* MUST be second */
};

struct	qhash	qhash[NQHASH];

/*
 * quota free list
 */
struct	quota	*qfreelist, **qfreetail;

/*
 * dquot cache - hash chain headers
 */

#define	NDQHASH		51		/* a smallish prime */
#define	DQHASH(uid, dev)	((unsigned)(((int)(dev)*4) + (uid)) % NDQHASH)

struct	dqhead	{
	struct	dqhead	*dqh_forw;	/* MUST be first */
	struct	dqhead	*dqh_back;	/* MUST be second */
};

struct	dqhead	dqhead[NDQHASH];

/*
 * dquot free list
 */
struct	dquot	*dqfreel, **dqback;

struct	dquot *dqp();

typedef	struct quota *Qptr;
typedef	struct dquot *DQptr;

/*
 * Initialize quota system (hash tables ...)
 */

qtinit()
{
	register i;

	{
		register struct quota *q = quota;
		register struct qhash *qh = qhash;

		for (i = NQHASH; --i >= 0; qh++)
			qh->qh_forw = qh->qh_back = qh;
		
		qfreelist = q;
		qfreetail = &q->q_freef;
		q->q_freeb = &qfreelist;
		q->q_forw = q;
		q->q_back = q;

		for (i = nquota; --i > 0; ) {
			++q;
			q->q_forw = q;
			q->q_back = q;
			*qfreetail = q;
			q->q_freeb = qfreetail;
			qfreetail = &q->q_freef;
		}

		q->q_freef = NOQUOT;
	}

	{
		register struct dquot *dq = dquot;
		register struct dqhead *dh = dqhead;

		for (i = NDQHASH; --i >= 0; dh++)
			dh->dqh_forw = dh->dqh_back = dh;

		dqfreel = dq;
		dqback = &dq->dq_freef;
		dq->dq_freeb = &dqfreel;
		dq->dq_forw = dq;
		dq->dq_back = dq;

		for (i = ndquot; --i > 0; ) {
			++dq;
			dq->dq_forw = dq;
			dq->dq_back = dq;
			*dqback = dq;
			dq->dq_freeb = dqback;
			dqback = &dq->dq_freef;
		}

		dq->dq_freef = NODQUOT;
	}
}

/*
 * Find the incore quota structure for a particular uid, or make one
 */

struct quota *
getquota(uid, nord, nodq)
register uid;
{
	register struct quota *q;	/* MUST be r10 */
	register struct qhash *qh;	/* MUST be r9 */
	register struct dquot **dqq;
	register struct mount *mp;
	register struct quota *qq;

	if ((q = u.u_quota) != NOQUOT && q->q_uid == uid)
		goto quick;
	qh = &qhash[QHASH(uid)];
	for (q = (Qptr)qh->qh_forw; q != (Qptr)qh; q = q->q_forw)
		if (q->q_uid == uid) {
			if (q->q_cnt == 0) {
				/*
				 * If quota isn't in use, & we don't want a
				 * new one, then just forget it
				 */
				if (nord)
					return(NOQUOT);
				/*
				 * Otherwise take it off the free list
				 */
				if ((qq = q->q_freef) != NOQUOT)
					qq->q_freeb = q->q_freeb;
				else
					qfreetail = q->q_freeb;
				*q->q_freeb = qq;

				q->q_rate = 0;	/* these were clobbered by */
				q->q_cost = 0;	/* the free list pointers */

				/*
				 * recover any lost dquot structs
				 */
				if (!nodq)
				for (dqq = q->q_dq, mp = mount;
				    dqq < &q->q_dq[NMOUNT]; dqq++, mp++)
					if (*dqq == LOSTDQUOT && mp->m_bufp) {
						*dqq = discquota(uid,
							      mp->m_qinod);
						if (*dqq != NODQUOT)
							(*dqq)->dq_own = q;
					}
			}
  quick:
			q->q_cnt++;
			while (q->q_flg & Q_LOCK) {
				q->q_flg |= Q_WANT;
				sleep((char *) q, PINOD+1);
			}
			if (q->q_cnt == 1)
				q->q_flg |= Q_NEW|nodq;
			return(q);
		}

	if (nord)			/* if we are not setting quota */
		return(NOQUOT);

	/*
	 * take the quota that is at the head of the free list
	 * (the longest unused quota)
	 */

	if ((q = qfreelist) == NOQUOT) {
		tablefull("quota");
		u.u_error = EUSERS;
		q = quota;		/* the su's slot - we must have one */
		q->q_cnt++;
		return(q);
	}

	/*
	 * There is one - it is free no longer
	 */
	
	if ((qq = q->q_freef) != NOQUOT)
		qq->q_freeb = &qfreelist;
	qfreelist = qq;

	/*
	 * Now we are about to change this from one user to another
	 * Must take this off hash chain for old user immediately, in
	 * case some other process claims it before we are done.
	 * We must then put it on the hash chain for the new user,
	 * to make sure that we don't make two quota structs for one uid.
	 * (the quota struct will then be locked till we are done)
	 */
#ifdef lint
	/* verfiy this is correct later */
	remque(q);
	insque(qh, q);
#else
	{ asm("remque	(r10),r0"); }
	{ asm("insque	(r10),(r9)"); }
#endif
	q->q_uid = uid;
	q->q_flg = Q_LOCK;
	q->q_cnt++;			/* q->q_cnt = 1; */

	/*
	 * Next, before filling in info for the new owning user,
	 * we must get rid of any dquot structs that we own
	 */
	for (mp = mount, dqq = q->q_dq; mp < &mount[NMOUNT]; mp++, dqq++)
		if (*dqq != NODQUOT && *dqq != LOSTDQUOT) {
			(*dqq)->dq_own = NOQUOT;
			putdq(mp, *dqq, 1);
		}
	
	q->q_lcnt = 0;
	q->q_rate = 0;
	q->q_cost = 0;
	q->q_class = 0;
	q->q_syflags = 0;
	q->q_usage = MINUSAGE;
	if (uid == 0) {
		q->q_plim = 0;
		q->q_shares = 0;
	} else {
		q->q_plim = MAXUPRC;
		q->q_shares = 1;
	}
	for (mp = mount, dqq = q->q_dq; dqq < &q->q_dq[NMOUNT]; mp++, dqq++)
		if (!nodq && mp->m_bufp) {
			*dqq = discquota(uid, mp->m_qinod);
			if (*dqq != NODQUOT) {
				if ((*dqq)->dq_uid != uid)
					panic("got bad quota uid");
				(*dqq)->dq_own = q;
			}
		} else
			*dqq = NODQUOT;

	if (q->q_flg & Q_WANT)
		wakeup((char *) q);

	q->q_flg = Q_NEW|nodq;
	return(q);
}

/*
 * If we can create a quota, so can we delete it again
 */

delquota(q)
register struct quota *q;
{
	register struct dquot **dqq;
	register struct mount *mp;

 top:
	if (q->q_cnt != 1) {
		q->q_cnt--;
		return;
	}
	if (q->q_flg & Q_LOCK) {
		q->q_flg |= Q_WANT;
		sleep((char *)q, PINOD+2);
		goto top;	/* just so we don't sync dquots if not needed */
				/* 'if' would be 'while' if this was deleted */
	}

	/*
	 * If we own dquot structs, sync them to disc, but don't release
	 * them - we might be recalled from the LRU chain
	 * As we will sit on the free list while we are waiting for that,
	 * if dquot structs run out, ours will be taken away.
	 */
	q->q_flg = Q_LOCK;
	if (!(q->q_flg & Q_NDQ)) {
		for (mp = mount, dqq = q->q_dq; dqq < &q->q_dq[NMOUNT];
		    dqq++, mp++) {
			if (mp->m_bufp)
				putdq(mp, *dqq, 0);
		}
	}
	if (q->q_flg & Q_WANT)
		wakeup((char *) q);

	/*
	 * this test looks unnecessary, but someone might have claimed this
	 * quota while we have been getting rid of the dquot info
	 */
	if (--q->q_cnt == 0) {		/* now able to be reallocated */
		if (qfreelist != NOQUOT) {
			*qfreetail = q;
			q->q_freeb = qfreetail;
		} else {
			qfreelist = q;
			q->q_freeb = &qfreelist;
		}
		q->q_freef = NOQUOT;
		qfreetail = &q->q_freef;
		q->q_flg = 0;
	} else
		q->q_flg &= ~(Q_LOCK|Q_WANT);
}

/*
 * Obtain the user's quota limit from the file specified
 */

struct dquot *
discquota(uid, ip)
register struct inode *ip;
{
	register struct dquot *dq;	/* MUST be r10 */
	register struct dqhead *dh;
	register struct dquot *dp;
	int fail;

	if (ip == NULL)
		return(NODQUOT);

	dh = &dqhead[DQHASH(uid, ip->i_dev)];
	for (dq = (DQptr)dh->dqh_forw; dq != (DQptr)dh; dq = dq->dq_forw) {

		if (dq->dq_uid == uid && dq->dq_dev == ip->i_dev) {

			if (dq->dq_cnt++ == 0) {
				if ((dp = dq->dq_freef) != NODQUOT)
					dp->dq_freeb = dq->dq_freeb;
				else
					dqback = dq->dq_freeb;
				*dq->dq_freeb = dp;
				dq->dq_own = NOQUOT;
			}
			/*
			 * we do this test after the previous one so that
			 * the dquot will be moved to the end of the free
			 * list - frequently accessed ones ought to hang around
			 */
			if (dq->dq_iq == 0 && dq->dq_quot == 0) {
				dqrele(dq);
				return(NODQUOT);
			}
			return(dq);
		}
	}

	if ((dq = dqalloc(uid, ip->i_dev)) == NODQUOT)
		return(dq);

	dq->dq_flg = DQ_LOCK;
	ilock(ip);
	fail = rdwri(UIO_READ, ip, (caddr_t)&dq->dq_dqb, sizeof (struct dqblk),
	    uid * sizeof (struct dqblk), 1, (int *)0);
	iunlock(ip);
	if (dq->dq_flg & DQ_WANT)
		wakeup((caddr_t)dq);
	dq->dq_flg = 0;

	if (fail) {				/* err getting quota */

#ifdef lint
		/* verify this is correct later */
		remque(dq);
#else
		{ asm("remque	(r10),r0"); }
#endif
		dq->dq_forw = dq;	/* on a private, unfindable hash list */
		dq->dq_back = dq;
		/*
		 * dqrele() (just below) will put dquot back on free list
		 */
	}

	if (fail || dq->dq_iq == 0 && dq->dq_quot == 0) { /* no quota exists */
		dqrele(dq);
		return(NODQUOT);
	}

	return(dq);
}

struct dquot *
dqalloc(uid, dev)
dev_t dev;
{
	register struct dquot *dq;	/* MUST be r11 */
	register struct dqhead *dh;	/* MUST be r10 */
	register struct dquot *dp;
	register struct quota *q;
	register struct mount *mp;
	static struct dqblk zdqb = { 0 };

 top:
	for (mp = mount; mp < &mount[NMOUNT]; mp++) {
		if (mp->m_dev == dev && mp->m_bufp) {
			if (mp->m_qinod == NULL) {
				u.u_error = EINVAL;
				return(NODQUOT);
			}
			break;
		}
	}
	if (mp >= &mount[NMOUNT]) {
		u.u_error = EINVAL;
		return(NODQUOT);
	}

	if ((dq = dqfreel) == NODQUOT && (q = qfreelist) != NOQUOT) {

		do {
			register struct dquot **dqq;
			register struct mount *mountp = mount;

			dqq = q->q_dq;
			while (dqq < &q->q_dq[NMOUNT]) {
				if ((dq = *dqq) != NODQUOT &&
				    dq != LOSTDQUOT) {
					if (dq->dq_cnt == 1) {
						*dqq = LOSTDQUOT;
						putdq(mountp, dq, 1);
						goto top;
					}
				}
				mountp++;
				dqq++;
			}
			q = q->q_freef;
		} while ((dq = dqfreel) == NODQUOT && q != NOQUOT);
	}

	if (dq == NODQUOT) {
		tablefull("dquot");
		u.u_error = EUSERS;
		return(dq);
	}

	if (dq->dq_flg & DQ_MOD)	/* shouldn't happen */
		panic("discquota");	/* we sync dquot before freeing it */

	/*
	 * now take the dquot off the free list
	 */
	if ((dp = dq->dq_freef) != NODQUOT)
		dp->dq_freeb = &dqfreel;
	dqfreel = dp;

	/*
	 * and off the hash chain it was on, & onto the new one
	 */
	dh = &dqhead[DQHASH(uid, dev)];
#ifdef lint
	/* verify this is correct sometime later */
	remque(dq);
	insque(dh, dq);
#else
	{ asm("remque	(r11),r0"); }
	{ asm("insque	(r11),(r10)"); }
#endif

	dq->dq_cnt = 1;
	dq->dq_flg = 0;
	dq->dq_uid = uid;
	dq->dq_dev = dev;
	dq->dq_dqb = zdqb;
	dq->dq_own = NOQUOT;
	return(dq);
}

/*
 * dqrele - layman's interface to putdq
 */
dqrele(dq)
register struct dquot *dq;
{
	register struct mount *mp;

	if (dq == NODQUOT || dq == LOSTDQUOT)
		return;
	if (dq->dq_cnt > 1) {
		dq->dq_cnt--;
		return;
	}

	for (mp = mount; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp && mp->m_dev == dq->dq_dev) {
			putdq(mp, dq, 1);
			return;
		}
	panic("dqrele");
}

/*
 * Update the disc quota in the quota file
 */

putdq(mp, dq, free)
	register struct mount *mp;
	register struct dquot *dq;
{
	register struct inode *ip;

	if (dq == NODQUOT || dq == LOSTDQUOT)
		return;
	if (free && dq->dq_cnt > 1) {
		dq->dq_cnt--;
		return;
	}
	if (!(dq->dq_flg & DQ_MOD)) {
		if (free) {
			dq->dq_cnt = 0;
 release:
			if (dqfreel != NODQUOT) {
				*dqback = dq;
				dq->dq_freeb = dqback;
			} else {
				dqfreel = dq;
				dq->dq_freeb = &dqfreel;
			}
			dq->dq_freef = NODQUOT;
			dqback = &dq->dq_freef;
		}
		return;
	}
	while (dq->dq_flg & DQ_LOCK) {
		dq->dq_flg |= DQ_WANT;
		sleep((char *)dq, PINOD+2);
		if (free && dq->dq_cnt > 1) {
			dq->dq_cnt--;
			return;
		}
	}
	dq->dq_flg |= DQ_LOCK;
	if ((ip = mp->m_qinod) == NULL)
		panic("lost quota file");
	ilock(ip);
	(void) rdwri(UIO_WRITE, ip, (caddr_t)&dq->dq_dqb, sizeof (struct dqblk),
	    dq->dq_uid * sizeof (struct dqblk), 1, (int *)0);
	iunlock(ip);
	if (dq->dq_flg & DQ_WANT)
		wakeup((char *)dq);
	dq->dq_flg &= ~(DQ_MOD|DQ_LOCK|DQ_WANT);
	if (free && --dq->dq_cnt == 0)
		goto release;
}

/*
 * See if there is a quota struct in core for user 'uid'
 */
struct quota *
qfind(uid)
	register int uid;
{
	register struct quota *q;
	register struct qhash *qh;

	if ((q = u.u_quota) != NOQUOT && q->q_uid == uid)
		return(q);

	if (uid == 0)		/* the second most likely case */
		return(quota);

	qh = &qhash[QHASH(uid)];
	for (q = (Qptr)qh->qh_forw; q != (Qptr)qh; q = q->q_forw)
		if (q->q_uid == uid)
			return(q);
	return(NOQUOT);
}

/*
 * To find the dquot that should be used for i/o on inode ip
 */

struct dquot *
inoquota(ip)
register struct inode *ip;
{
	register struct quota *q;
	register struct dquot **dqq;
	register struct mount *mp;
	register i;

 top:
	if ((q = qfind(ip->i_uid)) == NOQUOT) {
		for (mp = mount; mp < &mount[NMOUNT]; mp++)
			if (mp->m_bufp && mp->m_dev == ip->i_dev)
				return(discquota(ip->i_uid, mp->m_qinod));
		panic("inoquota");
	}

	/*
	 * We have a quota struct in core (most likely our own) that
	 * belongs to the same user as the inode
	 */
	
	if (q->q_flg & Q_NDQ)
		return(NODQUOT);

	if (q->q_flg & Q_LOCK) {
		q->q_flg |= Q_WANT;
		sleep((caddr_t) q, PINOD+1);
		goto top;		/* might just have been freed */
	}

	dqq = &q->q_dq[i = getfsx(ip->i_dev)];

	if (*dqq == LOSTDQUOT) {
		q->q_flg |= Q_LOCK;
		*dqq = discquota(q->q_uid, mount[i].m_qinod);
		if (*dqq != NODQUOT)
			(*dqq)->dq_own = q;
		if (q->q_flg & Q_WANT)
			wakeup((caddr_t) q);
		q->q_flg &= ~(Q_LOCK|Q_WANT);
	}
	if (*dqq != NODQUOT)
		(*dqq)->dq_cnt++;
	return(*dqq);
}

/*
 * Now to update disc usage, and take corrective action
 */

chkdq(ip, chg, f)
register struct inode *ip;
register long chg;			/* +/- num blocks alloc/free */
{
	register struct dquot *dq;

	if (chg == 0)
		return(0);

	dq = ip->i_dquot;
	if (dq == NODQUOT)
		return(0);

	if (dq->dq_quot == 0)
		return(0);

	dq->dq_flg |= DQ_MOD;

	if (chg < 0) {
		if ((int)dq->dq_blks + chg >= 0)
			dq->dq_blks += chg;
		else
			dq->dq_blks = 0;
		dq->dq_flg &= ~DQ_BLKS;
		if (dq->dq_blks < dq->dq_quot)
			dq->dq_dwarn = MAX_DQ_WARN;
		return(0);
	}

	if (dq->dq_blks < dq->dq_quot) {
		if ((dq->dq_blks += chg) < dq->dq_quot)
			return(0);
		if (dq->dq_own == u.u_quota)
			uprintf("\n\7WARNING: disc quota (%s) exceeded\7\n",
			    getfs(dq->dq_dev)->fs_fsmnt);
		return(0);
	}

	/*
	 * If user is over quota, & has run out of warnings, then he
	 * is not allowed any more space (except su's are never stopped)
	 */
	if (u.u_uid == 0)
		f = 1;
	if (!f && dq->dq_dwarn == 0) {
		if (!(dq->dq_flg & DQ_BLKS) && dq->dq_own == u.u_quota) {
		     uprintf("\n\7OVER DISC QUOTA: (%s) NO MORE DISC SPACE\7\n"
			, getfs(dq->dq_dev)->fs_fsmnt);
		     dq->dq_flg |= DQ_BLKS;
		}
		u.u_error = EDQUOT;
		return(1);
	}

	if (!f && dq->dq_blim && dq->dq_blks+chg >= dq->dq_blim) {
		if (!(dq->dq_flg & DQ_BLKS) && dq->dq_own == u.u_quota) {
			uprintf("\n\7DISC LIMIT REACHED (%s) - NO WRITE\7\n",
			    getfs(dq->dq_dev)->fs_fsmnt);
			dq->dq_flg |= DQ_BLKS;
		}
		u.u_error = EDQUOT;
		return(1);
	}

	/* here user is over quota, but not over limit */
	/* or is over limit, but we have been told there is nothing we can do */

	dq->dq_blks += chg;
	return(0);
}

/*
 * next - the inode limit check
 */

chkiq(dev, ip, uid, f)
register dev_t dev;
register struct inode *ip;
{
	register struct dquot *dq;
	register struct quota *q;

	if (ip == NULL)	{		/* allocation */
		if ((q = qfind(uid)) != NOQUOT)
			dq = dqp(q, dev);
		else
			dq = discquota(uid, mount[getfsx(dev)].m_qinod);
	} else {			/* free */
		dq = ip->i_dquot;
		if (dq != NODQUOT)
			dq->dq_cnt++;
	}

	if (dq == NODQUOT)
		return(0);

	if (dq->dq_iq == 0) {
		dqrele(dq);
		return(0);
	}

	dq->dq_flg |= DQ_MOD;

	if (ip) {			/* a free */
		if (dq->dq_inod)
			dq->dq_inod--;
		dq->dq_flg &= ~DQ_INODS;
		if (dq->dq_inod < dq->dq_iq)
			dq->dq_iwarn = MAX_IQ_WARN;
		dqrele(dq);
		return(0);
	}

	if (dq->dq_inod < dq->dq_iq) {
		if (++dq->dq_inod >= dq->dq_iq && dq->dq_own == u.u_quota)
			uprintf("\n\7WARNING - too many files (%s)\7\n",
			    getfs(dq->dq_dev)->fs_fsmnt);
		dqrele(dq);
		return(0);
	}

	/*
	 * the following shouldn't be necessary, as if u.u_uid == 0
	 * then dq == NODQUOT & we wouldn't get here at all, but
	 * then again, its not going to harm anything ...
	 */
	if (u.u_uid == 0)		/* su's musn't be stopped */
		f = 1;

	if (!f && dq->dq_iwarn == 0) {
		if (!(dq->dq_flg & DQ_INODS) && dq->dq_own == u.u_quota) {
			uprintf("\n\7OVER FILE QUOTA - NO MORE FILES (%s)\7\n",
			    getfs(dq->dq_dev)->fs_fsmnt);
			dq->dq_flg |= DQ_INODS;
		}
		u.u_error = EDQUOT;
		dqrele(dq);
		return(1);
	}

	if (!f && dq->dq_ilim && dq->dq_inod+1 >= dq->dq_ilim) {
		if (!(dq->dq_flg & DQ_INODS) && dq->dq_own == u.u_quota) {
		     uprintf("\n\7FILE LIMIT REACHED - CREATE FAILED (%s)\7\n",
			getfs(dq->dq_dev)->fs_fsmnt);
		     dq->dq_flg |= DQ_INODS;
		}
		u.u_error = EDQUOT;
		dqrele(dq);
		return(1);
	}

	/* over quota but not at limit */
	/* or over limit, but we aren't allowed to stop it */

	dq->dq_inod++;
	dqrele(dq);
	return(0);
}

/*
 * Now we finally come to the bit where we set the quota file up in the
 * first place (ie: where the system learns of its existance)
 */

opendq(mp)
register struct mount *mp;
{
	register struct inode *ip;
	register struct quota *q;
	register i;

	if (mp->m_qinod)
		closedq(mp);
	if ((ip = namei(uchar, LOOKUP, 1)) == NULL)
		return;
	iunlock(ip);
	if (ip->i_dev != mp->m_dev) {
		u.u_error = EACCES;
		return;
	}
	if ((ip->i_mode & IFMT) != IFREG) {
		u.u_error = EACCES;
		return;
	}
	mp->m_qinod = ip;
	i = mp-mount;
	for (q = quota; q < quotaNQUOTA; q++)
		if (!(q->q_flg&Q_NDQ)) {
			if (q->q_cnt) {
				q->q_cnt++;	/* cannot be released */
				q->q_dq[i] = discquota(q->q_uid, ip);
				delquota(q);
			} else
				q->q_dq[i] = LOSTDQUOT;
		}
}

closedq(mp)
register struct mount *mp;
{
	register struct dquot *dq;		/* known to be r10 */
	register i = mp-mount;
	register struct quota *q;
	register struct inode *ip;

	if (mp->m_qinod == NULL)
		return;

	for (ip = inode; ip < inodeNINODE; ip++)
		if (ip->i_dev == mp->m_dev) {
			dq = ip->i_dquot;
			ip->i_dquot = NODQUOT;
			putdq(mp, dq, 1);
		}

	for (q = quota; q < quotaNQUOTA; q++) {
		if (!(q->q_flg&Q_NDQ)) {
			if (q->q_cnt) {
				q->q_cnt++;
				putdq(mp, q->q_dq[i], 1);
				delquota(q);
			} else
				putdq(mp, q->q_dq[i], 1);
		}
		q->q_dq[i] = NODQUOT;
	}

	/*
	 * move all dquot's that used to refer to this quota
	 * file of into the never-never (they will eventually
	 * fall off the head of the free list and be re-used)
	 */
	for (dq = dquot; dq < dquotNDQUOT; dq++)
		if (dq->dq_dev == mp->m_dev) {
			if (dq->dq_cnt)
				panic("closedq: stray dquot");
#ifdef lint
			/* verify this is correct later */
			remque(dq);
#else
			asm("remque (r10),r0");
#endif
			dq->dq_forw = dq;
			dq->dq_back = dq;
			dq->dq_dev = NODEV;
		}

	irele(mp->m_qinod);
	mp->m_qinod = NULL;
}

/*
 * the sys call that tells the system about a quota file
 */

setquota()
{
	register struct a {
		char *fblk;
		char *fname;
	} *uap;
	register struct mount *mp;
	register dev_t dev;

	uap = (struct a *)u.u_ap;
	dev = getmdev();
	if (u.u_error)
		return;
	u.u_dirp = (caddr_t) uap->fname;

	for (mp = mount; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp && mp->m_dev == dev) {
			if (uap->fname == NULL)
				closedq(mp);
			else
				opendq(mp);
			return;
		}
	u.u_error = EINVAL;
}

/*
 * sys call to allow users to find out their current position wrt quota's
 * and to allow super users to alter it
 *
 * also sets/gets various stuff related to MUSH
 */
qquota()
{
	register struct quota *q;
	register struct dquot *dq;
	register uid;
	register struct a {
		int	cmd;
		int	uid;
		int	arg;
		caddr_t	addr;
	} *uap;
	struct dqblk newlim;
	dlim_t usage[2];
	int f;
	data_t dat;

	uap = (struct a *)u.u_ap;
	if ((uid = uap->uid) < 0)
		uid = u.u_ruid;
	else if (uid != u.u_ruid && uid != u.u_quota->q_uid && !suser())
		return;

	f = 0;
	switch (uap->cmd) {
	case Q_SYNC:
	case Q_LOGIN:
	case Q_ISLOGIN:
	case Q_FLOGIN:
	case Q_SCURCL:
	case Q_GCURCL:
	case Q_SETCOST:
	case Q_GETCOST:
#ifdef	notdef
	case Q_ATJOB:
#endif
	case Q_SETUID:		/* the getquota must be done, but differently */
		break;

	case Q_PCOUNT:
	case Q_LCOUNT:
	case Q_ACOUNT:
	case Q_USAGE:
	case Q_SUSAGE:
	case Q_SCLASS:
	case Q_SPLIMIT:
	case Q_SFLAGS:
	case Q_SSHARE:
	case Q_SUINFO:
	case Q_GUINFO:
	case Q_NICE:
	case Q_KILL:
	case Q_DOWARN:
		f = 1;
		/* FALL THROUGH */
	default:
		q = getquota(uid, f, 0);
		if (q == NOQUOT) {		/* only where f == 1 */
			u.u_error = ESRCH;
			return;
		}
		if (u.u_error) {
			delquota(q);
			return;
		}
		break;
	}

	switch (uap->cmd) {

	default:
		u.u_error = EINVAL;
		break;

	case Q_SETDLIM:
	{
		register struct inode *ip;
		register struct dquot *odq;

		if (!suser())
			break;
		f = getfsx(uap->arg);
		if (f < 0 || f >= NMOUNT) {
			u.u_error = EINVAL;
			break;
		}
		if ((dq = dqp(q, uap->arg)) == NODQUOT) {
			if ((dq = dqalloc(q->q_uid, uap->arg)) == NODQUOT)
				break;
			dq->dq_cnt++;
			dq->dq_own = q;
			q->q_dq[f] = dq;
			odq = NODQUOT;
		} else
			odq = dq;

		if (dq->dq_uid != q->q_uid)
			panic("SETDLIM bad uid");
		while (dq->dq_flg & DQ_LOCK) {
			dq->dq_flg |= DQ_WANT;
			sleep((caddr_t)dq, PINOD+1);
		}

		u.u_error = copyin(uap->addr, (caddr_t)&newlim,
			sizeof(struct dqblk));
		if (u.u_error) {
			if (dq != odq) {
				q->q_dq[f] = odq;
				dq->dq_cnt--;
			}
			dqrele(dq);
			break;
		}
		bcopy((caddr_t)&newlim, (caddr_t)&dq->dq_dqb,
		    sizeof(struct dqblk));
		
		dq->dq_flg |= DQ_MOD;

		dqrele(dq);
		if (dq->dq_iq == 0 && dq->dq_quot == 0) {
			q->q_dq[f] = NODQUOT;
			dq->dq_own = NOQUOT;
			dqrele(dq);
			if (dq->dq_cnt == 0)	/* no files open using quota */
				break;		/* this is just for speed */
			dq = NODQUOT;
		}
		if (dq != odq) {
			for (ip = inode; ip < inodeNINODE; ip++)
				if (ip->i_uid == q->q_uid &&
				    ip->i_dev == uap->arg) {
					if (dq == NODQUOT)
						dqrele(ip->i_dquot);
					else
						dq->dq_cnt++;
					ip->i_dquot = dq;
				}
		}
		break;
	}

	case Q_GETDLIM:
		if ((dq = dqp(q, uap->arg)) == NODQUOT) {
			u.u_r.r_val1 = 1;
			break;
		}
		u.u_error = copyout((caddr_t)&dq->dq_dqb, uap->addr,
		    sizeof(struct dqblk));
		dqrele(dq);
		break;
	
	case Q_SETDUSE:
		if (!suser())
			break;
		if ((dq = dqp(q, uap->arg)) == NODQUOT) {
			u.u_r.r_val1 = 1;
			break;
		}
		while (dq->dq_flg & DQ_LOCK) {
			dq->dq_flg |= DQ_WANT;
			sleep((caddr_t)dq, PINOD+1);
		}
		if (dq->dq_uid != q->q_uid)
			panic("SETDUSE bad uid");
		u.u_error = copyin(uap->addr, (caddr_t)usage,
			2*sizeof(dlim_t));
		if (u.u_error) {
			dqrele(dq);
			break;
		}
		dq->dq_inod = usage[0];
		dq->dq_blks = usage[1];
		if (dq->dq_inod < dq->dq_iq)
			dq->dq_iwarn = MAX_IQ_WARN;
		if (dq->dq_blks < dq->dq_quot)
			dq->dq_dwarn = MAX_DQ_WARN;
		dq->dq_flg &= ~(DQ_INODS|DQ_BLKS);
		dq->dq_flg |= DQ_MOD;
		dqrele(dq);
		break;

	case Q_SETWARN:
		if (!suser())
			break;
		if ((dq = dqp(q, uap->arg)) == NODQUOT) {
			u.u_r.r_val1 = 1;
			break;
		}
		while (dq->dq_flg & DQ_LOCK) {
			dq->dq_flg |= DQ_WANT;
			sleep((caddr_t)dq, PINOD+1);
		}
		if (dq->dq_uid != q->q_uid)
			panic("SETWARN bad uid");
		u.u_error = copyin(uap->addr, (caddr_t)usage,
			2*sizeof(dlim_t));
		if (u.u_error) {
			dqrele(dq);
			break;
		}
		dq->dq_iwarn = usage[0];
		dq->dq_dwarn = usage[1];
		dq->dq_flg &= ~(DQ_INODS|DQ_BLKS);
		dq->dq_flg |= DQ_MOD;
		dqrele(dq);
		break;

	case Q_DOWARN:
		if (!suser() || u.u_ttyp == NULL)
			break;
		if (uap->arg != NODEV) {
			if ((dq = dqp(q, uap->arg)) != NODQUOT) {
				qwarn(dq);
				dqrele(dq);
			}
		} else {
			register struct dquot **dqq;

			for (dqq = q->q_dq; dqq < &q->q_dq[NMOUNT]; dqq++)
				if ((dq = *dqq) != NODQUOT && dq != LOSTDQUOT)
					qwarn(dq);
		}
		break;
	
	case Q_SYNC:
	    {
		register struct mount *mp;
		register i;

		if (!suser())
			return;
		for (mp = mount, i = 0; mp < &mount[NMOUNT]; mp++, i++)
			if (mp->m_bufp && mp->m_qinod &&
			    (uap->arg == NODEV || uap->arg == mp->m_dev)) {
				for (q = quota; q < quotaNQUOTA; q++)
					if (q->q_cnt) {
						q->q_cnt++;
						putdq(mp, q->q_dq[i], 0);
						delquota(q);
					}
			}
		return;
	    }

	case Q_LOGIN:
		if (suser()) {
			if (!(u.u_qflags & QUF_LOGIN))
				u.u_quota->q_lcnt++;
			u.u_procp->p_flag |= SLOGIN;
			u.u_qflags |= QUF_LOGIN;
		}
		return;
	
	case Q_FLOGIN:
		if (suser()) {
			if (!(u.u_qflags & QUF_LOGIN))
				u.u_quota->q_lcnt++;
			u.u_qflags |= QUF_LOGIN;
		}
		return;

#ifdef	notdef
	case Q_ATJOB:
		if (suser()) {
			if (!(u.u_qflags & QUF_ATJ))
				u.u_quota->q_acnt++;
			u.u_qflags |= QUF_ATJ;
			u.u_procp->p_flag |= SATJOB;
			dat.d_uid = u.u_quota->q_uid;
			dat.d_sdat = u.u_procp->p_pid;
			dat.d_req = MM_ATJOB;
		}
		return;
#endif

	case Q_LCOUNT:
		u.u_error = copyout((caddr_t)&q->q_lcnt, uap->addr,
			sizeof q->q_lcnt);
		break;

	case Q_PCOUNT:
		u.u_error = copyout((caddr_t)&q->q_cnt, uap->addr,
			sizeof q->q_cnt);
		break;

	case Q_ACOUNT:
		u.u_error = copyout((caddr_t)&q->q_acnt, uap->addr,
			sizeof q->q_acnt);
		break;

	case Q_USAGE:
		u.u_error = copyout((caddr_t)&q->q_usage, uap->addr,
			sizeof q->q_usage);
		break;

	case Q_SFLAGS:
		if (!suser())
			break;
		u.u_error = copyin(uap->addr, (caddr_t)&q->q_syflags,
		    sizeof q->q_syflags);
		break;

	case Q_SUSAGE:
		if (!suser())
			break;
		u.u_error = copyin(uap->addr, (caddr_t)&q->q_usage,
			sizeof q->q_usage);
		break;
	
	case Q_SPLIMIT:
		if (!suser())
			break;
		u.u_error = copyin(uap->addr, (caddr_t)&q->q_plim,
			sizeof q->q_plim);
		break;
	
	case Q_SSHARE:
		if (!suser())
			break;
		u.u_error = copyin(uap->addr,(caddr_t)&q->q_shares,
			sizeof q->q_shares);
		break;

	case Q_ISLOGIN:
		u.u_r.r_val1 = (u.u_procp->p_flag & SLOGIN) != 0;
		return;

	case Q_SCLASS:
		if (!suser())
			break;
		u.u_error = copyin(uap->addr, (caddr_t)&q->q_class,
			sizeof q->q_class);
		break;

	case Q_SCURCL:
		if (!suser())
			return;
		u.u_error = copyin(uap->addr, (caddr_t)curclass,
			sizeof curclass);
		dat.d_req = MM_NEWCLASS;
		dat.d_uid = 0;
		msgto(MUSHPID, dat);
		return;

	case Q_GCURCL:
		u.u_error = copyout((caddr_t)curclass, uap->addr,
			sizeof curclass);
		return;
	
	case Q_SETUID:
		if (uid == u.u_quota->q_uid)
			return;
		if (!suser())
			return;
		q = getquota(uid, 0, uap->arg ? Q_NDQ : 0);
		if (q->q_plim && q->q_cnt > q->q_plim) {
			u.u_error = EPROCLIM;
			break;
		}
		qclean();
		qstart(q);
		return;				/* no delquota of q */

	case Q_SETCOST:
		if (!suser())
			return;
		u.u_error = copyin(uap->addr, (caddr_t)&shconsts,
			sizeof shconsts);
		return;

	case Q_GETCOST:
		if (uap->arg && u.u_uid == 0)
			evalshare();
		u.u_error = copyout((caddr_t)&shconsts, uap->addr,
			sizeof shconsts);
		return;

	case Q_SUINFO:
		if (!suser())
			return;
		u.u_error = copyin(uap->addr, (caddr_t)&q->q_uinfo,
			sizeof q->q_uinfo);
		break;

	case Q_GUINFO:
		u.u_error = copyout((caddr_t)&q->q_uinfo, uap->addr,
			sizeof q->q_uinfo);
		break;

	case Q_KILL:
	    {
		register struct proc *p;

		if (uap->arg <= 0 || uap->arg > NSIG) {
			u.u_error = EINVAL;
			break;
		}
		for (p = proc; p < procNPROC; p++)
			if (p->p_quota == q)
				psignal(p, uap->arg);
		break;
	    }

	case Q_NICE:
	    {
		register struct proc *p;

		if ((uap->arg < 0 || uap->arg >= 2*NZERO) && !suser())
			break;
		for (p = proc; p < procNPROC; p++)
			if (p->p_quota == q)
				donice(p, uap->arg);
		break;
	    }

	}

	delquota(q);
}

struct dquot *
dqp(q, dev)
	struct quota *q;
	dev_t dev;
{
	register struct dquot **dqq;
	register i;

	if (q == NOQUOT || q->q_flg & Q_NDQ)
		return(NODQUOT);
	i = getfsx(dev);
	if (i < 0 || i >= NMOUNT)
		return(NODQUOT);
	dqq = &q->q_dq[i];
	if (*dqq == LOSTDQUOT) {
		*dqq = discquota(q->q_uid, mount[i].m_qinod);
		if (*dqq != NODQUOT)
			(*dqq)->dq_own = q;
	}
	if (*dqq != NODQUOT)
		(*dqq)->dq_cnt++;
	return(*dqq);
}

/*
 * quota cleanup at process exit, or when switching to another user
 */

qclean()
{
	register struct proc *p = u.u_procp;
	register struct quota *q = p->p_quota;
#ifdef MUSH
	DATA_T dat;
	int err = u.u_error;

	dat.d_req = 0;
#endif
	if (q == NOQUOT)
		return;
	if (u.u_qflags & QUF_LOGIN) {	/* this was a login process */
		if (q->q_lcnt)
			q->q_lcnt--;
		if (q->q_syflags & (QF_KASYNC|QF_NASYNC)) {
			/* here we race around & kill or nice all descendants */
			/* which is not really easy to do, as some of them */
			/* might now be owned by init */
			/* maybe we won't implement this for a while !!! */

			/* HINT: make p_pptr point to grandparent rather */
			/* than init when daddy dies (leave ppid == 1) */
			/* then can use p_cptr & exhaustive tree walk to */
			/* do this (p_cptr of parent will point at grandchild */
			/* if natural child / parent has died already) */
			/* will need some tests for p_ppid != 1 in cases */
			/* where this isn't needed now, but otherwise looks ok*/
		}
#ifdef MUSH
		dat.d_req = MM_LOGOUT;
#endif
	}
#ifdef notdef
	if (u.u_qflags & QUF_ATJ && q->q_acnt)
		q->q_acnt--;
#endif
#ifdef MUSH
	if (q->q_cnt == 1)
		dat.d_req = MM_PROCX;
	if (dat.d_req && (dat.d_uid = q->q_uid)) {
		dat.d_info = q->q_lcnt;
		dat.d_fdat = q->q_usage;
		msgto(MUSHPID, dat);
		u.u_error = err;
	}
#endif
	/*
	 * before we rid ourselves of this quota, we must be sure that
	 * we no longer reference it (otherwise clock might do nasties).
	 * But we have to have some quota (or clock will get upset).
	 * (Who is this clock anyway ??). So we will give ourselves
	 * root's quota for a short while, without counting this as
	 * a reference in the ref count (as either this proc is just
	 * about to die, in which case it refers to nothing, or it is
	 * about to be given a new quota, which will just overwrite this
	 * one).
	 */
	p->p_quota = quota;
	u.u_quota = quota;

	delquota(q);
}

qstart(q)
register struct quota *q;
{
#ifdef MUSH
	DATA_T dat;
	int err = u.u_error;
#endif

	u.u_quota = q;
	u.u_procp->p_quota = q;
	if (u.u_qflags & QUF_LOGIN)
		q->q_lcnt++;
#ifdef notdef
	if (u.u_qflags & QUF_ATJ)
		q->q_acnt++;
#endif
#ifdef MUSH
	if (q->q_flg & Q_NEW) { 	/* first proc for user */
		q->q_flg &= ~Q_NEW;
		dat.d_req = MM_PROC1;
	} else if (u.u_qflags & QUF_LOGIN)
		dat.d_req = MM_LOGIN;
#ifdef notdef
	else if (u.u_procp->p_flag & SATJOB)
		dat.d_req = MM_ATJOB;
#endif
	else
		return;
	if ((dat.d_uid = q->q_uid) == 0)
		return;
	dat.d_info = q->q_lcnt;
	dat.d_xdat = q->q_acnt;
	if (u.u_ttyp)
		dat.d_sdat = (short)u.u_ttyd;
	else
		dat.d_sdat = NODEV;
	msgto(MUSHPID, dat);
	u.u_error = err;
#endif
}

qwarn(dq)
register struct dquot *dq;
{
	register struct fs *fs = NULL;

	if (dq->dq_iq && dq->dq_inod >= dq->dq_iq) {
		dq->dq_flg |= DQ_MOD;
		fs = getfs(dq->dq_dev);
		if (dq->dq_iwarn && --dq->dq_iwarn)
			uprintf(
			    "Warning: too many files on %s, %d warning%s left\n"
			    , fs->fs_fsmnt
			    , dq->dq_iwarn
			    , dq->dq_iwarn > 1 ? "s" : ""
			);
		else
			uprintf(
			    "\7WARNING: too many files on %s, NO MORE !!\7\n"
			    , fs->fs_fsmnt
			);
	}

	if (dq->dq_quot && dq->dq_blks >= dq->dq_quot) {
		dq->dq_flg |= DQ_MOD;
		if (!fs)
			fs = getfs(dq->dq_dev);
		if (dq->dq_dwarn && --dq->dq_dwarn)
			uprintf(
		    "Warning: too much disc space on %s, %d warning%s left\n"
			    , fs->fs_fsmnt
			    , dq->dq_dwarn
			    , dq->dq_dwarn > 1 ? "s" : ""
			);
		else
			uprintf(
		    "\7WARNING: too much disc space on %s, NO MORE !!\7\n"
			    , fs->fs_fsmnt
			);
	}
}

#endif	QUOTA
