/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)quota_kern.c	7.4 (Berkeley) 6/29/88
 */

#ifdef QUOTA
/*
 * MELBOURNE QUOTAS
 *
 * Code pertaining to management of the in-core data structures.
 */
#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "inode.h"
#include "quota.h"
#include "mount.h"
#include "fs.h"
#include "uio.h"

/*
 * Quota cache - hash chain headers.
 */
#define	NQHASH		32	/* small power of two */
#define	QHASH(uid)	((unsigned)(uid) & (NQHASH-1))

struct	qhash	{
	struct	qhash	*qh_forw;	/* MUST be first */
	struct	qhash	*qh_back;	/* MUST be second */
};

struct	qhash	qhash[NQHASH];

/*
 * Quota free list.
 */
struct	quota	*qfreelist, **qfreetail;
typedef	struct quota *Qptr;

/*
 * Dquot cache - hash chain headers.
 */
#define	NDQHASH		51		/* a smallish prime */
#define	DQHASH(uid, dev) \
	((unsigned)(((int)(dev) * 4) + (uid)) % NDQHASH)

struct	dqhead	{
	struct	dqhead	*dqh_forw;	/* MUST be first */
	struct	dqhead	*dqh_back;	/* MUST be second */
};

struct	dqhead	dqhead[NDQHASH];

/*
 * Dquot free list.
 */
struct	dquot	*dqfreel, **dqback;

typedef	struct dquot *DQptr;

/*
 * Initialize quota caches.
 */
qtinit()
{
	register i;
	register struct quota *q = quota;
	register struct qhash *qh = qhash;
	register struct dquot *dq = dquot;
	register struct dqhead *dh = dqhead;

	/*
	 * First the cache of structures assigned users.
	 */
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
	q->q_freef = NOQUOTA;
	/*
	 * Next, the cache between the in-core structures
	 * and the per-filesystem quota files on disk.
	 */
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

/*
 * Find an incore quota structure for a particular uid,
 * or make one.  If lookuponly is non-zero, just the lookup is performed.
 * If nodq is non-zero, the dquot structures are left uninitialized.
 */
struct quota *
getquota(uid, lookuponly, nodq)
	register uid_t uid;
	int lookuponly, nodq;
{
	register struct quota *q;
	register struct qhash *qh;
	register struct dquot **dqq;
	register struct mount *mp;
	register struct quota *qq;

	/*
	 * Fast check to see if an existing structure
	 * can be reused with just a reference count change.
	 */
	q = u.u_quota;
	if (q != NOQUOTA && q->q_uid == uid)
		goto quick;
	/*
	 * Search the quota chache for a hit.
	 */
	qh = &qhash[QHASH(uid)];
	for (q = (Qptr)qh->qh_forw; q != (Qptr)qh; q = q->q_forw) {
		if (q->q_uid == uid) {
			if (q->q_cnt == 0) {
				if (lookuponly)
					return (NOQUOTA);
				/*
				 * Take it off the free list.
				 */
				if ((qq = q->q_freef) != NOQUOTA)
					qq->q_freeb = q->q_freeb;
				else
					qfreetail = q->q_freeb;
				*q->q_freeb = qq;

				/*
				 * Recover any lost dquot structs.
				 */
				if (!nodq)
				for (dqq = q->q_dq, mp = mount;
				    dqq < &q->q_dq[NMOUNT]; dqq++, mp++)
					if (*dqq == LOSTDQUOT && mp->m_fs) {
						*dqq = discquota(uid,
							      mp->m_qinod);
						if (*dqq != NODQUOT)
							(*dqq)->dq_own = q;
					}
			}
  quick:
			q->q_cnt++;
			while (q->q_flags & Q_LOCK) {
				q->q_flags |= Q_WANT;
				sleep((caddr_t) q, PINOD+1);
			}
			if (q->q_cnt == 1)
				q->q_flags |= Q_NEW | nodq;
			return (q);
		}
	}
	if (lookuponly)
		return (NOQUOTA);
	/*
	 * Take the quota that is at the head of the free list
	 * (the longest unused quota).
	 */
	q = qfreelist;
	if (q == NOQUOTA) {
		tablefull("quota");
		u.u_error = EUSERS;
		q = quota;		/* the su's slot - we must have one */
		q->q_cnt++;
		return (q);
	}
	/*
	 * There is one - it is free no longer.
	 */
	qq = q->q_freef;
	if (qq != NOQUOTA)
		qq->q_freeb = &qfreelist;
	qfreelist = qq;
	/*
	 * Now we are about to change this from one user to another
	 * Must take this off hash chain for old user immediately, in
	 * case some other process claims it before we are done.
	 * We must then put it on the hash chain for the new user,
	 * to make sure that we don't make two quota structs for one uid.
	 * (the quota struct will then be locked till we are done).
	 */
	remque(q);
	insque(q, qh);
	q->q_uid = uid;
	q->q_flags = Q_LOCK;
	q->q_cnt++;			/* q->q_cnt = 1; */
	/*
	 * Next, before filling in info for the new owning user,
	 * we must get rid of any dquot structs that we own.
	 */
	for (mp = mount, dqq = q->q_dq; mp < &mount[NMOUNT]; mp++, dqq++)
		if (*dqq != NODQUOT && *dqq != LOSTDQUOT) {
			(*dqq)->dq_own = NOQUOTA;
			putdq(mp, *dqq, 1);
		}
	for (mp = mount, dqq = q->q_dq; dqq < &q->q_dq[NMOUNT]; mp++, dqq++)
		if (!nodq && mp->m_fs) {
			*dqq = discquota(uid, mp->m_qinod);
			if (*dqq != NODQUOT) {
				if ((*dqq)->dq_uid != uid)
					panic("got bad quota uid");
				(*dqq)->dq_own = q;
			}
		} else
			*dqq = NODQUOT;
	if (q->q_flags & Q_WANT)
		wakeup((caddr_t)q);
	q->q_flags = Q_NEW | nodq;
	return (q);
}

/*
 * Delete a quota, wakeup anyone waiting.
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
	if (q->q_flags & Q_LOCK) {
		q->q_flags |= Q_WANT;
		sleep((caddr_t)q, PINOD+2);
		/*
		 * Just so we don't sync dquots if not needed;
		 * 'if' would be 'while' if this was deleted.
		 */
		goto top;
	}

	/*
	 * If we own dquot structs, sync them to disc, but don't release
	 * them - we might be recalled from the LRU chain.
	 * As we will sit on the free list while we are waiting for that,
	 * if dquot structs run out, ours will be taken away.
	 */
	q->q_flags = Q_LOCK;
	if ((q->q_flags & Q_NDQ) == 0) {
		mp = mount;
		for (dqq = q->q_dq; dqq < &q->q_dq[NMOUNT]; dqq++, mp++)
			if (mp->m_fs)
				putdq(mp, *dqq, 0);
	}
	if (q->q_flags & Q_WANT)
		wakeup((caddr_t)q);

	/*
	 * This test looks unnecessary, but someone might have claimed this
	 * quota while we have been getting rid of the dquot info
	 */
	if (--q->q_cnt == 0) {		/* now able to be reallocated */
		if (qfreelist != NOQUOTA) {
			*qfreetail = q;
			q->q_freeb = qfreetail;
		} else {
			qfreelist = q;
			q->q_freeb = &qfreelist;
		}
		q->q_freef = NOQUOTA;
		qfreetail = &q->q_freef;
		q->q_flags = 0;
	} else
		q->q_flags &= ~(Q_LOCK|Q_WANT);
}

/*
 * Obtain the user's on-disk quota limit
 * from the file specified.
 */
struct dquot *
discquota(uid, ip)
	uid_t uid;
	register struct inode *ip;
{
	register struct dquot *dq;
	register struct dqhead *dh;
	register struct dquot *dp;
	int fail;

	if (ip == NULL)
		return (NODQUOT);
	/*
	 * Check the cache first.
	 */
	dh = &dqhead[DQHASH(uid, ip->i_dev)];
	for (dq = (DQptr)dh->dqh_forw; dq != (DQptr)dh; dq = dq->dq_forw) {
		if (dq->dq_uid != uid || dq->dq_dev != ip->i_dev)
			continue;
		/*
		 * Cache hit with no references.  Take
		 * the structure off the free list.
		 */
		if (dq->dq_cnt++ == 0) {
			dp = dq->dq_freef;
			if (dp != NODQUOT)
				dp->dq_freeb = dq->dq_freeb;
			else
				dqback = dq->dq_freeb;
			*dq->dq_freeb = dp;
			dq->dq_own = NOQUOTA;
		}
		/*
		 * We do this test after the previous one so that
		 * the dquot will be moved to the end of the free
		 * list - frequently accessed ones ought to hang around.
		 */
		if (dq->dq_isoftlimit == 0 && dq->dq_bsoftlimit == 0) {
			dqrele(dq);
			return (NODQUOT);
		}
		return (dq);
	}
	/*
	 * Not in cache, allocate a new one and
	 * bring info in off disk.
	 */
	dq = dqalloc(uid, ip->i_dev);
	if (dq == NODQUOT)
		return (dq);
	dq->dq_flags = DQ_LOCK;
	ILOCK(ip);
	fail = rdwri(UIO_READ, ip, (caddr_t)&dq->dq_dqb, sizeof (struct dqblk),
	    (off_t)uid * sizeof (struct dqblk), 1, (int *)0);
	IUNLOCK(ip);
	if (dq->dq_flags & DQ_WANT)
		wakeup((caddr_t)dq);
	dq->dq_flags = 0;
	/*
	 * I/O error in reading quota file, release
	 * quota structure and reflect problem to caller.
	 */
	if (fail) {
		remque(dq);
		dq->dq_forw = dq;	/* on a private, unfindable hash list */
		dq->dq_back = dq;
		/* dqrele() (just below) will put dquot back on free list */
	}
	/* no quota exists */
	if (fail || dq->dq_isoftlimit == 0 && dq->dq_bsoftlimit == 0) {
		dqrele(dq);
		return (NODQUOT);
	}
	return (dq);
}

/*
 * Allocate a dquot structure.  If there are
 * no free slots in the cache, flush LRU entry from
 * the cache to the appropriate quota file on disk.
 */
struct dquot *
dqalloc(uid, dev)
	uid_t uid;
	dev_t dev;
{
	register struct dquot *dq;
	register struct dqhead *dh;
	register struct dquot *dp;
	register struct quota *q;
	register struct mount *mp;
	static struct dqblk zdqb = { 0 };

 top:
	/*
	 * Locate inode of quota file for
	 * indicated file system in case i/o
	 * is necessary in claiming an entry.
	 */
	for (mp = mount; mp < &mount[NMOUNT]; mp++) {
		if (mp->m_dev == dev && mp->m_fs) {
			if (mp->m_qinod == NULL) {
				u.u_error = EINVAL;
				return (NODQUOT);
			}
			break;
		}
	}
	if (mp >= &mount[NMOUNT]) {
		u.u_error = EINVAL;
		return (NODQUOT);
	}
	/*
	 * Check free list.  If table is full, pull entries
	 * off the quota free list and flush any associated
	 * dquot references until something frees up on the
	 * dquot free list.
	 */
	if ((dq = dqfreel) == NODQUOT && (q = qfreelist) != NOQUOTA) {

		do {
			register struct dquot **dqq;
			register struct mount *mountp = mount;

			dqq = q->q_dq;
			while (dqq < &q->q_dq[NMOUNT]) {
				if ((dq = *dqq) != NODQUOT &&
				    dq != LOSTDQUOT) {
					/*
					 * Mark entry as "lost" due to
					 * scavenging operation.
					 */
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
		} while ((dq = dqfreel) == NODQUOT && q != NOQUOTA);
	}
	if (dq == NODQUOT) {
		tablefull("dquot");
		u.u_error = EUSERS;
		return (dq);
	}
	/*
	 * This shouldn't happen, as we sync
	 * dquot before freeing it up.
	 */
	if (dq->dq_flags & DQ_MOD)
		panic("discquota");

	/*
	 * Now take the dquot off the free list,
	 */
	dp = dq->dq_freef;
	if (dp != NODQUOT)
		dp->dq_freeb = &dqfreel;
	dqfreel = dp;
	/*
	 * and off the hash chain it was on, & onto the new one.
	 */
	dh = &dqhead[DQHASH(uid, dev)];
	remque(dq);
	insque(dq, dh);
	dq->dq_cnt = 1;
	dq->dq_flags = 0;
	dq->dq_uid = uid;
	dq->dq_dev = dev;
	dq->dq_dqb = zdqb;
	dq->dq_own = NOQUOTA;
	return (dq);
}

/*
 * dqrele - layman's interface to putdq.
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
	/*
	 * I/O required, find appropriate file system
	 * to sync the quota information to.
	 */
	for (mp = mount; mp < &mount[NMOUNT]; mp++)
		if (mp->m_fs && mp->m_dev == dq->dq_dev) {
			putdq(mp, dq, 1);
			return;
		}
	panic("dqrele");
}

/*
 * Update the disc quota in the quota file.
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
	/*
	 * Disk quota not modified, just discard
	 * or return (having adjusted the reference
	 * count), as indicated by the "free" param.
	 */
	if ((dq->dq_flags & DQ_MOD) == 0) {
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
	/*
	 * Quota modified, write back to disk.
	 */
	while (dq->dq_flags & DQ_LOCK) {
		dq->dq_flags |= DQ_WANT;
		sleep((caddr_t)dq, PINOD+2);
		/* someone could sneak in and grab it */
		if (free && dq->dq_cnt > 1) {
			dq->dq_cnt--;
			return;
		}
	}
	dq->dq_flags |= DQ_LOCK;
	if ((ip = mp->m_qinod) == NULL)
		panic("lost quota file");
	ILOCK(ip);
	(void) rdwri(UIO_WRITE, ip, (caddr_t)&dq->dq_dqb, sizeof (struct dqblk),
	    (off_t)dq->dq_uid * sizeof (struct dqblk), 1, (int *)0);
	IUNLOCK(ip);
	if (dq->dq_flags & DQ_WANT)
		wakeup((caddr_t)dq);
	dq->dq_flags &= ~(DQ_MOD|DQ_LOCK|DQ_WANT);
	if (free && --dq->dq_cnt == 0)
		goto release;
}

/*
 * See if there is a quota struct in core for user 'uid'.
 */
struct quota *
qfind(uid)
	register uid_t uid;
{
	register struct quota *q;
	register struct qhash *qh;

	/* 
	 * Check common cases first: asking for own quota,
	 * or that of the super user (has reserved slot 0
	 * in the table).
	 */
	q = u.u_quota;
	if (q != NOQUOTA && q->q_uid == uid)
		return (q);
	if (uid == 0)		/* the second most likely case */
		return (quota);
	/*
	 * Search cache.
	 */
	qh = &qhash[QHASH(uid)];
	for (q = (Qptr)qh->qh_forw; q != (Qptr)qh; q = q->q_forw)
		if (q->q_uid == uid)
			return (q);
	return (NOQUOTA);
}

/*
 * Set the quota file up for a particular file system.
 * Called as the result of a setquota system call.
 */
opendq(mp, fname)
	register struct mount *mp;
	caddr_t fname;
{
	register struct inode *ip;
	register struct quota *q;
	struct dquot *dq;
	register struct nameidata *ndp = &u.u_nd;
	int i;

	if (mp->m_qinod)
		closedq(mp);
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	IUNLOCK(ip);
	if (ip->i_dev != mp->m_dev) {
		u.u_error = EACCES;
		return;
	}
	if ((ip->i_mode & IFMT) != IFREG) {
		u.u_error = EACCES;
		return;
	}
	/*
	 * Flush in-core references to any previous
	 * quota file for this file system.
	 */
	mp->m_qinod = ip;
	i = mp - mount;
	for (q = quota; q < quotaNQUOTA; q++)
		if ((q->q_flags & Q_NDQ) == 0) {
			if (q->q_cnt == 0)
				q->q_dq[i] = LOSTDQUOT;
			else {
				q->q_cnt++;	/* cannot be released */
				dq = discquota(q->q_uid, ip);
				q->q_dq[i] = dq;
				if (dq != NODQUOT)
					dq->dq_own = q;
				delquota(q);
			}
		}
}

/*
 * Close off disc quotas for a file system.
 */
closedq(mp)
	register struct mount *mp;
{
	register struct dquot *dq;
	register i = mp - mount;
	register struct quota *q;
	register struct inode *ip;

	if (mp->m_qinod == NULL)
		return;
	/*
	 * Search inode table, delete any references
	 * to quota file being closed.
	 */
	for (ip = inode; ip < inodeNINODE; ip++)
		if (ip->i_dev == mp->m_dev) {
			dq = ip->i_dquot;
			ip->i_dquot = NODQUOT;
			putdq(mp, dq, 1);
		}
	/*
	 * Search quota table, flush any pending
	 * quota info to disk and also delete
	 * references to closing quota file.
	 */
	for (q = quota; q < quotaNQUOTA; q++) {
		if ((q->q_flags & Q_NDQ) == 0) {
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
	 * Move all dquot's that used to refer to this quota
	 * file of into the never-never (they will eventually
	 * fall off the head of the free list and be re-used).
	 */
	for (dq = dquot; dq < dquotNDQUOT; dq++)
		if (dq->dq_dev == mp->m_dev) {
			if (dq->dq_cnt)
				panic("closedq: stray dquot");
			remque(dq);
			dq->dq_forw = dq;
			dq->dq_back = dq;
			dq->dq_dev = NODEV;
		}
	irele(mp->m_qinod);
	mp->m_qinod = NULL;
}
#endif
