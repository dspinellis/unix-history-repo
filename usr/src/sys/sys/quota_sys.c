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
 *	@(#)quota_sys.c	7.4 (Berkeley) 6/29/88
 */

/*
 * MELBOURNE QUOTAS
 *
 * System calls.
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
 * The sys call that tells the system about a quota file.
 */
setquota()
{
	register struct a {
		char	*fblk;
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct mount *mp;
	dev_t dev;

#ifndef QUOTA
	u.u_error = EINVAL;
	return;
#else
	u.u_error = getmdev(&dev, uap->fblk);
	if (u.u_error)
		return;
	for (mp = mount; mp < &mount[NMOUNT]; mp++)
		if (mp->m_fs && mp->m_dev == dev) {
			if (uap->fname == NULL)
				closedq(mp);
			else
				opendq(mp, (caddr_t)uap->fname);
			return;
		}
#endif
}

/*
 * Sys call to allow users to find out
 * their current position wrt quota's
 * and to allow super users to alter it.
 */
qquota()
{
	register struct a {
		int	cmd;
		int	uid;
		int	arg;
		caddr_t	addr;
	} *uap = (struct a *)u.u_ap;
	register struct quota *q;

#ifndef QUOTA
	u.u_error = EINVAL;
	return;
#else
	if (uap->uid < 0)
		uap->uid = u.u_ruid;
	if (uap->uid != u.u_ruid && uap->uid != u.u_quota->q_uid && !suser())
		return;
	if (uap->cmd != Q_SYNC && uap->cmd != Q_SETUID) {
		q = getquota((uid_t)uap->uid, uap->cmd == Q_DOWARN, 0);
		if (q == NOQUOTA) {
			u.u_error = ESRCH;
			return;
		}
		if (u.u_error)
			goto bad;
	}
	switch (uap->cmd) {

	case Q_SETDLIM:
		u.u_error = setdlim(q, (dev_t)uap->arg, uap->addr);
		break;

	case Q_GETDLIM:
		u.u_error = getdlim(q, (dev_t)uap->arg, uap->addr);
		break;

	case Q_SETDUSE:
		u.u_error = setduse(q, (dev_t)uap->arg, uap->addr);
		break;

	case Q_SETWARN:
		u.u_error = setwarn(q, (dev_t)uap->arg, uap->addr);
		break;

	case Q_DOWARN:
		u.u_error = dowarn(q, (dev_t)uap->arg);
		break;
	
	case Q_SYNC:
		u.u_error = qsync((dev_t)uap->arg);
		return;

	case Q_SETUID:
		u.u_error = qsetuid((uid_t)uap->uid, uap->arg);
		return;

	default:
		u.u_error = EINVAL;
		break;
	}
bad:
	delquota(q);
#endif
}

#ifdef QUOTA
/*
 * Q_SETDLIM - assign an entire dqblk structure.
 */
setdlim(q, dev, addr)
	register struct quota *q;
	dev_t dev;
	caddr_t addr;
{
	register struct inode *ip;
	register struct dquot *dq, *odq;
	struct dqblk newlim;
	int index, error = 0;

	if (!suser())
		return (u.u_error);			/* XXX */
	index = getfsx(dev);
	if (index < 0 || index >= NMOUNT)
		return (ENODEV);
	dq = dqp(q, dev);
	if (dq == NODQUOT) {
		dq = dqalloc(q->q_uid, dev);
		if (dq == NODQUOT)
			return (error);
		dq->dq_cnt++;
		dq->dq_own = q;
		q->q_dq[index] = dq;
		odq = NODQUOT;
	} else
		odq = dq;

	if (dq->dq_uid != q->q_uid)
		panic("setdlim");
	while (dq->dq_flags & DQ_LOCK) {
		dq->dq_flags |= DQ_WANT;
		sleep((caddr_t)dq, PINOD+1);
	}
	error = copyin(addr, (caddr_t)&newlim, sizeof (struct dqblk));
	if (error) {
		if (dq != odq) {
			q->q_dq[index] = odq;
			dq->dq_cnt--;
		}
		dqrele(dq);
		return (error);
	}
	dq->dq_dqb = newlim;
	dq->dq_flags |= DQ_MOD;
	dqrele(dq);
	if (dq->dq_isoftlimit == 0 && dq->dq_bsoftlimit == 0) {
		q->q_dq[index] = NODQUOT;
		dq->dq_own = NOQUOTA;
		dqrele(dq);
		if (dq->dq_cnt == 0)	/* no files open using quota */
			return (error);
		dq = NODQUOT;
	}
	if (dq == odq)
		return (error);
	for (ip = inode; ip < inodeNINODE; ip++)
		if (ip->i_uid == q->q_uid && ip->i_dev == dev && ip->i_mode) {
			if (dq == NODQUOT)
				dqrele(ip->i_dquot);
			else
				dq->dq_cnt++;
			ip->i_dquot = dq;
		}
	return (error);
}

/*
 * Q_GETDLIM - return current values in a dqblk structure.
 */
getdlim(q, dev, addr)
	struct quota *q;
	dev_t dev;
	caddr_t addr;
{
	register struct dquot *dq;
	int error;

	dq = dqp(q, dev);
	if (dq == NODQUOT)
		return (ESRCH);
	error = copyout((caddr_t)&dq->dq_dqb, addr, sizeof (struct dqblk));
	dqrele(dq);
	return (error);
}

/*
 * Q_SETDUSE - set current inode and disc block totals.
 * Resets warnings and associated flags.
 */
setduse(q, dev, addr)
	register struct quota *q;
	dev_t dev;
	caddr_t addr;
{
	register struct dquot *dq;
	struct dqusage usage;
	int error = 0;

	if (!suser())
		return (u.u_error);
	dq = dqp(q, dev);
	if (dq == NODQUOT)
		return (ESRCH);
	while (dq->dq_flags & DQ_LOCK) {
		dq->dq_flags |= DQ_WANT;
		sleep((caddr_t)dq, PINOD+1);
	}
	if (dq->dq_uid != q->q_uid)
		panic("setduse");
	error = copyin(addr, (caddr_t)&usage, sizeof (usage));
	if (error == 0) {
		dq->dq_curinodes = usage.du_curinodes;
		dq->dq_curblocks = usage.du_curblocks;
		if (dq->dq_curinodes < dq->dq_isoftlimit)
			dq->dq_iwarn = MAX_IQ_WARN;
		if (dq->dq_curblocks < dq->dq_bsoftlimit)
			dq->dq_bwarn = MAX_DQ_WARN;
		dq->dq_flags &= ~(DQ_INODS | DQ_BLKS);
		dq->dq_flags |= DQ_MOD;
	}
	dqrele(dq);
	return (error);
}

/*
 * Q_SETWARN - set warning counters.
 */
setwarn(q, dev, addr)
	register struct quota *q;
	dev_t dev;
	caddr_t addr;
{
	register struct dquot *dq;
	int error = 0;
	struct dqwarn warn;

	if (!suser())
		return (u.u_error);			/* XXX */
	dq = dqp(q, dev);
	if (dq == NODQUOT)
		return (ESRCH);
	while (dq->dq_flags & DQ_LOCK) {
		dq->dq_flags |= DQ_WANT;
		sleep((caddr_t)dq, PINOD+1);
	}
	if (dq->dq_uid != q->q_uid)
		panic("setwarn");
	error = copyin(addr, (caddr_t)&warn, sizeof (warn));
	if (error == 0) {
		dq->dq_iwarn = warn.dw_iwarn;
		dq->dq_bwarn = warn.dw_bwarn;
		dq->dq_flags &= ~(DQ_INODS | DQ_BLKS);
		dq->dq_flags |= DQ_MOD;
	}
	dqrele(dq);
	return (error);
}

/*
 * Q_DOWARN - force warning(s) to user(s).
 */
dowarn(q, dev)
	register struct quota *q;
	dev_t dev;
{
	register struct dquot *dq, **dqq;

	if (!suser() || u.u_ttyp == NULL)
		return (u.u_error);			/* XXX */
	if (dev != NODEV) {
		dq = dqp(q, dev);
		if (dq != NODQUOT) {
			qwarn(dq);
			dqrele(dq);
		}
		return (0);
	}
	for (dqq = q->q_dq; dqq < &q->q_dq[NMOUNT]; dqq++) {
		dq = *dqq;
		if (dq != NODQUOT && dq != LOSTDQUOT)
			qwarn(dq);
	}
	return (0);
}

/*
 * Q_SYNC - sync quota files to disc.
 */
qsync(dev)
	dev_t dev;
{
	register struct quota *q;
	register struct mount *mp;
	register index;

	if (!suser())
		return (u.u_error);			/* XXX */
	for (mp = mount, index = 0; mp < &mount[NMOUNT]; mp++, index++)
		if (mp->m_fs && mp->m_qinod &&
		    (dev == NODEV || dev == mp->m_dev)) {
			for (q = quota; q < quotaNQUOTA; q++)
				if (q->q_cnt) {
					q->q_cnt++;
					putdq(mp, q->q_dq[index], 0);
					delquota(q);
				}
		}
	return (0);
}

/*
 * Q_SETUID - change quota to a particular uid.
 */
qsetuid(uid, noquota)
	uid_t uid;
	int noquota;
{
	register struct quota *q;

	if (uid == u.u_quota->q_uid)
		return (0);
	if (!suser())
		return (u.u_error);			/* XXX */
	q = getquota(uid, 0, noquota ? Q_NDQ : 0);
	qclean();
	qstart(q);
	return (0);
}
#endif
