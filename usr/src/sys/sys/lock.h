/* 
 * Copyright (c) 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code contains ideas from software contributed to Berkeley by
 * Avadis Tevanian, Jr., Michael Wayne Young, and the Mach Operating
 * System project at Carnegie-Mellon University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lock.h	8.3 (Berkeley) %G%
 */

#ifndef	_LOCK_H_
#define	_LOCK_H_

/*
 * XXX This belongs in <machine/param.h>, but is here for now.
 */
#define NCPUS 1

/*
 * An atomic spin lock.
 *
 * This structure only sets one bit of data, but is sized based on the
 * minimum word size that can be operated on by the hardware test-and-set
 * instruction. It is only needed for multiprocessors, as uniprocessors
 * will always run to completion or a sleep. It is an error to hold one
 * of these locks while a process is sleeping.
 */
struct atomic_lk {
	int	lock_data;
};

/*
 * The general lock structure.  Provides for multiple shared locks,
 * upgrading from shared to exclusive, and sleeping until the lock
 * can be gained.
 */
struct lock {
	struct	atomic_lk lk_interlock;	/* lock on remaining fields */
	u_int	lk_flags;		/* see below */
	int	lk_sharecount;		/* # of accepted shared locks */
	int	lk_exclusivecount;	/* # of recursive exclusive locks */
	int	lk_prio;		/* priority at which to sleep */
	char	*lk_wmesg;		/* resource sleeping (for tsleep) */
	int	lk_timo;		/* maximum sleep time (for tsleep) */
	pid_t	lk_lockholder;		/* pid of exclusive lock holder */
};
/*
 * Lock request types:
 *   LK_SHARED - get one of many possible shared locks. If a process
 *	holding an exclusive lock requests a shared lock, the exclusive
 *	lock(s) will be downgraded to shared locks.
 *   LK_EXCLUSIVE - stop further shared locks, when they are cleared,
 *	grant a pending upgrade if it exists, then grant an exclusive
 *	lock. Only one exclusive lock may exist at a time, except that
 *	a process holding an exclusive lock may get additional exclusive
 *	locks if it explicitly sets the LK_CANRECURSE flag in the lock
 *	request, or if the LK_CANRECUSE flag was set when the lock was
 *	initialized.
 *   LK_UPGRADE - the process must hold a shared lock that it wants to
 *	have upgraded to an exclusive lock. Other processes may get
 *	exclusive access to the resource between the time that the upgrade
 *	is requested and the time that it is granted.
 *   LK_EXCLUPGRADE - the process must hold a shared lock that it wants to
 *	have upgraded to an exclusive lock. If the request succeeds, no
 *	other processes will have gotten exclusive access to the resource
 *	between the time that the upgrade is requested and the time that
 *	it is granted. However, if another process has already requested
 *	an upgrade, the request will fail (see error returns below).
 *   LK_DOWNGRADE - the process must hold an exclusive lock that it wants
 *	to have downgraded to a shared lock. If the process holds multiple
 *	(recursive) exclusive locks, they will all be downgraded to shared
 *	locks.
 *   LK_RELEASE - release one instance of a lock.
 *
 * These are flags that are passed to the lockmgr routine.
 */
#define LK_TYPE_MASK	0x00000007	/* type of lock sought */
#define LK_SHARED	0x00000001	/* shared lock */
#define LK_EXCLUSIVE	0x00000002	/* exclusive lock */
#define LK_UPGRADE	0x00000003	/* shared-to-exclusive upgrade */
#define LK_EXCLUPGRADE	0x00000004	/* first shared-to-exclusive upgrade */
#define LK_DOWNGRADE	0x00000005	/* exclusive-to-shared downgrade */
#define LK_RELEASE	0x00000006	/* release any type of lock */
/*
 * External lock flags.
 *
 * These flags may be set in lock_init to set their mode permanently,
 * or passed in as arguments to the lock manager.
 */
#define LK_EXTFLG_MASK	0x000000f0	/* mask of external flags */
#define LK_NOWAIT	0x00000010	/* do not sleep to await lock */
#define LK_SLEEPFAIL	0x00000020	/* sleep, then return failure */
#define LK_CANRECURSE	0x00000040	/* allow recursive exclusive lock */
/*
 * Internal lock flags.
 *
 * These flags are used internally to the lock manager.
 */
#define LK_WAITING	0x00000100	/* process is sleeping on lock */
#define LK_WANT_UPGRADE	0x00000200	/* waiting for share-to-excl upgrade */
#define LK_WANT_EXCL	0x00000400	/* exclusive lock sought */
#define LK_HAVE_EXCL	0x00000800	/* exclusive lock obtained */
/*
 * Lock return status.
 *
 * Successfully obtained locks return 0. Locks will always succeed
 * unless one of the following is true:
 *	LK_FORCEUPGRADE is requested and some other process has already
 *	    requested a lock upgrade (returns EBUSY).
 *	LK_WAIT is set and a sleep would be required (returns EBUSY).
 *	LK_SLEEPFAIL is set and a sleep was done (returns ENOLCK).
 *	PCATCH is set in lock priority and a signal arrives (returns
 *	    either EINTR or ERESTART if system calls is to be restarted).
 *	Non-null lock timeout and timeout expires (returns EWOULDBLOCK).
 * A failed lock attempt always returns a non-zero error value. No lock
 * is held after an error return (in particular, a failed LK_UPGRADE
 * or LK_FORCEUPGRADE will have released its shared access lock).
 */

/*
 * Indicator that no process holds exclusive lock
 */
#define LK_NOPROC ((pid_t) -1)

void	lock_init __P((struct lock *, int prio, char *wmesg, int timo,
			int flags));
int	lockmgr __P((volatile struct lock *, struct proc *, u_int flags));

#if NCPUS > 1
/*
 * The simple-lock routines are the primitives out of which the lock
 * package is built. The machine-dependent code must implement an
 * atomic test_and_set operation that indivisibly sets the atomic_lk
 * to non-zero and returns its old value. It also assumes that the
 * setting of the lock to zero below is indivisible. Atomic locks may
 * only be used for exclusive locks.
 */
__inline void
atomic_lock_init(lkp)
	struct atomic_lk *lkp;
{

	lkp->lock_data = 0;
}

__inline void
atomic_lock(lkp)
	volatile struct atomic_lk *lkp;
{

	while (test_and_set(&lkp->lock_data))
		continue;
}

__inline void
atomic_unlock(lkp)
	struct atomic_lk *lkp;
{

	lkp->lock_data = 0;
}

#else /* NCPUS == 1, so no multiprocessor locking is necessary */

#ifdef DEBUG
__inline void
atomic_lock_init(alp)
	struct atomic_lk *alp;
{

	alp->lock_data = 0;
}

__inline void
atomic_lock(alp)
	volatile struct atomic_lk *alp;
{

	if (alp->lock_data == 1)
		panic("atomic lock held");
	else
		alp->lock_data = 1;
}

__inline void
atomic_unlock(alp)
	struct atomic_lk *alp;
{

	if (alp->lock_data == 0)
		panic("atomic lock not held");
	else
		alp->lock_data = 0;
}

#else /* !DIAGNOSTIC */
#define	atomic_lock_init(alp)
#define	atomic_lock(alp)
#define	atomic_unlock(alp)
#endif /* !DIAGNOSTIC */

#endif /* NCPUS == 1 */

#endif /* !_LOCK_H_ */
