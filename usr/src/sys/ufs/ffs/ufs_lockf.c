/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Scooter Morris at Genentech Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_lockf.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "file.h"
#include "proc.h"
#include "socketvar.h"
#include "socket.h"
#include "vnode.h"
#include "ioctl.h"
#include "tty.h"
#include "malloc.h"
#include "fcntl.h"
#include "../ufs/lockf.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"

#ifdef	LOCKF_DEBUG
int	lockf_debug = 0;
#endif /* LOCKF_DEBUG */

/*
 * Common code for ufs byte range locking
 */

/*
 * Add a lock to the list.  Upgrade or downgrade our locks, if
 * they conflict.
 */
struct lockf *
lf_addlock(lock)
	register struct lockf *lock;
{
	register struct lockf *lf = lock->lf_inode->i_lockf;
	struct lockf *lastlock = (struct lockf *)0;
	struct lockf *prev, *overlap, *ltmp;
	int ovcase;

	/*
	 * First, see if we overlap with anything.
	 */
	ovcase = lf_findoverlap(lf, lock, &prev, &overlap);
	/*
	 * Add the new lock
	 */
	if (prev != (struct lockf *)0)
		prev->lf_next = lock;
	else
		lock->lf_inode->i_lockf = lock;
	lock->lf_next = overlap;
	/*
	 * Skip over locks owned by other processes.
	 * Merge any locks that are owned by ourselves.
	 */
	for (;;) {
		for (;;) {
			/*
			 * If no overlap, we are done.
			 */
			if (ovcase == 0)
				return;
			lf = overlap->lf_next;
			if (overlap->lf_id == lock->lf_id)
				break;
			/*
			 * We overlap with another process.
			 * If it is a block, panic.
			 */
			if (lock->lf_type == F_WRLCK ||
			    overlap->lf_type == F_WRLCK)
				panic("blocked in addlock");
			ovcase = lf_findoverlap(lf, lock, &prev, &overlap);
		}
		/*
		 * Five cases:
		 *	1) overlap == lock
		 *	2) overlap contains lock
		 *	3) lock contains overlap
		 *	4) overlap starts before lock
		 *	5) overlap ends after lock
		 */
		switch (ovcase) {

		case 1: /* overlap == lock */
			/*
			 * Undo spurious addition of lock to list.
			 */
			if (prev != (struct lockf *)0)
				prev->lf_next = overlap;
			else
				lock->lf_inode->i_lockf = overlap;
			/*
			 * If downgrading lock, others may be
			 * able to acquire it.
			 */
			if (lock->lf_type == F_RDLCK &&
			    overlap->lf_type == F_WRLCK)
				lf_wakelock(overlap);
			overlap->lf_type = lock->lf_type;
			free(lock, M_LOCKF);
			return;

		case 2: /* overlap contains lock */
			/*
			 * Undo spurious addition of lock to list.
			 */
			if (prev != (struct lockf *)0)
				prev->lf_next = overlap;
			else
				lock->lf_inode->i_lockf = overlap;
			if (overlap->lf_type == lock->lf_type) {
				free(lock, M_LOCKF);
				return;
			}
			lf_split(overlap, lock);
			lf_wakelock(overlap);
			return;

		case 3: /* lock contains overlap */
			/*
			 * If downgrading lock, others may be able to
			 * acquire it, otherwise take the list.
			 */
			if (lock->lf_type == F_RDLCK &&
			    overlap->lf_type == F_WRLCK) {
				lf_wakelock(overlap);
			} else {
				ltmp = lock->lf_block;
				lock->lf_block = overlap->lf_block;
				lf_addblock(lock, ltmp);
			}
			/*
			 * Delete the overlap.
			 */
			lock->lf_next = overlap->lf_next;
			free(overlap, M_LOCKF);
			break;

		case 4: /* overlap starts before lock */
			/*
			 * Reverse lock and overlap on the list
			 */
			if (prev != (struct lockf *)0)
				prev->lf_next = overlap;
			else
				lock->lf_inode->i_lockf = overlap;
			lock->lf_next = overlap->lf_next;
			overlap->lf_next = lock;
			overlap->lf_end = lock->lf_start - 1;
			lf_wakelock(overlap);
			break;

		case 5: /* overlap ends after lock */
			overlap->lf_start = lock->lf_end + 1;
			lf_wakelock(overlap);
			return;
		}
		ovcase = lf_findoverlap(lf, lock, &prev, &overlap);
	}
	/* NOTREACHED */
}

/*
 * Walk the list of locks for an inode and
 * return the first blocking lock.
 */
struct lockf *
lf_getblock(lock)
	register struct lockf *lock;
{
	struct lockf *prev, *overlap, *lf = lock->lf_inode->i_lockf;
	int ovcase;

	while (ovcase = lf_findoverlap(lf, lock, &prev, &overlap)) {
		/*
		 * We've found an overlap, see if it blocks us
		 */
		if ((lock->lf_type == F_WRLCK || overlap->lf_type == F_WRLCK) &&
		    overlap->lf_id != lock->lf_id)
			return (overlap);
		/*
		 * Nope, point to the next one on the list and
		 * see if it blocks us
		 */
		lf = overlap->lf_next;
	}
	return ((struct lockf *)0);
}

/*
 * Walk the list of locks for an inode to
 * find an overlapping lock (if any).
 *
 * NOTE: this returns only the FIRST overlapping lock.  There
 *	 may be more than one.
 */
lf_findoverlap(list, lock, prev, overlap)
	struct lockf *list;
	struct lockf *lock;
	struct lockf **prev;
	struct lockf **overlap;
{
	register struct lockf *lf = list;
	off_t start, end;

	*prev = (struct lockf *) 0;
	*overlap = lf;
	if ((lock == (struct lockf *)0) || (lf == (struct lockf *)0))
		return (0);
#ifdef LOCKF_DEBUG
	if (lockf_debug & 1)
		lf_print("lf_findoverlap: looking for overlap in", lock);
#endif /* LOCKF_DEBUG */
	start = lock->lf_start;
	end = lock->lf_end;
	while (lf != (struct lockf *)0) {
#ifdef	LOCKF_DEBUG
		if (lockf_debug & 1)
			lf_print("\tchecking", lf);
#endif /* LOCKF_DEBUG */
		/*
		 * Have we gone far enough?
		 */
		if (end != -1 && lf->lf_start > end)
#ifdef	LOCKF_DEBUG
			if (lockf_debug & 1)
				print("no overlap\n");
#endif /* LOCKF_DEBUG */
			return (0);
		/*
		 * OK, find the overlap
		 *
		 * Five cases:
		 *	1) overlap == lock
		 *	2) overlap contains lock
		 *	3) lock contains overlap
		 *	4) overlap starts before lock
		 *	5) overlap ends after lock
		 */
		if ((lf->lf_start == start) && (lf->lf_end == end)) {
			/* Case 1 */
#ifdef LOCKF_DEBUG
			if (lockf_debug & 1)
				printf("overlap == lock\n"); break;
#endif /* LOCKF_DEBUG */
			return (1);
		} else if ((lf->lf_start <= start) &&
		    (end != -1) &&
		    ((lf->lf_end >= end) || (lf->lf_end == -1))) {
			/* Case 2 */
#ifdef LOCKF_DEBUG
			if (lockf_debug & 1)
				printf("overlap contains lock\n"); break;
#endif /* LOCKF_DEBUG */
			return (2);
		} else if ((start <= lf->lf_start) &&
		    (lf->lf_end != -1) &&
		    ((end == -1) || (end >= lf->lf_end))) {
			/* Case 3 */
#ifdef LOCKF_DEBUG
			if (lockf_debug & 1)
				printf("lock contains overlap\n"); break;
#endif /* LOCKF_DEBUG */
			return (3);
		} else if ((lf->lf_start < start) &&
			((lf->lf_end >= start) || (lf->lf_end == -1))) {
			/* Case 4 */
#ifdef LOCKF_DEBUG
			if (lockf_debug & 1)
				printf("overlap starts before lock\n"); break;
#endif /* LOCKF_DEBUG */
			return (4);
		} else if ((lf->lf_start > start) &&
			(end != -1) &&
			((lf->lf_end > end) || (lf->lf_end == -1))) {
			/* Case 5 */
#ifdef LOCKF_DEBUG
			if (lockf_debug & 1)
				printf("overlap ends after lock\n"); break;
#endif /* LOCKF_DEBUG */
			return (5);
		}
		*prev = lf;
		*overlap = lf = lf->lf_next;
	}
	/* No overlap */
#ifdef	LOCKF_DEBUG
	if (lockf_debug & 1)
		print("no overlap\n");
#endif /* LOCKF_DEBUG */
	return (0);
}

/*
 * Add a lock to the end of the blocked list.
 */
lf_addblock(lock, blocked)
	struct lockf *lock;
	struct lockf *blocked;
{
	register struct lockf *lf;

	if (lock == (struct lockf *)0)
		return;
	if ((lf = lock->lf_block) == (struct lockf *)0) {
		lock->lf_block = blocked;
		return;
	}
	while (lf->lf_block != (struct lockf *)0)
		lf = lf->lf_block;
	lf->lf_block = blocked;
	return;
}

/*
 * Combine two locks into a single lock
 */
lf_combine(lock1, lock2)
	struct lockf *lock1;
	struct lockf *lock2;
{
#ifdef LOCKF_DEBUG
	if (lockf_debug & 1) {
		lf_print("lf_combine", lock1);
		lf_print("combining with", lock2);
	}
#endif /* LOCKF_DEBUG */
	/*
	 * Sanity check
	 */
	if ( (lock1->lf_id != lock2->lf_id) ||
	     (lock1->lf_type != lock2->lf_type) )
		panic("lf_combine");
	if (lock1->lf_start > lock2->lf_start)
		lock1->lf_start = lock2->lf_start;
	if ((lock1->lf_end == -1) || (lock2->lf_end == -1))
		lock1->lf_end == -1;
	else if (lock1->lf_end < lock2->lf_end)
		lock1->lf_end = lock2->lf_end;
	/* Add the block lists together */
	lf_addblock(lock1, lock2->lf_block);
	free(lock2, M_LOCKF);
}

/*
 * Split a lock and a contained region into
 * three locks
 */
lf_split(lock1, lock2)
	register struct lockf *lock1;
	register struct lockf *lock2;
{
	register struct lockf *splitlock;

#ifdef LOCKF_DEBUG
	if (lockf_debug & 1) {
		lf_print("lf_split", lock1);
		lf_print("splitting from", lock2);
	}
#endif /* LOCKF_DEBUG */
	/*
	 * Make a new lock consisting of the last part of
	 * the encompassing lock
	 */
	MALLOC(splitlock, struct lockf *, sizeof *splitlock, M_LOCKF, M_WAITOK);
	bcopy((caddr_t)lock1, (caddr_t)splitlock, sizeof *splitlock);
	splitlock->lf_end = lock2->lf_end + 1;
	lock1->lf_end = lock2->lf_start - 1;
	/*
	 * OK, now link it in
	 */
	splitlock->lf_next = lock1->lf_next;
	lock2->lf_next = splitlock;
	lock1->lf_next = lock2;
}

/*
 * lf_remove: remove a lock (or a portion of a lock) from the lock list
 */
struct lockf *
lf_remove(lfun)
	register struct lockf *lfun;
{
	struct inode *ip = lfun->lf_inode;
	register struct lockf *lf = ip->i_lockf;
	struct lockf *blocklist = (struct lockf *)0;
	struct lockf *overlap, *prev;
	int ovcase;

	if (lf == (struct lockf *)0)
		return((struct lockf *)0);
#ifdef	LOCKF_DEBUG
	if (lockf_debug & 1)
		printf("lf_remove", lfun);
#endif	LOCKF_DEBUG
	while (ovcase = lf_findoverlap(lf, lfun, &prev, &overlap)) {
		/*
		 * Point to the next element for the loop
		 */
		lf = overlap->lf_next;
		/*
		 * Check to see if it is our lock.
		 */
		if (lfun->lf_id == overlap->lf_id) {
			/*
			 * Save the list of locks to be retried.
			 */
			if (blocklist == (struct lockf *)0)
				blocklist = overlap->lf_block;
			else
				lf_addblock(blocklist, overlap->lf_block);

			switch (ovcase) {

			case 1: /* overlap == lock */
				if (prev != (struct lockf *)0)
					prev->lf_next = overlap->lf_next;
				else
					ip->i_lockf = overlap->lf_next;
				free(overlap, M_LOCKF);
				return (blocklist);

			case 2: /* overlap contains lock: split it */
				lf_split(overlap, lfun);
				overlap->lf_next = lfun->lf_next;
				return (blocklist);

			case 3: /* lock contains overlap */
				if (prev != (struct lockf *)0)
					prev->lf_next = overlap->lf_next;
				else
					ip->i_lockf = overlap->lf_next;
				free(overlap, M_LOCKF);
				break;

			case 4: /* overlap starts before lock */
				overlap->lf_end = lfun->lf_start - 1;
				break;

			case 5: /* overlap ends after lock */
				overlap->lf_start = lfun->lf_end + 1;
				return (blocklist);
			}
		}
	}
	return (blocklist);
}

/*
 * Wakeup a blocklist
 */
lf_wakelock(blocklist)
	register struct lockf *blocklist;
{
        register struct lockf *wakelock;

        while (blocklist != (struct lockf *)0) {
                wakelock = blocklist;
                blocklist = blocklist->lf_block;
		wakelock->lf_block = (struct lockf *)0;
		wakelock->lf_next = (struct lockf *)0;
#ifdef LOCKF_DEBUG
		if (lockf_debug & 4)
			lf_print("ufs_wakelock: awakening", wakelock);
#endif /* LOCKF_DEBUG */
                wakeup((caddr_t)wakelock);
        }
}

#ifdef LOCKF_DEBUG
/*
 * Print out a lock.
 */
lf_print(tag, lock)
	char *tag;
	register lockf *lock;
{
	
	printf("%s: lock 0x%X for proc %d in ino %d on dev <%d, %d>, ",
		tag, lock, lock->lp_proc->p_pid, lock->lf_inode->i_number,
		major(lock->lf_inode->i_dev), minor(lock->lf_inode->i_dev));
	printf("type %s, start %d, end %d\n",
		lock->lf_type == F_RDLCK ? "shared" :
		lock->lf_type == F_WRLCK ? "exclusive" :
		"unknown", start, end);
}
#endif /* LOCKF_DEBUG */
