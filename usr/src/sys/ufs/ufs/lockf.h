/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Scooter Morris at Genentech Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lockf.h	7.1 (Berkeley) %G%
 */

/*
 * The lockf structure is a kernel structure which contains all the
 * information associated with a byte range lock. The lockf structures
 * are linked into the inode structure. Locks are sorted by the starting
 * byte of the lock for efficiency.
 */
struct lockf {
	short	lf_flags;	 /* Lock semantics: F_POSIX, F_FLOCK, F_WAIT */
	short	lf_type;	 /* Lock type: F_RDLCK, F_WRLCK */
	off_t	lf_start;	 /* The byte # of the start of the lock */
	off_t	lf_end;		 /* The byte # of the end of the lock (-1=EOF)*/
	caddr_t	lf_id;		 /* The id of the resource holding the lock */
	struct	inode *lf_inode; /* Back pointer to the inode */
	struct	lockf *lf_next;	 /* A pointer to the next lock on this inode */
	struct	lockf *lf_block; /* The list of blocked locks */
};

/*
 * Maximum length of sleep chains to traverse to try and detect deadlock.
 */
#define MAXDEPTH 50

#ifdef	KERNEL
/*
 * Public lock manipulation routines
 */
extern struct lockf *lf_remove();	/* Remove a lock */
extern struct lockf *lf_getblock();	/* Return the first blocking lock */

#ifdef	LOCKF_DEBUG
extern int lockf_debug;
#endif	LOCKF_DEBUG
#endif	KERNEL
