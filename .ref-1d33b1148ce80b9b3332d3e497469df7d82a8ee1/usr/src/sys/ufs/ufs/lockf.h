/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Scooter Morris at Genentech Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lockf.h	8.2 (Berkeley) %G%
 */

/*
 * The lockf structure is a kernel structure which contains the information
 * associated with a byte range lock.  The lockf structures are linked into
 * the inode structure. Locks are sorted by the starting byte of the lock for
 * efficiency.
 */
TAILQ_HEAD(locklist, lockf);

struct lockf {
	short	lf_flags;	    /* Semantics: F_POSIX, F_FLOCK, F_WAIT */
	short	lf_type;	    /* Lock type: F_RDLCK, F_WRLCK */
	off_t	lf_start;	    /* Byte # of the start of the lock */
	off_t	lf_end;		    /* Byte # of the end of the lock (-1=EOF) */
	caddr_t	lf_id;		    /* Id of the resource holding the lock */
	struct	inode *lf_inode;    /* Back pointer to the inode */
	struct	lockf *lf_next;	    /* Pointer to the next lock on this inode */
	struct	locklist lf_blkhd;  /* List of requests blocked on this lock */
	TAILQ_ENTRY(lockf) lf_block;/* A request waiting for a lock */
};

/* Maximum length of sleep chains to traverse to try and detect deadlock. */
#define MAXDEPTH 50

__BEGIN_DECLS
void	 lf_addblock __P((struct lockf *, struct lockf *));
int	 lf_clearlock __P((struct lockf *));
int	 lf_findoverlap __P((struct lockf *,
	    struct lockf *, int, struct lockf ***, struct lockf **));
struct lockf *
	 lf_getblock __P((struct lockf *));
int	 lf_getlock __P((struct lockf *, struct flock *));
int	 lf_setlock __P((struct lockf *));
void	 lf_split __P((struct lockf *, struct lockf *));
void	 lf_wakelock __P((struct lockf *));
__END_DECLS

#ifdef LOCKF_DEBUG
extern int lockf_debug;

__BEGIN_DECLS
void	lf_print __P((char *, struct lockf *));
void	lf_printlist __P((char *, struct lockf *));
__END_DECLS
#endif
