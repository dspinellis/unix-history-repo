/*	quota.h	4.3	83/05/21	*/

#ifdef QUOTA
/*
 *	Various junk to do with various quotas (etc) imposed upon
 *	the average user (big brother finally hits unix)
 *
 *	The following structure exists in core for each logged on user
 *	It contains global junk relevant to that user's quotas
 *
 *	The u_quota field of each user struct contains a pointer to
 *	the quota struct relevant to the current process, this is changed
 *	by 'setuid' sys call, &/or by the Q_SETUID quota() call
 */
#ifdef	KERNEL
struct quota {
	struct	quota *q_forw, *q_back;	/* hash chain, MUST be first */
	short	q_cnt;			/* ref count (# processes) */
	short	q_uid;			/* real uid of owner */
	char	q_flg;			/* struct management flags */
#define	Q_LOCK	0x01		/* quota struct locked (for disc i/o) */
#define	Q_WANT	0x02		/* issue a wakeup when lock goes off */
#define	Q_NEW	0x04		/* new quota - no proc1 msg sent yet */
#define	Q_NDQ	0x08		/* account has NO disc quota */
	struct	quota *q_freef, **q_freeb;
	struct	dquot *q_dq[NMOUNT];	/* disc quotas for mounted filesys's */
};
#define	NOQUOT	((struct quota *) 0)
#endif

#ifdef	KERNEL
struct	quota *quota, *quotaNQUOTA;
int	nquota;
struct	quota *getquota(), *qfind();
#endif

/*
 *	The following structure defines the format of the disc quota file
 *	(as it appears on disc) - the file is an array of these structures
 *	indexed by user number. A sys call (setquota) establishes the
 *	inode for each applicable file (a pointer is retained in the mount
 *	structure)
 *
 *	nb: warning fields contain the number of warnings left before
 *		allocation is halted completely
 */
typedef	unsigned short	dlim_t;

struct	dqblk {
	dlim_t	dqb_ilim;	/* max num allocated inodes + 1 */
	dlim_t	dqb_iq;		/* preferred inode limit */
	dlim_t	dqb_inod;	/* current num allocated inodes */
	dlim_t	dqb_iwarn;	/* # warnings about excessive inodes */
#define	MAX_IQ_WARN	3
	dlim_t	dqb_blim;	/* abs limit on disc blks alloc */
	dlim_t	dqb_quot;	/* preferred limit on disc blks */
	dlim_t	dqb_blks;	/* current block count */
	dlim_t	dqb_dwarn;	/* # warnings about excessive disc use */
#define	MAX_DQ_WARN	3
};

/*
 *	The following structure records disc usage for a user on a filesystem
 *	There is one allocated for each quota that exists on any filesystem
 *	for the current user. A cache is kept of other recently used entries.
 */
struct	dquot {
	struct	dquot *dq_forw, *dq_back;/* MUST be first entry */
	union	{
		struct	quota *Dq_own;	/* the quota that points to this */
		struct {		/* free list */
			struct	dquot *Dq_freef, **Dq_freeb;
		} dq_f;
	} dq_u;
	short	dq_flg;
#define	DQ_LOCK		0x01		/* this quota locked (no MODS) */
#define	DQ_WANT		0x02		/* wakeup on unlock */
#define	DQ_MOD		0x04		/* this quota modified since read */
#define	DQ_FAKE		0x08		/* no limits here, just usage */
#define	DQ_BLKS		0x10		/* has been warned about blk limit */
#define	DQ_INODS	0x20		/* has been warned about inode limit */
	short	dq_cnt;			/* count of active references */
	short	dq_uid;			/* user this applies to */
	dev_t	dq_dev;			/* filesystem this relates to */
	struct dqblk dq_dqb;		/* actual usage & quotas */
};
#define	dq_own		dq_u.Dq_own
#define	dq_freef	dq_u.dq_f.Dq_freef
#define	dq_freeb	dq_u.dq_f.Dq_freeb
#define	dq_ilim		dq_dqb.dqb_ilim
#define	dq_iq		dq_dqb.dqb_iq
#define	dq_inod		dq_dqb.dqb_inod
#define	dq_iwarn	dq_dqb.dqb_iwarn
#define	dq_blim		dq_dqb.dqb_blim
#define	dq_quot		dq_dqb.dqb_quot
#define	dq_blks		dq_dqb.dqb_blks
#define	dq_dwarn	dq_dqb.dqb_dwarn
#define	NODQUOT		((struct dquot *) 0)
#define	LOSTDQUOT	((struct dquot *) 1)

#ifdef	KERNEL
struct	dquot *dquot, *dquotNDQUOT;
int	ndquot;
struct	dquot *discquota(), *inoquota(), *dqalloc(), *dqp();
#endif

/*
 * Commands for the 'quota' system call
 */
#define	Q_SETDLIM	1	/* set disc limits & usage */
#define	Q_GETDLIM	2	/* get disc limits & usage */
#define	Q_SETDUSE	3	/* set disc usage only */
#define	Q_SYNC		4	/* update disc copy if quota usages */
#define	Q_SETUID	16	/* change proc to use quotas for uid */
#define	Q_SETWARN	25	/* alter inode/block warning counts */
#define	Q_DOWARN	26	/* warn user about excessive space/inodes */
#endif
