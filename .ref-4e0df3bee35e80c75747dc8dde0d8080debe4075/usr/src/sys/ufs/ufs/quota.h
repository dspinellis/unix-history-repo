/*	quota.h	Melb 4.2	82/10/20	*/

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

typedef	long	class_t;

struct quinfo {
	u_short	qu_shares;	/* allocated shares (MUSH) */
	short	qu_plim;	/* process limit */
	long	qu_syflags;	/* system permission flags */
	float	qu_usage;	/* current resource usage (MUSH) */
	class_t	qu_class;	/* user classes (MUSH) */
};

#ifdef	KERNEL
struct quota {
	struct	quota	*q_forw;	/* hash chain - MUST be first */
	struct	quota	*q_back;	/* hash chain - MUST be last */
	char	q_flg;			/* struct management flags */
	char	q_lcnt;			/* count of logins for user */
	short	q_cnt;			/* ref count (# processes) */
	short	q_uid;			/* real uid of owner */
	short	q_nice;			/* nice added to p_cpu (MUSH) */
	short	q_acnt;			/* count of 'at' processes (MUSH) */
	union	{
		struct	{
			long	Q_rate;	/* recent work rate (MUSH) */
			long	Q_cost;	/* cost in recent period (MUSH) */
		} q_s1;
		struct	{
			struct quota *Q_freef;
			struct quota **Q_freeb;
		} q_s2;
	} q_u;
	struct	quinfo q_uinfo;		/* user limits & usage (MUSH) */
	struct	dquot *q_dq[NMOUNT];	/* disc quotas for mounted filesys's */
};
#define	NOQUOT	((struct quota *) 0)
#define	q_rate		q_u.q_s1.Q_rate
#define	q_cost		q_u.q_s1.Q_cost
#define	q_freef		q_u.q_s2.Q_freef
#define	q_freeb		q_u.q_s2.Q_freeb
#define	q_shares	q_uinfo.qu_shares
#define	q_plim		q_uinfo.qu_plim
#define	q_syflags	q_uinfo.qu_syflags
#define	q_usage		q_uinfo.qu_usage
#define	q_class		q_uinfo.qu_class
#endif

#define	QF_KASYNC	0x02		/* kill async procs at logout */
#define	QF_FASTTY	0x04		/* permitted to raise tty speed */
#define	QF_NASYNC	0x08		/* nice async procs at logout */
#define	QF_MODTTY	0x10		/* permitted to modify other tty */
#define	QF_UMASK	0x20		/* not permitted to alter umask */

#ifdef	KERNEL
struct quota *quota, *quotaNQUOTA;
int	nquota;
struct quota *getquota(), *qfind();
#endif

/*	q_flg flags	*/
#define	Q_LOCK		0x01		/* quota struct locked (for disc i/o) */
#define	Q_WANT		0x02		/* issue a wakeup when lock goes off */
#define	Q_NEW		0x04		/* new quota - no proc1 msg sent yet */
#define	Q_NDQ		0x08		/* account has NO disc quota */

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
	dlim_t	dqb_blim;	/* abs limit on disc blks alloc */
	dlim_t	dqb_quot;	/* preferred limit on disc blks */
	dlim_t	dqb_blks;	/* current block count */
	dlim_t	dqb_dwarn;	/* # warnings about excessive disc use */
};
#define	MAX_IQ_WARN	3
#define	MAX_DQ_WARN	3

/*
 *	The following structure records disc usage for a user on a filesystem
 *	There is one allocated for each quota that exists on any filesystem
 *	for the current user. A cache is kept of other recently used entries.
 */

struct	dquot {
	struct	dquot	*dq_forw;	/* MUST be first entry */
	struct	dquot	*dq_back;	/* MUST be second entry */
	union	{
		struct quota *Dq_own;	/* the quota that points to this */
		struct {
			struct dquot *Dq_freef;	/* forward free chain ptr */
			struct dquot **Dq_freeb;/* backward free chain ptr */
		} dq_f;
	} dq_u;
	short	dq_flg;
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
struct	dquot	*dquot, *dquotNDQUOT;
int	ndquot;
struct	dquot	*discquota(), *inoquota(), *dqalloc();
#endif

/*	dq_flg flags	*/
#define	DQ_LOCK		0x01		/* this quota locked (no MODS) */
#define	DQ_WANT		0x02		/* wakeup on unlock */
#define	DQ_MOD		0x04		/* this quota modified since read */
#define	DQ_FAKE		0x08		/* no limits here, just usage */
#define	DQ_BLKS		0x10		/* has been warned about blk limit */
#define	DQ_INODS	0x20		/* has been warned about inode limit */

/*
 * Commands for the 'quota' system call
 */
#define	Q_SETDLIM	1	/* set disc limits & usage */
#define	Q_GETDLIM	2	/* get disc limits & usage */
#define	Q_SETDUSE	3	/* set disc usage only */
#define	Q_SYNC		4	/* update disc copy if quota usages */
#define	Q_LOGIN		5	/* Count this as a login process */
#define	Q_LCOUNT	6	/* get count of login processes */
#define	Q_PCOUNT	7	/* get count of processes */
#define	Q_USAGE		8	/* get current usage */
#define	Q_SFLAGS	9	/* set system flags */
#define	Q_SUSAGE	10	/* set usage */
#define	Q_SPLIMIT	11	/* set process limit */
#define	Q_ISLOGIN	12	/* is this a login process ?? */
#define	Q_SCLASS	13	/* set user class */
#define	Q_SCURCL	14	/* set current system classes */
#define	Q_GCURCL	15	/* get current system classes */
#define	Q_SETUID	16	/* change proc to use quotas for uid */
#define	Q_FLOGIN	17	/* "fake" login (counts as 1, but isn't) */
#define	Q_SETCOST	18	/* set system charge rates */
#define	Q_GETCOST	19	/* get system charge rates */
#define	Q_SSHARE	20	/* set shares */
#define	Q_SUINFO	21	/* set user info */
#define	Q_GUINFO	22	/* get user info */
#define	Q_ATJOB		23	/* this process is an 'at' job (background) */
#define	Q_ACOUNT	24	/* return count of procs descended from ATJ */
#define	Q_SETWARN	25	/* alter inode/block warning counts */
#define	Q_DOWARN	26	/* warn user about excessive space/inodes */
#define	Q_KILL		27	/* send signal to procs attatched to quota */
#define	Q_NICE		28	/* set niceness for procs attatched to quota */

/*
 * current class information
 *
 *	records sched biasing for classes that are to have priority
 *	enhanced or degraded
 */

#define	NCLASS	8

struct qclass {
	class_t	class;		/* classes this applies to */
	long	cost;		/* +/- mod to cost incurred */
	short	maxn;		/* in this class, no nice > this */
	short	minn;		/* in this class, no nice < this */
};

#ifdef	KERNEL
struct	qclass	curclass[NCLASS];
#endif

/*
 * Flag definitions for u_qflags in user struct (u_qflags)
 */
#define	QUF_LOGIN	0x0001		/* this process incremented login cnt */
#define	QUF_ATJ		0x0002		/* this process descended from atrun */

#endif QUOTA
