/*
 * variables
 */

#define	NBUF	15
#define	NINODE	100
#define	NFILE	100
#define	NMOUNT	5
#define	NEXEC	4
#define	MAXMEM	(32*32)
#define	SSIZE	20
#define	SINCR	20
#define	NOFILE	15
#define	CANBSIZ	256
#define	CMAPSIZ	100
#define	SMAPSIZ	100
#define	NCALL	20
#define	NPROC	50
#define	NTEXT	20
#define	NCLIST	100

/*
 * priorities
 * probably should not be
 * altered too much
 */

#define	PSWP	-100
#define	PINOD	-90
#define	PRIBIO	-50
#define	PPIPE	1
#define	PWAIT	40
#define	PSLEP	90
#define	PUSER	100

/*
 * signals
 * dont change
 */

#define	NSIG	13
#define		SIGHUP	1
#define		SIGINT	2
#define		SIGQIT	3
#define		SIGINS	4
#define		SIGTRC	5
#define		SIGIOT	6
#define		SIGEMT	7
#define		SIGFPT	8
#define		SIGKIL	9
#define		SIGBUS	10
#define		SIGSEG	11
#define		SIGSYS	12

/*
 * fundamental constants
 * cannot be changed
 */

#define	USIZE	16
#define	NULL	0
#define	NODEV	(-1)
#define	ROOTINO	1
#define	DIRSIZ	14

struct
{
	char	lobyte;
	char	hibyte;
};
