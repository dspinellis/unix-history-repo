/*
 * tunable variables
 */

#define	NBUF	15		/* size of buffer cache */
#define	NINODE	20		/* number of in core inodes */
#define	NFILE	20		/* number of in core file structures */
#define	NMOUNT	2		/* number of mountable file systems */
#define	MAXMEM	(64*32)		/* max core per process - first # is Kw */
#define	MAXUPRC	15		/* max processes per user */
#define	SSIZE	20		/* initial stack size (*64 bytes) */
#define	SINCR	20		/* increment of stack (*64 bytes) */
#define	NOFILE	20		/* max open files per process */
#define	CANBSIZ	256		/* max size of typewriter line */
#define	CMAPSIZ	20		/* size of core allocation area */
#define	SMAPSIZ	20		/* size of swap allocation area */
#define	NCALL	10		/* max simultaneous time callouts */
#define	NPROC	20		/* max number of processes */
#define	NTEXT	10		/* max number of pure texts */
#define	NCLIST	40		/* max total clist size */
#define	HZ	60		/* Ticks/second of the clock */
#define	TIMEZONE (5*60)		/* Minutes westward from Greenwich */
#define	DSTFLAG	1		/* Daylight Saving Time applies in this locality */
#define	MSGBUFS	128		/* Characters saved from error messages */
#define	NCARGS	5120		/* # characters in exec arglist */

/*
 * priorities
 * probably should not be
 * altered too much
 */

#define	PSWP	0
#define	PINOD	10
#define	PRIBIO	20
#define	PZERO	25
#define	NZERO	20
#define	PPIPE	26
#define	PWAIT	30
#define	PSLEP	40
#define	PUSER	50

/*
 * signals
 * dont change
 */

#define	NSIG	17
/*
 * No more than 16 signals (1-16) because they are
 * stored in bits in a word.
 */
#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt (rubout) */
#define	SIGQUIT	3	/* quit (FS) */
#define	SIGINS	4	/* illegal instruction */
#define	SIGTRC	5	/* trace or breakpoint */
#define	SIGIOT	6	/* iot */
#define	SIGEMT	7	/* emt */
#define	SIGFPT	8	/* floating exception */
#define	SIGKIL	9	/* kill, uncatchable termination */
#define	SIGBUS	10	/* bus error */
#define	SIGSEG	11	/* segmentation violation */
#define	SIGSYS	12	/* bad system call */
#define	SIGPIPE	13	/* end of pipe */
#define	SIGCLK	14	/* alarm clock */
#define	SIGTRM	15	/* Catchable termination */

/*
 * fundamental constants of the implementation--
 * cannot be changed easily
 */

#define	NBPW	sizeof(int)	/* number of bytes in an integer */
#define	BSIZE	512		/* size of secondary block (bytes) */
/* BSLOP can be 0 unless you have a TIU/Spider */
#define	BSLOP	2		/* In case some device needs bigger buffers */
#define	NINDIR	(BSIZE/sizeof(daddr_t))
#define	BMASK	0777		/* BSIZE-1 */
#define	BSHIFT	9		/* LOG2(BSIZE) */
#define	NMASK	0177		/* NINDIR-1 */
#define	NSHIFT	7		/* LOG2(NINDIR) */
#define	USIZE	16		/* size of user block (*64) */
#define	UBASE	0140000		/* abs. addr of user block */
#define	NULL	0
#define	CMASK	0		/* default mask for file creation */
#define	NODEV	(dev_t)(-1)
#define	ROOTINO	((ino_t)2)	/* i number of all roots */
#define	SUPERB	((daddr_t)1)	/* block number of the super block */
#define	DIRSIZ	14		/* max characters per directory */
#define	NICINOD	100		/* number of superblock inodes */
#define	NICFREE	50		/* number of superblock free blocks */
#define	INFSIZE	138		/* size of per-proc info for users */
#define	CBSIZE	6		/* number of chars in a clist block */
#define	CROUND	07		/* clist rounding: sizeof(int *) + CBSIZE - 1*/

/*
 * Some macros for units conversion
 */
/* Core clicks (64 bytes) to segments and vice versa */
#define	ctos(x)	((x+127)/128)
#define stoc(x) ((x)*128)

/* Core clicks (64 bytes) to disk blocks */
#define	ctod(x)	((x+7)>>3)

/* inumber to disk address */
#define	itod(x)	(daddr_t)((((unsigned)x+15)>>3))

/* inumber to disk offset */
#define	itoo(x)	(int)((x+15)&07)

/* clicks to bytes */
#define	ctob(x)	(x<<6)

/* bytes to clicks */
#define	btoc(x)	((((unsigned)x+63)>>6))

/* major part of a device */
#define	major(x)	(int)(((unsigned)x>>8))

/* minor part of a device */
#define	minor(x)	(int)(x&0377)

/* make a device number */
#define	makedev(x,y)	(dev_t)((x)<<8 | (y))

typedef	struct { int r[1]; } *	physadr;
typedef	long		daddr_t;
typedef char *		caddr_t;
typedef	unsigned int	ino_t;
typedef	long		time_t;
typedef	int		label_t[6];	/* regs 2-7 */
typedef	int		dev_t;
typedef	long		off_t;

/*
 * Machine-dependent bits and macros
 */
#define	UMODE	0170000		/* usermode bits */
#define	USERMODE(ps)	((ps & UMODE)==UMODE)

#define	INTPRI	0340		/* Priority bits */
#define	BASEPRI(ps)	((ps & INTPRI) != 0)
