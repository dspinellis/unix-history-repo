/*	acct.h	4.5	82/10/10	*/

/*
 * Accounting structures;
 * these use a comp_t type which is a 3 bits base 8
 * exponent, 13 bit fraction ``floating point'' number.
 */
typedef	u_short comp_t;

struct	acct
{
	char	ac_comm[10];		/* Accounting command name */
	comp_t	ac_utime;		/* Accounting user time */
	comp_t	ac_stime;		/* Accounting system time */
	comp_t	ac_etime;		/* Accounting elapsed time */
	time_t	ac_btime;		/* Beginning time */
	short	ac_uid;			/* Accounting user ID */
	short	ac_gid;			/* Accounting group ID */
	short	ac_mem;			/* average memory usage */
	comp_t	ac_io;			/* number of disk IO blocks */
	dev_t	ac_tty;			/* control typewriter */
	char	ac_flag;		/* Accounting flag */
};

#define	AFORK	0001		/* has executed fork, but no exec */
#define	ASU	0002		/* used super-user privileges */
#define	ACOMPAT	0004		/* used compatibility mode */
#define	ACORE	0010		/* dumped core */
#define	AXSIG	0020		/* killed by a signal */

#define	ACCTLO	30		/* acctg off when space < this */
#define	ACCTHI	100		/* acctg resumes at this level */

#ifdef KERNEL
struct	acct	acctbuf;
struct	inode	*acctp;
#endif
