/*
 * operations to ktrace system call  (op & 0x3)
 */
#define KTROP_SET		0	/* set traces */
#define KTROP_CLEAR		1	/* clear traces */
#define KTROP_CLEARFILE		2	/* stop all tracing to file */

#define KTROP_INHERITFLAG	4	/* pass to children flag */

/*
 * ktrace record header
 */
struct ktr_header {
	short	ktr_type;		/* trace record type */
	short	ktr_len;		/* length of buf */
	pid_t	ktr_pid;		/* process id */
	char	ktr_comm[MAXCOMLEN+1];	/* command name */
	struct	timeval ktr_time;	/* timestamp */
	caddr_t	ktr_buf;
};

/*
 * Test for kernel trace point
 */
#define KTRPOINT(p, type)	((p)->p_traceflag & (1<<(type)))
/*
 * ktrace record types
 */

/*
 * KTR_SYSCALL - system call record
 */
#define KTR_SYSCALL	0x1
struct ktr_syscall {
	short	ktr_code;		/* syscall number */
	short	ktr_narg;		/* number of arguments */
	/*
	 * followed by ktr_narg ints
	 */
};

/*
 * KTR_SYSRET - return from system call record
 */
#define KTR_SYSRET	0x2
struct ktr_sysret {
	short	ktr_code;
	short	ktr_eosys;
	int	ktr_error;
	int	ktr_retval;
};

/*
 * KTR_NAMEI - namei record
 */
#define KTR_NAMEI	0x3
/* record contains pathname */

/*
 * kernel trace facilities
 */
#define KTRFAC_SYSCALL	(1<<KTR_SYSCALL)
#define KTRFAC_SYSRET	(1<<KTR_SYSRET)
#define KTRFAC_NAMEI	(1<<KTR_NAMEI)
