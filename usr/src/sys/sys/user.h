/*	user.h	3.2	%H%	*/

#ifdef KERNEL
#include "../h/pcb.h"
#include "../h/dmap.h"
#include "../h/vtimes.h"
#ifdef FASTVAX
asm(".set U_ARG,120");
asm(".set U_QSAV,140");
#endif
#else
#include <sys/pcb.h>
#include <sys/dmap.h>
#include <sys/vtimes.h>
#endif
/*
 * The user structure.
 * One allocated per process.
 * Contains all per process data
 * that doesn't need to be referenced
 * while the process is swapped.
 * The user block is UPAGES*NBPG bytes
 * long; resides at virtual user
 * loc 0x80000000-UPAGES*NBPG; contains the system
 * stack per user; is cross referenced
 * with the proc structure for the
 * same process.
 */
 
#define	EXCLOSE 01
 
struct	user
{
	struct	pcb u_pcb;
	int	u_arg[5];		/* arguments to current system call */
	label_t	u_qsav;			/* for non-local gotos on interrupts */
	char	u_segflg;		/* 0:user D; 1:system; 2:user I */
	char	u_error;		/* return error code */
	short	u_uid;			/* effective user id */
	short	u_gid;			/* effective group id */
	short	u_ruid;			/* real user id */
	short	u_rgid;			/* real group id */
	struct	proc *u_procp;		/* pointer to proc structure */
	int	*u_ap;			/* pointer to arglist */
	union {				/* syscall return values */
		struct	{
			int	R_val1;
			int	R_val2;
		} u_rv;
#define	r_val1	u_rv.R_val1
#define	r_val2	u_rv.R_val2
		off_t	r_off;
		time_t	r_time;
	} u_r;
	caddr_t	u_base;			/* base address for IO */
	unsigned int u_count;		/* bytes remaining for IO */
	off_t	u_offset;		/* offset in file for IO */
	struct	inode *u_cdir;		/* pointer to inode of current directory */
	struct	inode *u_rdir;		/* root directory of current process */
	char	u_dbuf[DIRSIZ];		/* current pathname component */
	caddr_t	u_dirp;			/* pathname pointer */
	struct	direct u_dent;		/* current directory entry */
	struct	inode *u_pdir;		/* inode of parent directory of dirp */
	struct	file *u_ofile[NOFILE];	/* pointers to file structures of open files */
	char	u_pofile[NOFILE];	/* per-process flags of open files */
	label_t u_ssav;			/* label variable for swapping */
	int	u_signal[NSIG];		/* disposition of signals */
	int	u_cfcode;		/* ``code'' to trap when CM faulted */
	int	*u_ar0;			/* address of users saved R0 */
	struct uprof {			/* profile arguments */
		short	*pr_base;	/* buffer base */
		unsigned pr_size;	/* buffer size */
		unsigned pr_off;	/* pc offset */
		unsigned pr_scale;	/* pc scaling */
	} u_prof;
	char	u_intflg;		/* catch intr from sys */
	char	u_sep;			/* flag for I and D separation */
	struct	tty *u_ttyp;		/* controlling tty pointer */
	dev_t	u_ttyd;			/* controlling tty dev */
	struct {			/* header of executable file */
		int	ux_mag;		/* magic number */
		unsigned ux_tsize;	/* text size */
		unsigned ux_dsize;	/* data size */
		unsigned ux_bsize;	/* bss size */
		unsigned ux_ssize;	/* symbol table size */
		unsigned ux_entloc;	/* entry location */
		unsigned ux_unused;
		unsigned ux_relflg;
	} u_exdata;
	char	u_comm[DIRSIZ];
	time_t	u_start;
	char	u_acflag;
	short	u_fpflag;		/* unused now, will be later */
	short	u_cmask;		/* mask for file creation */
	size_t	u_tsize;		/* text size (clicks) */
	size_t	u_dsize;		/* data size (clicks) */
	size_t	u_ssize;		/* stack size (clicks) */
	struct	vtimes u_vm;		/* stats for this proc */
	struct	vtimes u_cvm;		/* sum of stats for reaped children */
	struct	dmap u_dmap;		/* disk map for data segment */
	struct	dmap u_smap;		/* disk map for stack segment */
	struct	dmap u_cdmap, u_csmap;	/* shadows of u_dmap, u_smap, for
					   use of parent during fork */
	time_t	u_outime;		/* user time at last sample */
	size_t	u_odsize, u_ossize;	/* for (clumsy) expansion swaps */
	size_t	u_vrpages[NOFILE];	/* number vread pages hanging on fd */
	int	u_stack[1];

					/*
					 * kernel stack per user
					 * extends from u + UPAGES*512
					 * backward not to reach here
					 */
};

/* u_error codes */
#define	EPERM	1
#define	ENOENT	2
#define	ESRCH	3
#define	EINTR	4
#define	EIO	5
#define	ENXIO	6
#define	E2BIG	7
#define	ENOEXEC	8
#define	EBADF	9
#define	ECHILD	10
#define	EAGAIN	11
#define	ENOMEM	12
#define	EACCES	13
#define	EFAULT	14
#define	ENOTBLK	15
#define	EBUSY	16
#define	EEXIST	17
#define	EXDEV	18
#define	ENODEV	19
#define	ENOTDIR	20
#define	EISDIR	21
#define	EINVAL	22
#define	ENFILE	23
#define	EMFILE	24
#define	ENOTTY	25
#define	ETXTBSY	26
#define	EFBIG	27
#define	ENOSPC	28
#define	ESPIPE	29
#define	EROFS	30
#define	EMLINK	31
#define	EPIPE	32
#define	EDOM	33
#define	ERANGE	34

#ifdef KERNEL
extern	struct user u;
extern	struct user swaputl;
extern	struct user forkutl;
extern	struct user xswaputl;
extern	struct user xswap2utl;
extern	struct user pushutl;
extern	struct user vfutl;
#endif
