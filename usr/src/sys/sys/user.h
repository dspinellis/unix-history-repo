/*	user.h	4.25	83/02/10	*/

#ifdef KERNEL
#include "../machine/pcb.h"
#include "../h/dmap.h"
#else
#include <machine/pcb.h>
#include <sys/dmap.h>
#endif
#include <time.h>
#include <resource.h>
/*
 * Per process structure containing data that
 * isn't needed in core when the process is swapped out.
 */
 
#define	SHSIZE	32
 
struct	user {
	struct	pcb u_pcb;
	struct	proc *u_procp;		/* pointer to proc structure */
	int	*u_ar0;			/* address of users saved R0 */
	char	u_comm[MAXNAMLEN + 1];

/* syscall parameters, results and catches */
	int	u_arg[8];		/* arguments to current system call */
	int	*u_ap;			/* pointer to arglist */
	label_t	u_qsave;		/* for non-local gotos on interrupts */
	char	u_error;		/* return error code */
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
	char	u_eosys;		/* special action on end of syscall */

/* 1.1 - processes and protection */
	short	u_uid;			/* effective user id */
	short	u_gid;			/* effective group id */
	int	u_groups[NGROUPS];	/* groups, 0 terminated */
	short	u_ruid;			/* real user id */
	short	u_rgid;			/* real group id */

/* 1.2 - memory management */
	size_t	u_tsize;		/* text size (clicks) */
	size_t	u_dsize;		/* data size (clicks) */
	size_t	u_ssize;		/* stack size (clicks) */
	struct	dmap u_dmap;		/* disk map for data segment */
	struct	dmap u_smap;		/* disk map for stack segment */
	struct	dmap u_cdmap, u_csmap;	/* shadows of u_dmap, u_smap, for
					   use of parent during fork */
	label_t u_ssave;		/* label variable for swapping */
	size_t	u_odsize, u_ossize;	/* for (clumsy) expansion swaps */
	time_t	u_outime;		/* user time at last sample */

/* 1.3 - signal management */
	int	(*u_signal[NSIG])();	/* disposition of signals */
	long	u_sigmask[NSIG];	/* signals to be blocked */
	int	u_code;			/* ``code'' to trap */
	caddr_t	u_sigstack;		/* 0 means no sigstack */
	int	u_onsigstack;

/* 1.4 - descriptor management */
	struct	file *u_ofile[NOFILE];	/* file structures for open files */
	char	u_pofile[NOFILE];	/* per-process flags of open files */
#define	UF_EXCLOSE 	0x1		/* auto-close on exec */
#define	UF_SHLOCK	0x2		/* shared lock present */
#define	UF_EXLOCK	0x4		/* exclusive lock present */
#define	UF_MAPPED 	0x8
	struct	inode *u_cdir;		/* current directory */
	struct	inode *u_rdir;		/* root directory of current process */
	struct	tty *u_ttyp;		/* controlling tty pointer */
	dev_t	u_ttyd;			/* controlling tty dev */
	short	u_cmask;		/* mask for file creation */

/* 1.5 - timing and statistics */
	struct	rusage u_ru;		/* stats for this proc */
	struct	rusage u_cru;		/* sum of stats for reaped children */
	struct	itimerval u_timer[3];
	int	u_XXX[3];
	time_t	u_start;
	short	u_acflag;

/* 1.6 - resource controls */
	struct	rlimit u_rlimit[RLIM_NLIMITS];
	struct	quota *u_quota;	/* user's quota structure */
	int	u_qflags;		/* per process quota flags */

/* BEGIN TRASH */
	char	u_segflg;		/* 0:user D; 1:system; 2:user I */
	caddr_t	u_base;			/* base address for IO */
	unsigned int u_count;		/* bytes remaining for IO */
	off_t	u_offset;		/* offset in file for IO */
	union {
	   struct {			/* header of executable file */
		int	Ux_mag;		/* magic number */
		unsigned Ux_tsize;	/* text size */
		unsigned Ux_dsize;	/* data size */
		unsigned Ux_bsize;	/* bss size */
		unsigned Ux_ssize;	/* symbol table size */
		unsigned Ux_entloc;	/* entry location */
		unsigned Ux_unused;
		unsigned Ux_relflg;
	   } Ux_A;
	   char ux_shell[SHSIZE];	/* #! and name of interpreter */
	} u_exdata;
#define	ux_mag		Ux_A.Ux_mag
#define	ux_tsize	Ux_A.Ux_tsize
#define	ux_dsize	Ux_A.Ux_dsize
#define	ux_bsize	Ux_A.Ux_bsize
#define	ux_ssize	Ux_A.Ux_ssize
#define	ux_entloc	Ux_A.Ux_entloc
#define	ux_unused	Ux_A.Ux_unused
#define	ux_relflg	Ux_A.Ux_relflg
	caddr_t	u_dirp;			/* pathname pointer */
	struct	direct u_dent;		/* current directory entry */
	struct	inode *u_pdir;		/* inode of parent directory of dirp */
/* END TRASH */
	struct uprof {			/* profile arguments */
		short	*pr_base;	/* buffer base */
		unsigned pr_size;	/* buffer size */
		unsigned pr_off;	/* pc offset */
		unsigned pr_scale;	/* pc scaling */
	} u_prof;
	int	u_stack[1];
};

/* u_eosys values */
#define	JUSTRETURN	0
#define	RESTARTSYS	1
#define	SIMULATERTI	2
#define	REALLYRETURN	3

/* u_error codes */
#include <errno.h>

#ifdef KERNEL
extern	struct user u;
extern	struct user swaputl;
extern	struct user forkutl;
extern	struct user xswaputl;
extern	struct user xswap2utl;
extern	struct user pushutl;
extern	struct user vfutl;
#endif
