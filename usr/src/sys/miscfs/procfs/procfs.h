/*
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1993 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)procfs.h	8.3 (Berkeley) %G%
 *
 * From:
 *	$Id: procfs.h,v 3.2 1993/12/15 09:40:17 jsp Exp $
 */

/*
 * The different types of node in a procfs filesystem
 */
typedef enum {
	Proot,		/* the filesystem root */
	Pproc,		/* a process-specific sub-directory */
	Pfile,		/* the executable file */
	Pmem,		/* the process's memory image */
	Pregs,		/* the process's register set */
	Pctl,		/* process control */
	Pstatus,	/* process status */
	Pnote,		/* process notifier */
	Pnotepg		/* process group notifier */
} pfstype;

/*
 * control data for the proc file system.
 */
struct pfsnode {
	struct pfsnode	*pfs_next;	/* next on list */
	struct vnode	*pfs_vnode;	/* vnode associated with this pfsnode */
	pfstype		pfs_type;	/* type of procfs node */
	pid_t		pfs_pid;	/* associated process */
	u_short		pfs_mode;	/* mode bits for stat() */
	u_long		pfs_flags;	/* open flags */
	u_long		pfs_fileno;	/* unique file id */
};

#define PROCFS_NOTELEN	64	/* max length of a note (/proc/$pid/note) */
#define PROCFS_CTLLEN 	8	/* max length of a ctl msg (/proc/$pid/ctl */

/*
 * Kernel stuff follows
 */
#ifdef KERNEL
#define CNEQ(cnp, s, len) \
	 ((cnp)->cn_namelen == (len) && \
	  (bcmp((s), (cnp)->cn_nameptr, (len)) == 0))

/*
 * Format of a directory entry in /proc, ...
 * This must map onto struct dirent (see <dirent.h>)
 */
#define PROCFS_NAMELEN 8
struct pfsdent {
	u_long	d_fileno;
	u_short	d_reclen;
	u_char	d_type;
	u_char	d_namlen;
	char	d_name[PROCFS_NAMELEN];
};
#define UIO_MX sizeof(struct pfsdent)
#define PROCFS_FILENO(pid, type) \
	(((type) == Proot) ? \
			2 : \
			((((pid)+1) << 3) + ((int) (type))))

/*
 * Convert between pfsnode vnode
 */
#define VTOPFS(vp)	((struct pfsnode *)(vp)->v_data)
#define PFSTOV(pfs)	((pfs)->pfs_vnode)

typedef struct vfs_namemap vfs_namemap_t;
struct vfs_namemap {
	const char *nm_name;
	int nm_val;
};

extern int vfs_getuserstr __P((struct uio *, char *, int *));
extern vfs_namemap_t *vfs_findname __P((vfs_namemap_t *, char *, int));

/* <machine/reg.h> */
struct reg;

#define PFIND(pid) ((pid) ? pfind(pid) : &proc0)
extern int procfs_freevp __P((struct vnode *));
extern int procfs_allocvp __P((struct mount *, struct vnode **, long, pfstype));
extern struct vnode *procfs_findtextvp __P((struct proc *));
extern int procfs_sstep __P((struct proc *));
extern void procfs_fix_sstep __P((struct proc *));
extern int procfs_read_regs __P((struct proc *, struct reg *));
extern int procfs_write_regs __P((struct proc *, struct reg *));
extern int procfs_donote __P((struct proc *, struct proc *, struct pfsnode *pfsp, struct uio *uio));
extern int procfs_doregs __P((struct proc *, struct proc *, struct pfsnode *pfsp, struct uio *uio));
extern int procfs_domem __P((struct proc *, struct proc *, struct pfsnode *pfsp, struct uio *uio));
extern int procfs_doctl __P((struct proc *, struct proc *, struct pfsnode *pfsp, struct uio *uio));
extern int procfs_dostatus __P((struct proc *, struct proc *, struct pfsnode *pfsp, struct uio *uio));

#define PROCFS_LOCKED	0x01
#define PROCFS_WANT	0x02

extern int (**procfs_vnodeop_p)();
extern struct vfsops procfs_vfsops;

/*
 * Prototypes for procfs vnode ops
 */
int	procfs_badop();	/* varargs */
int	procfs_rw __P((struct vop_read_args *));
int	procfs_lookup __P((struct vop_lookup_args *));
#define procfs_create ((int (*) __P((struct vop_create_args *))) procfs_badop)
#define procfs_mknod ((int (*) __P((struct vop_mknod_args *))) procfs_badop)
int	procfs_open __P((struct vop_open_args *));
int	procfs_close __P((struct vop_close_args *));
int	procfs_access __P((struct vop_access_args *));
int	procfs_getattr __P((struct vop_getattr_args *));
int	procfs_setattr __P((struct vop_setattr_args *));
#define	procfs_read procfs_rw
#define	procfs_write procfs_rw
int	procfs_ioctl __P((struct vop_ioctl_args *));
#define procfs_select ((int (*) __P((struct vop_select_args *))) procfs_badop)
#define procfs_mmap ((int (*) __P((struct vop_mmap_args *))) procfs_badop)
#define procfs_fsync ((int (*) __P((struct vop_fsync_args *))) procfs_badop)
#define procfs_seek ((int (*) __P((struct vop_seek_args *))) procfs_badop)
#define procfs_remove ((int (*) __P((struct vop_remove_args *))) procfs_badop)
#define procfs_link ((int (*) __P((struct vop_link_args *))) procfs_badop)
#define procfs_rename ((int (*) __P((struct vop_rename_args *))) procfs_badop)
#define procfs_mkdir ((int (*) __P((struct vop_mkdir_args *))) procfs_badop)
#define procfs_rmdir ((int (*) __P((struct vop_rmdir_args *))) procfs_badop)
#define procfs_symlink ((int (*) __P((struct vop_symlink_args *))) procfs_badop)
int	procfs_readdir __P((struct vop_readdir_args *));
#define procfs_readlink ((int (*) __P((struct vop_readlink_args *))) procfs_badop)
int	procfs_abortop __P((struct vop_abortop_args *));
int	procfs_inactive __P((struct vop_inactive_args *));
int	procfs_reclaim __P((struct vop_reclaim_args *));
#define procfs_lock ((int (*) __P((struct vop_lock_args *))) nullop)
#define procfs_unlock ((int (*) __P((struct vop_unlock_args *))) nullop)
int	procfs_bmap __P((struct vop_bmap_args *));
#define	procfs_strategy ((int (*) __P((struct vop_strategy_args *))) procfs_badop)
int	procfs_print __P((struct vop_print_args *));
#define procfs_islocked ((int (*) __P((struct vop_islocked_args *))) nullop)
#define procfs_advlock ((int (*) __P((struct vop_advlock_args *))) procfs_badop)
#define procfs_blkatoff ((int (*) __P((struct vop_blkatoff_args *))) procfs_badop)
#define procfs_valloc ((int (*) __P((struct vop_valloc_args *))) procfs_badop)
#define procfs_vfree ((int (*) __P((struct vop_vfree_args *))) nullop)
#define procfs_truncate ((int (*) __P((struct vop_truncate_args *))) procfs_badop)
#define procfs_update ((int (*) __P((struct vop_update_args *))) nullop)
#endif /* KERNEL */
