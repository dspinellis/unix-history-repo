/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)specdev.h	8.2 (Berkeley) %G%
 */

/*
 * This structure defines the information maintained about
 * special devices. It is allocated in checkalias and freed
 * in vgone.
 */
struct specinfo {
	struct	vnode **si_hashchain;
	struct	vnode *si_specnext;
	long	si_flags;
	dev_t	si_rdev;
};
/*
 * Exported shorthand
 */
#define v_rdev v_specinfo->si_rdev
#define v_hashchain v_specinfo->si_hashchain
#define v_specnext v_specinfo->si_specnext
#define v_specflags v_specinfo->si_flags

/*
 * Flags for specinfo
 */
#define	SI_MOUNTEDON	0x0001	/* block special device is mounted on */

/*
 * Special device management
 */
#define	SPECHSZ	64
#if	((SPECHSZ&(SPECHSZ-1)) == 0)
#define	SPECHASH(rdev)	(((rdev>>5)+(rdev))&(SPECHSZ-1))
#else
#define	SPECHASH(rdev)	(((unsigned)((rdev>>5)+(rdev)))%SPECHSZ)
#endif

struct vnode *speclisth[SPECHSZ];

/*
 * Prototypes for special file operations on vnodes.
 */
extern	int (**spec_vnodeop_p)();
struct	nameidata;
struct	componentname;
struct	ucred;
struct	flock;
struct	buf;
struct	uio;

int	spec_badop(),
	spec_ebadf();

int	spec_lookup __P((struct vop_lookup_args *));
#define spec_create ((int (*) __P((struct  vop_create_args *)))spec_badop)
#define spec_mknod ((int (*) __P((struct  vop_mknod_args *)))spec_badop)
int	spec_open __P((struct vop_open_args *));
int	spec_close __P((struct vop_close_args *));
#define spec_access ((int (*) __P((struct  vop_access_args *)))spec_ebadf)
#define spec_getattr ((int (*) __P((struct  vop_getattr_args *)))spec_ebadf)
#define spec_setattr ((int (*) __P((struct  vop_setattr_args *)))spec_ebadf)
int	spec_read __P((struct vop_read_args *));
int	spec_write __P((struct vop_write_args *));
int	spec_ioctl __P((struct vop_ioctl_args *));
int	spec_select __P((struct vop_select_args *));
#define spec_mmap ((int (*) __P((struct  vop_mmap_args *)))spec_badop)
int	spec_fsync __P((struct  vop_fsync_args *));
#define spec_seek ((int (*) __P((struct  vop_seek_args *)))spec_badop)
#define spec_remove ((int (*) __P((struct  vop_remove_args *)))spec_badop)
#define spec_link ((int (*) __P((struct  vop_link_args *)))spec_badop)
#define spec_rename ((int (*) __P((struct  vop_rename_args *)))spec_badop)
#define spec_mkdir ((int (*) __P((struct  vop_mkdir_args *)))spec_badop)
#define spec_rmdir ((int (*) __P((struct  vop_rmdir_args *)))spec_badop)
#define spec_symlink ((int (*) __P((struct  vop_symlink_args *)))spec_badop)
#define spec_readdir ((int (*) __P((struct  vop_readdir_args *)))spec_badop)
#define spec_readlink ((int (*) __P((struct  vop_readlink_args *)))spec_badop)
#define spec_abortop ((int (*) __P((struct  vop_abortop_args *)))spec_badop)
#define spec_inactive ((int (*) __P((struct  vop_inactive_args *)))nullop)
#define spec_reclaim ((int (*) __P((struct  vop_reclaim_args *)))nullop)
int	spec_lock __P((struct vop_lock_args *));
int	spec_unlock __P((struct vop_unlock_args *));
int	spec_bmap __P((struct vop_bmap_args *));
int	spec_strategy __P((struct vop_strategy_args *));
int	spec_print __P((struct vop_print_args *));
#define spec_islocked ((int (*) __P((struct  vop_islocked_args *)))nullop)
int	spec_pathconf __P((struct vop_pathconf_args *));
int	spec_advlock __P((struct vop_advlock_args *));
#define spec_blkatoff ((int (*) __P((struct  vop_blkatoff_args *)))spec_badop)
#define spec_valloc ((int (*) __P((struct  vop_valloc_args *)))spec_badop)
#define spec_reallocblks \
	((int (*) __P((struct  vop_reallocblks_args *)))spec_badop)
#define spec_vfree ((int (*) __P((struct  vop_vfree_args *)))spec_badop)
#define spec_truncate ((int (*) __P((struct  vop_truncate_args *)))nullop)
#define spec_update ((int (*) __P((struct  vop_update_args *)))nullop)
#define spec_bwrite ((int (*) __P((struct  vop_bwrite_args *)))nullop)
