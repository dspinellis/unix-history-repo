/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo.h	7.8 (Berkeley) %G%
 */

#ifdef FIFO
/*
 * Prototypes for fifo operations on vnodes.
 */
int	fifo_badop(),
	fifo_ebadf();

int	fifo_lookup __P((struct vop_lookup_args *));
#define fifo_create ((int (*) __P((struct  vop_create_args *)))fifo_badop)
#define fifo_mknod ((int (*) __P((struct  vop_mknod_args *)))fifo_badop)
int	fifo_open __P((struct vop_open_args *));
int	fifo_close __P((struct vop_close_args *));
#define fifo_access ((int (*) __P((struct  vop_access_args *)))fifo_ebadf)
#define fifo_getattr ((int (*) __P((struct  vop_getattr_args *)))fifo_ebadf)
#define fifo_setattr ((int (*) __P((struct  vop_setattr_args *)))fifo_ebadf)
int	fifo_read __P((struct vop_read_args *));
int	fifo_write __P((struct vop_write_args *));
int	fifo_ioctl __P((struct vop_ioctl_args *));
int	fifo_select __P((struct vop_select_args *));
#define fifo_mmap ((int (*) __P((struct  vop_mmap_args *)))fifo_badop)
#define fifo_fsync ((int (*) __P((struct  vop_fsync_args *)))nullop)
#define fifo_seek ((int (*) __P((struct  vop_seek_args *)))fifo_badop)
#define fifo_remove ((int (*) __P((struct  vop_remove_args *)))fifo_badop)
#define fifo_link ((int (*) __P((struct  vop_link_args *)))fifo_badop)
#define fifo_rename ((int (*) __P((struct  vop_rename_args *)))fifo_badop)
#define fifo_mkdir ((int (*) __P((struct  vop_mkdir_args *)))fifo_badop)
#define fifo_rmdir ((int (*) __P((struct  vop_rmdir_args *)))fifo_badop)
#define fifo_symlink ((int (*) __P((struct  vop_symlink_args *)))fifo_badop)
#define fifo_readdir ((int (*) __P((struct  vop_readdir_args *)))fifo_badop)
#define fifo_readlink ((int (*) __P((struct  vop_readlink_args *)))fifo_badop)
#define fifo_abortop ((int (*) __P((struct  vop_abortop_args *)))fifo_badop)
#define fifo_inactive ((int (*) __P((struct  vop_inactive_args *)))nullop)
#define fifo_reclaim ((int (*) __P((struct  vop_reclaim_args *)))nullop)
int	fifo_lock __P((struct vop_lock_args *));
int	fifo_unlock __P((struct vop_unlock_args *));
int	fifo_bmap __P((struct vop_bmap_args *));
#define fifo_strategy ((int (*) __P((struct  vop_strategy_args *)))fifo_badop)
int	fifo_print __P((struct vop_print_args *));
#define fifo_islocked ((int (*) __P((struct  vop_islocked_args *)))nullop)
int	fifo_pathconf __P((struct vop_pathconf_args *));
int	fifo_advlock __P((struct vop_advlock_args *));
#define fifo_blkatoff ((int (*) __P((struct  vop_blkatoff_args *)))fifo_badop)
#define fifo_valloc ((int (*) __P((struct  vop_valloc_args *)))fifo_badop)
#define fifo_vfree ((int (*) __P((struct  vop_vfree_args *)))fifo_badop)
#define fifo_truncate ((int (*) __P((struct  vop_truncate_args *)))nullop)
#define fifo_update ((int (*) __P((struct  vop_update_args *)))nullop)
#define fifo_bwrite ((int (*) __P((struct  vop_bwrite_args *)))nullop)
#endif /* FIFO */
