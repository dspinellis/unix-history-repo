/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dead_vnops.c	7.18 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "vnode.h"
#include "errno.h"
#include "namei.h"
#include "buf.h"

/*
 * Prototypes for dead operations on vnodes.
 */
int	dead_badop(),
	dead_ebadf();
int	dead_lookup __P((struct vop_lookup_args *));
#define dead_create ((int (*) __P((struct  vop_create_args *)))dead_badop)
#define dead_mknod ((int (*) __P((struct  vop_mknod_args *)))dead_badop)
int	dead_open __P((struct vop_open_args *));
#define dead_close ((int (*) __P((struct  vop_close_args *)))nullop)
#define dead_access ((int (*) __P((struct  vop_access_args *)))dead_ebadf)
#define dead_getattr ((int (*) __P((struct  vop_getattr_args *)))dead_ebadf)
#define dead_setattr ((int (*) __P((struct  vop_setattr_args *)))dead_ebadf)
int	dead_read __P((struct vop_read_args *));
int	dead_write __P((struct vop_write_args *));
int	dead_ioctl __P((struct vop_ioctl_args *));
int	dead_select __P((struct vop_select_args *));
#define dead_mmap ((int (*) __P((struct  vop_mmap_args *)))dead_badop)
#define dead_fsync ((int (*) __P((struct  vop_fsync_args *)))nullop)
#define dead_seek ((int (*) __P((struct  vop_seek_args *)))nullop)
#define dead_remove ((int (*) __P((struct  vop_remove_args *)))dead_badop)
#define dead_link ((int (*) __P((struct  vop_link_args *)))dead_badop)
#define dead_rename ((int (*) __P((struct  vop_rename_args *)))dead_badop)
#define dead_mkdir ((int (*) __P((struct  vop_mkdir_args *)))dead_badop)
#define dead_rmdir ((int (*) __P((struct  vop_rmdir_args *)))dead_badop)
#define dead_symlink ((int (*) __P((struct  vop_symlink_args *)))dead_badop)
#define dead_readdir ((int (*) __P((struct  vop_readdir_args *)))dead_ebadf)
#define dead_readlink ((int (*) __P((struct  vop_readlink_args *)))dead_ebadf)
#define dead_abortop ((int (*) __P((struct  vop_abortop_args *)))dead_badop)
#define dead_inactive ((int (*) __P((struct  vop_inactive_args *)))nullop)
#define dead_reclaim ((int (*) __P((struct  vop_reclaim_args *)))nullop)
int	dead_lock __P((struct vop_lock_args *));
#define dead_unlock ((int (*) __P((struct  vop_unlock_args *)))nullop)
int	dead_bmap __P((struct vop_bmap_args *));
int	dead_strategy __P((struct vop_strategy_args *));
int	dead_print __P((struct vop_print_args *));
#define dead_islocked ((int (*) __P((struct  vop_islocked_args *)))nullop)
#define dead_advlock ((int (*) __P((struct  vop_advlock_args *)))dead_ebadf)
#define dead_blkatoff ((int (*) __P((struct  vop_blkatoff_args *)))dead_badop)
#define dead_vget ((int (*) __P((struct  vop_vget_args *)))dead_badop)
#define dead_valloc ((int (*) __P((struct  vop_valloc_args *)))dead_badop)
#define dead_vfree ((int (*) __P((struct  vop_vfree_args *)))dead_badop)
#define dead_truncate ((int (*) __P((struct  vop_truncate_args *)))nullop)
#define dead_update ((int (*) __P((struct  vop_update_args *)))nullop)
#define dead_bwrite ((int (*) __P((struct  vop_bwrite_args *)))nullop)

int (**dead_vnodeop_p)();
struct vnodeopv_entry_desc dead_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, dead_lookup },	/* lookup */
	{ &vop_create_desc, dead_create },	/* create */
	{ &vop_mknod_desc, dead_mknod },	/* mknod */
	{ &vop_open_desc, dead_open },	/* open */
	{ &vop_close_desc, dead_close },	/* close */
	{ &vop_access_desc, dead_access },	/* access */
	{ &vop_getattr_desc, dead_getattr },	/* getattr */
	{ &vop_setattr_desc, dead_setattr },	/* setattr */
	{ &vop_read_desc, dead_read },	/* read */
	{ &vop_write_desc, dead_write },	/* write */
	{ &vop_ioctl_desc, dead_ioctl },	/* ioctl */
	{ &vop_select_desc, dead_select },	/* select */
	{ &vop_mmap_desc, dead_mmap },	/* mmap */
	{ &vop_fsync_desc, dead_fsync },	/* fsync */
	{ &vop_seek_desc, dead_seek },	/* seek */
	{ &vop_remove_desc, dead_remove },	/* remove */
	{ &vop_link_desc, dead_link },	/* link */
	{ &vop_rename_desc, dead_rename },	/* rename */
	{ &vop_mkdir_desc, dead_mkdir },	/* mkdir */
	{ &vop_rmdir_desc, dead_rmdir },	/* rmdir */
	{ &vop_symlink_desc, dead_symlink },	/* symlink */
	{ &vop_readdir_desc, dead_readdir },	/* readdir */
	{ &vop_readlink_desc, dead_readlink },	/* readlink */
	{ &vop_abortop_desc, dead_abortop },	/* abortop */
	{ &vop_inactive_desc, dead_inactive },	/* inactive */
	{ &vop_reclaim_desc, dead_reclaim },	/* reclaim */
	{ &vop_lock_desc, dead_lock },	/* lock */
	{ &vop_unlock_desc, dead_unlock },	/* unlock */
	{ &vop_bmap_desc, dead_bmap },	/* bmap */
	{ &vop_strategy_desc, dead_strategy },	/* strategy */
	{ &vop_print_desc, dead_print },	/* print */
	{ &vop_islocked_desc, dead_islocked },	/* islocked */
	{ &vop_advlock_desc, dead_advlock },	/* advlock */
	{ &vop_blkatoff_desc, dead_blkatoff },	/* blkatoff */
	{ &vop_vget_desc, dead_vget },	/* vget */
	{ &vop_valloc_desc, dead_valloc },	/* valloc */
	{ &vop_vfree_desc, dead_vfree },	/* vfree */
	{ &vop_truncate_desc, dead_truncate },	/* truncate */
	{ &vop_update_desc, dead_update },	/* update */
	{ &vop_bwrite_desc, dead_bwrite },	/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc dead_vnodeop_opv_desc =
	{ &dead_vnodeop_p, dead_vnodeop_entries };

/*
 * Trivial lookup routine that always fails.
 */
/* ARGSUSED */
int
dead_lookup (ap)
	struct vop_lookup_args *ap;
#define dvp (ap->a_dvp)
#define vpp (ap->a_vpp)
#define cnp (ap->a_cnp)
{

	*vpp = NULL;
	return (ENOTDIR);
}
#undef dvp
#undef vpp
#undef cnp

/*
 * Open always fails as if device did not exist.
 */
/* ARGSUSED */
dead_open (ap)
	struct vop_open_args *ap;
#define vp (ap->a_vp)
#define mode (ap->a_mode)
#define cred (ap->a_cred)
#define p (ap->a_p)
{

	return (ENXIO);
}
#undef vp
#undef mode
#undef cred
#undef p

/*
 * Vnode op for read
 */
/* ARGSUSED */
dead_read (ap)
	struct vop_read_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{

	if (chkvnlock(vp))
		panic("dead_read: lock");
	/*
	 * Return EOF for character devices, EIO for others
	 */
	if (vp->v_type != VCHR)
		return (EIO);
	return (0);
}
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Vnode op for write
 */
/* ARGSUSED */
dead_write (ap)
	struct vop_write_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{

	if (chkvnlock(vp))
		panic("dead_write: lock");
	return (EIO);
}
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
dead_ioctl (ap)
	struct vop_ioctl_args *ap;
#define vp (ap->a_vp)
#define com (ap->a_command)
#define data (ap->a_data)
#define fflag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	USES_VOP_IOCTL;

	if (!chkvnlock(vp))
		return (EBADF);
	return (VOP_IOCTL(vp, com, data, fflag, cred, p));
}
#undef vp
#undef com
#undef data
#undef fflag
#undef cred
#undef p

/* ARGSUSED */
dead_select (ap)
	struct vop_select_args *ap;
#define vp (ap->a_vp)
#define which (ap->a_which)
#define fflags (ap->a_fflags)
#define cred (ap->a_cred)
#define p (ap->a_p)
{

	/*
	 * Let the user find out that the descriptor is gone.
	 */
	return (1);
}
#undef vp
#undef which
#undef fflags
#undef cred
#undef p

/*
 * Just call the device strategy routine
 */
dead_strategy (ap)
	struct vop_strategy_args *ap;
#define bp (ap->a_bp)
{
	USES_VOP_STRATEGY;

	if (bp->b_vp == NULL || !chkvnlock(bp->b_vp)) {
		bp->b_flags |= B_ERROR;
		biodone(bp);
		return (EIO);
	}
	return (VOP_STRATEGY(bp));
}
#undef bp

/*
 * Wait until the vnode has finished changing state.
 */
dead_lock (ap)
	struct vop_lock_args *ap;
#define vp (ap->a_vp)
{
	USES_VOP_LOCK;

	if (!chkvnlock(vp))
		return (0);
	return (VOP_LOCK(vp));
}
#undef vp

/*
 * Wait until the vnode has finished changing state.
 */
dead_bmap (ap)
	struct vop_bmap_args *ap;
#define vp (ap->a_vp)
#define bn (ap->a_bn)
#define vpp (ap->a_vpp)
#define bnp (ap->a_bnp)
{
	USES_VOP_BMAP;

	if (!chkvnlock(vp))
		return (EIO);
	return (VOP_BMAP(vp, bn, vpp, bnp));
}
#undef vp
#undef bn
#undef vpp
#undef bnp

/*
 * Print out the contents of a dead vnode.
 */
/* ARGSUSED */
dead_print (ap)
	struct vop_print_args *ap;
#define vp (ap->a_vp)
{

	printf("tag VT_NON, dead vnode\n");
}
#undef vp

/*
 * Empty vnode failed operation
 */
dead_ebadf()
{

	return (EBADF);
}

/*
 * Empty vnode bad operation
 */
dead_badop()
{

	panic("dead_badop called");
	/* NOTREACHED */
}

/*
 * Empty vnode null operation
 */
dead_nullop()
{

	return (0);
}

/*
 * We have to wait during times when the vnode is
 * in a state of change.
 */
chkvnlock(vp)
	register struct vnode *vp;
{
	int locked = 0;

	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		locked = 1;
	}
	return (locked);
}
