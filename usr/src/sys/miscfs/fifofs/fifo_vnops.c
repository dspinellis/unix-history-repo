/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo_vnops.c	7.14 (Berkeley) %G%
 */

#include "param.h"
#include "proc.h"
#include "time.h"
#include "namei.h"
#include "vnode.h"
#include "socket.h"
#include "socketvar.h"
#include "stat.h"
#include "systm.h"
#include "ioctl.h"
#include "file.h"
#include "fifo.h"
#include "errno.h"
#include "malloc.h"

/*
 * This structure is associated with the FIFO vnode and stores
 * the state associated with the FIFO.
 */
struct fifoinfo {
	struct socket	*fi_readsock;
	struct socket	*fi_writesock;
	long		fi_readers;
	long		fi_writers;
};

int (**fifo_vnodeop_p)();
struct vnodeopv_entry_desc fifo_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, fifo_lookup },		/* lookup */
	{ &vop_create_desc, fifo_create },		/* create */
	{ &vop_mknod_desc, fifo_mknod },		/* mknod */
	{ &vop_open_desc, fifo_open },		/* open */
	{ &vop_close_desc, fifo_close },		/* close */
	{ &vop_access_desc, fifo_access },		/* access */
	{ &vop_getattr_desc, fifo_getattr },		/* getattr */
	{ &vop_setattr_desc, fifo_setattr },		/* setattr */
	{ &vop_read_desc, fifo_read },		/* read */
	{ &vop_write_desc, fifo_write },		/* write */
	{ &vop_ioctl_desc, fifo_ioctl },		/* ioctl */
	{ &vop_select_desc, fifo_select },		/* select */
	{ &vop_mmap_desc, fifo_mmap },		/* mmap */
	{ &vop_fsync_desc, fifo_fsync },		/* fsync */
	{ &vop_seek_desc, fifo_seek },		/* seek */
	{ &vop_remove_desc, fifo_remove },		/* remove */
	{ &vop_link_desc, fifo_link },		/* link */
	{ &vop_rename_desc, fifo_rename },		/* rename */
	{ &vop_mkdir_desc, fifo_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, fifo_rmdir },		/* rmdir */
	{ &vop_symlink_desc, fifo_symlink },		/* symlink */
	{ &vop_readdir_desc, fifo_readdir },		/* readdir */
	{ &vop_readlink_desc, fifo_readlink },		/* readlink */
	{ &vop_abortop_desc, fifo_abortop },		/* abortop */
	{ &vop_inactive_desc, fifo_inactive },		/* inactive */
	{ &vop_reclaim_desc, fifo_reclaim },		/* reclaim */
	{ &vop_lock_desc, fifo_lock },		/* lock */
	{ &vop_unlock_desc, fifo_unlock },		/* unlock */
	{ &vop_bmap_desc, fifo_bmap },		/* bmap */
	{ &vop_strategy_desc, fifo_strategy },		/* strategy */
	{ &vop_print_desc, fifo_print },		/* print */
	{ &vop_islocked_desc, fifo_islocked },		/* islocked */
	{ &vop_advlock_desc, fifo_advlock },		/* advlock */
	{ &vop_blkatoff_desc, fifo_blkatoff },		/* blkatoff */
	{ &vop_vget_desc, fifo_vget },		/* vget */
	{ &vop_valloc_desc, fifo_valloc },		/* valloc */
	{ &vop_vfree_desc, fifo_vfree },		/* vfree */
	{ &vop_truncate_desc, fifo_truncate },		/* truncate */
	{ &vop_update_desc, fifo_update },		/* update */
	{ &vop_bwrite_desc, fifo_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc fifo_vnodeop_opv_desc =
	{ &fifo_vnodeop_p, fifo_vnodeop_entries };

/*
 * Trivial lookup routine that always fails.
 */
/* ARGSUSED */
fifo_lookup (ap)
	struct vop_lookup_args *ap;
{
	
	*ap->a_vpp = NULL;
	return (ENOTDIR);
}

/*
 * Open called to set up a new instance of a fifo or
 * to find an active instance of a fifo.
 */
/* ARGSUSED */
fifo_open (ap)
	struct vop_open_args *ap;
{
	USES_VOP_CLOSE;
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	register struct fifoinfo *fip;
	struct socket *rso, *wso;
	int error;
	static char openstr[] = "fifo";

	if ((ap->a_mode & (FREAD|FWRITE)) == (FREAD|FWRITE))
		return (EINVAL);
	if ((fip = ap->a_vp->v_fifoinfo) == NULL) {
		MALLOC(fip, struct fifoinfo *, sizeof(*fip), M_VNODE, M_WAITOK);
		ap->a_vp->v_fifoinfo = fip;
		if (error = socreate(AF_UNIX, &rso, SOCK_STREAM, 0)) {
			free(fip, M_VNODE);
			ap->a_vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_readsock = rso;
		if (error = socreate(AF_UNIX, &wso, SOCK_STREAM, 0)) {
			(void)soclose(rso);
			free(fip, M_VNODE);
			ap->a_vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_writesock = wso;
		if (error = unp_connect2(wso, rso)) {
			(void)soclose(wso);
			(void)soclose(rso);
			free(fip, M_VNODE);
			ap->a_vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_readers = fip->fi_writers = 0;
		wso->so_state |= SS_CANTRCVMORE;
		rso->so_state |= SS_CANTSENDMORE;
	}
	error = 0;
	if (ap->a_mode & FREAD) {
		fip->fi_readers++;
		if (fip->fi_readers == 1) {
			fip->fi_writesock->so_state &= ~SS_CANTSENDMORE;
			if (fip->fi_writers > 0)
				wakeup((caddr_t)&fip->fi_writers);
		}
		if (ap->a_mode & O_NONBLOCK)
			return (0);
		while (fip->fi_writers == 0) {
			VOP_UNLOCK(ap->a_vp);
			error = tsleep((caddr_t)&fip->fi_readers, PSOCK,
				openstr, 0);
			VOP_LOCK(ap->a_vp);
			if (error)
				break;
		}
	} else {
		fip->fi_writers++;
		if (fip->fi_readers == 0 && (ap->a_mode & O_NONBLOCK)) {
			error = ENXIO;
		} else {
			if (fip->fi_writers == 1) {
				fip->fi_readsock->so_state &= ~SS_CANTRCVMORE;
				if (fip->fi_readers > 0)
					wakeup((caddr_t)&fip->fi_readers);
			}
			while (fip->fi_readers == 0) {
				VOP_UNLOCK(ap->a_vp);
				error = tsleep((caddr_t)&fip->fi_writers,
					PSOCK, openstr, 0);
				VOP_LOCK(ap->a_vp);
				if (error)
					break;
			}
		}
	}
	if (error)
		VOP_CLOSE(ap->a_vp, ap->a_mode, ap->a_cred, ap->a_p);
	return (error);
}

/*
 * Vnode op for read
 */
/* ARGSUSED */
fifo_read (ap)
	struct vop_read_args *ap;
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	register struct socket *rso = ap->a_vp->v_fifoinfo->fi_readsock;
	int error, startresid;

#ifdef DIAGNOSTIC
	if (ap->a_uio->uio_rw != UIO_READ)
		panic("fifo_read mode");
#endif
	if (ap->a_uio->uio_resid == 0)
		return (0);
	if (ap->a_ioflag & IO_NDELAY)
		rso->so_state |= SS_NBIO;
	startresid = ap->a_uio->uio_resid;
	VOP_UNLOCK(ap->a_vp);
	error = soreceive(rso, (struct mbuf **)0, ap->a_uio, (int *)0,
		(struct mbuf **)0, (struct mbuf **)0);
	VOP_LOCK(ap->a_vp);
	/*
	 * Clear EOF indication after first such return.
	 */
	if (ap->a_uio->uio_resid == startresid)
		rso->so_state &= ~SS_CANTRCVMORE;
	if (ap->a_ioflag & IO_NDELAY)
		rso->so_state &= ~SS_NBIO;
	return (error);
}

/*
 * Vnode op for write
 */
/* ARGSUSED */
fifo_write (ap)
	struct vop_write_args *ap;
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct socket *wso = ap->a_vp->v_fifoinfo->fi_writesock;
	int error;

#ifdef DIAGNOSTIC
	if (ap->a_uio->uio_rw != UIO_WRITE)
		panic("fifo_write mode");
#endif
	if (ap->a_ioflag & IO_NDELAY)
		wso->so_state |= SS_NBIO;
	VOP_UNLOCK(ap->a_vp);
	error = sosend(wso, (struct mbuf *)0, ap->a_uio, 0, (struct mbuf *)0, 0);
	VOP_LOCK(ap->a_vp);
	if (ap->a_ioflag & IO_NDELAY)
		wso->so_state &= ~SS_NBIO;
	return (error);
}

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
fifo_ioctl (ap)
	struct vop_ioctl_args *ap;
{
	struct file filetmp;
	int error;

	if (ap->a_command == FIONBIO)
		return (0);
	if (ap->a_fflag & FREAD)
		filetmp.f_data = (caddr_t)ap->a_vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)ap->a_vp->v_fifoinfo->fi_writesock;
	return (soo_ioctl(&filetmp, ap->a_command, ap->a_data, ap->a_p));
}

/* ARGSUSED */
fifo_select (ap)
	struct vop_select_args *ap;
{
	struct file filetmp;
	int error;

	if (ap->a_fflags & FREAD)
		filetmp.f_data = (caddr_t)ap->a_vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)ap->a_vp->v_fifoinfo->fi_writesock;
	return (soo_select(&filetmp, ap->a_which, ap->a_p));
}

/*
 * This is a noop, simply returning what one has been given.
 */
fifo_bmap (ap)
	struct vop_bmap_args *ap;
{

	if (ap->a_vpp != NULL)
		*ap->a_vpp = ap->a_vp;
	if (ap->a_bnp != NULL)
		*ap->a_bnp = ap->a_bn;
	return (0);
}

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
fifo_lock (ap)
	struct vop_lock_args *ap;
{

	return (0);
}

/* ARGSUSED */
fifo_unlock (ap)
	struct vop_unlock_args *ap;
{

	return (0);
}

/*
 * Device close routine
 */
/* ARGSUSED */
fifo_close (ap)
	struct vop_close_args *ap;
{
	register struct fifoinfo *fip = ap->a_vp->v_fifoinfo;
	int error1, error2;

	if (ap->a_fflag & FWRITE) {
		fip->fi_writers--;
		if (fip->fi_writers == 0)
			socantrcvmore(fip->fi_readsock);
	} else {
		fip->fi_readers--;
		if (fip->fi_readers == 0)
			socantsendmore(fip->fi_writesock);
	}
	if (ap->a_vp->v_usecount > 1)
		return (0);
	error1 = soclose(fip->fi_readsock);
	error2 = soclose(fip->fi_writesock);
	FREE(fip, M_VNODE);
	ap->a_vp->v_fifoinfo = NULL;
	if (error1)
		return (error1);
	return (error2);
}

/*
 * Print out the contents of a fifo vnode.
 */
fifo_print (ap)
	struct vop_print_args *ap;
{

	printf("tag VT_NON");
	fifo_printinfo(ap->a_vp);
	printf("\n");
}

/*
 * Print out internal contents of a fifo vnode.
 */
fifo_printinfo(vp)
	struct vnode *vp;
{
	register struct fifoinfo *fip = vp->v_fifoinfo;

	printf(", fifo with %d readers and %d writers",
		fip->fi_readers, fip->fi_writers);
}

/*
 * Fifo failed operation
 */
fifo_ebadf()
{

	return (EBADF);
}

/*
 * Fifo advisory byte-level locks.
 */
/* ARGSUSED */
fifo_advlock (ap)
	struct vop_advlock_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * Fifo bad operation
 */
fifo_badop()
{

	panic("fifo_badop called");
	/* NOTREACHED */
}
