/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo_vnops.c	7.13 (Berkeley) %G%
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
 * Open called to set up a new instance of a fifo or
 * to find an active instance of a fifo.
 */
/* ARGSUSED */
fifo_open (ap)
	struct vop_open_args *ap;
#define vp (ap->a_vp)
#define mode (ap->a_mode)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	USES_VOP_CLOSE;
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	register struct fifoinfo *fip;
	struct socket *rso, *wso;
	int error;
	static char openstr[] = "fifo";

	if ((mode & (FREAD|FWRITE)) == (FREAD|FWRITE))
		return (EINVAL);
	if ((fip = vp->v_fifoinfo) == NULL) {
		MALLOC(fip, struct fifoinfo *, sizeof(*fip), M_VNODE, M_WAITOK);
		vp->v_fifoinfo = fip;
		if (error = socreate(AF_UNIX, &rso, SOCK_STREAM, 0)) {
			free(fip, M_VNODE);
			vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_readsock = rso;
		if (error = socreate(AF_UNIX, &wso, SOCK_STREAM, 0)) {
			(void)soclose(rso);
			free(fip, M_VNODE);
			vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_writesock = wso;
		if (error = unp_connect2(wso, rso)) {
			(void)soclose(wso);
			(void)soclose(rso);
			free(fip, M_VNODE);
			vp->v_fifoinfo = NULL;
			return (error);
		}
		fip->fi_readers = fip->fi_writers = 0;
		wso->so_state |= SS_CANTRCVMORE;
		rso->so_state |= SS_CANTSENDMORE;
	}
	error = 0;
	if (mode & FREAD) {
		fip->fi_readers++;
		if (fip->fi_readers == 1) {
			fip->fi_writesock->so_state &= ~SS_CANTSENDMORE;
			if (fip->fi_writers > 0)
				wakeup((caddr_t)&fip->fi_writers);
		}
		if (mode & O_NONBLOCK)
			return (0);
		while (fip->fi_writers == 0) {
			VOP_UNLOCK(vp);
			error = tsleep((caddr_t)&fip->fi_readers, PSOCK,
				openstr, 0);
			VOP_LOCK(vp);
			if (error)
				break;
		}
	} else {
		fip->fi_writers++;
		if (fip->fi_readers == 0 && (mode & O_NONBLOCK)) {
			error = ENXIO;
		} else {
			if (fip->fi_writers == 1) {
				fip->fi_readsock->so_state &= ~SS_CANTRCVMORE;
				if (fip->fi_readers > 0)
					wakeup((caddr_t)&fip->fi_readers);
			}
			while (fip->fi_readers == 0) {
				VOP_UNLOCK(vp);
				error = tsleep((caddr_t)&fip->fi_writers,
					PSOCK, openstr, 0);
				VOP_LOCK(vp);
				if (error)
					break;
			}
		}
	}
	if (error)
		VOP_CLOSE(vp, mode, cred, p);
	return (error);
}
#undef vp
#undef mode
#undef cred
#undef p

/*
 * Vnode op for read
 */
/* ARGSUSED */
fifo_read (ap)
	struct vop_read_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	register struct socket *rso = vp->v_fifoinfo->fi_readsock;
	int error, startresid;

#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_READ)
		panic("fifo_read mode");
#endif
	if (uio->uio_resid == 0)
		return (0);
	if (ioflag & IO_NDELAY)
		rso->so_state |= SS_NBIO;
	startresid = uio->uio_resid;
	VOP_UNLOCK(vp);
	error = soreceive(rso, (struct mbuf **)0, uio, (int *)0,
		(struct mbuf **)0, (struct mbuf **)0);
	VOP_LOCK(vp);
	/*
	 * Clear EOF indication after first such return.
	 */
	if (uio->uio_resid == startresid)
		rso->so_state &= ~SS_CANTRCVMORE;
	if (ioflag & IO_NDELAY)
		rso->so_state &= ~SS_NBIO;
	return (error);
}
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Vnode op for write
 */
/* ARGSUSED */
fifo_write (ap)
	struct vop_write_args *ap;
#define vp (ap->a_vp)
#define uio (ap->a_uio)
#define ioflag (ap->a_ioflag)
#define cred (ap->a_cred)
{
	USES_VOP_LOCK;
	USES_VOP_UNLOCK;
	struct socket *wso = vp->v_fifoinfo->fi_writesock;
	int error;

#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_WRITE)
		panic("fifo_write mode");
#endif
	if (ioflag & IO_NDELAY)
		wso->so_state |= SS_NBIO;
	VOP_UNLOCK(vp);
	error = sosend(wso, (struct mbuf *)0, uio, 0, (struct mbuf *)0, 0);
	VOP_LOCK(vp);
	if (ioflag & IO_NDELAY)
		wso->so_state &= ~SS_NBIO;
	return (error);
}
#undef vp
#undef uio
#undef ioflag
#undef cred

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
fifo_ioctl (ap)
	struct vop_ioctl_args *ap;
#define vp (ap->a_vp)
#define com (ap->a_command)
#define data (ap->a_data)
#define fflag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	struct file filetmp;
	int error;

	if (com == FIONBIO)
		return (0);
	if (fflag & FREAD)
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_writesock;
	return (soo_ioctl(&filetmp, com, data, p));
}
#undef vp
#undef com
#undef data
#undef fflag
#undef cred
#undef p

/* ARGSUSED */
fifo_select (ap)
	struct vop_select_args *ap;
#define vp (ap->a_vp)
#define which (ap->a_which)
#define fflag (ap->a_fflags)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	struct file filetmp;
	int error;

	if (fflag & FREAD)
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_writesock;
	return (soo_select(&filetmp, which, p));
}
#undef vp
#undef which
#undef fflag
#undef cred
#undef p

/*
 * This is a noop, simply returning what one has been given.
 */
fifo_bmap (ap)
	struct vop_bmap_args *ap;
#define vp (ap->a_vp)
#define bn (ap->a_bn)
#define vpp (ap->a_vpp)
#define bnp (ap->a_bnp)
{

	if (vpp != NULL)
		*vpp = vp;
	if (bnp != NULL)
		*bnp = bn;
	return (0);
}
#undef vp
#undef bn
#undef vpp
#undef bnp

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
fifo_lock (ap)
	struct vop_lock_args *ap;
#define vp (ap->a_vp)
{

	return (0);
}
#undef vp

/* ARGSUSED */
fifo_unlock (ap)
	struct vop_unlock_args *ap;
#define vp (ap->a_vp)
{

	return (0);
}
#undef vp

/*
 * Device close routine
 */
/* ARGSUSED */
fifo_close (ap)
	struct vop_close_args *ap;
#define vp (ap->a_vp)
#define fflag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	register struct fifoinfo *fip = vp->v_fifoinfo;
	int error1, error2;

	if (fflag & FWRITE) {
		fip->fi_writers--;
		if (fip->fi_writers == 0)
			socantrcvmore(fip->fi_readsock);
	} else {
		fip->fi_readers--;
		if (fip->fi_readers == 0)
			socantsendmore(fip->fi_writesock);
	}
	if (vp->v_usecount > 1)
		return (0);
	error1 = soclose(fip->fi_readsock);
	error2 = soclose(fip->fi_writesock);
	FREE(fip, M_VNODE);
	vp->v_fifoinfo = NULL;
	if (error1)
		return (error1);
	return (error2);
}
#undef vp
#undef fflag
#undef cred
#undef p

/*
 * Print out the contents of a fifo vnode.
 */
fifo_print (ap)
	struct vop_print_args *ap;
#define vp (ap->a_vp)
{

	printf("tag VT_NON");
	fifo_printinfo(vp);
	printf("\n");
}
#undef vp

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
#define vp (ap->a_vp)
#define id (ap->a_id)
#define op (ap->a_op)
#define fl (ap->a_fl)
#define flags (ap->a_flags)
{

	return (EOPNOTSUPP);
}
#undef vp
#undef id
#undef op
#undef fl
#undef flags

/*
 * Fifo bad operation
 */
fifo_badop()
{

	panic("fifo_badop called");
	/* NOTREACHED */
}
