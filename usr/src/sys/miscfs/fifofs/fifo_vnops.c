/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo_vnops.c	7.4 (Berkeley) %G%
 */

#include "param.h"
#include "time.h"
#include "namei.h"
#include "vnode.h"
#include "socket.h"
#include "socketvar.h"
#include "stat.h"
#include "ioctl.h"
#include "file.h"
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

int	fifo_lookup(),
	fifo_open(),
	fifo_read(),
	fifo_write(),
	fifo_strategy(),
	fifo_bmap(),
	fifo_ioctl(),
	fifo_select(),
	fifo_lock(),
	fifo_unlock(),
	fifo_close(),
	fifo_print(),
	fifo_ebadf(),
	fifo_badop(),
	fifo_nullop();

struct vnodeops fifo_vnodeops = {
	fifo_lookup,		/* lookup */
	fifo_badop,		/* create */
	fifo_badop,		/* mknod */
	fifo_open,		/* open */
	fifo_close,		/* close */
	fifo_ebadf,		/* access */
	fifo_ebadf,		/* getattr */
	fifo_ebadf,		/* setattr */
	fifo_read,		/* read */
	fifo_write,		/* write */
	fifo_ioctl,		/* ioctl */
	fifo_select,		/* select */
	fifo_badop,		/* mmap */
	fifo_nullop,		/* fsync */
	fifo_badop,		/* seek */
	fifo_badop,		/* remove */
	fifo_badop,		/* link */
	fifo_badop,		/* rename */
	fifo_badop,		/* mkdir */
	fifo_badop,		/* rmdir */
	fifo_badop,		/* symlink */
	fifo_badop,		/* readdir */
	fifo_badop,		/* readlink */
	fifo_badop,		/* abortop */
	fifo_nullop,		/* inactive */
	fifo_nullop,		/* reclaim */
	fifo_lock,		/* lock */
	fifo_unlock,		/* unlock */
	fifo_bmap,		/* bmap */
	fifo_badop,		/* strategy */
	fifo_print,		/* print */
	fifo_nullop,		/* islocked */
};

/*
 * Trivial lookup routine that always fails.
 */
fifo_lookup(vp, ndp)
	struct vnode *vp;
	struct nameidata *ndp;
{

	ndp->ni_dvp = vp;
	ndp->ni_vp = NULL;
	return (ENOTDIR);
}

/*
 * Open called to set up a new instance of a fifo or
 * to find an active instance of a fifo.
 */
/* ARGSUSED */
fifo_open(vp, mode, cred)
	register struct vnode *vp;
	int mode;
	struct ucred *cred;
{
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
		while (fip->fi_writers == 0)
			if (error = tsleep((caddr_t)&fip->fi_readers, PSOCK,
			    openstr, 0))
				break;
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
			while (fip->fi_readers == 0)
				if (error = tsleep((caddr_t)&fip->fi_writers,
				    PSOCK, openstr, 0))
					break;
		}
	}
	if (error)
		fifo_close(vp, mode, cred);
	return (error);
}

/*
 * Vnode op for read
 */
/* ARGSUSED */
fifo_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	register struct socket *rso = vp->v_fifoinfo->fi_readsock;
	int error, startresid;

	if (uio->uio_rw != UIO_READ)
		panic("fifo_read mode");
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

/*
 * Vnode op for write
 */
/* ARGSUSED */
fifo_write(vp, uio, ioflag, cred)
	struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	struct socket *wso = vp->v_fifoinfo->fi_writesock;
	int error;

	if (uio->uio_rw != UIO_WRITE)
		panic("fifo_write mode");
	if (ioflag & IO_NDELAY)
		wso->so_state |= SS_NBIO;
	VOP_UNLOCK(vp);
	error = sosend(wso, (struct mbuf *)0, uio, 0, (struct mbuf *)0);
	VOP_LOCK(vp);
	if (ioflag & IO_NDELAY)
		wso->so_state &= ~SS_NBIO;
	return (error);
}

/*
 * Device ioctl operation.
 */
/* ARGSUSED */
fifo_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{
	struct file filetmp;
	int error;

	if (com == FIONBIO)
		return (0);
	if (fflag & FREAD)
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_writesock;
	return (soo_ioctl(&filetmp, com, data));
}

/* ARGSUSED */
fifo_select(vp, which, fflag, cred)
	struct vnode *vp;
	int which, fflag;
	struct ucred *cred;
{
	struct file filetmp;
	int error;

	if (fflag & FREAD)
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_readsock;
	else
		filetmp.f_data = (caddr_t)vp->v_fifoinfo->fi_writesock;
	return (soo_select(&filetmp, which));
}

/*
 * This is a noop, simply returning what one has been given.
 */
fifo_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{

	if (vpp != NULL)
		*vpp = vp;
	if (bnp != NULL)
		*bnp = bn;
	return (0);
}

/*
 * At the moment we do not do any locking.
 */
/* ARGSUSED */
fifo_lock(vp)
	struct vnode *vp;
{

	return (0);
}

/* ARGSUSED */
fifo_unlock(vp)
	struct vnode *vp;
{

	return (0);
}

/*
 * Device close routine
 */
/* ARGSUSED */
fifo_close(vp, fflag, cred)
	register struct vnode *vp;
	int fflag;
	struct ucred *cred;
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

/*
 * Print out the contents of a fifo vnode.
 */
fifo_print(vp)
	struct vnode *vp;
{

	printf("tag VT_NON");
	fifo_printinfo(vp);
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
 * Fifo bad operation
 */
fifo_badop()
{

	panic("fifo_badop called");
	/* NOTREACHED */
}

/*
 * Fifo null operation
 */
fifo_nullop()
{

	return (0);
}
