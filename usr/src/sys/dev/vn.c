/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: fd.c 1.1 90/07/09$
 *
 *	@(#)vn.c	7.3 (Berkeley) %G%
 */

/*
 * File (vnode) disk driver.
 *
 * Block/character interface to a vnode.  Note that this uses the
 * VOP_BMAP/VOP_STRATEGY interface to the vnode instead of a simple
 * VOP_RDWR.  We do this to avoid distorting the local buffer cache.
 *
 * NOTE: There is a security issue involved with this driver.
 * Once mounted all access to the contents of the "mapped" file via
 * the special file is controlled by the permissions on the special
 * file, the protection of the mapped file is ignored (effectively,
 * by using root credentials in all transactions).
 */
#include "fd.h"
#if NFD > 0

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/errno.h"
#include "sys/dkstat.h"
#include "sys/ioctl.h"
#include "sys/user.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "sys/uio.h"
#include "sys/malloc.h"

#include "fdioctl.h"

#ifdef DEBUG
int fddebug = 0x00;
#define FDB_FOLLOW	0x01
#define FDB_INIT	0x02
#define FDB_IO		0x04
#endif

struct	buf fdbuf[NFD];
struct	buf fdtab[NFD];

#define b_cylin	b_resid

#define	fdunit(x)	((minor(x) >> 3) & 0x7)	/* for consistency */

#define	getfdbuf()	\
	((struct buf *)malloc(sizeof(struct buf), M_DEVBUF, M_WAITOK))
#define putfdbuf(bp)	\
	free((caddr_t)(bp), M_DEVBUF)

struct fd_softc {
	int		 sc_flags;	/* flags */
	size_t		 sc_size;	/* size of fd */
	struct vnode	*sc_vp;		/* vnode */
	struct ucred	*sc_cred;	/* credentials */
	int		 sc_maxactive;	/* max # of active requests */
} fd_softc[NFD];

/* sc_flags */
#define	FDF_ALIVE	0x01
#define FDF_INITED	0x02

fdopen(dev, flags)
	dev_t dev;
{
	int unit = fdunit(dev);

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdopen(%x, %x)\n", dev, flags);
#endif
	if (unit >= NFD)
		return(ENXIO);
	return(0);
}

/*
 * Break the request into bsize pieces and submit using VOP_BMAP/VOP_STRATEGY.
 * Note that this driver can only be used for swapping over NFS on the hp
 * since nfs_strategy on the vax cannot handle u-areas and page tables.
 */
fdstrategy(bp)
	register struct buf *bp;
{
	int unit = fdunit(bp->b_dev);
	register struct fd_softc *fs = &fd_softc[unit];
	register struct buf *nbp;
	register int bn, bsize, resid;
	register caddr_t addr;
	int sz, flags;
	extern int fdiodone();

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdstrategy(%x): unit %d\n", bp, unit);
#endif
	if ((fs->sc_flags & FDF_INITED) == 0) {
		bp->b_error = ENXIO;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bn = bp->b_blkno;
	sz = howmany(bp->b_bcount, DEV_BSIZE);
	bp->b_resid = bp->b_bcount;
	if (bn < 0 || bn + sz > fs->sc_size) {
		if (bn != fs->sc_size) {
			bp->b_error = EINVAL;
			bp->b_flags |= B_ERROR;
		}
		iodone(bp);
		return;
	}
	bn = dbtob(bn);
	bsize = fs->sc_vp->v_vfsp->vfs_bsize;
	addr = bp->b_un.b_addr;
	flags = bp->b_flags | B_CALL;
	for (resid = bp->b_resid; resid; resid -= sz) {
		struct vnode *vp;
		daddr_t nbn;
		int off, s;

		nbp = getfdbuf();
		off = bn % bsize;
		sz = MIN(bsize - off, resid);
		(void) VOP_BMAP(fs->sc_vp, bn / bsize, &vp, &nbn);
#ifdef DEBUG
		if (fddebug & FDB_IO)
			printf("fdstrategy: vp %x/%x bn %x/%x dev %x\n",
			       fs->sc_vp, vp, bn, nbn, vp->v_rdev);
#endif
		nbp->b_flags = flags;
		nbp->b_bcount = sz;
		nbp->b_bufsize = bp->b_bufsize;
		nbp->b_error = 0;
		nbp->b_dev = vp->v_rdev;
		nbp->b_un.b_addr = addr;
		nbp->b_blkno = nbn + btodb(off);
		nbp->b_proc = bp->b_proc;
		nbp->b_iodone = fdiodone;
		nbp->b_vp = vp;
		nbp->b_pfcent = (int) bp;	/* XXX */
		/*
		 * Just sort by block number
		 */
		nbp->b_cylin = nbp->b_blkno;
		s = splbio();
		disksort(&fdtab[unit], nbp);
		if (fdtab[unit].b_active < fs->sc_maxactive) {
			fdtab[unit].b_active++;
			fdstart(unit);
		}
		splx(s);
		bn += sz;
		addr += sz;
	}
}

/*
 * Feed requests sequentially.
 * We do it this way to keep from flooding NFS servers if we are connected
 * to an NFS file.  This places the burden on the client rather than the
 * server.
 */
fdstart(unit)
{
	register struct fd_softc *fs = &fd_softc[unit];
	register struct buf *bp;

	/*
	 * Dequeue now since lower level strategy routine might
	 * queue using same links
	 */
	bp = fdtab[unit].b_actf;
	fdtab[unit].b_actf = bp->b_actf;
#ifdef DEBUG
	if (fddebug & FDB_IO)
		printf("fdstart(%d): bp %x vp %x blkno %x addr %x cnt %x\n",
		       unit, bp, bp->b_vp, bp->b_blkno, bp->b_un.b_addr,
		       bp->b_bcount);
#endif
	VOP_STRATEGY(bp);
}

fdiodone(bp)
	register struct buf *bp;
{
	register struct buf *pbp = (struct buf *)bp->b_pfcent;	/* XXX */
	register int unit = fdunit(pbp->b_dev);
	int s;

	s = splbio();
#ifdef DEBUG
	if (fddebug & FDB_IO)
		printf("fdiodone(%d): bp %x vp %x blkno %x addr %x cnt %x\n",
		       unit, bp, bp->b_vp, bp->b_blkno, bp->b_un.b_addr,
		       bp->b_bcount);
#endif
	if (bp->b_error) {
#ifdef DEBUG
		if (fddebug & FDB_IO)
			printf("fdiodone: bp %x error %d\n", bp, bp->b_error);
#endif
		pbp->b_flags |= B_ERROR;
		pbp->b_error = geterror(bp);
	}
	pbp->b_resid -= bp->b_bcount;
	putfdbuf(bp);
	if (pbp->b_resid == 0) {
#ifdef DEBUG
		if (fddebug & FDB_IO)
			printf("fdiodone: pbp %x iodone\n", pbp);
#endif
		iodone(pbp);
	}
	if (fdtab[unit].b_actf)
		fdstart(unit);
	else
		fdtab[unit].b_active--;
	splx(s);
}

fdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = fdunit(dev);

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdread(%x, %x)\n", dev, uio);
#endif
	return(physio(fdstrategy, &fdbuf[unit], dev, B_READ, minphys, uio));
}

fdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = fdunit(dev);

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdwrite(%x, %x)\n", dev, uio);
#endif
	return(physio(fdstrategy, &fdbuf[unit], dev, B_WRITE, minphys, uio));
}

/* ARGSUSED */
fdioctl(dev, cmd, data, flag)
	dev_t dev;
	u_long cmd;
	caddr_t data;
	int flag;
{
	int unit = fdunit(dev);
	register struct fd_softc *fs;
	struct fd_ioctl *fio;
	struct vattr vattr;
	struct vnode *vp;
	int error;

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdioctl(%x, %x, %x, %x): unit %d\n",
		       dev, cmd, data, flag, unit);
#endif
	error = suser(u.u_cred, &u.u_acflag);
	if (error)
		return (error);
	if (unit >= NFD)
		return (ENXIO);

	fs = &fd_softc[unit];
	fio = (struct fd_ioctl *)data;
	switch (cmd) {

	case FDIOCSET:
		if (fs->sc_flags & FDF_INITED)
			return(EBUSY);
		/*
		 * Always open for read and write.
		 * This is probably bogus, but it lets vn_open()
		 * weed out directories, sockets, etc. so we don't
		 * have to worry about them.
		 */
		error = vn_open(fio->fd_file, UIO_USERSPACE,
				FREAD|FWRITE, 0, &vp);
		if (error)
			return(error);
		error = VOP_GETATTR(vp, &vattr, u.u_cred);
		if (error) {
			vn_close(vp, FREAD|FWRITE);
			VN_RELE(vp);
			return(error);
		}
		fs->sc_vp = vp;
		fs->sc_size = btodb(vattr.va_size);	/* note truncation */
		error = fdsetcred(fs);
		if (error) {
			vn_close(vp, FREAD|FWRITE);
			VN_RELE(vp);
			return(error);
		}
		fdthrottle(fs, vp);
		fio->fd_size = dbtob(fs->sc_size);
		fs->sc_flags |= FDF_INITED;
#ifdef DEBUG
		if (fddebug & FDB_INIT)
			printf("fdioctl: SET vp %x size %x\n",
			       fs->sc_vp, fs->sc_size);
#endif
		break;

	case FDIOCCLR:
		if ((fs->sc_flags & FDF_INITED) == 0)
			return(ENXIO);
		fdclear(fs);
#ifdef DEBUG
		if (fddebug & FDB_INIT)
			printf("fdioctl: CLRed\n");
#endif
		break;

	default:
		return(ENXIO);
	}
	return(0);
}

/*
 * Duplicate the current processes' credentials.  Since we are called only
 * as the result of a SET ioctl and only root can do that, any future access
 * to this "disk" is essentially as root.  Note that credentials may change
 * if some other uid can write directly to the mapped file (NFS).
 */
fdsetcred(fs)
	register struct fd_softc *fs;
{
	struct uio auio;
	struct iovec aiov;
	char tmpbuf[DEV_BSIZE];

	fs->sc_cred = crdup(u.u_cred);
	/* XXX: Horrible kludge to establish credentials for NFS */
	aiov.iov_base = tmpbuf;
	aiov.iov_len = MIN(DEV_BSIZE, dbtob(fs->sc_size));
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = 0;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_SYSSPACE;
	auio.uio_resid = aiov.iov_len;
	return(VOP_READ(fs->sc_vp, &auio, 0, fs->sc_cred));
}

/*
 * Set maxactive based on FS type
 */
fdthrottle(fs, vp)
	register struct fd_softc *fs;
	struct vnode *vp;
{
	extern struct vnodeops ufs_vnodeops, nfs_vnodeops;

	if (vp->v_op == &nfs_vnodeops)
		fs->sc_maxactive = 2;
	else
		fs->sc_maxactive = 8;

	if (fs->sc_maxactive < 1)
		fs->sc_maxactive = 1;
}

fdshutdown()
{
	register struct fd_softc *fs;

	for (fs = &fd_softc[0]; fs < &fd_softc[NFD]; fs++)
		if (fs->sc_flags & FDF_INITED)
			fdclear(fs);
}

fdclear(fs)
	register struct fd_softc *fs;
{
	register struct vnode *vp = fs->sc_vp;

#ifdef DEBUG
	if (fddebug & FDB_FOLLOW)
		printf("fdclear(%x): vp %x\n", vp);
#endif
	fs->sc_flags &= ~FDF_INITED;
	if (vp == (struct vnode *)0)
		panic("fdioctl: null vp");
#if 0
	/* XXX - this doesn't work right now */
	(void) VOP_FSYNC(vp, fs->sc_cred);
#endif
	vn_close(vp, FREAD|FWRITE);
	VN_RELE(vp);
	crfree(fs->sc_cred);
	fs->sc_vp = (struct vnode *)0;
	fs->sc_cred = (struct ucred *)0;
	fs->sc_size = 0;
}

fdsize(dev)
	dev_t dev;
{
	int unit = fdunit(dev);
	register struct fd_softc *fs = &fd_softc[unit];

	if (unit >= NFD || (fs->sc_flags & FDF_INITED) == 0)
		return(-1);
	return(fs->sc_size);
}

fddump(dev)
{
	return(ENXIO);
}
#endif
