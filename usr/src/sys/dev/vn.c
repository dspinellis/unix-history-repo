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
 * from: Utah $Hdr: fd.c 1.3 89/12/03$
 *
 *	@(#)vn.c	7.13 (Berkeley) %G%
 */

/*
 * Vnode disk driver.
 *
 * Block/character interface to a vnode.  Allows one to treat a file
 * as a disk (e.g. build a filesystem in it, mount it, etc.).
 *
 * NOTE 1: This uses the VOP_BMAP/VOP_STRATEGY interface to the vnode
 * instead of a simple VOP_RDWR.  We do this to avoid distorting the
 * local buffer cache.
 *
 * NOTE 2: There is a security issue involved with this driver.
 * Once mounted all access to the contents of the "mapped" file via
 * the special file is controlled by the permissions on the special
 * file, the protection of the mapped file is ignored (effectively,
 * by using root credentials in all transactions).
 */
#include "vn.h"
#if NVN > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/proc.h>
#include <sys/errno.h>
#include <sys/dkstat.h>
#include <sys/buf.h>
#include <sys/malloc.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/file.h>
#include <sys/uio.h>

#include <miscfs/specfs/specdev.h>

#include "vnioctl.h"

#ifdef DEBUG
int vndebug = 0x00;
#define VDB_FOLLOW	0x01
#define VDB_INIT	0x02
#define VDB_IO		0x04
#endif

struct	buf vnbuf[NVN];
struct	buf vntab[NVN];

#define b_cylin	b_resid

#define	vnunit(x)	((minor(x) >> 3) & 0x7)	/* for consistency */

#define	getvnbuf()	\
	((struct buf *)malloc(sizeof(struct buf), M_DEVBUF, M_WAITOK))
#define putvnbuf(bp)	\
	free((caddr_t)(bp), M_DEVBUF)

struct vn_softc {
	int		 sc_flags;	/* flags */
	size_t		 sc_size;	/* size of vn */
	struct vnode	*sc_vp;		/* vnode */
	struct ucred	*sc_cred;	/* credentials */
	int		 sc_maxactive;	/* max # of active requests */
} vn_softc[NVN];

/* sc_flags */
#define	VNF_ALIVE	0x01
#define VNF_INITED	0x02

int
vnopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	int unit = vnunit(dev);

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnopen(%x, %x, %x, %x)\n", dev, flags, mode, p);
#endif
	if (unit >= NVN)
		return(ENXIO);
	return(0);
}

/*
 * Break the request into bsize pieces and submit using VOP_BMAP/VOP_STRATEGY.
 * Note that this driver can only be used for swapping over NFS on the hp
 * since nfs_strategy on the vax cannot handle u-areas and page tables.
 */
vnstrategy(bp)
	register struct buf *bp;
{
	int unit = vnunit(bp->b_dev);
	register struct vn_softc *vn = &vn_softc[unit];
	register struct buf *nbp;
	register int bn, bsize, resid;
	register caddr_t addr;
	int sz, flags;
	extern void vniodone();

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnstrategy(%x): unit %d\n", bp, unit);
#endif
	if ((vn->sc_flags & VNF_INITED) == 0) {
		bp->b_error = ENXIO;
		bp->b_flags |= B_ERROR;
		biodone(bp);
		return;
	}
	bn = bp->b_blkno;
	sz = howmany(bp->b_bcount, DEV_BSIZE);
	bp->b_resid = bp->b_bcount;
	if (bn < 0 || bn + sz > vn->sc_size) {
		if (bn != vn->sc_size) {
			bp->b_error = EINVAL;
			bp->b_flags |= B_ERROR;
		}
		biodone(bp);
		return;
	}
	bn = dbtob(bn);
	bsize = vn->sc_vp->v_mount->mnt_stat.f_iosize;
	addr = bp->b_un.b_addr;
	flags = bp->b_flags | B_CALL;
	for (resid = bp->b_resid; resid; resid -= sz) {
		struct vnode *vp;
		daddr_t nbn;
		int off, s;

		nbp = getvnbuf();
		off = bn % bsize;
		sz = min(bsize - off, resid);
		(void) VOP_BMAP(vn->sc_vp, bn / bsize, &vp, &nbn, NULL);
#ifdef DEBUG
		if (vndebug & VDB_IO)
			printf("vnstrategy: vp %x/%x bn %x/%x\n",
			       vn->sc_vp, vp, bn, nbn);
#endif
		nbp->b_flags = flags;
		nbp->b_bcount = sz;
		nbp->b_bufsize = bp->b_bufsize;
		nbp->b_error = 0;
		if (vp->v_type == VBLK || vp->v_type == VCHR)
			nbp->b_dev = vp->v_rdev;
		else
			nbp->b_dev = NODEV;
		nbp->b_un.b_addr = addr;
		nbp->b_blkno = nbn + btodb(off);
		nbp->b_proc = bp->b_proc;
		nbp->b_iodone = vniodone;
		nbp->b_vp = vp;
		nbp->b_pfcent = (int) bp;	/* XXX */
		/*
		 * Just sort by block number
		 */
		nbp->b_cylin = nbp->b_blkno;
		s = splbio();
		disksort(&vntab[unit], nbp);
		if (vntab[unit].b_active < vn->sc_maxactive) {
			vntab[unit].b_active++;
			vnstart(unit);
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
vnstart(unit)
{
	register struct vn_softc *vn = &vn_softc[unit];
	register struct buf *bp;

	/*
	 * Dequeue now since lower level strategy routine might
	 * queue using same links
	 */
	bp = vntab[unit].b_actf;
	vntab[unit].b_actf = bp->b_actf;
#ifdef DEBUG
	if (vndebug & VDB_IO)
		printf("vnstart(%d): bp %x vp %x blkno %x addr %x cnt %x\n",
		       unit, bp, bp->b_vp, bp->b_blkno, bp->b_un.b_addr,
		       bp->b_bcount);
#endif
	VOP_STRATEGY(bp);
}

void
vniodone(bp)
	register struct buf *bp;
{
	register struct buf *pbp = (struct buf *)bp->b_pfcent;	/* XXX */
	register int unit = vnunit(pbp->b_dev);
	int s;

	s = splbio();
#ifdef DEBUG
	if (vndebug & VDB_IO)
		printf("vniodone(%d): bp %x vp %x blkno %x addr %x cnt %x\n",
		       unit, bp, bp->b_vp, bp->b_blkno, bp->b_un.b_addr,
		       bp->b_bcount);
#endif
	if (bp->b_error) {
#ifdef DEBUG
		if (vndebug & VDB_IO)
			printf("vniodone: bp %x error %d\n", bp, bp->b_error);
#endif
		pbp->b_flags |= B_ERROR;
		pbp->b_error = biowait(bp);
	}
	pbp->b_resid -= bp->b_bcount;
	putvnbuf(bp);
	if (pbp->b_resid == 0) {
#ifdef DEBUG
		if (vndebug & VDB_IO)
			printf("vniodone: pbp %x iodone\n", pbp);
#endif
		biodone(pbp);
	}
	if (vntab[unit].b_actf)
		vnstart(unit);
	else
		vntab[unit].b_active--;
	splx(s);
}

vnread(dev, uio, flags, p)
	dev_t dev;
	struct uio *uio;
	int flags;
	struct proc *p;
{
	register int unit = vnunit(dev);

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnread(%x, %x, %x, %x)\n", dev, uio, flags, p);
#endif
	return(physio(vnstrategy, &vnbuf[unit], dev, B_READ, minphys, uio));
}

vnwrite(dev, uio, flags, p)
	dev_t dev;
	struct uio *uio;
	int flags;
	struct proc *p;
{
	register int unit = vnunit(dev);

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnwrite(%x, %x, %x, %x)\n", dev, uio, flags, p);
#endif
	return(physio(vnstrategy, &vnbuf[unit], dev, B_WRITE, minphys, uio));
}

/* ARGSUSED */
vnioctl(dev, cmd, data, flag, p)
	dev_t dev;
	u_long cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	int unit = vnunit(dev);
	register struct vn_softc *vn;
	struct vn_ioctl *vio;
	struct vattr vattr;
	struct nameidata nd;
	int error;

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnioctl(%x, %x, %x, %x, %x): unit %d\n",
		       dev, cmd, data, flag, p, unit);
#endif
	error = suser(p->p_ucred, &p->p_acflag);
	if (error)
		return (error);
	if (unit >= NVN)
		return (ENXIO);

	vn = &vn_softc[unit];
	vio = (struct vn_ioctl *)data;
	switch (cmd) {

	case VNIOCSET:
		if (vn->sc_flags & VNF_INITED)
			return(EBUSY);
		/*
		 * Always open for read and write.
		 * This is probably bogus, but it lets vn_open()
		 * weed out directories, sockets, etc. so we don't
		 * have to worry about them.
		 */
		NDINIT(&nd, LOOKUP, FOLLOW, UIO_USERSPACE, vio->vn_file, p);
		if (error = vn_open(&nd, FREAD|FWRITE, 0))
			return(error);
		if (error = VOP_GETATTR(nd.ni_vp, &vattr, p->p_ucred, p)) {
			VOP_UNLOCK(nd.ni_vp);
			(void) vn_close(nd.ni_vp, FREAD|FWRITE, p->p_ucred, p);
			return(error);
		}
		VOP_UNLOCK(nd.ni_vp);
		vn->sc_vp = nd.ni_vp;
		vn->sc_size = btodb(vattr.va_size);	/* note truncation */
		if (error = vnsetcred(vn, p->p_ucred)) {
			(void) vn_close(vn->sc_vp, FREAD|FWRITE, p->p_ucred, p);
			return(error);
		}
		vnthrottle(vn, vn->sc_vp);
		vio->vn_size = dbtob(vn->sc_size);
		vn->sc_flags |= VNF_INITED;
#ifdef DEBUG
		if (vndebug & VDB_INIT)
			printf("vnioctl: SET vp %x size %x\n",
			       vn->sc_vp, vn->sc_size);
#endif
		break;

	case VNIOCCLR:
		if ((vn->sc_flags & VNF_INITED) == 0)
			return(ENXIO);
		vnclear(vn);
#ifdef DEBUG
		if (vndebug & VDB_INIT)
			printf("vnioctl: CLRed\n");
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
vnsetcred(vn, cred)
	register struct vn_softc *vn;
	struct ucred cred;
{
	struct uio auio;
	struct iovec aiov;
	char tmpbuf[DEV_BSIZE];

	vn->sc_cred = crdup(cred);
	/* XXX: Horrible kludge to establish credentials for NFS */
	aiov.iov_base = tmpbuf;
	aiov.iov_len = min(DEV_BSIZE, dbtob(vn->sc_size));
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = 0;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_SYSSPACE;
	auio.uio_resid = aiov.iov_len;
	return(VOP_READ(vn->sc_vp, &auio, 0, vn->sc_cred));
}

/*
 * Set maxactive based on FS type
 */
vnthrottle(vn, vp)
	register struct vn_softc *vn;
	struct vnode *vp;
{
	extern int (**ufs_vnodeop_p)();
	extern int (**nfsv2_vnodeop_p)();

	if (vp->v_op == nfsv2_vnodeop_p)
		vn->sc_maxactive = 2;
	else
		vn->sc_maxactive = 8;

	if (vn->sc_maxactive < 1)
		vn->sc_maxactive = 1;
}

vnshutdown()
{
	register struct vn_softc *vn;

	for (vn = &vn_softc[0]; vn < &vn_softc[NVN]; vn++)
		if (vn->sc_flags & VNF_INITED)
			vnclear(vn);
}

vnclear(vn)
	register struct vn_softc *vn;
{
	register struct vnode *vp = vn->sc_vp;
	struct proc *p = curproc;		/* XXX */

#ifdef DEBUG
	if (vndebug & VDB_FOLLOW)
		printf("vnclear(%x): vp %x\n", vp);
#endif
	vn->sc_flags &= ~VNF_INITED;
	if (vp == (struct vnode *)0)
		panic("vnioctl: null vp");
#if 0
	/* XXX - this doesn't work right now */
	(void) VOP_FSYNC(vp, 0, vn->sc_cred, MNT_WAIT, p);
#endif
	(void) vn_close(vp, FREAD|FWRITE, vn->sc_cred, p);
	crfree(vn->sc_cred);
	vn->sc_vp = (struct vnode *)0;
	vn->sc_cred = (struct ucred *)0;
	vn->sc_size = 0;
}

vnsize(dev)
	dev_t dev;
{
	int unit = vnunit(dev);
	register struct vn_softc *vn = &vn_softc[unit];

	if (unit >= NVN || (vn->sc_flags & VNF_INITED) == 0)
		return(-1);
	return(vn->sc_size);
}

vndump(dev)
{
	return(ENXIO);
}
#endif
