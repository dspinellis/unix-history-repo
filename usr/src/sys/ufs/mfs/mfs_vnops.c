/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfs_vnops.c	7.31 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/map.h>
#include <sys/vnode.h>
#include <sys/malloc.h>

#include <machine/vmparam.h>
#if defined(tahoe)
#include <machine/mtpr.h>
#endif

#include <ufs/mfs/mfsnode.h>
#include <ufs/mfs/mfsiom.h>
#include <ufs/mfs/mfs_extern.h>

#if !defined(hp300) && !defined(i386) && !defined(mips) && !defined(sparc)
static int mfsmap_want;		/* 1 => need kernel I/O resources */
struct map mfsmap[MFS_MAPSIZE];
extern char mfsiobuf[];
#endif

/*
 * mfs vnode operations.
 */
int (**mfs_vnodeop_p)();
struct vnodeopv_entry_desc mfs_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, mfs_lookup },		/* lookup */
	{ &vop_create_desc, mfs_create },		/* create */
	{ &vop_mknod_desc, mfs_mknod },		/* mknod */
	{ &vop_open_desc, mfs_open },		/* open */
	{ &vop_close_desc, mfs_close },		/* close */
	{ &vop_access_desc, mfs_access },		/* access */
	{ &vop_getattr_desc, mfs_getattr },		/* getattr */
	{ &vop_setattr_desc, mfs_setattr },		/* setattr */
	{ &vop_read_desc, mfs_read },		/* read */
	{ &vop_write_desc, mfs_write },		/* write */
	{ &vop_ioctl_desc, mfs_ioctl },		/* ioctl */
	{ &vop_select_desc, mfs_select },		/* select */
	{ &vop_mmap_desc, mfs_mmap },		/* mmap */
	{ &vop_fsync_desc, mfs_fsync },		/* fsync */
	{ &vop_seek_desc, mfs_seek },		/* seek */
	{ &vop_remove_desc, mfs_remove },		/* remove */
	{ &vop_link_desc, mfs_link },		/* link */
	{ &vop_rename_desc, mfs_rename },		/* rename */
	{ &vop_mkdir_desc, mfs_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, mfs_rmdir },		/* rmdir */
	{ &vop_symlink_desc, mfs_symlink },		/* symlink */
	{ &vop_readdir_desc, mfs_readdir },		/* readdir */
	{ &vop_readlink_desc, mfs_readlink },		/* readlink */
	{ &vop_abortop_desc, mfs_abortop },		/* abortop */
	{ &vop_inactive_desc, mfs_inactive },		/* inactive */
	{ &vop_reclaim_desc, mfs_reclaim },		/* reclaim */
	{ &vop_lock_desc, mfs_lock },		/* lock */
	{ &vop_unlock_desc, mfs_unlock },		/* unlock */
	{ &vop_bmap_desc, mfs_bmap },		/* bmap */
	{ &vop_strategy_desc, mfs_strategy },		/* strategy */
	{ &vop_print_desc, mfs_print },		/* print */
	{ &vop_islocked_desc, mfs_islocked },		/* islocked */
	{ &vop_advlock_desc, mfs_advlock },		/* advlock */
	{ &vop_blkatoff_desc, mfs_blkatoff },		/* blkatoff */
	{ &vop_vget_desc, mfs_vget },		/* vget */
	{ &vop_valloc_desc, mfs_valloc },		/* valloc */
	{ &vop_vfree_desc, mfs_vfree },		/* vfree */
	{ &vop_truncate_desc, mfs_truncate },		/* truncate */
	{ &vop_update_desc, mfs_update },		/* update */
	{ &vop_bwrite_desc, mfs_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc mfs_vnodeop_opv_desc =
	{ &mfs_vnodeop_p, mfs_vnodeop_entries };

/*
 * Vnode Operations.
 *
 * Open called to allow memory filesystem to initialize and
 * validate before actual IO. Record our process identifier
 * so we can tell when we are doing I/O to ourself.
 */
/* ARGSUSED */
int
mfs_open (ap)
	struct vop_open_args *ap;
#define vp (ap->a_vp)
#define mode (ap->a_mode)
#define cred (ap->a_cred)
#define p (ap->a_p)
{

	if (vp->v_type != VBLK) {
		panic("mfs_ioctl not VBLK");
		/* NOTREACHED */
	}
	return (0);
}
#undef vp
#undef mode
#undef cred
#undef p

/*
 * Ioctl operation.
 */
/* ARGSUSED */
int
mfs_ioctl (ap)
	struct vop_ioctl_args *ap;
#define vp (ap->a_vp)
#define com (ap->a_command)
#define data (ap->a_data)
#define fflag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{

	return (ENOTTY);
}
#undef vp
#undef com
#undef data
#undef fflag
#undef cred
#undef p

/*
 * Pass I/O requests to the memory filesystem process.
 */
int
mfs_strategy (ap)
	struct vop_strategy_args *ap;
#define bp (ap->a_bp)
{
	register struct mfsnode *mfsp;
	struct vnode *vp;
	struct proc *p = curproc;		/* XXX */

	if (vfinddev(bp->b_dev, VBLK, &vp) || vp->v_usecount == 0)
		panic("mfs_strategy: bad dev");
	mfsp = VTOMFS(vp);
	/* check for mini-root access */
	if (mfsp->mfs_pid == 0) {
		caddr_t base;

		base = mfsp->mfs_baseoff + (bp->b_blkno << DEV_BSHIFT);
		if (bp->b_flags & B_READ)
			bcopy(base, bp->b_un.b_addr, bp->b_bcount);
		else
			bcopy(bp->b_un.b_addr, base, bp->b_bcount);
		biodone(bp);
	} else if (mfsp->mfs_pid == p->p_pid) {
		mfs_doio(bp, mfsp->mfs_baseoff);
	} else {
		bp->av_forw = mfsp->mfs_buflist;
		mfsp->mfs_buflist = bp;
		wakeup((caddr_t)vp);
	}
	return (0);
}
#undef bp

#if defined(vax) || defined(tahoe)
/*
 * Memory file system I/O.
 *
 * Essentially play ubasetup() and disk interrupt service routine by
 * doing the copies to or from the memfs process. If doing physio
 * (i.e. pagein), we must map the I/O through the kernel virtual
 * address space.
 */
void
mfs_doio(bp, base)
	register struct buf *bp;
	caddr_t base;
{
	register struct pte *pte, *ppte;
	register caddr_t vaddr;
	int off, npf, npf2, reg;
	caddr_t kernaddr, offset;

	/*
	 * For phys I/O, map the b_addr into kernel virtual space using
	 * the Mfsiomap pte's.
	 */
	if ((bp->b_flags & B_PHYS) == 0) {
		kernaddr = bp->b_un.b_addr;
	} else {
		if (bp->b_flags & (B_PAGET | B_UAREA | B_DIRTY))
			panic("swap on memfs?");
		off = (int)bp->b_un.b_addr & PGOFSET;
		npf = btoc(bp->b_bcount + off);
		/*
		 * Get some mapping page table entries
		 */
		while ((reg = rmalloc(mfsmap, (long)npf)) == 0) {
			mfsmap_want++;
			sleep((caddr_t)&mfsmap_want, PZERO-1);
		}
		reg--;
		pte = vtopte(bp->b_proc, btop(bp->b_un.b_addr));
		/*
		 * Do vmaccess() but with the Mfsiomap page table.
		 */
		ppte = &Mfsiomap[reg];
		vaddr = &mfsiobuf[reg * NBPG];
		kernaddr = vaddr + off;
		for (npf2 = npf; npf2; npf2--) {
			mapin(ppte, (u_int)vaddr, pte->pg_pfnum,
				(int)(PG_V|PG_KW));
#if defined(tahoe)
			if ((bp->b_flags & B_READ) == 0)
				mtpr(P1DC, vaddr);
#endif
			ppte++;
			pte++;
			vaddr += NBPG;
		}
	}
	offset = base + (bp->b_blkno << DEV_BSHIFT);
	if (bp->b_flags & B_READ)
		bp->b_error = copyin(offset, kernaddr, bp->b_bcount);
	else
		bp->b_error = copyout(kernaddr, offset, bp->b_bcount);
	if (bp->b_error)
		bp->b_flags |= B_ERROR;
	/*
	 * Release pte's used by physical I/O.
	 */
	if (bp->b_flags & B_PHYS) {
		rmfree(mfsmap, (long)npf, (long)++reg);
		if (mfsmap_want) {
			mfsmap_want = 0;
			wakeup((caddr_t)&mfsmap_want);
		}
	}
	biodone(bp);
}
#endif	/* vax || tahoe */

#if defined(hp300) || defined(i386) || defined(mips) || defined(sparc)
/*
 * Memory file system I/O.
 *
 * Trivial on the HP since buffer has already been mapping into KVA space.
 */
void
mfs_doio(bp, base)
	register struct buf *bp;
	caddr_t base;
{

	base += (bp->b_blkno << DEV_BSHIFT);
	if (bp->b_flags & B_READ)
		bp->b_error = copyin(base, bp->b_un.b_addr, bp->b_bcount);
	else
		bp->b_error = copyout(bp->b_un.b_addr, base, bp->b_bcount);
	if (bp->b_error)
		bp->b_flags |= B_ERROR;
	biodone(bp);
}
#endif

/*
 * This is a noop, simply returning what one has been given.
 */
int
mfs_bmap (ap)
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
 * Memory filesystem close routine
 */
/* ARGSUSED */
int
mfs_close (ap)
	struct vop_close_args *ap;
#define vp (ap->a_vp)
#define flag (ap->a_fflag)
#define cred (ap->a_cred)
#define p (ap->a_p)
{
	register struct mfsnode *mfsp = VTOMFS(vp);
	register struct buf *bp;

	/*
	 * Finish any pending I/O requests.
	 */
	while (bp = mfsp->mfs_buflist) {
		mfsp->mfs_buflist = bp->av_forw;
		mfs_doio(bp, mfsp->mfs_baseoff);
		wakeup((caddr_t)bp);
	}
	/*
	 * On last close of a memory filesystem
	 * we must invalidate any in core blocks, so that
	 * we can, free up its vnode.
	 */
	vflushbuf(vp, 0);
	if (vinvalbuf(vp, 1))
		return (0);
	/*
	 * There should be no way to have any more uses of this
	 * vnode, so if we find any other uses, it is a panic.
	 */
	if (vp->v_usecount > 1)
		printf("mfs_close: ref count %d > 1\n", vp->v_usecount);
	if (vp->v_usecount > 1 || mfsp->mfs_buflist)
		panic("mfs_close");
	/*
	 * Send a request to the filesystem server to exit.
	 */
	mfsp->mfs_buflist = (struct buf *)(-1);
	wakeup((caddr_t)vp);
	return (0);
}
#undef vp
#undef flag
#undef cred
#undef p

/*
 * Memory filesystem inactive routine
 */
/* ARGSUSED */
int
mfs_inactive (ap)
	struct vop_inactive_args *ap;
#define vp (ap->a_vp)
#define p (ap->a_p)
{
	register struct mfsnode *mfsp = VTOMFS(vp);

	if (mfsp->mfs_buflist && mfsp->mfs_buflist != (struct buf *)(-1))
		panic("mfs_inactive: not inactive (mfs_buflist %x)",
			mfsp->mfs_buflist);
	return (0);
}
#undef vp
#undef p

/*
 * Reclaim a memory filesystem devvp so that it can be reused.
 */
int
mfs_reclaim (ap)
	struct vop_reclaim_args *ap;
#define vp (ap->a_vp)
{

	FREE(vp->v_data, M_MFSNODE);
	vp->v_data = NULL;
	return (0);
}
#undef vp

/*
 * Print out the contents of an mfsnode.
 */
int
mfs_print (ap)
	struct vop_print_args *ap;
#define vp (ap->a_vp)
{
	register struct mfsnode *mfsp = VTOMFS(vp);

	printf("tag VT_MFS, pid %d, base %d, size %d\n", mfsp->mfs_pid,
		mfsp->mfs_baseoff, mfsp->mfs_size);
	return (0);
}
#undef vp

/*
 * Block device bad operation
 */
int
mfs_badop()
{

	panic("mfs_badop called\n");
	/* NOTREACHED */
}

/*
 * Memory based filesystem initialization.
 */
mfs_init()
{

#if !defined(hp300) && !defined(i386) && !defined(mips) && !defined(sparc)
	rminit(mfsmap, (long)MFS_MAPREG, (long)1, "mfs mapreg", MFS_MAPSIZE);
#endif
}
