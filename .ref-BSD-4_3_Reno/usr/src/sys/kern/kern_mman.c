/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_mman.c	7.18 (Berkeley) 6/30/90
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "user.h"
#include "proc.h"
#include "buf.h"
#include "vnode.h"
#include "specdev.h"
#include "seg.h"
#include "acct.h"
#include "wait.h"
#include "vm.h"
#include "text.h"
#include "file.h"
#include "vadvise.h"
#include "cmap.h"
#include "trace.h"
#include "mman.h"
#include "mapmem.h"
#include "malloc.h"
#include "conf.h"

#include "machine/cpu.h"
#include "machine/reg.h"
#include "machine/psl.h"
#include "machine/pte.h"
#include "machine/mtpr.h"

/*
 * The MMAP code here is temporary; it provides support
 * only for mmaping devices such as frame buffers.
 * All to be different next time...
 */
#ifndef MAPMEM
#undef MMAP		/* XXX */
#endif

#ifdef MMAP
struct mapmemops mmapops = {
	(int (*)())0, (int (*)())0, (int (*)())0, (int (*)())0
};
#endif

/* ARGSUSED */
sbrk(p, uap, retval)
	struct proc *p;
	struct args {
		int	incr;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* ARGSUSED */
sstk(p, uap, retval)
	struct proc *p;
	struct args {
		int	incr;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* ARGSUSED */
getpagesize(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = NBPG * CLSIZE;
	return (0);
}

/* ARGSUSED */
smmap(p, uap, retval)
	register struct proc *p;
	register struct args {
		caddr_t	addr;
		int	len;
		int	prot;
		int	share;
		int	fd;
		off_t	pos;
	} *uap;
	int *retval;
{
#ifndef MMAP
	return (EOPNOTSUPP);
#else
	register struct file *fp;
	struct mapmem *mp;
	struct vnode *vp;
	register struct pte *pte;
	struct pte *dpte;
	register int off;
	int error, fv, lv, pm, (*mapfun)();
	dev_t dev;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		return (error);
	vp = (struct vnode *)fp->f_data;
	if (vp->v_type != VCHR)
		return (EINVAL);
	dev = vp->v_rdev;
	mapfun = cdevsw[major(dev)].d_mmap;
	if (mapfun == NULL)
		return (EINVAL);
	if (((int)uap->addr & CLOFSET) || (uap->pos & CLOFSET) ||
	    uap->len <= 0 || (uap->len & CLOFSET))
		return (EINVAL);
	if ((uap->prot & PROT_WRITE) && (fp->f_flag&FWRITE) == 0)
		return (EINVAL);
	if ((uap->prot & PROT_READ) && (fp->f_flag&FREAD) == 0)
		return (EINVAL);
	if (uap->share != MAP_SHARED)
		return (EINVAL);
	for (off = 0; off < uap->len; off += NBPG)
		if ((*mapfun)(dev, uap->pos+off, uap->prot) == -1)
			return (EINVAL);		/* Needs translation */
	/*
	 * Allocate a descriptor for this region and expand page
	 * table to accomodate.
	 */
	if (uap->prot & PROT_WRITE) {
		pm = PG_UW|PG_FOD|PG_V;
		off = MM_RW;
	} else {
		pm = PG_URKR|PG_FOD|PG_V;
		off = MM_RO;
	}
#if defined(hp300)
	pm |= PG_CI;
	off |= MM_CI;
#endif
	error = mmalloc(p, uap->fd, &uap->addr, uap->len, off, &mmapops, &mp);
	if (error)
		return (error);
	/*
	 * Now map it in.
	 * Can't use mmmapin() because of args to map function.
	 */
	fv = btop(uap->addr);
	pte = vtopte(p, fv);
	dpte = dptopte(p, u.u_dsize);
	for (off = 0; off < uap->len; off += NBPG) {
		if ((off&CLOFSET) == 0 && pte < dpte)
			p->p_rssize -= vmemfree(pte, CLSIZE);
		*(int *)pte = pm;
		pte->pg_pfnum = (*mapfun)(dev, uap->pos+off, uap->prot);
		pte++;
	}
	newptes(vtopte(p, fv), fv, btoc(uap->len));
	u.u_pofile[uap->fd] |= UF_MAPPED;
	return (0);
#endif /* MMAP */
}

/* ARGSUSED */
msync(p, uap, retval)
	struct proc *p;
	struct args {
		char	*addr;
		int	len;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* ARGSUSED */
munmap(p, uap, retval)
	register struct proc *p;
	register struct args {
		caddr_t	addr;
		int	len;
	} *uap;
	int *retval;
{
#ifndef MMAP
	return (EOPNOTSUPP);
#else
	register struct mapmem *mp;
	register int fd;
	caddr_t eaddr;
	int error;

	if (((int)uap->addr & CLOFSET) ||
	    uap->len <= 0 || (uap->len & CLOFSET))
		return (EINVAL);
	/*
	 * Locate region mapping this range. If found, unmap it.
	 */
	eaddr = uap->addr + uap->len - 1;
	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops == &mmapops &&
		    uap->addr >= mp->mm_uva && eaddr < mp->mm_uva+mp->mm_size)
			break;
	if (mp == MMNIL)
		return (EINVAL);
	fd = mp->mm_id;
	mmmapout(p, mp);
	error = mmfree(mp);
	/*
	 * If no other range has this descriptor mapped, mark it as unmapped.
	 */
	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_id == fd)
			break;
	if (mp == MMNIL)
		u.u_pofile[fd] &= ~UF_MAPPED;
	return (error);
#endif /* MMAP */
}

munmapfd(fd)
	int fd;
{
	int error = 0;
#ifdef MMAP
	struct proc *p = u.u_procp;		/* XXX */
	register struct mapmem *mp, **mpp;

	if (p->p_flag & SVFORK)
		return (0);
	mpp = &u.u_mmap;
	for (mp = *mpp; mp; mp = *mpp) {
		if (mp->mm_ops == &mmapops && mp->mm_id == fd) {
			mmmapout(p, mp);
			error = mmfree(mp);
		} else
			mpp = &mp->mm_next;
	}
#endif
	u.u_pofile[fd] &= ~UF_MAPPED;
	return (error);
}

/* ARGSUSED */
mprotect(p, uap, retval)
	struct proc *p;
	struct args {
		char	*addr;
		int	len;
		int	prot;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* ARGSUSED */
madvise(p, uap, retval)
	struct proc *p;
	struct args {
		char	*addr;
		int	len;
		int	behav;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* ARGSUSED */
mincore(p, uap, retval)
	struct proc *p;
	struct args {
		char	*addr;
		int	len;
		char	*vec;
	} *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

/* BEGIN DEFUNCT */
/* ARGSUSED */
obreak(p, uap, retval)
	struct proc *p;
	struct args {
		char	*nsiz;
	} *uap;
	int *retval;
{
	register segsz_t n, d, ds;
	int error;

	/*
	 * set n to new data size
	 */
	n = btoc(uap->nsiz) - dptov(p, 0);
	if (n < 0)
		n = 0;
	/*
	 * since we can't pass a -ve argument for the difference to chksize,
	 * if d is negative, make ds equal to the final value and clear d.
	 * keep the real difference in n for later use in expand.
	 */
	ds = u.u_dsize;
	if ((n = d = clrnd(n - u.u_dsize)) < 0) {
		ds += d;
		d = 0;
	}
	if (ctob(ds + d) > u.u_rlimit[RLIMIT_DATA].rlim_cur)
		return (ENOMEM);
	if (error =
	    chksize((u_int)u.u_tsize, (u_int)ds, (u_int)d, (u_int)u.u_ssize))
		return (error);
#ifdef MAPMEM
	/*
	 * If change would conflict with any mapped memory segment
	 * return ENOMEM.
	 */
	if (u.u_mmap && n != 0) {
		caddr_t low, high;

		low = (caddr_t) ctob(dptov(p, ds));
		high = low + ctob((n < 0) ? -n : n);
		if (mmclash(u.u_mmap, low, high))
			return (ENOMEM);
	}
#endif
	if (error = swpexpand(ds + d, u.u_ssize, &u.u_dmap, &u.u_smap))
		return (error);
	if (p->p_mmsize && (p->p_mmsize -= n) < 0)
		p->p_mmsize = 0;
	expand((int)n, 0);
	return (0);
}

/*
 * Macros for clearing a page's reference bits.
 */
#ifdef REFBIT
#if !defined(tahoe)
#define uncache(pte)		/* XXX */
#endif

#define CLRREF(pte, c, p, i) { \
	if (!isatpte(p, pte)) \
		uncache(pte); \
	if (pte->pg_u) { \
		c = &cmap[pgtocm(pte->pg_pfnum)]; \
		if (c->c_lock) \
			continue; \
		pte->pg_u = 0; \
		if (anycl(pte, pg_m)) \
			pte->pg_m = 1; \
		distcl(pte); \
		if (isatpte(p, pte)) \
			distpte(p->p_textp, i, pte); \
	} \
}
#else
#define CLRREF(pte, c, p, i) { \
	c = &cmap[pgtocm(pte->pg_pfnum)]; \
	if (c->c_lock) \
		continue; \
	pte->pg_v = 0; \
	if (anycl(pte, pg_m)) \
		pte->pg_m = 1; \
	distcl(pte); \
	if (isatpte(p, pte)) \
		distpte(p->p_textp, i, pte); \
}
#endif

/* ARGSUSED */
ovadvise(rp, uap, retval)
	register struct proc *rp;
	struct args {
		int	anom;
	} *uap;
	int *retval;
{
	int oanom = rp->p_flag & SUANOM;
	register struct pte *pte;
	register struct cmap *c;
	register unsigned i;

	trace(TR_VADVISE, uap->anom, rp->p_pid);
	rp->p_flag &= ~(SSEQL|SUANOM);
	switch (uap->anom) {

	case VA_ANOM:
		rp->p_flag |= SUANOM;
		break;

	case VA_SEQL:
		rp->p_flag |= SSEQL;
		break;
	}
	if ((oanom && (rp->p_flag & SUANOM) == 0) || uap->anom == VA_FLUSH) {
		for (i = 0; i < rp->p_dsize; i += CLSIZE) {
			pte = dptopte(rp, i);
#ifdef MAPMEM
			/* don't do mmap pages */
			if (pte->pg_v && !pte->pg_fod)
#else
			if (pte->pg_v)
#endif
				CLRREF(pte, c, rp, i);
		}
	}
	if (uap->anom == VA_FLUSH) {	/* invalidate all pages */
		for (i = 1; i < rp->p_ssize; i += CLSIZE) {
			pte = sptopte(rp, i);
			if (pte->pg_v)
				CLRREF(pte, c, rp, i);
		}
		for (i = 0; i < rp->p_tsize; i += CLSIZE) {
			pte = tptopte(rp, i);
			if (pte->pg_v)
				CLRREF(pte, c, rp, i);
		}
	}
#if defined(vax) || defined(tahoe)
	mtpr(TBIA, 0);
#endif
#if defined(hp300)
	TBIAU();
#endif
#if defined(i386)
	tlbflush();
#endif
	return (0);
}
/* END DEFUNCT */

/*
 * Grow the stack to include the SP; true return if successful.
 * Clients do not care about the cause of the error.
 */
grow(sp)
	unsigned sp;
{
	int si, error;

	if (sp >= USRSTACK-ctob(u.u_ssize))
		return (0);
	si = clrnd(btoc((USRSTACK-sp)) - u.u_ssize + SINCR);
	if (ctob(si) > u.u_rlimit[RLIMIT_STACK].rlim_cur)
		return (0);
	if (error = chksize((u_int)u.u_tsize, (u_int)u.u_dsize, (u_int)0,
	    (u_int)u.u_ssize+si))
		return (0);
	if (error = swpexpand(u.u_dsize, u.u_ssize + si, &u.u_dmap, &u.u_smap))
		return (0);
	expand(si, 1);
	return (1);
}

#ifdef MAPMEM

/*
 * Called from vpassvm() after full context has been passed from fup to tup.
 * Always called in the context of the parent.  NOTE: routines should NOT
 * destroy regions.
 */
mmvfork(fup, tup)
	struct user *fup, *tup;
{
	register struct mapmem *mp;

	tup->u_mmap = fup->u_mmap;
	fup->u_mmap = (struct mapmem *) 0;
	for (mp = tup->u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops->mm_vfork)
			(*mp->mm_ops->mm_vfork)(mp, fup, tup);
}

/*
 * Called from procdup() for both parent and child.  If in parent
 * we need to duplicate mapped memory regions.  In both parent and
 * child, we call object specific routine.
 */
mmfork(pup, cup)
	struct user *pup, *cup;
{
	register struct mapmem *mp, **mpp;
	int error = 0;

	if (pup) {
		mmdup(pup, cup);
		for (mp = pup->u_mmap; mp; mp = mp->mm_next)
			if (mp->mm_ops->mm_fork)
				(*mp->mm_ops->mm_fork)(mp, 0);
	} else {
		mpp = &u.u_mmap;
		for (mp = *mpp; mp; mp = *mpp) {
			if (mp->mm_ops->mm_fork)
				(*mp->mm_ops->mm_fork)(mp, 1);
			if (*mpp == mp)
				mpp = &mp->mm_next;
		}
		error = mmexpand(u.u_procp);
	}
	return (error);
}

/*
 * Its not clear that having a seperate exec routine is useful since
 * exec frees the address space immediately afterwards.  We probably
 * need a post-exec hook to reestablish any mappings that persist
 * across execs.
 */
mmexec(p)
	struct proc *p;
{
	register struct mapmem *mp, **mpp;
	int error1, error = 0;

	mpp = &u.u_mmap;
	for (mp = *mpp; mp; mp = *mpp) {
		if (mp->mm_ops->mm_exec)
			error = (*mp->mm_ops->mm_exec)(mp);
		if (*mpp == mp) {
			*mpp = mp->mm_next;
			MMFREE(mp);
		}
	}
	if (error1 = mmexpand(p))
		return (error1);
	if (p->p_mmsize)
		panic("mmexec");
	return (error);
}

/*
 * Called from exit just before releasing address space.
 * We always reclaim resources regardless of what the object routine does.
 */
mmexit(p)
	struct proc *p;
{
	register struct mapmem *mp, **mpp;
	int error1, error = 0;

	mpp = &u.u_mmap;
	for (mp = *mpp; mp; mp = *mpp) {
		if (mp->mm_ops->mm_exit)
			error = (*mp->mm_ops->mm_exit)(mp);
		if (*mpp == mp) {
			*mpp = mp->mm_next;
			MMFREE(mp);
		}
	}
	if (error1 = mmexpand(p))
		return (error1);
	if (p->p_mmsize)
		panic("mmexit");
	return (error);
}

/*
 * Called from core just before dumping process image to core file.
 * Used to unmap regions which cannot be dumped; e.g. a region mapping
 * hardware registers which are write-only or must be accessed as bytes.
 */
mmcore(p)
	struct proc *p;
{
	register struct mapmem *mp, **mpp;
	int error = 0, error1, changed = 0;

	mpp = &u.u_mmap;
	for (mp = *mpp; mp; mp = *mpp) {
		if ((mp->mm_prot & MM_NOCORE) == 0) {
			mpp = &mp->mm_next;
			continue;
		}
		if (mp->mm_ops->mm_exit)
			error = (*mp->mm_ops->mm_exit)(mp);
		if (*mpp == mp) {
			*mpp = mp->mm_next;
			MMFREE(mp);
		}
		changed++;
	}
	if (changed && (error1 = mmexpand(p)))
		return (error1);
	return (error);
}

/*
 * Duplicate mapped memory regions in a forked process.
 * XXX child may wind up short a few regions if not enough resources.
 */
mmdup(pu, cu)
	struct user *pu, *cu;
{
	register struct mapmem *pmp, *cmp;
	register struct pte *ppte, *cpte;
	register segsz_t count;

	/*
	 * First duplicate the mmap chain
	 */
	MMALLOC(cu->u_mmap);
	pmp = pu->u_mmap;
	cmp = cu->u_mmap;
	while (pmp && cmp) {
		*cmp = *pmp;
		if (pmp->mm_next)
			MMALLOC(cmp->mm_next);
		pmp = pmp->mm_next;
		cmp = cmp->mm_next;
	}
	/*
	 * Now duplicate user address space that vmdup() won't do
	 * i.e. mapped regions outside of data segment.
	 */
	ppte = dptopte(pu->u_procp, pu->u_procp->p_dsize);
	cpte = dptopte(cu->u_procp, cu->u_procp->p_dsize);
	for (count = pu->u_procp->p_mmsize; count; count--) {
		if (ppte->pg_fod && ppte->pg_v)
			*(int *)cpte = *(int *)ppte;
		ppte++, cpte++;
	}
	cu->u_procp->p_flag |= SPTECHG;
}

mmalloc(p, id, uvap, size, prot, ops, mpp)
	struct proc *p;
	caddr_t *uvap;
	segsz_t size;
	struct mapmemops *ops;
	struct mapmem **mpp;
{
	register struct mapmem *mp;
	register u_int uva;
	int error;

	/*
	 * Validate size first
	 */
	if (size <= 0 || (size & CLOFSET))
		return(EINVAL);
	/*
	 * A uva of zero means to map at our discretion.
	 * Our strategy is to place the segment at the max of:
	 *	- the current data + mapped memory size
	 *	- the default data size limit
	 *	  (if it will fit within the MAXDSIZ limit)
	 * If this is the first mapped memory region beyond the data
	 * segment we round to a MMSEG boundary to allow for data
	 * segment growth.
	 */
	uva = (u_int) *uvap;
	if (uva == 0) {
		register u_int uva2;

		uva = ctob(dptov(p, u.u_dsize + p->p_mmsize));
		uva2 = ctob(dptov(p, btoc(DFLDSIZ)));
		uva2 = ((uva2 + (MMSEG-1)) & ~(MMSEG-1));
		if (uva < uva2 &&
		    uva2 + size < ctob(dptov(p, btoc(MAXDSIZ))))
			uva = uva2;
		else if (p->p_mmsize == 0)
			uva = ((uva + (MMSEG-1)) & ~(MMSEG-1));
	}
	/*
	 * Impose necessary constraints on address.
	 */
	if ((uva & CLOFSET) || uva < ctob(dptov(p, 0)) ||
	    uva+size >= ctob(sptov(p, u.u_ssize)))
		return (EINVAL);
	if (mmclash(u.u_mmap, (caddr_t)uva, (caddr_t)uva+size))
		return (EINVAL);
	/*
	 * Finally, allocate and initialize descriptor and expand
	 * user address space as necessary.
	 */
	MMALLOC(mp);
	if (mp == MMNIL)
		return (ENOMEM);
	mp->mm_next = u.u_mmap;
	mp->mm_id = id;
	mp->mm_uva = (caddr_t) uva;
	mp->mm_size = size;
	mp->mm_prot = prot;
	mp->mm_ops = ops;
	u.u_mmap = mp;
	if (error = mmexpand(p)) {
		u.u_mmap = mp->mm_next;
		MMFREE(mp);
		return(error);
	}
	*uvap = (caddr_t) uva;
	*mpp = mp;
	return(0);
}

mmfree(p, mp)
	struct proc *p;
	register struct mapmem *mp;
{
	register struct mapmem *cmp, **mpp;

	/*
	 * Remove region from chain
	 */
	mpp = &u.u_mmap;
	for (cmp = *mpp; cmp; cmp = *mpp) {
		if (cmp == mp)
			break;
		mpp = &cmp->mm_next;
	}
	if (cmp == MMNIL)
		panic("mmfree");
	*mpp = mp->mm_next;
	MMFREE(mp);
	return (mmexpand(p));
}

mmmapin(p, mp, mapfunc)
	register struct proc *p;
	register struct mapmem *mp;
	int (*mapfunc)();
{
	register struct pte *pte;
	register int off;
	struct pte *dpte;
	int pm, fv, lv;

	/*
	 * Verify that range can be mapped
	 */
	for (off = 0; off < mp->mm_size; off += NBPG)
		if ((*mapfunc)(mp, off) == -1)
			return (EINVAL);
	/*
	 * Now verify that region is in range
	 */
	fv = btop(mp->mm_uva);
	lv = btop(mp->mm_uva + mp->mm_size - 1);
	if (fv < dptov(p, 0) ||
	    lv >= dptov(p, u.u_dsize + p->p_mmsize))
		return (ENOMEM);
	/*
	 * Finally, do the mapping.
	 */
	if (mp->mm_prot & MM_RO)
		pm = PG_URKR|PG_FOD|PG_V;
	else
		pm = PG_UW|PG_FOD|PG_V;
#if defined(hp300)
	if (mp->mm_prot & MM_CI)
		pm |= PG_CI;
#endif
	pte = vtopte(p, fv);
	dpte = dptopte(p, u.u_dsize);
	for (off = 0; off < mp->mm_size; off += NBPG) {
		if ((off&CLOFSET) == 0 && pte < dpte)
			p->p_rssize -= vmemfree(pte, CLSIZE);
		*(int *)pte = pm;
		pte->pg_pfnum = (*mapfunc)(mp, off);
		pte++;
	}
	newptes(vtopte(p, fv), (u_int)fv, (int)btoc(mp->mm_size));
	return (0);
}

mmmapout(p, mp)
	register struct proc *p;
	register struct mapmem *mp;
{
	register struct pte *pte;
	register int off;
	struct pte *dpte;
	int fv, lv;

	fv = btop(mp->mm_uva);
	lv = btop(mp->mm_uva + mp->mm_size - 1);
	if (fv < dptov(p, 0) ||
	    lv >= dptov(p, u.u_dsize + p->p_mmsize))
		panic("mmmapout");
	pte = vtopte(p, fv);
	dpte = dptopte(p, u.u_dsize);
	for (off = 0; off < mp->mm_size; off += NBPG) {
		if (pte < dpte) {
			if ((off & CLOFSET) == 0)
				p->p_rssize -= vmemfree(pte, CLSIZE);
			*(int *)pte = (PG_UW|PG_FOD);
			((struct fpte *)pte)->pg_fileno = PG_FZERO;
		} else
			*(int *)pte = 0;
		pte++;
	}
	newptes(vtopte(p, fv), (u_int)fv, (int)btoc(mp->mm_size));
}

mmexpand(p)
	struct proc *p;
{
	register int szpt, change;
	caddr_t high;
	segsz_t nsize, oms;

	oms = p->p_mmsize;
	/*
	 * Get new mmsize based on existing regions and use
	 * that to calculate change in page table size.
	 */
	if (u.u_mmap) {
		mmrange(u.u_mmap, (caddr_t *)0, &high);
		nsize = btop(high) - dptov(p, u.u_dsize) + 1;
		if (nsize < 0)
			nsize = 0;
	} else
		nsize = 0;
	change = nsize - oms;
	if (change == 0)
		return(0);

	/*
	 * Ensure data + mapped memory fits within maximum data limit.
	 * This is possibly a little restrictive, but it helps keep
	 * page table sizes down.
	 */
	if (change > 0 &&
	    (ctob(oms+change) > u.u_rlimit[RLIMIT_DATA].rlim_max ||
	     ctob(u.u_dsize+oms+change) > u.u_rlimit[RLIMIT_DATA].rlim_max))
		return(ENOMEM);
	/*
	 * Expand page table if necessary.
	 * Note that ptexpand takes care of flushing the translation buffer.
	 */
	p->p_mmsize += change;
#if defined(hp300) || defined(i386)
	szpt = ptsize(p) - u.u_pcb.pcb_szpt;
	if (szpt > 0)
		ptexpand(szpt, u.u_dsize, oms, u.u_ssize);
	setp0lr(u.u_pcb.pcb_p0lr + change);
#endif
#if defined(vax) || defined(tahoe)
#if defined(vax)
	szpt = (u.u_pcb.pcb_p1br + (u.u_pcb.pcb_p1lr&~PME_CLR)) -
		(u.u_pcb.pcb_p0br + (u.u_pcb.pcb_p0lr&~AST_CLR));
#else
	szpt = (u.u_pcb.pcb_p2br + u.u_pcb.pcb_p2lr) -
		(u.u_pcb.pcb_p0br + u.u_pcb.pcb_p0lr);
#endif
	if (change > szpt)
		ptexpand(clrnd(ctopt(change - szpt)), u.u_dsize, oms, u.u_ssize);
	/*
	 * Clear new ptes.
	 * We need to do this because there may be bogus (yet technically
	 * valid) ptes above the old p0lr value.  This can happen if the
	 * data segment has shrunk in the past leaving such ptes behind.
	 * There is no need to invalidate such ptes at that time since the
	 * length register will prevent their use.  We are safe on the HPs
	 * because we do invalidate old ptes in setp0lr() when shrinking.
	 */
	if (change > 0) {
		struct pte *bpte;

#if defined(vax)
		bpte = u.u_pcb.pcb_p0br + (u.u_pcb.pcb_p0lr&~AST_CLR);
#else
		bpte = u.u_pcb.pcb_p0br + u.u_pcb.pcb_p0lr;
#endif
		bzero((caddr_t)bpte, change * sizeof(struct pte));
		mtpr(TBIA, 0);
	}
	/* avoid side-effects of setp0lr */
#if defined(vax)
	change += u.u_pcb.pcb_p0lr &~ AST_CLR;
#else
	change += u.u_pcb.pcb_p0lr;
#endif
	setp0lr(change);
#endif
	return(0);
}

mmrange(mp, lap, hap)
	register struct mapmem *mp;
	caddr_t *lap, *hap;
{
	register caddr_t low, high, top;

	low = high = 0;
	while (mp) {
		if (low == 0 || mp->mm_uva < low)
			low = mp->mm_uva;
		top = mp->mm_uva + mp->mm_size - 1;
		if (high == 0 || top > high)
			high = top;
		mp = mp->mm_next;
	}
	if (lap)
		*lap = low;
	if (hap)
		*hap = high;
}

mmclash(mp, la, ha)
	register struct mapmem *mp;
	caddr_t la, ha;
{
	while (mp) {
		if (ha > mp->mm_uva && la < mp->mm_uva + mp->mm_size)
			return(1);
		mp = mp->mm_next;
	}
	return(0);
}

#endif	/* MAPMEM */
