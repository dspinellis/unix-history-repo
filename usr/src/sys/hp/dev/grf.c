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
 * from: Utah $Hdr: grf.c 1.28 89/08/14$
 *
 *	@(#)grf.c	7.6 (Berkeley) %G%
 */

/*
 * Graphics display driver for the HP300.
 * This is the hardware-independent portion of the driver.
 * Hardware access is through the grfdev routines below.
 */

#include "grf.h"
#if NGRF > 0

#include "sys/param.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/ioctl.h"
#include "sys/file.h"
#include "sys/malloc.h"

#include "device.h"
#include "grfioctl.h"
#include "grfvar.h"

#include "../include/cpu.h"

#ifdef HPUXCOMPAT
#include "../hpux/hpux.h"
#endif

#include "vm/vm_param.h"
#include "vm/vm_map.h"
#include "vm/vm_kern.h"
#include "vm/vm_page.h"
#include "vm/vm_pager.h"
#include "sys/specdev.h"
#include "sys/vnode.h"
#include "sys/mman.h"

#include "ite.h"
#if NITE == 0
#define	iteon(u,f)
#define	iteoff(u,f)
#endif

int	grfprobe();
int	tc_init(), tc_mode();
int	gb_init(), gb_mode();
int	rb_init(), rb_mode();
int	dv_init(), dv_mode();

struct grfdev grfdev[] = {
	GID_TOPCAT,	GRFBOBCAT,	tc_init,	tc_mode,
	"topcat",
	GID_GATORBOX,	GRFGATOR,	gb_init,	gb_mode,
	"gatorbox",
	GID_RENAISSANCE,GRFRBOX,	rb_init,	rb_mode,
	"renaissance",
	GID_LRCATSEYE,	GRFCATSEYE,	tc_init,	tc_mode,
	"lo-res catseye",
	GID_HRCCATSEYE,	GRFCATSEYE,	tc_init,	tc_mode,
	"hi-res catseye",
	GID_HRMCATSEYE,	GRFCATSEYE,	tc_init,	tc_mode,
	"hi-res catseye",
	GID_DAVINCI,    GRFDAVINCI,	dv_init,	dv_mode,
	"davinci",
};
int	ngrfdev = sizeof(grfdev) / sizeof(grfdev[0]);

struct	driver grfdriver = { grfprobe, "grf" };
struct	grf_softc grf_softc[NGRF];

#ifdef DEBUG
int grfdebug = 0;
#define GDB_DEVNO	0x01
#define GDB_MMAP	0x02
#define GDB_IOMAP	0x04
#define GDB_LOCK	0x08
#endif

/*
 * XXX: called from ite console init routine.
 * Does just what configure will do later but without printing anything.
 */
grfconfig()
{
	register caddr_t addr;
	register struct hp_hw *hw;
	register struct hp_device *hd, *nhd;

	for (hw = sc_table; hw->hw_type; hw++) {
	        if (hw->hw_type != BITMAP)
			continue;
		/*
		 * Found one, now match up with a logical unit number
		 */
		nhd = NULL;		
		addr = hw->hw_addr;
		for (hd = hp_dinit; hd->hp_driver; hd++) {
			if (hd->hp_driver != &grfdriver || hd->hp_alive)
				continue;
			/*
			 * Wildcarded.  If first, remember as possible match.
			 */
			if (hd->hp_addr == NULL) {
				if (nhd == NULL)
					nhd = hd;
				continue;
			}
			/*
			 * Not wildcarded.
			 * If exact match done searching, else keep looking.
			 */
			if ((caddr_t)sctoaddr(hd->hp_addr) == addr) {
				nhd = hd;
				break;
			}
		}
		/*
		 * Found a match, initialize
		 */
		if (nhd && grfinit(addr, nhd->hp_unit)) {
			nhd->hp_addr = addr;
		}
	}
}

/*
 * Normal init routine called by configure() code
 */
grfprobe(hd)
	struct hp_device *hd;
{
	struct grf_softc *gp = &grf_softc[hd->hp_unit];

	if ((gp->g_flags & GF_ALIVE) == 0 &&
	    !grfinit(hd->hp_addr, hd->hp_unit))
		return(0);
	printf("grf%d: %d x %d ", hd->hp_unit,
	       gp->g_display.gd_dwidth, gp->g_display.gd_dheight);
	if (gp->g_display.gd_colors == 2)
		printf("monochrome");
	else
		printf("%d color", gp->g_display.gd_colors);
	printf(" %s display\n", grfdev[gp->g_type].gd_desc);
	return(1);
}

grfinit(addr, unit)
	caddr_t addr;
{
	struct grf_softc *gp = &grf_softc[unit];
	struct grfreg *gr;
	register struct grfdev *gd;

	gr = (struct grfreg *) addr;
	if (gr->gr_id != GRFHWID)
		return(0);
	for (gd = grfdev; gd < &grfdev[ngrfdev]; gd++)
		if (gd->gd_hardid == gr->gr_id2)
			break;
	if (gd < &grfdev[ngrfdev] && (*gd->gd_init)(gp, addr)) {
		gp->g_display.gd_id = gd->gd_softid;
		gp->g_type = gd - grfdev;
		gp->g_flags = GF_ALIVE;
		return(1);
	}
	return(0);
}

/*ARGSUSED*/
grfopen(dev, flags)
	dev_t dev;
{
	int unit = GRFUNIT(dev);
	register struct grf_softc *gp = &grf_softc[unit];
	int error = 0;

	if (unit >= NGRF || (gp->g_flags & GF_ALIVE) == 0)
		return(ENXIO);
	if ((gp->g_flags & (GF_OPEN|GF_EXCLUDE)) == (GF_OPEN|GF_EXCLUDE))
		return(EBUSY);
#ifdef HPUXCOMPAT
	/*
	 * XXX: cannot handle both HPUX and BSD processes at the same time
	 */
	if (u.u_procp->p_flag & SHPUX)
		if (gp->g_flags & GF_BSDOPEN)
			return(EBUSY);
		else
			gp->g_flags |= GF_HPUXOPEN;
	else
		if (gp->g_flags & GF_HPUXOPEN)
			return(EBUSY);
		else
			gp->g_flags |= GF_BSDOPEN;
#endif
	/*
	 * First open.
	 * XXX: always put in graphics mode.
	 */
	error = 0;
	if ((gp->g_flags & GF_OPEN) == 0) {
		gp->g_flags |= GF_OPEN;
		error = grfon(dev);
	}
	return(error);
}

/*ARGSUSED*/
grfclose(dev, flags)
	dev_t dev;
{
	register struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];

	(void) grfoff(dev);
	(void) grfunlock(gp);
	gp->g_flags &= GF_ALIVE;
	return(0);
}

/*ARGSUSED*/
grfioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];
	int error;

#ifdef HPUXCOMPAT
	if (u.u_procp->p_flag & SHPUX)
		return(hpuxgrfioctl(dev, cmd, data, flag));
#endif
	error = 0;
	switch (cmd) {

	/* XXX: compatibility hack */
	case OGRFIOCGINFO:
		bcopy((caddr_t)&gp->g_display, data, sizeof(struct ogrfinfo));
		break;

	case GRFIOCGINFO:
		bcopy((caddr_t)&gp->g_display, data, sizeof(struct grfinfo));
		break;

	case GRFIOCON:
		error = grfon(dev);
		break;

	case GRFIOCOFF:
		error = grfoff(dev);
		break;

	case GRFIOCMAP:
		error = grfmmap(dev, (caddr_t *)data);
		break;

	case GRFIOCUNMAP:
		error = grfunmmap(dev, *(caddr_t *)data);
		break;

	default:
		error = EINVAL;
		break;

	}
	return(error);
}

/*ARGSUSED*/
grfselect(dev, rw)
	dev_t dev;
{
	if (rw == FREAD)
		return(0);
	return(1);
}

grflock(gp, block)
	register struct grf_softc *gp;
	int block;
{
	struct proc *p = u.u_procp;		/* XXX */
	int error;
	extern char devioc[];

#ifdef DEBUG
	if (grfdebug & GDB_LOCK)
		printf("grflock(%d): dev %x flags %x lockpid %x\n",
		       p->p_pid, gp-grf_softc, gp->g_flags,
		       gp->g_lockp ? gp->g_lockp->p_pid : -1);
#endif
#ifdef HPUXCOMPAT
	if (gp->g_pid) {
#ifdef DEBUG
		if (grfdebug & GDB_LOCK)
			printf("  lock[0] %d lockslot %d lock[lockslot] %d\n",
			       gp->g_locks[0], gp->g_lockpslot,
			       gp->g_locks[gp->g_lockpslot]);
#endif
		gp->g_locks[0] = 0;
		if (gp->g_locks[gp->g_lockpslot] == 0) {
			gp->g_lockp = NULL;
			gp->g_lockpslot = 0;
		}
	}
#endif
	if (gp->g_lockp) {
		if (gp->g_lockp == p)
			return(EBUSY);
		if (!block)
			return(EAGAIN);
		do {
			gp->g_flags |= GF_WANTED;
			if (error = tsleep((caddr_t)&gp->g_flags,
					   (PZERO+1) | PCATCH, devioc, 0))
				return (error);
		} while (gp->g_lockp);
	}
	gp->g_lockp = p;
#ifdef HPUXCOMPAT
	if (gp->g_pid) {
		int slot = grffindpid(gp);
#ifdef DEBUG
		if (grfdebug & GDB_LOCK)
			printf("  slot %d\n", slot);
#endif
		gp->g_lockpslot = gp->g_locks[0] = slot;
		gp->g_locks[slot] = 1;
	}
#endif
	return(0);
}

grfunlock(gp)
	register struct grf_softc *gp;
{
#ifdef DEBUG
	if (grfdebug & GDB_LOCK)
		printf("grfunlock(%d): dev %x flags %x lockpid %d\n",
		       u.u_procp->p_pid, gp-grf_softc, gp->g_flags,
		       gp->g_lockp ? gp->g_lockp->p_pid : -1);
#endif
	if (gp->g_lockp != u.u_procp)
		return(EBUSY);
#ifdef HPUXCOMPAT
	if (gp->g_pid) {
#ifdef DEBUG
		if (grfdebug & GDB_LOCK)
			printf("  lock[0] %d lockslot %d lock[lockslot] %d\n",
			       gp->g_locks[0], gp->g_lockpslot,
			       gp->g_locks[gp->g_lockpslot]);
#endif
		gp->g_locks[gp->g_lockpslot] = gp->g_locks[0] = 0;
		gp->g_lockpslot = 0;
	}
#endif
	if (gp->g_flags & GF_WANTED) {
		wakeup((caddr_t)&gp->g_flags); 
		gp->g_flags &= ~GF_WANTED;
	}
	gp->g_lockp = NULL;
	return(0);
}

/*ARGSUSED*/
grfmap(dev, off, prot)
	dev_t dev;
{
	return(grfaddr(&grf_softc[GRFUNIT(dev)], off));
}

#ifdef HPUXCOMPAT

/*ARGSUSED*/
hpuxgrfioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];
	int error;

	error = 0;
	switch (cmd) {

	case GCID:
		*(int *)data = gp->g_display.gd_id;
		break;

	case GCON:
		error = grfon(dev);
		break;

	case GCOFF:
		error = grfoff(dev);
		break;

	case GCLOCK:
		error = grflock(gp, 1);
		break;

	case GCUNLOCK:
		error = grfunlock(gp);
		break;

	case GCAON:
	case GCAOFF:
		break;

	/* GCSTATIC is implied by our implementation */
	case GCSTATIC_CMAP:
	case GCVARIABLE_CMAP:
		break;

	/* map in control regs and frame buffer */
	case GCMAP:
		error = grfmmap(dev, (caddr_t *)data);
		break;

	case GCUNMAP:
		error = grfunmmap(dev, *(caddr_t *)data);
		/* XXX: HP-UX uses GCUNMAP to get rid of GCSLOT memory */
		if (error)
			error = grflckunmmap(dev, *(caddr_t *)data);
		break;

	case GCSLOT:
	{
		struct grf_slot *sp = (struct grf_slot *)data;

		sp->slot = grffindpid(gp);
		if (sp->slot) {
			error = grflckmmap(dev, (caddr_t *)&sp->addr);
			if (error && gp->g_pid) {
				free((caddr_t)gp->g_pid, M_DEVBUF);
				gp->g_pid = NULL;
			}
		} else
			error = EINVAL;		/* XXX */
		break;
	}

	/*
	 * XXX: only used right now to map in rbox control registers
	 * Will be replaced in the future with a real IOMAP interface.
	 */
	case IOMAPMAP:
		error = iommap(dev, (caddr_t *)data);
#if 0
		/*
		 * It may not be worth kludging this (using p_devtmp) to
		 * make this work.  It was an undocumented side-effect
		 * in HP-UX that the mapped address was the return value
		 * of the ioctl.  The only thing I remember that counted
		 * on this behavior was the rbox X10 server.
		 */
		if (!error)
			u.u_r.r_val1 = *(int *)data;	/* XXX: this sux */
#endif
		break;

	case IOMAPUNMAP:
		error = iounmmap(dev, *(caddr_t *)data);
		break;

	default:
		error = EINVAL;
		break;
	}
	return(error);
}

#endif

grfon(dev)
	dev_t dev;
{
	int unit = GRFUNIT(dev);
	struct grf_softc *gp = &grf_softc[unit];

	/*
	 * XXX: iteoff call relies on devices being in same order
	 * as ITEs and the fact that iteoff only uses the minor part
	 * of the dev arg.
	 */
	iteoff(unit, 3);
	return((*grfdev[gp->g_type].gd_mode)
			(gp, (dev&GRFOVDEV) ? GM_GRFOVON : GM_GRFON));
}

grfoff(dev)
	dev_t dev;
{
	int unit = GRFUNIT(dev);
	struct grf_softc *gp = &grf_softc[unit];
	int error;

	(void) grfunmmap(dev, (caddr_t)0);
	error = (*grfdev[gp->g_type].gd_mode)
			(gp, (dev&GRFOVDEV) ? GM_GRFOVOFF : GM_GRFOFF);
	/* XXX: see comment for iteoff above */
	iteon(unit, 2);
	return(error);
}

grfaddr(gp, off)
	struct grf_softc *gp;
	register int off;
{
	register struct grfinfo *gi = &gp->g_display;

	/* control registers */
	if (off >= 0 && off < gi->gd_regsize)
		return(((u_int)gi->gd_regaddr + off) >> PGSHIFT);

	/* frame buffer */
	if (off >= gi->gd_regsize && off < gi->gd_regsize+gi->gd_fbsize) {
		off -= gi->gd_regsize;
		return(((u_int)gi->gd_fbaddr + off) >> PGSHIFT);
	}
	/* bogus */
	return(-1);
}

#ifdef HPUXCOMPAT
/*
 * Convert a BSD style minor devno to HPUX style.
 * We cannot just create HPUX style nodes as they require 24 bits
 * of minor device number and we only have 8.
 * XXX: This may give the wrong result for remote stats of other
 * machines where device 10 exists.
 */
grfdevno(dev)
	dev_t dev;
{
	int unit = GRFUNIT(dev);
	struct grf_softc *gp = &grf_softc[unit];
	int newdev;

	if (unit >= NGRF || (gp->g_flags&GF_ALIVE) == 0)
		return(bsdtohpuxdev(dev));
	/* magic major number */
	newdev = 12 << 24;
	/* now construct minor number */
	if (gp->g_display.gd_regaddr != (caddr_t)GRFIADDR)
		newdev |= ((u_int)gp->g_display.gd_regaddr-EXTIOBASE) | 0x200;
	if (dev & GRFIMDEV)
		newdev |= 0x02;
	else if (dev & GRFOVDEV)
		newdev |= 0x01;
#ifdef DEBUG
	if (grfdebug & GDB_DEVNO)
		printf("grfdevno: dev %x newdev %x\n", dev, newdev);
#endif
	return(newdev);
}
#endif

grfmmap(dev, addrp)
	dev_t dev;
	caddr_t *addrp;
{
	struct proc *p = u.u_procp;		/* XXX */
	struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];
	int len, error;
	struct vnode vn;
	struct specinfo si;
	int flags;

#ifdef DEBUG
	if (grfdebug & GDB_MMAP)
		printf("grfmmap(%d): addr %x\n", p->p_pid, *addrp);
#endif
	len = gp->g_display.gd_regsize + gp->g_display.gd_fbsize;
	flags = MAP_FILE|MAP_SHARED;
	if (*addrp)
		flags |= MAP_FIXED;
	else
		*addrp = (caddr_t)0x1000000;	/* XXX */
	vn.v_type = VCHR;			/* XXX */
	vn.v_specinfo = &si;			/* XXX */
	vn.v_rdev = dev;			/* XXX */
	error = vm_mmap(u.u_procp->p_map, (vm_offset_t *)addrp,
			(vm_size_t)len, VM_PROT_ALL, flags, (caddr_t)&vn, 0);
	return(error);
}

grfunmmap(dev, addr)
	dev_t dev;
	caddr_t addr;
{
	struct proc *p = u.u_procp;		/* XXX */
	struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];
	vm_size_t size;
	int rv;

#ifdef DEBUG
	if (grfdebug & GDB_MMAP)
		printf("grfunmmap(%d): dev %x addr %x\n", p->p_pid, dev, addr);
#endif
	if (addr == 0)
		return(EINVAL);		/* XXX: how do we deal with this? */
	size = round_page(gp->g_display.gd_regsize + gp->g_display.gd_fbsize);
	rv = vm_deallocate(p->p_map, (vm_offset_t)addr, size);
	return(rv == KERN_SUCCESS ? 0 : EINVAL);
}

#ifdef HPUXCOMPAT
iommap(dev, addrp)
	dev_t dev;
	caddr_t *addrp;
{
	struct proc *p = u.u_procp;		/* XXX */
	struct grf_softc *gp = &grf_softc[GRFUNIT(dev)];

#ifdef DEBUG
	if (grfdebug & (GDB_MMAP|GDB_IOMAP))
		printf("iommap(%d): addr %x\n", p->p_pid, *addrp);
#endif
	return(EINVAL);
}

iounmmap(dev, addr)
	dev_t dev;
	caddr_t addr;
{
	int unit = minor(dev);

#ifdef DEBUG
	if (grfdebug & (GDB_MMAP|GDB_IOMAP))
		printf("iounmmap(%d): id %d addr %x\n",
		       u.u_procp->p_pid, unit, addr);
#endif
	return(0);
}

/*
 * Processes involved in framebuffer mapping via GCSLOT are recorded in
 * an array of pids.  The first element is used to record the last slot used
 * (for faster lookups).  The remaining elements record up to GRFMAXLCK-1
 * process ids.  Returns a slot number between 1 and GRFMAXLCK or 0 if no
 * slot is available. 
 */
grffindpid(gp)
	struct grf_softc *gp;
{
	register short pid, *sp;
	register int i, limit;
	int ni;

	if (gp->g_pid == NULL) {
		gp->g_pid = (short *)
			malloc(GRFMAXLCK * sizeof(short), M_DEVBUF, M_WAITOK);
		bzero((caddr_t)gp->g_pid, GRFMAXLCK * sizeof(short));
	}
	pid = u.u_procp->p_pid;
	ni = limit = gp->g_pid[0];
	for (i = 1, sp = &gp->g_pid[1]; i <= limit; i++, sp++) {
		if (*sp == pid)
			goto done;
		if (*sp == 0)
			ni = i;
	}
	i = ni;
	if (i < limit) {
		gp->g_pid[i] = pid;
		goto done;
	}
	if (++i == GRFMAXLCK)
		return(0);
	gp->g_pid[0] = i;
	gp->g_pid[i] = pid;
done:
#ifdef DEBUG
	if (grfdebug & GDB_LOCK)
		printf("grffindpid(%d): slot %d of %d\n",
		       pid, i, gp->g_pid[0]);
#endif
	return(i);
}

grfrmpid(gp)
	struct grf_softc *gp;
{
	register short pid, *sp;
	register int limit, i;
	int mi;

	if (gp->g_pid == NULL || (limit = gp->g_pid[0]) == 0)
		return;
	pid = u.u_procp->p_pid;
	limit = gp->g_pid[0];
	mi = 0;
	for (i = 1, sp = &gp->g_pid[1]; i <= limit; i++, sp++) {
		if (*sp == pid)
			*sp = 0;
		else if (*sp)
			mi = i;
	}
	i = mi;
	if (i < limit)
		gp->g_pid[0] = i;
#ifdef DEBUG
	if (grfdebug & GDB_LOCK)
		printf("grfrmpid(%d): slot %d of %d\n",
		       pid, sp-gp->g_pid, gp->g_pid[0]);
#endif
}

grflckmmap(dev, addrp)
	dev_t dev;
	caddr_t *addrp;
{
	struct proc *p = u.u_procp;		/* XXX */

#ifdef DEBUG
	if (grfdebug & (GDB_MMAP|GDB_LOCK))
		printf("grflckmmap(%d): addr %x\n",
		       p->p_pid, *addrp);
#endif
	return(EINVAL);
}

grflckunmmap(dev, addr)
	dev_t dev;
	caddr_t addr;
{
	int unit = minor(dev);

#ifdef DEBUG
	if (grfdebug & (GDB_MMAP|GDB_LOCK))
		printf("grflckunmmap(%d): id %d addr %x\n",
		       u.u_procp->p_pid, unit, addr);
#endif
	return(EINVAL);
}
#endif	/* HPUXCOMPAT */

#endif	/* NGRF > 0 */
