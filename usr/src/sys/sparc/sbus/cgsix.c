/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)cgsix.c	8.4 (Berkeley) 1/21/94
 *
 * from: $Header: cgsix.c,v 1.2 93/10/18 00:01:51 torek Exp $
 */

/*
 * color display (cgsix) driver.
 *
 * Does not handle interrupts, even though they can occur.
 *
 * XXX should defer colormap updates to vertical retrace interrupts
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/device.h>
#include <sys/fbio.h>
#include <sys/ioctl.h>
#include <sys/malloc.h>
#include <sys/mman.h>
#include <sys/tty.h>

#ifdef DEBUG
#include <sys/proc.h>
#include <sys/syslog.h>
#endif

#include <machine/autoconf.h>
#include <machine/pmap.h>
#include <machine/fbvar.h>

#include <sparc/sbus/btreg.h>
#include <sparc/sbus/btvar.h>
#include <sparc/sbus/cgsixreg.h>
#include <sparc/sbus/sbusvar.h>

union cursor_cmap {		/* colormap, like bt_cmap, but tiny */
	u_char	cm_map[2][3];	/* 2 R/G/B entries */
	u_int	cm_chip[2];	/* 2 chip equivalents */
};

struct cg6_cursor {		/* cg6 hardware cursor status */
	short	cc_enable;		/* cursor is enabled */
	struct	fbcurpos cc_pos;	/* position */
	struct	fbcurpos cc_hot;	/* hot-spot */
	struct	fbcurpos cc_size;	/* size of mask & image fields */
	u_int	cc_bits[2][32];		/* space for mask & image bits */
	union	cursor_cmap cc_color;	/* cursor colormap */
};

/* per-display variables */
struct cgsix_softc {
	struct	device sc_dev;		/* base device */
	struct	sbusdev sc_sd;		/* sbus device */
	struct	fbdevice sc_fb;		/* frame buffer device */
	volatile struct cg6_layout *sc_physadr;	/* phys addr of h/w */
	volatile struct bt_regs *sc_bt;		/* Brooktree registers */
	volatile int *sc_fhc;			/* FHC register */
	volatile struct cg6_thc *sc_thc;	/* THC registers */
	volatile struct cg6_tec_xxx *sc_tec;	/* TEC registers */
	short	sc_fhcrev;		/* hardware rev */
	short	sc_blanked;		/* true if blanked */
	struct	cg6_cursor sc_cursor;	/* software cursor info */
	union	bt_cmap sc_cmap;	/* Brooktree color map */
};

/* autoconfiguration driver */
static void	cgsixattach(struct device *, struct device *, void *);
struct cfdriver cgsixcd =
    { NULL, "cgsix", matchbyname, cgsixattach,
      DV_DULL, sizeof(struct cgsix_softc) };

/* frame buffer generic driver */
static void	cg6_unblank(struct device *);
static struct fbdriver cg6_fbdriver = { cg6_unblank };

/*
 * Unlike the bw2 and cg3 drivers, we do not need to provide an rconsole
 * interface, as the cg6 is fast enough.
 */

extern int fbnode;

#define	CGSIX_MAJOR	67		/* XXX */

static void cg6_reset __P((struct cgsix_softc *));
static void cg6_loadcmap __P((struct cgsix_softc *, int, int));
static void cg6_loadomap __P((struct cgsix_softc *));
static void cg6_setcursor __P((struct cgsix_softc *));/* set position */
static void cg6_loadcursor __P((struct cgsix_softc *));/* set shape */

/*
 * Attach a display.
 */
void
cgsixattach(parent, self, args)
	struct device *parent, *self;
	void *args;
{
	register struct cgsix_softc *sc = (struct cgsix_softc *)self;
	register struct sbus_attach_args *sa = args;
	register int node = sa->sa_ra.ra_node, ramsize, i;
	register volatile struct bt_regs *bt;
	register volatile struct cg6_layout *p;

	sc->sc_fb.fb_major = CGSIX_MAJOR;	/* XXX to be removed */

	sc->sc_fb.fb_driver = &cg6_fbdriver;
	sc->sc_fb.fb_device = &sc->sc_dev;
	sc->sc_fb.fb_type.fb_type = FBTYPE_SUNFAST_COLOR;
	sc->sc_fb.fb_type.fb_width = getpropint(node, "width", 1152);
	sc->sc_fb.fb_type.fb_height = getpropint(node, "height", 900);
	sc->sc_fb.fb_linebytes = getpropint(node, "linebytes", 1152);
	ramsize = sc->sc_fb.fb_type.fb_height * sc->sc_fb.fb_linebytes;
	sc->sc_fb.fb_type.fb_depth = 8;
	sc->sc_fb.fb_type.fb_cmsize = 256;
	sc->sc_fb.fb_type.fb_size = ramsize;
	printf(": %s, %d x %d", getpropstring(node, "model"),
	    sc->sc_fb.fb_type.fb_width, sc->sc_fb.fb_type.fb_height);

	/*
	 * Dunno what the PROM has mapped, though obviously it must have
	 * the video RAM mapped.  Just map what we care about for ourselves
	 * (the FHC, THC, and Brooktree registers).
	 */
	sc->sc_physadr = p = (struct cg6_layout *)sa->sa_ra.ra_paddr;
	sc->sc_bt = bt = (volatile struct bt_regs *)
	    mapiodev((caddr_t)&p->cg6_bt_un.un_btregs, sizeof *sc->sc_bt);
	sc->sc_fhc = (volatile int *)
	    mapiodev((caddr_t)&p->cg6_fhc_un.un_fhc, sizeof *sc->sc_fhc);
	sc->sc_thc = (volatile struct cg6_thc *)
	    mapiodev((caddr_t)&p->cg6_thc_un.un_thc, sizeof *sc->sc_thc);
	sc->sc_tec = (volatile struct cg6_tec_xxx *)
	    mapiodev((caddr_t)&p->cg6_tec_un.un_tec, sizeof *sc->sc_tec);

	sc->sc_fhcrev = (*sc->sc_fhc >> FHC_REV_SHIFT) &
	    (FHC_REV_MASK >> FHC_REV_SHIFT);
	printf(", rev %d", sc->sc_fhcrev);

	/* reset cursor & frame buffer controls */
	cg6_reset(sc);

	/* grab initial (current) color map (DOES THIS WORK?) */
	bt->bt_addr = 0;
	for (i = 0; i < 256 * 3; i++)
		((char *)&sc->sc_cmap)[i] = bt->bt_cmap >> 24;

	/* enable video */
	sc->sc_thc->thc_misc |= THC_MISC_VIDEN;

	printf("\n");
	sbus_establish(&sc->sc_sd, &sc->sc_dev);
	if (node == fbnode)
		fb_attach(&sc->sc_fb);
}

int
cgsixopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	int unit = minor(dev);

	if (unit >= cgsixcd.cd_ndevs || cgsixcd.cd_devs[unit] == NULL)
		return (ENXIO);
	return (0);
}

int
cgsixclose(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	struct cgsix_softc *sc = cgsixcd.cd_devs[minor(dev)];

	cg6_reset(sc);
	return (0);
}

int
cgsixioctl(dev, cmd, data, flags, p)
	dev_t dev;
	int cmd;
	register caddr_t data;
	int flags;
	struct proc *p;
{
	register struct cgsix_softc *sc = cgsixcd.cd_devs[minor(dev)];
	u_int count;
	int i, v, error;
	union cursor_cmap tcm;

	switch (cmd) {

	case FBIOGTYPE:
		*(struct fbtype *)data = sc->sc_fb.fb_type;
		break;

	case FBIOGATTR:
#define fba ((struct fbgattr *)data)
		fba->real_type = sc->sc_fb.fb_type.fb_type;
		fba->owner = 0;		/* XXX ??? */
		fba->fbtype = sc->sc_fb.fb_type;
		fba->sattr.flags = 0;
		fba->sattr.emu_type = sc->sc_fb.fb_type.fb_type;
		fba->sattr.dev_specific[0] = -1;
		fba->emu_types[0] = sc->sc_fb.fb_type.fb_type;
		fba->emu_types[1] = -1;
#undef fba
		break;

	case FBIOGETCMAP:
		return (bt_getcmap((struct fbcmap *)data, &sc->sc_cmap, 256));

	case FBIOPUTCMAP:
		/* copy to software map */
#define	p ((struct fbcmap *)data)
		error = bt_putcmap(p, &sc->sc_cmap, 256);
		if (error)
			return (error);
		/* now blast them into the chip */
		/* XXX should use retrace interrupt */
		cg6_loadcmap(sc, p->index, p->count);
#undef p
		break;

	case FBIOGVIDEO:
		*(int *)data = sc->sc_blanked;
		break;

	case FBIOSVIDEO:
		if (*(int *)data)
			cg6_unblank(&sc->sc_dev);
		else if (!sc->sc_blanked) {
			sc->sc_blanked = 1;
			sc->sc_thc->thc_misc &= ~THC_MISC_VIDEN;
		}
		break;

/* these are for both FBIOSCURSOR and FBIOGCURSOR */
#define p ((struct fbcursor *)data)
#define cc (&sc->sc_cursor)

	case FBIOGCURSOR:
		/* do not quite want everything here... */
		p->set = FB_CUR_SETALL;	/* close enough, anyway */
		p->enable = cc->cc_enable;
		p->pos = cc->cc_pos;
		p->hot = cc->cc_hot;
		p->size = cc->cc_size;

		/* begin ugh ... can we lose some of this crap?? */
		if (p->image != NULL) {
			count = cc->cc_size.y * 32 / NBBY;
			error = copyout((caddr_t)cc->cc_bits[1],
			    (caddr_t)p->image, count);
			if (error)
				return (error);
			error = copyout((caddr_t)cc->cc_bits[0],
			    (caddr_t)p->mask, count);
			if (error)
				return (error);
		}
		if (p->cmap.red != NULL) {
			error = bt_getcmap(&p->cmap,
			    (union bt_cmap *)&cc->cc_color, 2);
			if (error)
				return (error);
		} else {
			p->cmap.index = 0;
			p->cmap.count = 2;
		}
		/* end ugh */
		break;

	case FBIOSCURSOR:
		/*
		 * For setcmap and setshape, verify parameters, so that
		 * we do not get halfway through an update and then crap
		 * out with the software state screwed up.
		 */
		v = p->set;
		if (v & FB_CUR_SETCMAP) {
			/*
			 * This use of a temporary copy of the cursor
			 * colormap is not terribly efficient, but these
			 * copies are small (8 bytes)...
			 */
			tcm = cc->cc_color;
			error = bt_putcmap(&p->cmap, (union bt_cmap *)&tcm, 2);
			if (error)
				return (error);
		}
		if (v & FB_CUR_SETSHAPE) {
			if ((u_int)p->size.x > 32 || (u_int)p->size.y > 32)
				return (EINVAL);
			count = p->size.y * 32 / NBBY;
			if (!useracc(p->image, count, B_READ) ||
			    !useracc(p->mask, count, B_READ))
				return (EFAULT);
		}

		/* parameters are OK; do it */
		if (v & (FB_CUR_SETCUR | FB_CUR_SETPOS | FB_CUR_SETHOT)) {
			if (v & FB_CUR_SETCUR)
				cc->cc_enable = p->enable;
			if (v & FB_CUR_SETPOS)
				cc->cc_pos = p->pos;
			if (v & FB_CUR_SETHOT)
				cc->cc_hot = p->hot;
			cg6_setcursor(sc);
		}
		if (v & FB_CUR_SETCMAP) {
			cc->cc_color = tcm;
			cg6_loadomap(sc); /* XXX defer to vertical retrace */
		}
		if (v & FB_CUR_SETSHAPE) {
			cc->cc_size = p->size;
			count = p->size.y * 32 / NBBY;
			bzero((caddr_t)cc->cc_bits, sizeof cc->cc_bits);
			bcopy(p->mask, (caddr_t)cc->cc_bits[0], count);
			bcopy(p->image, (caddr_t)cc->cc_bits[1], count);
			cg6_loadcursor(sc);
		}
		break;

#undef p
#undef cc

	case FBIOGCURPOS:
		*(struct fbcurpos *)data = sc->sc_cursor.cc_pos;
		break;

	case FBIOSCURPOS:
		sc->sc_cursor.cc_pos = *(struct fbcurpos *)data;
		cg6_setcursor(sc);
		break;

	case FBIOGCURMAX:
		/* max cursor size is 32x32 */
		((struct fbcurpos *)data)->x = 32;
		((struct fbcurpos *)data)->y = 32;
		break;

	default:
#ifdef DEBUG
		log(LOG_NOTICE, "cgsixioctl(%x) (%s[%d])\n", cmd,
		    p->p_comm, p->p_pid);
#endif
		return (ENOTTY);
	}
	return (0);
}

/*
 * Clean up hardware state (e.g., after bootup or after X crashes).
 */
static void
cg6_reset(sc)
	register struct cgsix_softc *sc;
{
	register volatile struct cg6_tec_xxx *tec;
	register int fhc;
	register volatile struct bt_regs *bt;

	/* hide the cursor, just in case */
	sc->sc_thc->thc_cursxy = (THC_CURSOFF << 16) | THC_CURSOFF;

	/* turn off frobs in transform engine (makes X11 work) */
	tec = sc->sc_tec;
	tec->tec_mv = 0;
	tec->tec_clip = 0;
	tec->tec_vdc = 0;

	/* take care of hardware bugs in old revisions */
	if (sc->sc_fhcrev < 5) {
		/*
		 * Keep current resolution; set cpu to 68020, set test
		 * window (size 1Kx1K), and for rev 1, disable dest cache.
		 */
		fhc = (*sc->sc_fhc & FHC_RES_MASK) | FHC_CPU_68020 |
		    FHC_TEST |
		    (11 << FHC_TESTX_SHIFT) | (11 << FHC_TESTY_SHIFT);
		if (sc->sc_fhcrev < 2)
			fhc |= FHC_DST_DISABLE;
		*sc->sc_fhc = fhc;
	}

	/* Enable cursor in Brooktree DAC. */
	bt = sc->sc_bt;
	bt->bt_addr = 0x06 << 24;
	bt->bt_ctrl |= 0x03 << 24;
}

static void
cg6_setcursor(sc)
	register struct cgsix_softc *sc;
{

	/* we need to subtract the hot-spot value here */
#define COORD(f) (sc->sc_cursor.cc_pos.f - sc->sc_cursor.cc_hot.f)
	sc->sc_thc->thc_cursxy = sc->sc_cursor.cc_enable ?
	    ((COORD(x) << 16) | (COORD(y) & 0xffff)) :
	    (THC_CURSOFF << 16) | THC_CURSOFF;
#undef COORD
}

static void
cg6_loadcursor(sc)
	register struct cgsix_softc *sc;
{
	register volatile struct cg6_thc *thc;
	register u_int edgemask, m;
	register int i;

	/*
	 * Keep the top size.x bits.  Here we *throw out* the top
	 * size.x bits from an all-one-bits word, introducing zeros in
	 * the top size.x bits, then invert all the bits to get what
	 * we really wanted as our mask.  But this fails if size.x is
	 * 32---a sparc uses only the low 5 bits of the shift count---
	 * so we have to special case that.
	 */
	edgemask = ~0;
	if (sc->sc_cursor.cc_size.x < 32)
		edgemask = ~(edgemask >> sc->sc_cursor.cc_size.x);
	thc = sc->sc_thc;
	for (i = 0; i < 32; i++) {
		m = sc->sc_cursor.cc_bits[0][i] & edgemask;
		thc->thc_cursmask[i] = m;
		thc->thc_cursbits[i] = m & sc->sc_cursor.cc_bits[1][i];
	}
}

/*
 * Load a subset of the current (new) colormap into the color DAC.
 */
static void
cg6_loadcmap(sc, start, ncolors)
	register struct cgsix_softc *sc;
	register int start, ncolors;
{
	register volatile struct bt_regs *bt;
	register u_int *ip, i;
	register int count;

	ip = &sc->sc_cmap.cm_chip[BT_D4M3(start)];	/* start/4 * 3 */
	count = BT_D4M3(start + ncolors - 1) - BT_D4M3(start) + 3;
	bt = sc->sc_bt;
	bt->bt_addr = BT_D4M4(start) << 24;
	while (--count >= 0) {
		i = *ip++;
		/* hardware that makes one want to pound boards with hammers */
		bt->bt_cmap = i;
		bt->bt_cmap = i << 8;
		bt->bt_cmap = i << 16;
		bt->bt_cmap = i << 24;
	}
}

/*
 * Load the cursor (overlay `foreground' and `background') colors.
 */
static void
cg6_loadomap(sc)
	register struct cgsix_softc *sc;
{
	register volatile struct bt_regs *bt;
	register u_int i;

	bt = sc->sc_bt;
	bt->bt_addr = 0x01 << 24;	/* set background color */
	i = sc->sc_cursor.cc_color.cm_chip[0];
	bt->bt_omap = i;		/* R */
	bt->bt_omap = i << 8;		/* G */
	bt->bt_omap = i << 16;		/* B */

	bt->bt_addr = 0x03 << 24;	/* set foreground color */
	bt->bt_omap = i << 24;		/* R */
	i = sc->sc_cursor.cc_color.cm_chip[1];
	bt->bt_omap = i;		/* G */
	bt->bt_omap = i << 8;		/* B */
}

static void
cg6_unblank(dev)
	struct device *dev;
{
	struct cgsix_softc *sc = (struct cgsix_softc *)dev;

	if (sc->sc_blanked) {
		sc->sc_blanked = 0;
		sc->sc_thc->thc_misc |= THC_MISC_VIDEN;
	}
}

/* XXX the following should be moved to a "user interface" header */
/*
 * Base addresses at which users can mmap() the various pieces of a cg6.
 * Note that although the Brooktree color registers do not occupy 8K,
 * the X server dies if we do not allow it to map 8K there (it just maps
 * from 0x70000000 forwards, as a contiguous chunk).
 */
#define	CG6_USER_FBC	0x70000000
#define	CG6_USER_TEC	0x70001000
#define	CG6_USER_BTREGS	0x70002000
#define	CG6_USER_FHC	0x70004000
#define	CG6_USER_THC	0x70005000
#define	CG6_USER_ROM	0x70006000
#define	CG6_USER_RAM	0x70016000
#define	CG6_USER_DHC	0x80000000

struct mmo {
	u_int	mo_uaddr;	/* user (virtual) address */
	u_int	mo_size;	/* size, or 0 for video ram size */
	u_int	mo_physoff;	/* offset from sc_physadr */
};

/*
 * Return the address that would map the given device at the given
 * offset, allowing for the given protection, or return -1 for error.
 *
 * XXX	needs testing against `demanding' applications (e.g., aviator)
 */
int
cgsixmap(dev, off, prot)
	dev_t dev;
	int off, prot;
{
	register struct cgsix_softc *sc = cgsixcd.cd_devs[minor(dev)];
	register struct mmo *mo;
	register u_int u, sz;
#define	O(memb) ((u_int)(&((struct cg6_layout *)0)->memb))
	static struct mmo mmo[] = {
		{ CG6_USER_RAM, 0, O(cg6_ram) },

		/* do not actually know how big most of these are! */
		{ CG6_USER_FBC, 1, O(cg6_fbc_un) },
		{ CG6_USER_TEC, 1, O(cg6_tec_un) },
		{ CG6_USER_BTREGS, 8192 /* XXX */, O(cg6_bt_un) },
		{ CG6_USER_FHC, 1, O(cg6_fhc_un) },
		{ CG6_USER_THC, sizeof(struct cg6_thc), O(cg6_thc_un) },
		{ CG6_USER_ROM, 65536, O(cg6_rom_un) },
		{ CG6_USER_DHC, 1, O(cg6_dhc_un) },
	};
#define NMMO (sizeof mmo / sizeof *mmo)

	if (off & PGOFSET)
		panic("cgsixmap");

	/*
	 * Entries with size 0 map video RAM (i.e., the size in fb data).
	 *
	 * Since we work in pages, the fact that the map offset table's
	 * sizes are sometimes bizarre (e.g., 1) is effectively ignored:
	 * one byte is as good as one page.
	 */
	for (mo = mmo; mo < &mmo[NMMO]; mo++) {
		if ((u_int)off < mo->mo_uaddr)
			continue;
		u = off - mo->mo_uaddr;
		sz = mo->mo_size ? mo->mo_size : sc->sc_fb.fb_type.fb_size;
		if (u < sz)
			return ((int)sc->sc_physadr + u + mo->mo_physoff +
			    PMAP_OBIO + PMAP_NC);
	}
#ifdef DEBUG
	{
	  register struct proc *p = curproc;	/* XXX */
	  log(LOG_NOTICE, "cgsixmap(%x) (%s[%d])\n", off, p->p_comm, p->p_pid);
	}
#endif
	return (-1);	/* not a user-map offset */
}
