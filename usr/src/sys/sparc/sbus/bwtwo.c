/*
 * Copyright (c) 1992, 1993
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
 *	@(#)bwtwo.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: bwtwo.c,v 1.14 92/11/26 02:28:04 torek Exp $
 */

/*
 * black&white display (bwtwo) driver.
 *
 * Does not handle interrupts, even though they can occur.
 */

#include <sys/param.h>
#include <sys/device.h>
#include <sys/fbio.h>
#include <sys/ioctl.h>
#include <sys/malloc.h>
#include <sys/mman.h>
#include <sys/tty.h>

#include <machine/autoconf.h>
#include <machine/pmap.h>
#include <machine/fbvar.h>

#include <sparc/sbus/bwtworeg.h>
#include <sparc/sbus/sbusvar.h>

/* per-display variables */
struct bwtwo_softc {
	struct	device sc_dev;		/* base device */
	struct	sbusdev sc_sd;		/* sbus device */
	struct	fbdevice sc_fb;		/* frame buffer device */
	volatile struct bwtworeg *sc_reg;/* control registers */
	caddr_t	sc_phys;		/* display RAM (phys addr) */
};

/* autoconfiguration driver */
static void	bwtwoattach(struct device *, struct device *, void *);
struct cfdriver bwtwocd =
    { NULL, "bwtwo", matchbyname, bwtwoattach,
      DV_DULL, sizeof(struct bwtwo_softc) };

/* XXX we do not handle frame buffer interrupts (do not know how) */

/* frame buffer generic driver */
static void	bwtwounblank(struct device *);
static struct fbdriver bwtwofbdriver = { bwtwounblank };

extern int fbnode;
extern struct tty *fbconstty;
extern int (*v_putc)();
extern int nullop();
static int bwtwo_cnputc();
static struct bwtwo_softc *bwcons;

#define	BWTWO_MAJOR	27		/* XXX */

/*
 * Attach a display.  We need to notice if it is the console, too.
 */
void
bwtwoattach(parent, self, args)
	struct device *parent, *self;
	void *args;
{
	register struct bwtwo_softc *sc = (struct bwtwo_softc *)self;
	register struct sbus_attach_args *sa = args;
	register int node = sa->sa_ra.ra_node, ramsize;
	register struct bwtwo_all *p;
	int isconsole;

	sc->sc_fb.fb_major = BWTWO_MAJOR;	/* XXX to be removed */

	sc->sc_fb.fb_driver = &bwtwofbdriver;
	sc->sc_fb.fb_device = &sc->sc_dev;
	/*
	 * The defaults below match my screen, but are not guaranteed
	 * to be correct as defaults go...
	 */
	sc->sc_fb.fb_type.fb_type = FBTYPE_SUN2BW;
	sc->sc_fb.fb_type.fb_width = getpropint(node, "width", 1152);
	sc->sc_fb.fb_type.fb_height = getpropint(node, "height", 900);
	sc->sc_fb.fb_linebytes = getpropint(node, "linebytes", 144);
	ramsize = sc->sc_fb.fb_type.fb_height * sc->sc_fb.fb_linebytes;
	sc->sc_fb.fb_type.fb_depth = 1;
	sc->sc_fb.fb_type.fb_cmsize = 0;
	sc->sc_fb.fb_type.fb_size = ramsize;
	printf(": %s, %d x %d", getpropstring(node, "model"),
	    sc->sc_fb.fb_type.fb_width, sc->sc_fb.fb_type.fb_height);

	/*
	 * When the ROM has mapped in a bwtwo display, the address
	 * maps only the video RAM, so in any case we have to map the
	 * registers ourselves.  We only need the video RAM if we are
	 * going to print characters via rconsole.
	 */
	isconsole = node == fbnode && fbconstty != NULL;
	p = (struct bwtwo_all *)sa->sa_ra.ra_paddr;
	if ((sc->sc_fb.fb_pixels = sa->sa_ra.ra_vaddr) == NULL && isconsole) {
		/* this probably cannot happen, but what the heck */
		sc->sc_fb.fb_pixels = mapiodev(p->ba_ram, ramsize);
	}
	sc->sc_reg = (volatile struct bwtworeg *)
	    mapiodev((caddr_t)&p->ba_reg, sizeof(p->ba_reg));
	sc->sc_phys = p->ba_ram;

	/* Insure video is enabled */
	sc->sc_reg->bw_ctl |= CTL_VE;

	if (isconsole) {
		printf(" (console)\n");
#ifdef RCONSOLE
		rcons_init(&sc->sc_fb);
#endif
	} else
		printf("\n");
	sbus_establish(&sc->sc_sd, &sc->sc_dev);
	if (node == fbnode)
		fb_attach(&sc->sc_fb);
}

int
bwtwoopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	int unit = minor(dev);

	if (unit >= bwtwocd.cd_ndevs || bwtwocd.cd_devs[unit] == NULL)
		return (ENXIO);
	return (0);
}

int
bwtwoclose(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{

	return (0);
}

int
bwtwoioctl(dev, cmd, data, flags, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flags;
	struct proc *p;
{
	struct bwtwo_softc *sc = bwtwocd.cd_devs[minor(dev)];

	switch (cmd) {

	case FBIOGTYPE:
		*(struct fbtype *)data = sc->sc_fb.fb_type;
		break;

	case FBIOGVIDEO:
		*(int *)data = (sc->sc_reg->bw_ctl & CTL_VE) != 0;
		break;

	case FBIOSVIDEO:
		if (*(int *)data)
			sc->sc_reg->bw_ctl |= CTL_VE;
		else
			sc->sc_reg->bw_ctl &= ~CTL_VE;
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

static void
bwtwounblank(dev)
	struct device *dev;
{
	struct bwtwo_softc *sc = (struct bwtwo_softc *)dev;

	sc->sc_reg->bw_ctl |= CTL_VE;
}

/*
 * Return the address that would map the given device at the given
 * offset, allowing for the given protection, or return -1 for error.
 */
int
bwtwomap(dev, off, prot)
	dev_t dev;
	int off, prot;
{
	register struct bwtwo_softc *sc = bwtwocd.cd_devs[minor(dev)];

	if (off & PGOFSET)
		panic("bwtwomap");
	if ((unsigned)off >= sc->sc_fb.fb_type.fb_size)
		return (-1);
	/*
	 * I turned on PMAP_NC here to disable the cache as I was
	 * getting horribly broken behaviour with it on.
	 */
	return ((int)sc->sc_phys + off + PMAP_OBIO + PMAP_NC);
}
