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
 *	@(#)sbus.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: sbus.c,v 1.10 92/11/26 02:28:13 torek Exp $ (LBL)
 */

/*
 * Sbus stuff.
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>

#include <sparc/sbus/sbusreg.h>
#include <sparc/sbus/sbusvar.h>

/* autoconfiguration driver */
void	sbus_attach __P((struct device *, struct device *, void *));
struct cfdriver sbuscd =
    { NULL, "sbus", matchbyname, sbus_attach,
      DV_DULL, sizeof(struct sbus_softc) };

/*
 * Print the location of some sbus-attached device (called just
 * before attaching that device).  If `sbus' is not NULL, the
 * device was found but not configured; print the sbus as well.
 * Return UNCONF (config_find ignores this if the device was configured).
 */
int
sbus_print(args, sbus)
	void *args;
	char *sbus;
{
	register struct sbus_attach_args *sa = args;

	if (sbus)
		printf("%s at %s", sa->sa_ra.ra_name, sbus);
	printf(" slot %d offset 0x%x", sa->sa_slot, sa->sa_offset);
	return (UNCONF);
}

/*
 * Attach an Sbus.
 */
void
sbus_attach(parent, self, aux)
	struct device *parent;
	struct device *self;
	void *aux;
{
	register struct sbus_softc *sc = (struct sbus_softc *)self;
	register int base, node, slot;
	register char *name;
	struct sbus_attach_args sa;
	register struct romaux *ra;

	/*
	 * XXX there is only one Sbus, for now -- do not know how to
	 * address children on others
	 */
	if (sc->sc_dev.dv_unit > 0) {
		printf(" unsupported\n");
		return;
	}

	/*
	 * Record clock frequency for synchronous SCSI.
	 * IS THIS THE CORRECT DEFAULT??
	 */
	ra = aux;
	node = ra->ra_node;
	sc->sc_clockfreq = getpropint(node, "clock-frequency", 25*1000*1000);
	printf(": clock = %s MHz\n", clockfreq(sc->sc_clockfreq));

	if (ra->ra_bp != NULL && strcmp(ra->ra_bp->name, "sbus") == 0)
		sa.sa_ra.ra_bp = ra->ra_bp + 1;
	else
		sa.sa_ra.ra_bp = NULL;

	/*
	 * Loop through ROM children, fixing any relative addresses
	 * and then configuring each device.
	 */
	for (node = firstchild(node); node; node = nextsibling(node)) {
		name = getpropstring(node, "name");
		if (!romprop(&sa.sa_ra, name, node))
			continue;
		base = (int)sa.sa_ra.ra_paddr;
		if (SBUS_ABS(base)) {
			sa.sa_slot = SBUS_ABS_TO_SLOT(base);
			sa.sa_offset = SBUS_ABS_TO_OFFSET(base);
		} else {
			sa.sa_slot = slot = sa.sa_ra.ra_iospace;
			sa.sa_offset = base;
			sa.sa_ra.ra_paddr = (void *)SBUS_ADDR(slot, base);
		}
		(void) config_found(&sc->sc_dev, (void *)&sa, sbus_print);
	}
}

/*
 * Each attached device calls sbus_establish after it initializes
 * its sbusdev portion.
 */
void
sbus_establish(sd, dev)
	register struct sbusdev *sd;
	register struct device *dev;
{
	register struct sbus_softc *sc = (struct sbus_softc *)dev->dv_parent;

	sd->sd_dev = dev;
	sd->sd_bchain = sc->sc_sbdev;
	sc->sc_sbdev = sd;
}

/*
 * Reset the given sbus. (???)
 */
void
sbusreset(sbus)
	int sbus;
{
	register struct sbusdev *sd;
	struct sbus_softc *sc = sbuscd.cd_devs[sbus];
	struct device *dev;

	printf("reset %s:", sc->sc_dev.dv_xname);
	for (sd = sc->sc_sbdev; sd != NULL; sd = sd->sd_bchain) {
		if (sd->sd_reset) {
			dev = sd->sd_dev;
			(*sd->sd_reset)(dev);
			printf(" %s", dev->dv_xname);
		}
	}
}
