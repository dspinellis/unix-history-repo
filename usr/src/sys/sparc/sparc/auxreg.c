/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)auxreg.c	7.3 (Berkeley) %G%
 *
 * from: $Header: auxreg.c,v 1.7 92/06/17 05:21:54 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>
#include <sys/kernel.h>

#include <machine/autoconf.h>

#include <sparc/sparc/vaddrs.h>
#include <sparc/sparc/auxreg.h>

static void auxregattach __P((struct device *, struct device *, void *));
struct cfdriver auxregcd =
    { NULL, "auxiliary-io", matchbyname, auxregattach,
      DV_DULL, sizeof(struct device) };

#ifdef BLINK
static int
blink(zero)
	void *zero;
{
	register int s;
	register fixpt_t lav;

	s = splhigh();
	*AUXIO_REG = (*AUXIO_REG | AUXIO_MB1) ^ AUXIO_LED;
	splx(s);
	/*
	 * Blink rate is:
	 *	full cycle every second if completely idle (loadav = 0)
	 *	full cycle every 2 seconds if loadav = 1
	 *	full cycle every 3 seconds if loadav = 2
	 * etc.
	 */
	s = (((averunnable[0] + FSCALE) * hz) >> (FSHIFT + 1));
	timeout(blink, (caddr_t)0, s);
}
#endif

/* ARGSUSED */
static void
auxregattach(parent, self, aux)
	struct device *parent, *self;
	void *aux;
{
	struct romaux *ra = aux;

	(void)mapdev(ra->ra_paddr, AUXREG_VA, sizeof(long));
	printf("\n");
#ifdef BLINK
	blink((caddr_t)0);
#endif
}
