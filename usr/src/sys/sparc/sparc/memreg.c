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
 *	@(#)memreg.c	7.3 (Berkeley) %G%
 *
 * from: $Header: memreg.c,v 1.4 92/06/17 05:22:17 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>

#include <sparc/sparc/ctlreg.h>
#include <sparc/sparc/memreg.h>

static void memregattach __P((struct device *, struct device *, void *));
struct cfdriver memregcd =
    { NULL, "memory-error", matchbyname, memregattach,
      DV_DULL, sizeof(struct device) };

/* ARGSUSED */
static void
memregattach(parent, self, aux)
	struct device *parent, *self;
	void *aux;
{
	struct romaux *ra = aux;

	par_err_reg = ra->ra_vaddr ? (volatile int *)ra->ra_vaddr :
	    (volatile int *)mapiodev(ra->ra_paddr, sizeof(int));
	printf("\n");
}

/*
 * Synchronous and asynchronous memory error handler.
 * (This is the level 15 interrupt, which is not vectored.)
 * Should kill the process that got its bits clobbered,
 * and take the page out of the page pool, but for now...
 */
void
memerr(issync, ser, sva, aer, ava)
	int issync, ser, sva, aer, ava;
{

	printf("%ssync mem err: ser=%b sva=%x aer=%b ava=%x\n",
	    issync ? "" : "a", ser, SER_BITS, sva, aer & 0xff, AER_BITS, ava);
	if (par_err_reg)
		printf("parity error register = %b\n", *par_err_reg, PER_BITS);
#ifdef DEBUG
	callrom();
#else
	panic("memory error");		/* XXX */
#endif
}
