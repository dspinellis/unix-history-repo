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
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)memreg.c	7.4 (Berkeley) %G%
 *
 * from: $Header: memreg.c,v 1.7 92/11/26 03:05:04 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>
#include <machine/ctlreg.h>

#include <sparc/sparc/memreg.h>

static int memregmatch __P((struct device *, struct cfdata *, void *));
static void memregattach __P((struct device *, struct device *, void *));
struct cfdriver memregcd =
    { 0, "memreg", memregmatch, memregattach, DV_DULL, sizeof(struct device) };

/*
 * The OPENPROM calls this "memory-error".
 */
static int
memregmatch(parent, cf, aux)
	struct device *parent;
	struct cfdata *cf;
	void *aux;
{

	return (strcmp("memory-error", ((struct romaux *)aux)->ra_name) == 0);
}

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
