/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
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
 *	@(#)bi.c	7.3 (Berkeley) 6/28/90
 */

/*
 * VAXBI specific routines.
 */

#include "param.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vax/nexus.h"

#include "bireg.h"

bi_reset(bi)
	register struct biiregs *bi;
{

	bi->bi_csr |= BICSR_NRST;
	DELAY(10000);		/* ??? */
}

/*
 * Reset with self test.  Return true iff reset fails.
 * BEWARE, THIS RESETS THE BI ARBITRATION LEVEL TO ARB_NONE
 * does self test ever cause a bi bus error?
 */
bi_selftest(bi)
	register struct biiregs *bi;
{
	register int timo;

	bi->bi_csr |= BICSR_ARB_NONE;	/* why? */
	bi->bi_csr |= BICSR_STS | BICSR_INIT;/* must this be separate? */
	DELAY(50);			/* why? */
	timo = todr() + 1000;
	while (bi->bi_csr & BICSR_BROKE) {
		if (todr() > timo)	/* reset failed */
			return (-1);
	}
	return (0);			/* reset OK */
}

/*
 * THIS SHOULD PROBABLY WORK MORE LIKE ubaerror()
 * (but then we would need to be able to reset BI nodes)
 * (we need a per-BI-device driver structure!)
 */
bi_buserr(binum)
	int binum;
{
	register struct bi_node *bi;
	register int node;
	extern int bi_nodes;
	extern int cold;

	printf("vaxbi%d: bus error\n", binum);
	bi = (struct bi_node *) &nexus[binum * NNODEBI];/* XXX */
	for (node = 0; node < 16; node++, bi++) {
		if ((bi_nodes & (1 << node)) == 0)	/* XXX crude */
			continue;
		printf("node %x: ber=%b\n", node, bi->biic.bi_ber, BIBER_BITS);
	}
	panic("bi_buserr");
}
