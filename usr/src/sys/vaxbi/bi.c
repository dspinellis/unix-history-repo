/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)bi.c	7.2 (Berkeley) 7/9/88
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
