/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
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
 *	@(#)bi.c	7.4 (Berkeley) 12/16/90
 */

/*
 * VAXBI specific routines.
 */

#include "sys/param.h"
#include "../include/cpu.h"
#include "../include/mtpr.h"
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
