/*-
 * Copyright (c) 1982, 1986, 1988 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)ka730.c	7.4 (Berkeley) 5/9/91
 */

#if VAX730

/*
 * 730-specific code.
 */

#include "sys/param.h"

#include "../include/cpu.h"
#include "mem.h"
#include "../include/mtpr.h"

struct	mcr730 {
	int	mc_addr;		/* address and syndrome */
	int	mc_err;			/* error bits */
};

#define	M730_UNCORR	0x80000000	/* rds, uncorrectable error */
#define	M730_CRD	0x40000000	/* crd */
#define	M730_FTBPE	0x20000000	/* force tbuf parity error */
#define	M730_ENACRD	0x10000000	/* enable crd interrupt */
#define	M730_MME	0x08000000	/* mem-man enable (ala ipr) */
#define	M730_DM		0x04000000	/* diagnostic mode */
#define	M730_DISECC	0x02000000	/* disable ecc */

#define	M730_INH(mcr)	((mcr)->mc_err = M730_MME)
#define	M730_ENA(mcr)	((mcr)->mc_err = (M730_MME|M730_ENACRD))
#define	M730_ERR(mcr)	((mcr)->mc_err & (M730_UNCORR|M730_CRD))
#define	M730_SYN(addr)	((mcr)->mc_addr & 0x7f)
#define	M730_ADDR(addr)	(((mcr)->mc_addr >> 8) & 0x7fff)

/* enable crd interrupts */
ka730_memenable()
{

	M730_ENA((struct mcr730 *)mcraddr[0]);
}

/* log crd errors */
ka730_memerr()
{
	register struct mcr730 *mcr = (struct mcr730 *)mcraddr[0];
	struct mcr730 amcr;

	/*
	 * Must be careful on the 730 not to use invalid
	 * instructions in I/O space, so make a copy;
	 */
	amcr.mc_addr = mcr->mc_addr;
	amcr.mc_err = mcr->mc_err;
	if (M730_ERR(&amcr)) {
		printf("mcr0: %s", (amcr.mc_err & M730_UNCORR) ?
		    "hard error" : "soft ecc");
		printf(" addr %x syn %x\n", M730_ADDR(&amcr), M730_SYN(&amcr));
		M730_INH(mcr);
	}
}

#define	NMC730	12
char *mc730[] = {
	"tb par",	"bad retry",	"bad intr id",	"cant write ptem",
	"unkn mcr err",	"iib rd err",	"nxm ref",	"cp rds",
	"unalgn ioref",	"nonlw ioref",	"bad ioaddr",	"unalgn ubaddr",
};

struct mc730frame {
	int	mc3_bcnt;		/* byte count == 0xc */
	int	mc3_summary;		/* summary parameter */
	int	mc3_parm[2];		/* parameter 1 and 2 */
	int	mc3_pc;			/* trapped pc */
	int	mc3_psl;		/* trapped psl */
};

ka730_mchk(cmcf)
	caddr_t cmcf;
{
	register struct mc730frame *mcf = (struct mc730frame *)cmcf;
	register u_int type = mcf->mc3_summary;

	printf("machine check %x: %s\n", type,
	    type < NMC730 ? mc730[type] : "???");
	printf("\tparams %x,%x pc %x psl %x mcesr %x\n",
	    mcf->mc3_parm[0], mcf->mc3_parm[1],
	    mcf->mc3_pc, mcf->mc3_psl, mfpr(MCESR));
	mtpr(MCESR, 0xf);
	return (MCHK_PANIC);
}
#endif
