/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
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
 *	@(#)ioa.h	7.3 (Berkeley) 5/9/91
 */

/****************************************************************
 *                                                              *
 *        Licensed from Digital Equipment Corporation           *
 *                       Copyright (c)                          *
 *               Digital Equipment Corporation                  *
 *                   Maynard, Massachusetts                     *
 *                         1985, 1986                           *
 *                    All rights reserved.                      *
 *                                                              *
 *        The Information in this software is subject to change *
 *   without notice and should not be construed as a commitment *
 *   by  Digital  Equipment  Corporation.   Digital   makes  no *
 *   representations about the suitability of this software for *
 *   any purpose.  It is supplied "As Is" without expressed  or *
 *   implied  warranty.                                         *
 *                                                              *
 *        If the Regents of the University of California or its *
 *   licensees modify the software in a manner creating         *
 *   diriviative copyright rights, appropriate copyright        *
 *   legends may be placed on  the drivative work in addition   *
 *   to that set forth above.                                   *
 *								*
 ****************************************************************/

#if VAX8600
#define	MAXNIOA		4
#define	NIOA8600	2
#define IOASIZE		0x2000000
#define IOAMAPSIZ 	512		/* Map one page to get at SBIA regs */
#define	IOA8600(i)	((caddr_t)(0x20080000+IOASIZE*i))

#ifndef LOCORE
struct	sbia_regs
{
	int sbi_cfg;
	int sbi_csr;
	int sbi_errsum;
	int sbi_dctl;
	int sbi_dmaica;
	int sbi_dmaiid;
	int sbi_dmaaca;
	int sbi_dmaaid;
	int sbi_dmabcs;
	int sbi_dmabid;
	int sbi_dmaccs;
	int sbi_dmacid;
	int sbi_silo;
	int sbi_error;
	int sbi_timo;
	int sbi_fltsts;
	int sbi_silcmp;
	int sbi_maint;
	int sbi_unjam;
	int sbi_qclr;
	int sbi_unused[12];
	int sbi_iv10;
	int sbi_iv11;
	int sbi_iv12;
	int sbi_iv13;
	int sbi_iv14;
	int sbi_iv15;
	int sbi_iv16;
	int sbi_iv17;
	int sbi_iv18;
	int sbi_iv19;
	int sbi_iv1a;
	int sbi_iv1b;
	int sbi_iv1c;
	int sbi_iv1d;
	int sbi_iv1e;
};
struct	ioa {
	union ioacsr {
		long	ioa_csr;
		u_char	ioa_type;
	} ioacsr;
	long	ioa_pad[IOAMAPSIZ / sizeof (long) - 1];
};
#ifdef	KERNEL
struct ioa ioa[MAXNIOA];
#endif  KERNEL
#endif	LOCORE

#define IOA_TYPMSK 0xf0
#define IOA_SBIA	0x10

#endif VAX8600
