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
 *	@(#)intreg.h	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: intreg.h,v 1.7 92/11/26 03:04:53 torek Exp $ (LBL)
 */

/*
 * sun4c interrupt enable register.
 *
 * The register is a single byte.  C code must use the ienab_bis and
 * ienab_bic functions found in locore.s.
 *
 * The register's physical address is defined here as the register
 * must be mapped early in the boot process (otherwise NMI handling
 * will fail).
 */
#define	INT_ENABLE_REG_PHYSADR	0xf5000000	/* phys addr in IOspace */

/*
 * Bits in interrupt enable register.  Software interrupt requests must
 * be cleared in software.  This is done in locore.s.  The ALLIE bit must
 * be cleared to clear asynchronous memory error (level 15) interrupts.
 */
#define	IE_L14		0x80	/* enable level 14 (counter 1) interrupts */
#define	IE_L10		0x20	/* enable level 10 (counter 0) interrupts */
#define	IE_L8		0x10	/* enable level 8 interrupts */
#define	IE_L6		0x08	/* request software level 6 interrupt */
#define	IE_L4		0x04	/* request software level 4 interrupt */
#define	IE_L1		0x02	/* request software level 1 interrupt */
#define	IE_ALLIE	0x01	/* enable interrupts */

#ifndef LOCORE
void	ienab_bis __P((int bis));	/* set given bits */
void	ienab_bic __P((int bic));	/* clear given bits */
#endif
