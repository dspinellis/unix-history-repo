/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)psl.h	8.1 (Berkeley) 6/10/93
 */

#include <machine/machConst.h>

#define	PSL_LOWIPL	(MACH_INT_MASK | MACH_SR_INT_ENA_CUR)

#define	PSL_USERSET (	\
	MACH_SR_KU_OLD |	\
	MACH_SR_INT_ENA_OLD |	\
	MACH_SR_KU_PREV |	\
	MACH_SR_INT_ENA_PREV |	\
	MACH_INT_MASK)

#define	PSL_USERCLR (	\
	MACH_SR_COP_USABILITY |	\
	MACH_SR_BOOT_EXC_VEC |	\
	MACH_SR_TLB_SHUTDOWN |	\
	MACH_SR_PARITY_ERR |	\
	MACH_SR_CACHE_MISS |	\
	MACH_SR_PARITY_ZERO |	\
	MACH_SR_SWAP_CACHES |	\
	MACH_SR_ISOL_CACHES |	\
	MACH_SR_KU_CUR |	\
	MACH_SR_INT_ENA_CUR |	\
	MACH_SR_MBZ)

/*
 * Macros to decode processor status word.
 */
#define	USERMODE(ps)	((ps) & MACH_SR_KU_PREV)
#define	BASEPRI(ps)	(((ps) & (MACH_INT_MASK | MACH_SR_INT_ENA_PREV)) \
			== (MACH_INT_MASK | MACH_SR_INT_ENA_PREV))
