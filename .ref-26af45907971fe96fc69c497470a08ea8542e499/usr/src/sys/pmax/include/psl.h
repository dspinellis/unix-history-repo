/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)psl.h	7.2 (Berkeley) %G%
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
