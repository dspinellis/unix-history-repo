/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ktrace.h	5.2 (Berkeley) %G%
 */

#define DEF_POINTS (KTRFAC_SYSCALL | KTRFAC_SYSRET | KTRFAC_NAMEI | \
		  KTRFAC_GENIO | KTRFAC_PSIG)

#define ALL_POINTS (DEF_POINTS | KTRFAC_CSW)

#define DEF_TRACEFILE	"ktrace.out"
