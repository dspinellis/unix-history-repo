/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ktrace.h	5.1 (Berkeley) %G%
 */

#define ALL_POINTS (KTRFAC_SYSCALL | KTRFAC_SYSRET | KTRFAC_NAMEI | \
		  KTRFAC_GENIO | KTRFAC_PSIG)

#define DEF_TRACEFILE	"ktrace.out"
