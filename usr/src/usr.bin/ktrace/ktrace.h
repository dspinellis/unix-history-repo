/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ktrace.h	1.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/ktrace.h>
#include <stdio.h>

#define ALL_POINTS (KTRFAC_SYSCALL | KTRFAC_SYSRET | KTRFAC_NAMEI | \
		  KTRFAC_GENIO | KTRFAC_PSIG)

#define DEF_TRACEFILE	"ktrace.out"
