/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stinger.h	7.2 (Berkeley) %G%
 */

/*
 * stinger.h -- Stinger Kernel Interface Definitions
 * by A.Fujita, MAR-14-1992
 *
 * This file should be remade.
 */

#include <machine/rdvar.h>
#include <machine/memory.h>

struct KernInter {
	caddr_t	  maxaddr;
	struct prgmem prg;
	int	  argc;
	char	**argv;
	struct rd_partition *rd;
};

