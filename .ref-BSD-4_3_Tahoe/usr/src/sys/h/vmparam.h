/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmparam.h	7.2 (Berkeley) 5/14/88
 */

/*
 * Machine dependent constants
 */
#if defined(KERNEL) || defined(STANDALONE)
#include "../machine/vmparam.h"
#else
#include <machine/vmparam.h>
#endif

#if defined(KERNEL) && !defined(LOCORE)
int	klseql;
int	klsdist;
int	klin;
int	kltxt;
int	klout;
#endif
