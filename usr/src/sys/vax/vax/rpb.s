/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rpb.s	6.2 (Berkeley) %G%
 */

/*
 * This has to get loaded first (physical 0) as 780 memory restart rom
 * only looks for rpb on a 64K page boundary (doc isn't wrong,
 * it never says what size "page boundary" rpb has to be on).
 */
	.globl	_rpb
_rpb:
	.space	508
erpb:
	.space	4
