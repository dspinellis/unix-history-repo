/*	rpb.s	6.1	83/08/01	*/

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
