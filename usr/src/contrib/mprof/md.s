/*
 * mprof
 * md.s 1.2 8/17/90 16:22:14	
 * Copyright (c) 1988, Benjamin G. Zorn
 *
 * Machine dependent part.
 */

#ifdef sun4
#include <sun4/trap.h>

/*
 * This is severly misnamed, since we're really getting the sp, not
 * the fp. But we don't want to contort the logic of the rest of the
 * program, so here we are.
 */
.global	_get_current_fp
_get_current_fp:
	ta	ST_FLUSH_WINDOWS	/* we're going to walk the stack */
	retl
  	mov	%sp,%o0

#endif sun4

#ifdef mips
 #   3	extern int intloc;
 #   4	
	.extern	intloc 4
	.text
	.align	2
	.file	2 "md.s"
 #   1	/* place register 31 in location intloc */
 #   2	
	.globl	get31
	.ent get31
get31:
	.frame	$sp, 0, $31
	sw	$31, intloc
	j	$31
	.end	get31

	.text
	.align	2
 #   1	/* get the sp of the caller */
 #   2	
	.globl	getsp
	.ent getsp
getsp:
	.frame	$sp, 0, $31
	sw	$sp, intloc
	j	$31
	.end	getsp
#endif
