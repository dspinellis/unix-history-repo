#include "../h/config.h"
#ifdef VAX
/*
 * gcollect(n) - switch to expression stack for &main and call collect(n),
 *  switch to current expression stack when done.
 */
.globl	_collect			# garbage collection
.globl	_boundary			# Icon/C boundary
.globl	_current			# current co-expression
.globl	_k_main 			# main co-expression

.globl	_gcollect
_gcollect:
	.word 0x0			# save no registers

	movl	_current+4,r0		# r0 <- pointer to current stack header
	movl	sp,16(r0)	 	# save sp,
	movl	ap,20(r0)		#  ap,
	movl	_boundary,24(r0)	#  and current boundary
	movl	sp,r1			# r1 <- saved stack pointer
	movl	_k_main+4,r0		# r0 <- pointer to main stack header
	movl	16(r0),sp		# get sp,
	movl	20(r0),ap		#  ap,
	movl	24(r0),_boundary 	#  and boundary for &main
  	pushl	4(ap)			# 
	calls	$1,_collect	       	# call collect(n)
	movl	_current+4,r0		# r0 <- ptr to current stack header
	movl	16(r0),sp		# restore sp,
	movl	20(r0),ap		#  ap,
	movl	24(r0),_boundary   	#  and boundary
	ret
#endif VAX

#ifdef PDP11
/ gcollect(n) - switch to expression stack for &main and call collect(n),
/		switch to current expression stack when done.

/ Register Usage:
/	r0-r1		general utility

.globl	_collect			/ garbage collection
.globl	_boundary			/ Icon/C boundary
.globl	_current			/ current co-expression
.globl	_k_main 			/ main co-expression

.globl	_gcollect
_gcollect:
	mov	_current+2,r0		/ r0 <- pointer to current stack header
	mov	sp,8.(r0)		 / save current stack pointer
	mov	_boundary,12.(r0)	 / save current boundary
	mov	sp,r1			/ r1 <- saved stack pointer
	mov	_k_main+2,r0		/ r0 <- pointer to main stack header
	mov	8.(r0),sp		 / get stack pointer for &main
	mov	12.(r0),_boundary	 / get boundary for &main
	mov	2(r1),-(sp)		/ move n to this stack
	jsr	pc,_collect		/ call collect(n)
	tst	(sp)+			/ pop n off of this stack
	mov	_current+2,r0		/ r0 <- pointer to current stack header
	mov	8.(r0),sp		 / restore current stack pointer
	mov	12.(r0),_boundary	 / restore current boundary
	rts	pc			/ return
#endif PDP11
