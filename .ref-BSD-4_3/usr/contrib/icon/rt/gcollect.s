#include "../h/config.h"
/*
 * gcollect(n) - switch to expression stack for &main and call collect(n),
 *  switch back to current expression stack when done.
 */
Global(_collect)		/* Garbage collection */
Global(_boundary)		/* Icon/C boundary */
Global(_current)		/* Current co-expression */
Global(_k_main)			/* Main co-expression */

Global(_gcollect)
#ifdef VAX
_gcollect:
	Mask	0x0			# Don't need to save any registers

	movl	_current+4,r0		# Get pointer to heap block for
					#  current co-expression.
	movl	sp,16(r0)	 	# Save sp,
	movl	ap,20(r0)		#  ap,
	movl	_boundary,24(r0)	#  and current boundary in block.
	movl	_k_main+4,r0		# Get pointer to heap block for
					#  &main.
	movl	16(r0),sp		# Restore sp,
	movl	20(r0),ap		#  ap,
	movl	24(r0),_boundary 	#  and boundary for &main
  	pushl	4(ap)			# Push n on stack
	calls	$1,_collect	       	# collect(n)
	movl	_current+4,r0		# Get pointer to heap block for
					#  current co-expression
	movl	16(r0),sp		# Restore sp,
	movl	20(r0),ap		#  ap,
	movl	24(r0),_boundary   	#  and boundary
	ret				# Return from garbage collection
#endif VAX

#ifdef PORT
DummyFcn(_gcollect)
#endif PORT

#ifdef PDP11
/ gcollect(n) - switch to expression stack for &main and call collect(n),
/		switch to current expression stack when done.

/ Register Usage:
/	r0-r1		general utility
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
