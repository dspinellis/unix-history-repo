#include "../h/config.h"
/*
 * fail returns from a C function with failure.  The boundary points
 *  to the frame of the C function initially called so the stack
 *  is cleared back to there.  Then, that frame itself is popped
 *  (being sure to restore registers), and efail does the actual
 *  failure.
 */

Global(_efail)		/* Handle failure of expression */
Global(_boundary)	/* Icon/C boundary address */
Global(_fail)

#ifdef VAX
_fail:
	Mask	0		# Don't need to save anything because
				#  the frame is about to be thrown away.
  	movl	_boundary,fp	# Pop the stack back to the frame pointed
				#  at by the boundary.
	clrl	_boundary	# The net result of this routine is to
				#  get back into an Icon environment, so
				#  the boundary is cleared.
				#
				# The registers in the procedure frame now
				#  on the top of the stack need to be
				#  restored.  A mask is made in r0 and the
				#  appropriate registers are restored using
				#  a popr.
	ashl    $-16,4(fp),r0	# Shift mask field of psw stack word into
				#  the low order 16 bits of r0.
	bicl2   $0xf000,r0	# Only bits 0:11 are good, turn off the
				#  others.
	movab   20(fp),sp	# 20(fp) is the start of the saved registers,
				#  point the sp at the first one.
	popr	r0		# Pop the registers indicated by r0
	movl	8(fp),ap	# Get old ap
	movl	12(fp),fp	#  and fp from procedure frame.  This
				#  removes all traces of the C routine
				#  from the stack.
	jmp	_efail		# From here, it's just like expression
				#  failure, so let efail do it.

#endif VAX

#ifdef PORT
DummyFcn(_fail)
#endif PORT

#ifdef PDP11

/ fail - return from a (C) function with failure.
/ Exit to the current boundary, and branch to efail.

Global(cret)		/ return from C calling sequence
_fail:
	mov	_boundary,r5	/ pop all procedure frames to Icon/C boundary
	clr	_boundary
	mov	r5,r2		/ then exit that procedure frame
	mov	-(r2),r4
	mov	-(r2),r3
	mov	-(r2),r2
	mov	r5,sp
	mov	(sp)+,r5
	jmp	_efail
#endif PDP11
