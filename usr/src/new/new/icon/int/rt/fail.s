#include "../h/config.h"
#ifdef VAX

/* fail - return from a (C) function with failure.
 * Exit to the current boundary, and branch to efail.
 */

.globl	_efail			# fail from an expression frame

.globl	_boundary		# Icon/C boundary address

.globl	_fail

_fail:
	.word	0xffe		# save registers
  	movl	_boundary,fp	# pop all procedure frames to Icon/C boundary
        clrl	_boundary
				# exit that procedure frame
	ashl    $-16,4(fp),r0	# r0 <- mask for saved registers
	bicl2   $0xf000,r0
	movab   20(fp),sp	# set stack pointer to top of saved registers
	popr	r0		# pop all saved registers
	movl	8(fp),ap	# set ap
	movl	12(fp),fp	# and fp

	jmp	_efail		# let efail do the work.

#endif VAX

#ifdef PDP11

/ fail - return from a (C) function with failure.
/ Exit to the current boundary, and branch to efail.

.globl	cret			/ return from C calling sequence
.globl	_efail			/ fail from an expression frame

.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_line			/ current line number

.globl	_fail
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
