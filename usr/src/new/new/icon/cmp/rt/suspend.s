#include "../h/config.h"
#ifdef VAX
/* suspend - Suspend from a (C) function.
 * Duplicates the most recent generator frame outside the
 * current boundary.  Suspend for the VAX returns through a
 * copied procedure frame on top of the stack.
 * NOTE: the VAX version of suspend must be called by the procedure
 * that is suspending a value to Icon, and NOT from a procedure called
 * by that procedure.
 */

.globl	_boundary		# Icon/C boundary address
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_line			# current line number

.globl  _suspend

_suspend:
	.word   r(2)|r(3)|r(4)|r(5)|r(6)|r(7)|r(8)|r(9)|r(10)|r(11)
	movl	_boundary,r6	# r6 <- pointer to suspending procedure frame
	pushl	r6      	# save Icon/C boundary address

   /* Calculate addresses of new generator frame. */

	movl	sp,r10		# r10 <- pointer to new generator frame
	pushl	_k_level      	# save &level
	pushl	_line      	# save current line number
	pushl	_file      	#   and file name
	movl	12(fp),r7	# r7 <- top of region to be copied
  	movl    8(fp),r2       	# r2 <- ap of calling procedure frame
	movq    -8(r2),r4	# r4 <- gfp, r5 <- efp
	tstl	r4		# r4 <- generator frame pointer from caller
	beql	1f		#   use saved r10 (gfp) - 12 if non-zero,
	subl2	$12,r4		#   else use saved r11 (efp) - 8
	jbr	2f
1:	subl3	$8,r5,r4

   /* Copy surrounding expression frame. */

2:
	subl2   r7,r4		# r4 <- # of bytes to copy
	subl2   r4,sp		# adjust stack pointer
	movc3	r4,(r7),(sp)	# copy generator frame

   /* Return from suspending function; resumption will return from suspend. */

    	subl3   12(fp),8(fp),r0 # r0 <- abs(fp - ap) of suspending procedure
    	addl2	sp,r0
    	subl2   $8,r0		# r0 <- addr. of saved gfp (assumes r11 saved)
    	movl    r10,(r0)	# store new gfp

	movl    sp,fp		# reset frame pointer
	clrl	_boundary	# returning to Icon code
	ret         		# this really suspends

#endif VAX

#ifdef PDP11
/ suspend - Suspend from a (C) function.
/ Duplicates the most recent generator frame outside the
/ current boundary.  Suspend does not return directly.
/ The caller is reactivated when an alternative is needed;
/ the return actually comes from efail.

/ Register usage:
/   r0:    pointer to top of stack region to be copied,
/	     which is just above the procedure descriptor (arg0) of the
/	     suspending procedure
/   r2:    suspending procedure frame pointer
/   r3:    new generator frame pointer
/   r4:	   old generator frame pointer, indexed down to r0 during copy
/   r5:    current procedure frame pointer

.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_line			/ current line number

.globl  _suspend
_suspend:
	mov	r5,-(sp)	/ create new procedure frame
	mov	sp,r5
	mov	r4,-(sp)    	/ save registers
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	_boundary,r2	/ r2 <- pointer to suspending procedure frame
	mov	r2,-(sp)	/ save Icon/C boundary address

/ Calculate addresses of new generator frame.

	mov	sp,r3		/ r3 <- pointer to new generator frame
	mov	_k_level,-(sp)	/ save &level
	mov	_line,-(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	4(r2),r0	/ r0 <- pointer to top of region to be copied
	asl	r0		/	(= r2 + 10 + 4*nargs)
	asl	r0
	add	r2,r0
	add	$10.,r0
	mov	-4(r2),r4	/ r4 <- generator frame pointer from caller
	bne	1f		/   use saved r3 (gfp) - 6 if non-zero,
	mov	-2(r2),r4	/   else use saved r4 (efp) - 4
	cmp	-(r4),-(r4)
	br	2f
1:
	sub	$6,r4
	br	2f

/ Copy surrounding expression frame.

1:
	mov	-(r4),-(sp)
2:
	cmp	r4,r0		/ stop at end of frame
	bhi	1b

/ Copy return value of suspending function.

	mov	-(r4),-(sp)
	mov	-(r4),-(sp)

/ Return from suspending function; reactivation will return from suspend.

	mov	2(r2),r1	/ r1 <- return pc
	mov	(r2),r5		/ restore old registers
	mov	-(r2),r4
	tst	-(r2)		/   except generator frame pointer
	mov	-(r2),r2
	clr	_boundary	/ returning to Icon code
	jmp	(r1)		/ this really suspends
#endif PDP11
