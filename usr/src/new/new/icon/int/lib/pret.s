#include "../h/config.h"

#ifdef VAX
/*
 * pret - return from an Icon procedure.
 * Return value is argument to pret at 6(r5).
 */

.globl	_deref			# dereference a variable
.globl	_rtrace			# return trace routine

.globl	_boundary		# Icon/C boundary address
.globl	_current		# current expression stack
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_k_trace		# value of &trace
.globl	_line			# current line number

.globl	_pret

_pret:
	.word 0xffe		# create new procedure frame

	movl	fp,_boundary	# set Icon/C boundary

   /* Decrement &level and calculate address of eventual return value. */

	decl	_k_level
	movl    8(fp),r2      	# r2 <- ap of returning procedure
	ashl    $3,4(r2),r11	# compute position of return value:
	addl2   $8,r11		#    r11 <- r2 + 8 + 8*nargs
	addl2   r2,r11

   /* Dereference return value if necessary. */

	movl	8(ap),r1	# r1 <- type field of return value
	bitl	$F_NQUAL,r1	# if return value is the
	beql	1f		#   name of a local variable
	bitl	$F_VAR,r1	#   or argument, then it
	beql	1f		#   needs dereferencing
	bitl	$F_TVAR,r1
	bneq	2f
	movl	12(ap),r1	# r1 <- pointer field of return value
	jmp	3f
2:
	bicl2	$0!TYPEMASK,r1	# check type code for substring t.v.
	cmpl	$T_TVSUBS,r1	#   if not, it doesn't need
	bneq	1f		#   dereferencing
	movl	12(ap),r1	# r1 <- pointer field from b_tvsubs
	movl	16(r1),r1
3:
	cmpl	r1,sp   	# if pointer is between
	blssu	1f		#   sp and sbase, it is a local
	movl	_current+4,r0	#   or an argument
	cmpl	r1,12(r0)
	bgtru	1f
	addl3	$8,ap,-(sp)	# push address of return value
	calls	$1,_deref	# dereference return value

   /* Print trace message if &trace is set. */

1:
	tstl	_k_trace
	beql	1f
	
	pushal	8(ap)		# push pointer to return value
	pushl	4(r11)		# push pointer to procedure block
	calls   $2,_rtrace	# rtrace(proc_addr,&return_value)

   /* Copy return value to the outer expression frame. */

1:
	movq	8(ap),(r11) 	# move return value down from top of stack

   /* Return. */

	movl	12(fp),fp
	movl	-4(fp),_line	# restore old values of registers, line and file
	movl	-8(fp),_file
	clrl	_boundary	# clear Icon/C boundary
	ret
#endif VAX
#ifdef PDP11
/ pret - return from an Icon procedure.
/ Return value is argument to pret at 6(r5).

/ Register usage:
/   r1: type or pointer field of returned value
/   r2: returning procedure frame pointer
/   r3: address of argument #0 (place-holder for returned value)
/   r5: current procedure frame pointer

.globl	_deref			/ dereference a variable
.globl	_rtrace			/ return trace routine

.globl	_boundary		/ Icon/C boundary address
.globl	_current		/ current expression stack
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_k_trace		/ value of &trace
.globl	_line			/ current line number

.globl	_pret
_pret:
	mov	r5,-(sp)	/ create new procedure frame
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r5,_boundary	/ set Icon/C boundary

/ Decrement &level and calculate address of eventual return value.

	dec	_k_level
	mov	(r5),r2		/ compute address for
	mov	4(r2),r3	/   return value:
	asl	r3		/   r3 = r2 + 6 + 4*nargs
	asl	r3
	add	r2,r3
	add	$6,r3

/ Dereference return value if necessary.

	mov	6(r5),r1	/ get type field of return value into r1
	bit	$F_NQUAL,r1	/ if return value is the
	beq	1f		/   name of a local variable
	bit	$F_VAR,r1	/   or argument, then it
	beq	1f		/   needs dereferencing
	bit	$F_TVAR,r1
	bne	2f
	mov	8.(r5),r1	/ get pointer field into r1
	br	3f
2:
	bic	$!TYPEMASK,r1	/ check type code for substring t.v.
	cmp	$T_TVSUBS,r1	/   if not, it doesn't need
	bne	1f		/   dereferencing
	mov	8.(r5),r1	/ get pointer field from b_tvsubs
	mov	8.(r1),r1	/   block into r1
3:
	cmp	r1,sp   	/ if pointer is between
	blo	1f		/   sp and sbase, it is a local
	mov	_current+2,r0	/   or an argument
	cmp	r1,6(r0)
	bhi	1f
	mov	r5,-(sp)	/ dereference it
	add	$6,(sp)
	jsr	pc,_deref
	tst	(sp)+

/ Print trace message if &trace is set.

1:
	tst	_k_trace
	beq	1f
	mov	r5,-(sp)	/   push address of return value
	add	$6,(sp)
	mov	2(r3),-(sp)	/   push pointer to procedure block
	jsr	pc,_rtrace	/   call rtrace; other arguments are in frame
	cmp	(sp)+,(sp)+

/ Copy return value to the outer expression frame.

1:
	mov	r3,r1		/ save r3 to pop stack to this point later
	mov	6(r5),(r3)+	/ move return value down from top of stack
	mov	8.(r5),(r3)

/ Return.

	mov	r2,r5		/ restore old values of registers
	mov	r2,r0
	mov	-(r0),r4	
	mov	-(r0),r3
	mov	-(r0),r2
	mov	-(r0),_line
	mov	-(r0),_file
	mov	r5,sp
	mov	(sp)+,r5
	mov	(sp)+,r0    	/ pop return pc
	mov	r1,sp		/ pop stack to return value
	clr	_boundary	/ clear Icon/C boundary
	jmp	(r0) 		/ return
#endif PDP11
