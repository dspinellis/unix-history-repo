#include "../h/config.h"

#ifdef VAX
/* psusp - suspend from an Icon procedure.
 * Duplicates the most recent generator frame outside the
 * calling procedure frame.  The procedure calling psusp is
 * suspending, and the saved value of r10 in its frame marker
 * points to the beginning of the generator frame to be
 * duplicated.  Psusp returns through the copied procedure frame on
 * top of the stack.  When an alternative is needed; the return
 * actually comes from efail.
 */

.globl	_deref			# dereference a variable
.globl	_strace			# suspend trace routine

.globl	_boundary		# Icon/C boundary address
.globl	_current		# current expression stack
.globl	_line			# current line number
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_k_trace		# value of &trace

.globl  _psusp

_psusp:
	.word	0xffe		# create new generator frame
	movl 	fp,_boundary	# create Icon/C boundary
	pushl   fp

   /* Dereference return value if necessary. */

    	movl	8(ap),r1	# r1 <- type field of return value
	bitl	$F_NQUAL,r1	# if return value is the
	jeql	1f		#   name of a local variable
	bitl	$F_VAR,r1	#   or argument, then it
	jeql	1f		#   needs dereferencing
	bitl	$F_TVAR,r1
	bneq	2f
	movl	12(ap),r1	# r1 <- pointer field of return value
	jmp	3f
2:
	bicl2	$0!TYPEMASK,r1	# check type code for substring t.v.
	cmpl	$T_TVSUBS,r1	#   if not, it doesn't need
	bneq   	1f		#   dereferencing
	movl    12(ap),r1    	# r1 <- pointer field from b_tvsubs block
	movl	16(r1),r1
3:
	cmpl	r1,sp   	# if pointer is between
	blssu	1f		#   sp and sbase, it is a local
	movl	_current+4,r0	#   or an argument
	cmpl	r1,12(r0)
	bgtru	1f
	addl3	$8,ap,-(sp)	# push pointer to return value
	calls   $1,_deref	# dereference the return value

  /* Calculate addresses of new generator frame. */

1:  	movl	sp,r10		# r10 <- pointer to new generator frame
        pushl	_k_level        # save &level, line number and file name
  	pushl	_line
	pushl	_file

  	movl    8(fp),r2       	# r2 <- ap of calling procedure frame
	movl	12(fp),r7	# r7 <- top of region to be copied
       	movl    -8(r2),r4       # r4 <- generator frame pointer from caller
	bneq	1f		#   use saved r10 (gfp) - 12 if non-zero,
	movl	-4(r2),r4	#   else use saved r11 (efp) - 8
	subl2   $8,r4
	jmp	2f
1:
	subl2	$12,r4

   /* Copy surrounding expression frame. */

2:
	subl2   r7,r4		# r4 <- # of bytes to copy
	subl2   r4,sp		# adjust stack pointer
	movc3	r4,(r7),(sp)	# copy generator frame

   /* Copy return value of suspending procedure. */

   ##	movq	8(ap),-(sp)

   /* Decrement &level; print trace message if &trace is set. */

	decl	_k_level
	tstl	_k_trace	# print trace if &trace != 0
	jeql	1f

	pushal	8(ap)		# push pointer to suspending value
	movl	8(fp),r1	# r1 is suspender's ap
	ashl	$3,4(r1),r0	# point at address of proc. block (arg0)
	addl2	$8,r0		#  = ap + 8 + (8*nargs)
	addl2	r1,r0
	pushl	4(r0)
	calls   $2,_strace	#   call strace

   /* Return from suspending function; resumption will return from suspend. */

1:	movl    12(fp),r1	# r1 <- fp of suspending procedure
	movl	-4(r1),_line    # restore &line, &file
	movl    -8(r1),_file
	subl3   r1,8(fp),r0     # r0 <- abs(fp - ap) of suspending procedure
	addl2	sp,r0		# r0 <- ap of new procedure frame on stack
	subl3   $8,r0,r1	# r1 <- address of saved gfp
	movl    r10,(r1)	# store new gfp
	ashl    $2,(r0),r1	# r1 <- number of bytes to arg0-1
	addl2   $4,r1		# r1 <- number of bytes to arg0
	addl2	r1,r0		# r0 <- address of arg0
	movq    8(ap),(r0)	# copy return value

	movl    sp,fp		# reset frame pointer
	clrl	_boundary	# returning to Icon code
	ret         		# this really suspends

#endif VAX
#ifdef PDP11
/ psusp - suspend from an Icon procedure.
/ Duplicates the most recent generator frame outside the
/ calling procedure frame.  The procedure calling psusp is
/ suspending, and the saved value of r3 in its frame marker
/ points to the beginning of the generator frame to be
/ duplicated.  Psusp does not return directly.  The caller
/ is reactivated when an alternative is needed; the return
/ actually comes from efail.

/ Register usage:
/   r0:    pointer to top of stack region to be copied,
/	     which is just above the procedure descriptor (arg0) of the
/	     suspending procedure
/   r2:    suspending procedure frame pointer
/   r3:    new generator frame pointer
/   r4:	   old generator frame pointer, indexed down to r0 during copy
/   r5:    current procedure frame pointer

.globl	_deref			/ dereference a variable
.globl	_strace			/ suspend trace routine

.globl	_boundary		/ Icon/C boundary address
.globl	_current		/ current expression stack
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_k_trace		/ value of &trace
.globl	_line			/ current line number

.globl  _psusp
_psusp:
	mov	r5,-(sp)	/ create new procedure frame
	mov	sp,r5
	mov	r4,-(sp)    	/ save registers
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r5,-(sp)	/ create Icon/C boundary
	mov	r5,_boundary

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
1:

/ Calculate addresses of new generator frame.

	mov	sp,r3		/ r3 <- pointer to new generator frame
	mov	_k_level,-(sp)	/ save &level
	mov	_line,-(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	(r5),r2		/ r2 <- pointer to calling procedure frame
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
	mov	-(r4),-(sp)	/ copy old generator frame
2:
	cmp	r4,r0		/ stop at end of frame
	bhi	1b

/ Copy return value of suspending procedure.

	mov	8.(r5),-(sp)
	mov	6(r5),-(sp)

/ Decrement &level; print trace message if &trace is set.

	dec	_k_level
	tst	_k_trace	/ print trace if &trace != 0
	beq	1f
	mov	r5,-(sp)	/   push address of suspending value
	add	$6,(sp)
	mov	-(r0),-(sp)	/   push address of procedure block
	jsr	pc,_strace	/   call strace
	cmp	(sp)+,(sp)+

/ Return from suspending procedure; reactivation will return from psusp.

1:
	mov	r2,r0
	mov	2(r0),r1	/ r1 <- return pc
	mov	(r0),r5		/ restore old registers
	mov	-(r0),r4
	tst	-(r0)		/   except generator frame pointer
	mov	-(r0),r2
	mov	-(r0),_line
	mov	-(r0),_file
	clr	_boundary	/ returning to Icon code
	jmp	(r1)		/ this really suspends
#endif PDP11
