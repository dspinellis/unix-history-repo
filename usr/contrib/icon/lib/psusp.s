#include "../h/config.h"

/* psusp - suspends a value from an Icon procedure.  The procedure
 *  calling psusp is suspending and the value to suspend appears as
 *  an argument to psusp.  The generator or expression frame
 *  immediately containing the frame of the suspending procedure is
 *  duplicated.
 *
 * psusp returns through the duplicated procedure frame and leaves the
 *  value being suspended on the top of the stack.  When an alternative
 *  is needed, efail causes a return through the original procedure frame
 *  which was created by invoke.
 */
Global(_deref)		/* Dereference a variable */
Global(_strace)		/* Trace procedure suspension */
Global(_boundary)	/* Icon/C boundary address */
Global(_current)	/* Current expression stack */
Global(_line)		/* Current line number */
Global(_file)		/* Current file name */
Global(_k_level)	/* Value of &level */
Global(_k_trace)	/* Value of &trace */

Global(_psusp)
#ifdef VAX
_psusp:
/*
 * Construct the generator frame.
 */
	Mask	STDSV		# Start new generator frame by saving
				#  registers upon entry to psusp.
	movl 	fp,_boundary	# Establish boundary value to be saved
				#  in frame.  boundary is also needed 
				#  because deref may be called.
	pushl   fp		# Save the boundary in the frame.
/*
 * Dereference the return value if it is a local variable or an
 *  argument.
 */
 				# The return value is on the stack as
				#  an argument, put type field of return
	movl	8(ap),r1	#  value in r1 for testing.
	bitl	$F_NQUAL,r1	# If return value is a string,
	beql	cmpltfrm	#  it doesn't need dereferencing.
	bitl	$F_VAR,r1	# If return value isn't a variable,
	beql	cmpltfrm	#  it doesn't need dereferencing.
	bitl	$F_TVAR,r1	# If return value is a trapped variable,
	bneq	chktv		#  it requires some work.
	movl	12(ap),r1	# Otherwise, get the address of the
	jmp	chkloc		#  data block for more testing.
	
chktv:				# A trapped variable is being returned,
				#  only substring trapped variables need
				#  dereferencing.
	bicl2	$~TYPEMASK,r1	# "and" off all but bits in type field
	cmpl	$T_TVSUBS,r1	# If the variable isn't a substring t.v.,
	bneq	cmpltfrm	#  it doesn't need dereferencing.
	movl	12(ap),r1	# Point r1 at data block for s.s.t.v.
	movl	16(r1),r1	# Then at actual address of variable
chkloc:				#
				# See if the variable is on the stack.
				#  If it is, it will lie between the
				#  sp and the base of the current
				#  expression stack. r1 holds address
				#  of variable.
	cmpl	r1,sp   	# If address is below the sp,
	blssu	cmpltfrm	#  it's not a local or an argument
	movl	_current+4,r0	# Point r0 at data block for current
				#  expression.
	cmpl	r1,12(r0)	# Fourth word is the base of the stack
				#  for the current expression.  If the
				#  variable address is above the stack
	bgtru	cmpltfrm	#  base, it's not a local or an argument.
				# Otherwise, it is a local or an argument
				#  and must be dereferenced.
	pushal	8(ap)		# Push address of return value
	calls	$1,_deref	#  and dereference it.
/*
 * Complete the generator frame.
 */
cmpltfrm:
  	movl	sp,gfp		# Boundary value is on top of stack,
				#  make it word 0 of generator frame
        pushl	_k_level	# Push &level,
  	pushl	_line		#  line number,
	pushl	_file		#  and file name to complete the frame.
/*
 * Determine region to be duplicated and copy it.
 */
 				# Note that because the call to psusp
				#  made a frame, the saved ap and fp
				#  values in that frame must be used.
	movl	12(fp),r7	# Low word of region to be copied is the
				#  low word of procedure frame of suspending
				#  procedure.
				
				# If the saved gfp is non-zero, the
				#  generator frame marker serves as the
				#  upper bound of the expression frame.
				# If it is zero, the expression frame
				#  marker pointed at by the saved
				#  efp is the upper bound of the frame
				#  to be copied.
				# Note that the marker itself is not
				#  copied, the region only extends to
				#  the marker and not through it.
				# This code counts on efp and gfp being
				#  saved in the frame of the suspender.
	movl	8(fp),r2	# Get ap of suspending procedure in r2
	movl	-8(r2),r4	# Get gfp from procedure frame of suspending
				#  procedure.
	bneq	f1		# If it is zero,
	movl	-4(r2),r4	#  get saved efp and
	subl2	$8,r4		#  use efp - 8.
	jmp	f2
f1:				# gfp is not zero,
	subl2	$12,r4		#  use gfp - 12.
/*
 * Copy region to be duplicated to top of stack.
 */
 				# r7 points at the low word of the region
				#  to be copied.  r4 points at the high end
				#  of the region.  (i.e. r4 is the first
				#  word not_ to copy.)
f2:
	subl2	r7,r4		# r4 = r4 - r7, giving r4 number of bytes
				#  in region.
	subl2	r4,sp		# Move stack pointer down to make space
				#  for region.
	movc3	r4,(r7),(sp)	# Copy the region by moving r4 bytes starting
				#  at r7 to the top of the stack.
/*
 * Produce trace message if tracing is on.
 */
	decl	_k_level	# Decrement &level because a procedure
				#  is being "exited".
	tstl	_k_trace	# If &trace is 0,
	jeql	tracedone	#  no tracing.
				# Otherwise, call strace with address
				#  of suspending procedure block and
				#  value being suspended.
	pushal	8(ap)		# Push pointer to value being suspended.
				# arg0 in the suspender's argument list
				#  is the descriptor for the suspending
				#  procedure.
	movl	8(fp),r1	# Get suspender's ap into r1.
	ashl	$3,4(r1),r0	# &arg0 = nargs * 8
	addl2	$8,r0		#  + 8
	addl2	r1,r0		#  + ap
	pushl	4(r0)		# Push second word (the address) of
				#  the descriptor for the procedure block
	calls	$2,_strace	# strace(&procblock,&suspending-value)
/*
 * Return from suspending function; resumption will return from suspend.
 */
tracedone:
	movl	12(fp),r1	# Get fp of suspending procedure into r1 and
	movl	-4(r1),_line	#  restore _line and
	movl	-8(r1),_file	#  _file from the frame.
				# The duplicated frame must be fixed up.
				#  Specifically, the saved gfp is replaced
				#  by the new gfp, and the value being
				#  suspended replaces arg0, the descriptor
				#  of the suspending procedure.
	subl3	r1,8(fp),r0	# Calculate distance between fp and ap
				#  in suspender's frame, specifically,
				#  r0 = ap - fp
	addl2	sp,r0		# sp points at the first word of the
				#  duplicated procedure frame on the
				#  stack.  By adding it to r0, r0 points
				#  at nwords word in argument list of
				#  duplicated frame.  That is, r0 is
				#  serving as a pseudo ap.
	subl3	$8,r0,r1	# Point r1 at location of saved gfp
				#  in duplicated frame.
	movl	gfp,(r1)	# Replace saved gfp with new gfp value
				# Calculate address of arg0 via
				#  &arg0 =
	ashl	$2,(r0),r1	#   nwords * 4
	addl2	$4,r1		#   + 4 (bytes for nwords word)
	addl2	r1,r0		#   + (pseudo) ap
	movq	8(ap),(r0)	# Replace arg0 with suspending value
				#
	movl	sp,fp		# Point fp at duplicated procedure frame
				#  in preparation for return through it.
	clrl	_boundary	# Clear the boundary since control is
				#  going back into Icon code.
	ret         		# Return through duplicated frame.  This
				#  looks like the original invoke for the
				#  suspending procedure has returned.  The
				#  suspended value is left on the top
				#  of the stack.

#endif VAX

#ifdef PORT
DummyFcn(_psusp)
#endif PORT
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
