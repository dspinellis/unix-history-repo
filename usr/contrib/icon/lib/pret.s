#include "../h/config.h"
/*
 * pret - returns a value from an Icon procedure.  pret takes
 *  a single argument which is the value to return.  The real
 *  work is in figuring out whether the return value needs to
 *  be dereferenced.
 */

Global(_deref)		/* Dereference a variable */
Global(_rtrace)		/* Return trace routine */
Global(_boundary)	/* Icon/C boundary address */
Global(_current)	/* Current expression stack */
Global(_file)		/* Current file name */
Global(_k_level)	/* Value of &level */
Global(_k_trace)	/* Value of &trace */
Global(_line)		/* Current line number */

Global(_pret)
#ifdef VAX
_pret:
	Mask	0		# Don't need to save any registers because
				#  the current frame will be discarded.
	movl	fp,_boundary	# The boundary is set because deref may
				#  cause garbage collection.
	decl	_k_level	# A procedure is being exited, so &level
				#  must be decremented.
/*
 * Calculate target address for return value in r11.
 */
				# The frame of the caller is the procedure
				#  frame for the Icon procedure returning
	movl    8(fp),r2      	#  a value.  Put it's ap in r2.
				# The return value will overwrite arg0,
				#  the address of arg0 is calculated via:
	ashl	$3,4(r2),r11	# r11 = 8 * nargs
	addl2	$8,r11		#  + 8
	addl2	r2,r11		#  + ap
				# Note that nargs and ap belong to the
				#  returning Icon procedure.
/*
 * Dereference the return value if it is a local variable or an
 *  argument.
 */
 				# The return value is on the stack as
				#  an argument, put type field of return
	movl	8(ap),r1	#  value in r1 for testing.
	bitl	$F_NQUAL,r1	# If return value is a string,
	beql	chktrace	#  it doesn't need dereferencing.
	bitl	$F_VAR,r1	# If return value isn't a variable,
	beql	chktrace	#  it doesn't need dereferencing.
	bitl	$F_TVAR,r1	# If return value is a trapped variable,
	bneq	chktv		#  it requires some work.
	movl	12(ap),r1	# Otherwise, get the address of the
	jmp	chkloc		#  data block for more testing.
	
chktv:				# A trapped variable is being returned,
				#  only substring trapped variables need
				#  dereferencing.
	bicl2	$~TYPEMASK,r1	# "and" off all but bits in type field
	cmpl	$T_TVSUBS,r1	# If the variable isn't a substring t.v.,
	bneq	chktrace	#  it doesn't need dereferencing.
	movl	12(ap),r1	# Point r1 at data block for s.s.t.v.
	movl	16(r1),r1	# Then at actual address of variable
chkloc:				#
				# See if the variable is on the stack.
				#  If it is, it will lie between the
				#  sp and the base of the current
				#  expression stack. r1 holds address
				#  of variable.
	cmpl	r1,sp   	# If address is below the sp,
	blssu	chktrace	#  it's not a local or an argument
	movl	_current+4,r0	# Point r0 at data block for current
				#  expression.
	cmpl	r1,12(r0)	# Fourth word is the base of the stack
				#  for the current expression.  If the
				#  variable address is above the stack
	bgtru	chktrace	#  base, it's not a local or an argument.
				# Otherwise, it is a local or an argument
				#  and must be dereferenced.
	pushal	8(ap)		# Push address of return value
	calls	$1,_deref	#  and dereference it.

/*
 * Print trace message if &trace is set.
 */
chktrace:
	tstl	_k_trace	# If &trace is zero,
	beql	tracedone	#  no tracing.
				# Otherwise, set up to call rtrace
				#  with address of proc block and
				#  return value.
	pushal	8(ap)		# Push address of return value
	pushl	4(r11)		# Push address of procedure block
	calls   $2,_rtrace	# rtrace(proc. block address,&return value)
	
tracedone:			# The descriptor for the procedure block
				#  (arg0) must be replaced by the descriptor
				#  of the return value.  r11 points at the
	movq	8(ap),(r11) 	#  procedure block, so a movq does the trick.
/*
 * Return from the Icon procedure.  What this really does is to return
 *  via the frame built by invoke.  Thus, the return below returns from
 *  the call to invoke.
 */
 				
	movl	12(fp),fp	# Get frame built by invoke on top of stack
	movl	-4(fp),_line	# Restore _line,
	movl	-8(fp),_file	#  and _file from procedure block.
	clrl	_boundary	# Reentering an Icon environment, so
				#  the boundary is cleared.
	ret			# Return.  This is manifested as a
				#  return from invoke.
#endif VAX

#ifdef PORT
DummyFcn(_pret)
#endif PORT
#ifdef PDP11
/ pret - return from an Icon procedure.
/ Return value is argument to pret at 6(r5).

/ Register usage:
/   r1: type or pointer field of returned value
/   r2: returning procedure frame pointer
/   r3: address of argument #0 (place-holder for returned value)
/   r5: current procedure frame pointer
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
