#include "../h/config.h"
/*
 * invoke is used to invoke something.  Among the candidates are:
 *  Call a built-in function
 *  Call an Icon procedure
 *  Create a record
 *  Perform mutual evaluation
 *
 * Note that all calls rise from a source code construct like
 *  expr0(expr1,...,exprn)
 */
Global(_interp)		/* interpreter loop */
Global(_cvstr)		/* convert to string */
#ifdef XPX
Global(_strprc)		/* convert string to procedure block address */
#endif XPX
Global(_ctrace)		/* call trace routine */
Global(_cvint)		/* convert to integer */
Global(_cvpos)		/* convert to position */
Global(_deref)		/* dereference a variable */
Global(_fail)		/* failure processing */
Global(_runerr)		/* issue a runtime error */

Global(_boundary)	/* Icon/C boundary address */
Global(_line)		/* current line number */
Global(_file)		/* current file name */
Global(_k_level)	/* value of &level */
Global(_k_trace)	/* value of &trace */

Global(_invoke)

#ifdef VAX
 .text
_invoke:
	Mask	0x0e02		# Save r1, r9, r10, and r11.  The return pc
				#  is stashed where r1 is saved.
#define INVREGS  4		/* number of registers saved */

	movl	fp,_boundary	# Set Icon/C boundary
	movl	4(ap),r8	# r8 holds number of arguments
	movaq	8(ap)[r8],r11	# r11 points to expr0
   	pushl	r11		# Push address of expr0 for deref
	calls	$1,_deref	# deref(&expr0)
	movl	(r11),r0	# r11 now points to a descriptor for
				#  expr0.  The type word of the descriptor
				#  is put in r0 for examination
	cmpl	$D_PROC,r0	# See if expr0 is a procedure
	jeql	doinvk		#  if procedure, branch
/*
 * See if mutual evaluation is to be performed.
 */
				# If not a procedure, maybe an integer
	pushl	$longint	# Set up for cvint, longint is buffer to
	pushl	r11		#  receive result
	calls	$2,_cvint	# cvint(&expr0,&longint)
	cmpl	$T_INTEGER,r0	# Type comes back in r0, if not integer,
	jneq	trystr		#  branch.  Otherwise, longint holds
				#  integer value of expr0.

	pushl	4(ap)		# Got an integer,
	movl	longint,-(sp)	#  convert it to a canonical position
	calls	$2,_cvpos	# cvpos(longint), position
				#  comes back in r0
	cmpl	r0,4(ap)	# See if position is less than or equal
				#  to the number of arguments.
	bleq	f1		#  if so, branch
	calls	$0,_fail	#  otherwise, fail
/*
 * Do mutual evaluation by returning expr[expr0]
 */
f1:	ashl	$3,r0,r0	# Each expri is 8 bytes, so r0 is turned
				#  into a byte offset by multiplying it by 3.
	subl3	r0,r11,r1	# Point r1 at desired expri
	movq	(r1),(r11)	# r11 points at expr0, which is to replaced
				#  by result of mutual evaluation (the result of invoke),
				#  so move result of descriptor into expr0's
				#  place.

	clrl	_boundary	# mutual evaluation is done, clear the boundary and return
	ret

trystr:
#ifdef XPX
/*
 * If expr0 is a string and the name of an operation, expr0 is turned
 *  into a procedure and execution proceeds as if expr0 had been
 *  a procedure all along.
 */
	pushl	$strbuf		# Try to convert expr0 to a string
	pushl	r11
	calls	$2,_cvstr	# cvstr(&expr0,&strbuf), r0 is 
	tstl	r0		#  non-zero if expr0 is a string, and
				#  strbuf will contain the string.
	beql	f4		# If expr0 couldn't not be converted
				#  to a string, branch.
				
	pushl	r8		# Otherwise, see if the string names
	pushl	r11		#  a procedure or a function
	calls	$2,_strprc	# strprc(&expr0,r8), note that r8 contains
				#  the number of expri (number of arguments)
	tstl	r0		# If non-zero rc, r11 now points to a
	bneq	doinvk		#  descriptor that references the procedure
				#  to be invoked.
#endif XPX
f4:	pushl	r11		# if not procedure or integer, then error
	pushl	$106
	calls	$2,_runerr	# runerr(106,&expr0)

/*
 * If the procedure being invoked has a fixed number of arguments,
 *  the arguments supplied are adjusted to conform in number to
 *  the number expected.
 */
doinvk:	movl	4(r11),r9	# r11 is a procedure descriptor, r9
				#  gets the address of the procedure block.
	movl	12(r9),r10	# The fourth word of the procedure block
				#  is the number of arguments the procedure
				#  wants.
	jlss	builtin		# If < 0, the number of arguments is variable;
				#  branch to builtin.

	subl2	r10,r8  	# r8 = # args expected - # args given
	beql	doderef		# If # given is the # expected, no
				#  adjustment is required.
				# Otherwise, nargs and nwords must
				#  be adjusted.
	movl	r10,4(ap)     	# Change nargs on stack
	movb	r10,(ap)	# Set nwords to nargs
	addb2	(ap),(ap)	# Double nwords because each argument
				#  is two words long.
	addb2  	$1,(ap)		# Add 1 to nwords to allow for the
				#  nargs word.
/*
 * The arguments now need to be adjusted to conform with the
 *  number expected.
 */
	ashl	$3,r8,r8	# Convert r8 to byte count
	addl2	r8,sp		# Move the stack pointer up or down
				#  as required
				#
				# Now the portion of the stack from
				#  nargs to the condition handler (inclusive)
				#  must be moved up or down.  This
				#  region is
				#       5 (handler, psw, ap, fp, pc)
				#       +
				#       INVREGS (11 registers saved)
				#       +
				#       2 (nwords, nargs) words long
	movc3	$(INVREGS+7)*4,(fp),(sp) # do the move, note that the
				#  the VAX microcode is smart enough to
				#  allow the regions to overlap.
	movl	sp,fp		# Point fp at new top of stack
	movl	fp,_boundary	# The boundary follows the fp
	addl2 	r8,ap		# Also adjust argument pointer
	tstl	r8		# If r8 is positive, there were too
				#  many arguments, and the stack move
				#  overwrote excess ones.  If r8 is
	bgeq 	doderef		#  negative, the stack moved down
				#  leaving a "hole" where additional
				#  arguments are to be.  Branch
				#  if r8 is positive.
				#
				#
	mnegl   r8,r8		# Otherwise, make r8 positive and
				#  insert null bytes to form null
				#  descriptors for the missing
				#  arguments.
  	movc5	$0,(r0),$0,r8,(INVREGS+7)*4(sp) # Do it.  Note that
				#  this is a VAX idiom to move a bunch
				#  of null bytes to a location, r0
				#  is not used at all.
/*
 * Arguments to Icon procedures must be dereferenced
 */
doderef:
	tstl	16(r9)		# r9 still points at the procedure
				#  block of the procedure being invoked
				#  and the fifth word of the block is
				#  the number of dynamic locals.  If
	jlss	builtin		#  it's less than 0, the procedure is
				#  a builtin.
	tstl	r10		# r10 is the number of arguments, if
	jeql	cktrace		#  it's 0 (no arguments) no dereferencing
				#  is needed.

	moval	-8(r11),r6	# Point r6 at expr1 for later use
	movl	r10,r5		# Make copy of r10 for a counter
nxtarg:
	pushaq	-(r11)		# r11 points at expr0 initially, it
				#  is decremented by 8, and the resulting
				#  value is pushed on the stack.  This
				#  value is the address of the descriptor
				#  for a particular expri and the expri
  	calls	$1,_deref	#  is dereferenced
	sobgeq	r5,nxtarg	# Loop around, dereferencing each expri
/*
 * If tracing is on, indicated by _k_trace (&trace) being non-zero,
 *  ctrace is called to produce the appropriate trace message.
 */
cktrace:
	tstl	_k_trace	# If not tracing,
	beql	tracedone	#  then branch
				# Otherwise, must set up for the
				#  call to ctrace.
	pushl	r6		# Push &expr1
	pushl	r10		# Push nargs
	pushl	r9		# Push r9, procedure block address
	calls	$3,_ctrace	# ctrace(&procedure-block,nargs,&expr1)
/*
 * A procedure frame was partially built by the call to invoke,
 *  it is completed by adding _line, _file, and &null for each
 *  local variable.
 */
tracedone:
	pushl	_line		# Put _line
	pushl	_file		#  and _file on the stack

	ashl	$3,16(r9),r0	# r0 = #locals * 3
	subl2	r0,sp		# Make space on stack for locals
        movc5	$0,(r0),$0,r0,(sp)	# Move the required number of null
				#  bytes onto the stack
/*
 * Enter the procedure or function.
 */
	clrl	_boundary	# Clear the boundary since an Icon
				#  procedure is to be invoked.
	incl	_k_level	# Increment &level to indicate one more
				#  level of depth.
 	movl	8(r9),ipc	# Get the procedure entry point which
				#  is the third word of the procedure block
				#  and load the interpreter pc with it.
	clrq	gfp		# clear gfp and efp (r10 and r11)
 	jmp	_interp		# Jump back to the interpreter, note
				#  that at this point, the procedure
				#  is "in execution".
/*
 * Handle invocation of a builtin procedure.  Because of the extra
 *  "help" the VAX provides, this is inordinately complicated.
 */
builtin:
  	movl	16(fp),20(fp)	# Save real return address where r1
				#  "should be".
	movab	bprtn,16(fp)	# Use a fake return address so that
				#  control comes to "bprtn:" below when
				#  the built-in procedure returns.
  	movl 	fp,_boundary	# Going into C code, so the boundary
				#  must be set.
	jmp	*8(r9)		# Jump into the procedure.

bprtn:				# When the procedure returns, it comes
				#  right here.
	clrl	_boundary	# Clear Icon/C boundary since we're going
				#  back to Icon.  (Builtin's are C fcns.)
	jmp	(r1)		# Jump back to caller of invoke.  Recall
				#  that the pc was stashed where r1 should
				#  have been saved.

 .data
longint:  .long 0
strbuf:	  .space MAXSTRING
#endif VAX

#ifdef PORT
DummyFcn(_invoke)
#endif PORT

#ifdef PDP11
/ invoke - call a procedure or function or create a record or
/          perform mutual goal-directed evaluation.
/ Supplies missing arguments, deletes extras for Icon
/ procedures.

/ Register usage:
/   r0-r2: utility registers
/   r3:    pointer to procedure block
/   r4:    pointer to icon arguments on the stack
/   r5:    current procedure frame pointer

 .text
_invoke:
	mov	r5,-(sp)        / create new procedure frame
	mov	sp,r5
	mov	r5,_boundary	/ set Icon/C boundary
	mov	r4,-(sp)    	/ save registers
	mov	r3,-(sp)
	mov	r2,-(sp)

/ Find descriptor for procedure or function and dereference it.

	mov	4(r5),r4        / get # arguments supplied
	asl	r4		/ compute address
	asl	r4		/   of procedure name
	add	$6,r4		/   in r4
	add	r5,r4		
	mov	r4,-(sp)	/ dereference it
	jsr	pc,_deref
	tst	(sp)+
	mov	(r4),r0		/ get type field of descriptor
	cmp	$D_PROC,r0	/ check for procedure type
	beq	3f
        mov     $longint,-(sp)  / see if its an integer for MGDE
        mov     r4,-(sp)
        jsr     pc,_cvint
        cmp     (sp)+,(sp)+
        cmp     $T_INTEGER,r0
        bne     2f
        mov     4(r5),-(sp)     / push number of expressions
        mov     $longint,r0     / convert integer to position
        mov     2(r0),-(sp)
        mov     (r0),-(sp)
        jsr     pc,_cvpos       / r0 <- position
        cmp     (sp)+,(sp)+
        tst     (sp)+
        cmp     r0,4(r5)        / see if in range
        ble     1f
        jsr     pc,_fail        / if not then fail
1:      asl     r0              / convert position to offset from arg0
        asl     r0
        mov     r4,r1
        sub     r0,r1
        mov     (r1)+,(r4)+     /  copy result to arg0
        mov     (r1),(r4)
        tst     -(r4)           /  restore r4
        mov     r4,sp           /  set sp to end of returned result
        mov     r5,r0
        mov     (r5),r1
        mov     -(r0),r4        /  restore registers
        mov     -(r0),r3
        mov     -(r0),r2
        clr     _boundary
        mov     (r5)+,r0        /  r0 <- return pc.
        mov     (r5)+,r0
        mov     r1,r5
        jmp     (r0)            /  return to code
2:
#ifdef XPX
/*
 * If the invokee is a string and the name of an operation,
 *  we invoke the corresponding procedure.
 */
	mov	$strbuf,-(sp)
	mov	r4,-(sp)
	jsr	pc,_cvstr	/ see if string for string invocation
	cmp	(sp)+,(sp)+
	tst	r0
	beq	4f		/ if ok, we see if the string is the
				/  name of something
	mov	4(r5),-(sp)	/ push number of arguments
	mov	r4,-(sp)	/ address of string descriptor
	jsr	pc,_strprc
	cmp	(sp)+,(sp)+
	tst	r0
	bne	3f		/ if non-zero rc, r4 now points to a
				/  descriptor that references the
				/  procedure we want
#endif XPX
4:	mov	r4,-(sp)	/ if not procedure or integer, error
	mov	$106.,-(sp)
	jsr	pc,_runerr

/ Check number of arguments supplied vs. number expected.

3:
	mov	2(r4),r3	/ get pointer field of descriptor
	mov	6(r3),r0	/ get # arguments expected
	blt	builtin		/ if < 0, # arguments is variable
	mov	r0,nargs	/ save # expected for later dereferencing
	sub	4(r5),r0	/ subtract # supplied from # expected
	beq	1f		/ if zero difference, no adjustment
	mov	nargs,4(r5)	/ change nargs on stack
	neg	r0		/ negate the difference
	blt	2f		/ if too few supplied, branch

/ Too many arguments supplied:  delete extras, compressing the stack.

	mov	r5,r1		/ compute adjustment addresses
	add	$6,r1		/   r1 <- source
    	asl	r0		/   r0 <- dest
	asl	r0
	add	r0,r5		/ adjust r5
	add	r0,_boundary	/   and boundary
	add	r1,r0
3:				/ move top 6 words up
	mov	-(r1),-(r0)
	cmp	r1,sp
	bgt	3b

	mov	r0,sp		/ adjust stack pointer
	br	1f

/ Too few arguments supplied:  push null values, expanding the stack.

2:
	mov	4(r5),nargs	/ save # supplied for later dereferencing
	asl	r0		/ compute new top of stack
	asl	r0
	add	r0,r5		/ adjust r5
	add	r0,_boundary	/   and boundary
	add	sp,r0
	mov	r0,r2		/ save new stack pointer
	mov	$6,r1
3:				/ move top 6 words down
	mov	(sp)+,(r0)+
	sob	r1,3b
3:				/ supply &null for omitted arguments
	clr	(r0)+
	clr	(r0)+
	cmp	r0,sp
	blt	3b

	mov	r2,sp		/ restore new top of stack pointer

/ Dereference arguments to Icon procedures.

1:
	tst	8.(r3)		/ test # dynamic locals
	blt	builtin		/   if < 0, then builtin function
	mov	nargs,r2	/ dereference the arguments
	beq	1f
2:
	cmp	-(r4),-(r4)	/ point r4 to next argument
	mov	r4,-(sp)	/ dereference it
	jsr	pc,_deref	
	tst	(sp)+
	sob	r2,2b

/ Print trace message if &trace is set.

1:
	tst	_k_trace
	beq	1f
	mov	nargs,r0	/ calc address of arg1 via:
	dec	r0		/  sp + 12. + (nargs-1)*4
	asl	r0
	asl	r0
	add	$12.,r0
	add	sp,r0
	mov	r0,-(sp)	/ push &arg1
	mov	nargs,-(sp)	/ push nargs
	mov	r3,-(sp)	/ push proc address
	jsr	pc,_ctrace	/ ctrace(proc_address,nargs,&arg1)
	cmp	(sp)+,(sp)+
	tst	(sp)+		/ zap ctrace args

/ Save line number and file name

1:
	mov	_line,-(sp)
	mov	_file,-(sp)

/ Push null values onto stack for each dynamic local

	mov	8.(r3),r0	/ get # dynamic locals
	beq	1f
2:
	clr	-(sp)          	/ push null value on stack for each
        clr     -(sp)           / dynamic local
	sob	r0,2b

/ Enter the procedure or function.

1:
	clr	_boundary	/ clear boundary when going to Icon procedure
	inc	_k_level	/ increment &level
	mov	4(r3),r2	/ r2 <- procedure entry point
	clr	r3		/ clear	generator frame pointer
	clr	r4		/   and expression frame pointer
	jmp	_interp   	/ jump back to interpreter
builtin:			/ special-case builtin functions
	jsr	pc,*4(r3)	/ jump to procedure entry point

 .bss
nargs:	.=.+2
longint: .=.+4
strbuf: .=.+MAXSTRING
#endif PDP11
