#include "../h/config.h"
#ifdef VAX
/* invoke - call a procedure or function or create a record or
 *          perform mutual goal-directed evaluation.
 * Supplies missing arguments, deletes extras for Icon
 * procedures.
 *
 * Register usage:
 *   r0-r5: utility registers
 *   r8:    number of bytes to shift top of stack
 *   r9:    pointer to procedure block
 *   r10:   number of requested arguments
 *   r11:   pointer to icon arguments on the stack
 */

#ifdef INT
.globl	_interp			# interpreter loop
.globl	_cvstr			# convert to string
#ifdef EXT
.globl	_strprc			# convert string to procedure block address
#endif EXT
#endif INT
.globl	_ctrace			# call trace routine
.globl  _cvint                  # convert to integer
.globl  _cvpos                  # convert to position
.globl	_deref			# dereference a variable
.globl  _fail                   # failure processing
.globl  _runerr			# issue a runtime error

.globl	_boundary		# Icon/C boundary address
.globl	_line			# current line number
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_k_trace		# value of &trace

.text
.globl	_invoke

_invoke:
	.word	0xffe
				# must save all registers to be safe
#define INVREGS  11		/* number of registers saved */

	movl 	fp,_boundary    # set Icon/C boundary
        movl  	4(ap),r8	# r8 <- nargs
	movaq	8(ap)[r8],r11	# r11 <- address of procedure name
   	pushl	r11		# deference procedure name
	calls	$1,_deref
	movl	(r11),r0        # check for procedure type
	cmpl	$D_PROC,r0
	jeql	3f		#   if procedure, branch

	pushl	$longint	# see if integer for MGDE
	pushl	r11
	calls	$2,_cvint
	cmpl	$T_INTEGER,r0
	jneq	2f

	pushl	4(ap)		# convert integer to position
	movl	longint,-(sp)
	calls	$2,_cvpos	# r0 <- position
	cmpl	r0,4(ap)	# see if position is in range
	bleq	1f
	calls	$0,_fail	# if not, then fail

1:	ashl	$3,r0,r0	# find offset from arg0
	subl3	r0,r11,r1	# r1 <- address of selected arg.
	movq    (r1),(r11)	# copy return value

	clrl	_boundary	# clear Icon/C boundary
	ret

2:
#ifdef INT
#ifdef EXT
/*
 * If the invokee is a string and the name of an operation,
 *  we invoke the corresponding procedure.
 */
	pushl	$strbuf
	pushl	r11
	calls	$2,_cvstr	# see if string for string invokation
	tstl	r0
	beql	4f		# if ok, we see if the string is the
				#  name of something
	pushl	r8		# number of arguments
	pushl	r11		# address of string descriptor
	calls	$2,_strprc
	bneq	3f		# if non-zero rc, r11 now points to a
				#  descriptor that references the procedure
				#  we want
#endif EXT
#endif INT
4:	pushl   r11		# if not procedure or integer, then error
	pushl	$106
	calls   $2,_runerr

   /* Check number of arguments supplied vs. number expected. */

3:
	movl	4(r11),r9	# r9 <- pointer field of descriptor
	movl	12(r9),r10	# r10 <- # of arguments expected
	jlss	builtin		# if < 0, # arguments is variable

	subl2	r10,r8  	# r8 <- # supplied - # expected
	beql	1f		# if zero difference, no adjustment
	movl	r10,4(ap)     	# change nargs on stack
	movb	r10,(ap)	# adjust VAX nargs field
	addb2	(ap),(ap)       # ...VAX nargs is is words, not descriptors
	addb2  	$1,(ap)		# ...handle extra word on stack.

   /* adjust stack to proper # of arguments */

	ashl	$3,r8,r8	# convert r8 to byte count
	addl2	r8,sp		# adjust stack pointer
	movc3	$(INVREGS+7)*4,(fp),(sp)  # 7 is top 5 words + 2 nargs words
	movl	sp,fp		# adjust frame pointer
	movl	fp,_boundary	#  ... and the boundary.
	addl2 	r8,ap		# adjust argument pointer
	tstl	r8		# if too many arguments, then
	bgeq 	1f		#   are done with adjustment

   /* Too few arguments supplied:  push null values, expanding the stack. */

	mnegl   r8,r8		# make r8 positive
  	movc5	$0,(r0),$0,r8,(INVREGS+7)*4(sp)

1:
	tstl	16(r9)		# test # of dynamic locals
	jlss	builtin		#  if < 0, then have builtin
	tstl	r10		# skip if have no arguments
	jeql	1f

        # Dereference arguments to Icon procedures.
	movl	r10,r5		# save nargs
2:
	pushaq	-(r11)		# point r11 to next argument
  	calls	$1,_deref	
	sobgeq	r5,2b

        # Print trace message if &trace is set.

1:
	tstl	_k_trace
	beql	1f
	subl3	$1,r10,r0	# calculate address of arg1, via:
				#  &arg1 = ap + (nargs-1)*8 + 8
	ashl	$3,r0,r0
	addl2	$8,r0
	addl3	ap,r0,-(sp)
	pushl	r10
	pushl	r9	
	calls	$3,_ctrace	# ctrace(proc_address,nargs,&arg1)

        # Save line number and file name

1:
	pushl	_line
	pushl	_file

        # Push null values onto stack for each dynamic local

	ashl	$3,16(r9),r0
	subl2	r0,sp
        movc5	$0,(r0),$0,r0,(sp)

        # Enter the procedure or function.

	clrl	_boundary	# clear boundary when going to Icon procedure
	incl	_k_level	# increment &level
#ifdef INT
 	movl	8(r9),r9	# save procedure entry point
#endif INT
#ifdef CMP
  	movl	8(r9),r0	# save procedure entry point
#endif CMP
	clrq    r10		# clear gfp and efp (r10 and r11)
#ifdef INT
 	jmp	_interp		# jump back to interpreter
#endif INT
#ifdef CMP
	clrl    r9		# clear ipc (interpreter program counter)
	jmp	(r0)     	# jump to procedure entry point
#endif CMP

builtin:			# special-case builtin functions
    	bicb2	$0x20,7(fp)	# must fake a callg to protect arguments
  	movl	16(fp),20(fp)	# save return address in saved r1
	movab	1f,16(fp)	# fake a new return address
  	movl 	fp,_boundary    # set Icon/C boundary
	jmp	*8(r9)		# jump to procedure entry point

1:				# faked return address
	clrl	_boundary	# clear Icon/C boundary
    	subl2	$(INVREGS+1)*4,sp	# sp <- saved r1-5 minus a word
       	movl	r0,(sp)		# now sp points to saved r0-5
	movzbl	(INVREGS+1)*4(sp),r0	# r0 <- number of arguments
	moval	(INVREGS+2)*4(sp)[r0],24(sp) # save correct sp
	popr	$r(0)|r(1)|r(2)|r(3)|r(4)|r(5)|r(14)  # restore r0-5 and sp
        jmp	(r1)		# return
				# (NOTE: the above code assumes R1 not used
				#   for return value from builtin.)

.data
longint:  .long 0
strbuf:	  .space MAXSTRING
#endif VAX
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

#ifdef INT
.globl	_interp			/ interpreter loop
.globl	_cvstr			/ convert to string
#ifdef EXT
.globl	_strprc			/ convert string to procedure block address
#endif EXT
#endif INT
.globl	_ctrace			/ call trace routine
.globl  _cvint                  / convert to integer
.globl  _cvpos                  / convert to position
.globl	_deref			/ dereference a variable
.globl  _fail                   / failure processing
.globl  _runerr			/ issue a runtime error

.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_k_trace		/ value of &trace
.globl	_line			/ current line number

.text
.globl	_invoke
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
#ifdef INT
#ifdef EXT
/*
 * If the invokee is a string and the name of an operation,
 *  we invoke the corresponding procedure.
 */
	mov	$strbuf,-(sp)
	mov	r4,-(sp)
	jsr	pc,_cvstr	/ see if string for string invokation
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
#endif EXT
#endif INT
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
#ifdef INT
	mov	4(r3),r2	/ r2 <- procedure entry point
	clr	r3		/ clear	generator frame pointer
	clr	r4		/   and expression frame pointer
	jmp	_interp   	/ jump back to interpreter
#endif INT
#ifdef CMP
	mov	4(r3),r0	/ save procedure entry point
	clr	r2		/ clear r2,
	clr	r3		/      	generator frame pointer,
	clr	r4		/	and expression frame pointer
	jmp	(r0)     	/ jump to procedure entry point
#endif CMP
builtin:			/ special-case builtin functions
	jsr	pc,*4(r3)	/ jump to procedure entry point

.bss
nargs:	.=.+2
longint: .=.+4
strbuf: .=.+MAXSTRING
#endif PDP11

