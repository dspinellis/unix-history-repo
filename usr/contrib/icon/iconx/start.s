#include "../h/config.h"
/*
 * Icon runtime startup.  This routine gets Icon rolling and is called
 *  by main.  The basic functions of this routine are to initialize
 *  memory with a call to init, and then invoke the main procedure with
 *  single argument that is a list composed of command line arguments.
 */
Global(__cleanup)	/* close files on exit */
Global(_init)		/* initialize memory */
Global(_invoke)		/* procedure invocation */
Global(_llist)		/* form a literal list */
#ifdef AZ
Global(_monitor)	/* turn profiling on/off */
#endif AZ
Global(_runerr)		/* runtime error processor */
Global(_globals)	/* icon global data region */
Global(_monres)		/* profile resolution */
Global(_c_exit)
Global(_boundary)
Global(_tended)
Global(_etended)
Global(_mstart)
#ifdef VAX
 .text
_mstart:
/*
 * mstart(argv) -- start up Icon.
 */
	Mask	0x0000		# don't need to save any registers
	movl	8(ap),r9	# point r9 at first word of argv list
	
/*
 * Call init to initialize memory and set environment variables
 */
	pushl	4(r9)		# pass file name to be interpreted to init
	calls	$1,_init	# init(file)
/*
 * Create a dummy expression frame so that main procedure is no
 *  different from other procedures.
 */
	pushl	$0		# old expression frame pointer
	movl	sp,efp		# point the expression frame pointer
				#  at this word.
	pushl	$0		# old generator frame pointer
 	pushl	$flab		# failure label, we branch to flab
				#  if the expression (the evaluation
				#  of main()) fails

/*
 * Prepare to invoke main(), which should be first global variable.
 *  Note that _globals contains the address of the first
 *  global variable.
 */
 	movl	_globals,r0	# point r0 at first global variable
	cmpl	$D_PROC,(r0)	#  see if it's a procedure [*]
	bneq	nomain
 	movq	(r0),-(sp)	# Push variable 'main' onto stack
	clrq	-(sp)		# Push &null to receive result of llist()

	tstl	(r9)+		# r9 points to arg0, which is program
				#  name.  We ignore it, moving r9
				#  on to point at the first actual
				#  argument to the program
	tstl	(r9)+		# arg1 is the name of the file to
				#  interpret, so we ignore it as well
/*
 * Now we're ready to make an Icon list out of the program arguments.
 *  We build string descriptors one at a time on the stack and then
 *  call llist which makes a list out of its arguments.
 */
	clrl	r8		# r8 counts args, 0 to start with
b1:
	movl	(r9)+,r7	# get address of next argument
	beql	f2		# if 0, we're at end of argument list,
	pushl	r7		#  otherwise we push the address of the arg
	incl	r8		# count one more argument
	locc	$0,$0xffff,(r7) # calculate length of string, the 0xffff
				#  constant is 65k, which specifies the
				#  maximum distance to look to for the
				#  0 (null byte) that terminates the string
	subl3	r7,r1,-(sp)	# push length of string, note that r1
				#  was automagically set by the locc
				#  instruction
	jbr	b1		# loop around until we get to the 0 word
				#  at the end of the argument list

/*
 * llist is called as llist(nargs,argn,...,arg1).  We now have argn through
 *  arg1 on the stack.  We push nargs (number of arguments) and then
 *  we calculate the number of words occupied by the argument list
 *  because the calls instruction uses it.
 */
f2:
	pushl	r8		# push nargs
	calls	$0,_llist	# make a list of the arguments
	ashl	$1,r8,r8	# calc number of words in the argument list,
				#  each descriptor is 2 words, so we multiply
				#  nargs by 2.
	incl	r8		# nargs itself also takes a word, so we add 1
	ashl	$2,r8,r8	# r8 is length in bytes of argument list
	addl2	r8,sp

/*
 * Invoke main() with one argument (the list of command line arguments).
 */
	clrl	gfp		# clear generator frame pointer
	clrl	r13		# clear procedure frame pointer
	pushl	$1		# push nargs
 	calls	$3,_invoke

/*
 * If main() returns we end up here. Call _c_exit to exit with 0 status.
 */
f9:
	pushl	$0		# exit status = 0
	calls	$1,_c_exit

/*
 * If there was no main procedure we call runerr(117,0).
 */
nomain:
	pushl	$0
	pushl	$117
	calls	$2,_runerr
	pushl	$1
	calls	$1,_c_exit


/*
 * c_exit(i) - flush all buffers and exit with status i.
 */
_c_exit:
	Mask	0
#ifdef AZ
	tstl	_monres			# if we're monitoring,
	beql	f1
	pushl	$0			# we turn it off with
	calls	$1,_monitor		#  monitor(0)
#endif AZ
/*
 * We call __cleanup to clean up the i/o system and then
 *  call exit(i), where "i" is the argument to _c_exit.
 */
f1:
	calls	$0,__cleanup
	pushl	4(ap)
	calls	$1,_exit
 .data

/*
 * waste first few bytes of memory, because all pointers must be
 *  greater than MAXTYPE, lest we confuse the garbage collector.
 */
	.space	60

/*
 * flab is "branched to" by the interpreter if the main procedure
 *  fails.  The 0 is a "quit" opcode for the interpreter.
 */
flab:
	.byte	0

/*
 * The boundary marks the point where the stack is C above and
 *  Icon below.
 */
_boundary:
	.long	0
/*
 * The tended descriptors.
 */
_tended:
	.long	0,0	# tended[0]
	.long	0,0	# tended[1]
	.long	0,0	# tended[2]
	.long	0,0	# tended[3]
	.long	0,0	# tended[4]
	.long	0,0	# tended[5]
_etended:


#endif VAX
#ifdef PORT
DummyFcn(_mstart)
DummyFcn(_c_exit)
DummyData(_boundary)
DummyData(_tended)
DummyData(_etended)
#endif PORT

#ifdef PDP11
Global(csv)
#include <sys.s>
/
/ Icon runtime startup
/
#ifdef NOFP
Global(fptrap)
signal = 48.
#endif NOFP

 .text
_mstart:
/*
 * mstart(argv) -- start up Icon
 */
/ Register usage:
/   r1: counter for scanning argument list
/   r2: nargs - number of arguments to the program
/   r3: character pointer, for finding length of each argument string
/   r4: pointer to dummy expression frame marker
/   r5: pointer to command line argument list

	jsr	r5,csv
	clr	_boundary	/ undo boundary setting
#ifdef NOFP
/ Use software floating point
	sys	signal; 4; fptrap
	setd
#else
/ Enable floating point traps, double precision.
	ldfps	$3200
#endif NOFP

/ Create a dummy expression frame.

	clr	-(sp)		/ old r4
	mov	sp,r4
	clr	-(sp)		/ old r3
	mov	$flab,-(sp)	/ failure label

/ Initialize memory.
	mov	6(r5),r0	/ point at argv
	mov	2(r0),-(sp)	/ push address of arg1, the file name
	clr	-(sp)		/ pass nargs to init() for set/clrbound
        jsr     pc,_init
	tst	(sp)+
/ Prepare to invoke procedure main, which should be first global variable.
	mov	_globals,r0
	cmp	$D_PROC,(r0)	/ make sure procedure main exists
	bne	nomain
	mov	2(r0),-(sp)	/ push variable "main" on stack
	mov	(r0),-(sp)
/ Build a list from the command line arguments.

	clr	-(sp)		/ push &null for result from llist()
	clr	-(sp)
        mov     4(r5),r1        / r1 <- nargs
	mov	6(r5),r5	/ point r5 at argv[0]
	add	$4.,r5
	dec	r1		/ don't count argument 0 (command name)
	dec	r1		/   or argument 1 (icon program name)
        mov     r1,r2
	bgt	1f
	clr	r2
	br	3f
1:
        mov     (r5)+,r3        / build string descriptors for args
        mov     r3,-(sp)        /   push pointer to string
        clr     -(sp)		/   push string length of 0
2:
        tstb    (r3)+           /   calculate length of string
        beq     2f
        inc     (sp)		/   increment string length
        br      2b
2:
        sob     r1,1b
3:
        mov     r2,-(sp)        / push nargs
	jsr	pc,_llist	/ make a list of the arguments

/ Invoke main() with one argument (the list of command line arguments).

	mov	$1,-(sp)
	clr	r3		/ clear generator frame pointer
	clr	r5		/ clear procedure frame pointer
        jsr     pc,_invoke

/ If main() fails or returns, exit with 0 status.
9:
	clr	-(sp)		/ exit status = 0
        jsr     pc,*$_c_exit
        sys     exit

/ Issue runerr(117,NULL) if main() is missing.

nomain:				/ runtime error if procedure main missing
	clr	-(sp)
	mov	$117.,-(sp)
	jsr	pc,_runerr
	sys	exit

/ c_exit(i) - flush all buffers and exit with status i.

_c_exit:
	mov	r5,-(sp)
	mov	sp,r5
#ifdef AZ
	tst	_monres			/ is monitoring on?
	beq	1f
	clr	-(sp)
	jsr	pc,_monitor		/   yes, turn it off
	tst	(sp)+
#endif AZ
1:
	jsr	pc,__cleanup
	mov	4(r5),r0
	sys	exit

 .data

/ Waste first 30 or so bytes of memory, because all pointers must be
/ greater than MAXTYPE.

	.=.+30.

/ Failure label for outermost expression (used if main() fails)

flab:	0				/ terminate program

/ Reserve storage for general use tended descriptors.


_tended:
        0; 0	/ tended[0]
        0; 0	/ tended[1]
        0; 0	/ tended[2]
        0; 0	/ tended[3]
        0; 0	/ tended[4]
        0; 0	/ tended[5]
_etended:

_boundary: 0

 .bss
 .data
#endif PDP11

/*
 * The following DummyRefs force the loader to load everything that
 *  iconx needs.
 */
DummyRef(_clrbound)
DummyRef(_ckadd)
DummyRef(_ckmul)
DummyRef(_cksub)
DummyRef(_coact)
DummyRef(_cofail)
DummyRef(_coret)
DummyRef(_efail)
DummyRef(_efail)
DummyRef(_esusp)
DummyRef(_fail)
DummyRef(_gcollect)
DummyRef(_interp)
DummyRef(_invoke)
DummyRef(_lsusp)
DummyRef(_pfail)
DummyRef(_pret)
DummyRef(_psusp)
DummyRef(_setbound)
DummyRef(_suspend)

