#include "../h/config.h"
/*
 * Icon runtime startup
 */
#ifdef VAX
.text
.globl __cleanup		# close files on exit
.globl _init			# initialize memory
.globl _invoke			# procedure invocation
.globl _llist			# form a literal list
.globl _monitor			# turn profiling on/off
.globl _runerr			# runtime error processor

.globl _environ			# pointer to environment
.globl _globals			# icon global data region
.globl _monres			# profile resolution

#ifdef INT
.globl _nulldesc		# null descriptor -- whm
#endif INT

        # Icon startup code

start:
	.word	0x0000		# because C does this?

   /* Set up the environment pointer */

	movl	sp,ap		# normalize ap (in case we are "adb")
	movl 	(sp),r6		# r6 <- nargs
        movab	4(sp),r9	# r9 <-	pointer to argument list
	subl2	$8,sp		# Get some room on the stack
	movl	8(sp),(sp)	#   nargs
	movab   12(sp),r0
	movl    r0,4(sp)	#   pointer to command line arguments

1:
	tstl	(r0)+		# Look for end of command line arguments
	bneq	1b

        cmpl	r0,*4(sp)	# Test for environment
	blss	1f 		# ... if none, point to null pointer
	tstl 	-(r0)

1:
	movl 	r0,8(sp)	# Set the environment
	movl	r0,_environ

   /* Create a dummy expression frame */

	pushl	$0		# Old expression frame (ep) (r11)
	movl    sp,r11
	pushl   $0		# Old generator	frame (gp) (r10)
#ifdef INT
 	pushl	$flab		# Failure label
#endif INT
#ifdef CMP
	pushl	$9f		# Failure label
#endif CMP

        # Initialize memory
#ifdef INT
	pushl	28(sp)		# address of file name
	pushl	$0
	calls   $2,_init
#endif INT
#ifdef CMP
	calls   $0,_init
#endif CMP
	movl    (r9)+,r5	# r5 <-	address of 0th argument
				# ???-Is the above assignment to r5
				#  really necessary?  -- whm

   /* Prepare to invoke main(), which should be first global variable. */
#ifdef INT
 	movl	_globals,r0	# Insure existence of procedure main
	cmpl	$D_PROC,(r0)
#endif INT
#ifdef CMP
 	cmpl	$D_PROC,_globals  # Insure existence of procedure main
#endif CMP
        bneq	nomain
#ifdef INT
 	pushl	4(r0)		# Push variable 'main' onto stack
	pushl	(r0)
#endif INT
#ifdef CMP
	pushl   $_globals	# Push variable 'main' onto stack
	pushl	$D_VAR
#endif CMP
	clrq    -(sp)		# Push &null for result of llist()

  	decl	r6		# Ignore arg0 (command name)
#ifdef INT
	decl	r6		#  and arg1 (ucode name)
	tstl	(r9)+		# point to arg2 as first arg for icn pgm.
#endif INT
	movl	r6,r7		# r7 <- nargs
	beql    2f

1:
	movl	(r9)+,r8	# build string descriptors for args
        pushl   r8		# push pointer to string
        locc	$0,$0177777,(r8)	# calculate length of string
	subl3	r8,r1,-(sp)	# push length of string
	sobgtr  r6,1b
2:
	pushl	r7		# push nargs
	ashl    $1,r7,r7	# adjust r7 to long word count
 	addl2   $1,r7		# Why is this here? -- whm
	calls	r7,_llist	# make a list of the arguments

   /* Invoke main() with one argument (the list of command line arguments) */

	clrl	r10		# clear generator frame pointer
	clrl    r13		# clear procedure frame pointer
	pushl   $1		# push nargs
 	calls   $3,_invoke

/* If main() fails or returns, exit with 0 status. */
9:
	pushl	$0		# exit status = 0
	calls   $1,*$_c_exit

nomain:				# runtime error if no procedure main
        pushl	$0
        pushl	$117
	calls   $2,_runerr
        pushl	$1
        calls   $1,_c_exit


   /* c_exit(i) - flush all buffers and exit with status i. */

.globl	_c_exit

_c_exit:
	.word   0
	tstl	_monres			# is monitoring on?
	beql	1f
	pushl	$0			# .. yes, turn it off
	calls	$1,_monitor
1:
	calls	$0,__cleanup
	pushl	4(ap)
	calls	$1,_exit
.data

   /* waste first few bytes of memory, because all pointers must be
    *   greater than MAXTYPE
    */

        .space 60

.globl	_boundary
.globl  _environ
.globl	_tended
.globl	_etended
flab:
	.long	0
_tended:
	.long	0,0	# tended[0]
	.long	0,0	# tended[1]
	.long	0,0	# tended[2]
	.long	0,0	# tended[3]
	.long	0,0	# tended[4]
	.long	0,0	# tended[5]
_etended:

_boundary:
        .long	0

_environ:
	.space  4


#endif VAX

#ifdef PDP11
/
/ Icon runtime startup
/
.globl	__cleanup		/ closes all files on exit
.globl  _init	      		/ initialize
.globl  _invoke			/ invoke a Icon proc or builtin
.globl	_llist			/ build a literal list (for args)
.globl	_monitor		/ turn profiling on and off
.globl	_runerr			/ print runtime error and quit

.globl	_environ		/ shell environment pointer
.globl  _globals		/ start of global variables
.globl	_monres			/ non-zero indicates profiling
.globl	_boundary
#ifdef INT
.globl	_nulldesc   		/ null descriptor
#endif INT
#ifdef NOFP
.globl fptrap
signal = 48.
#endif NOFP


/ start - entry point for all Icon programs.
/ Sets up argument list, calls invoke for first global
/ variable, which must be procedure main.

/ Register usage:
/   r0: pointer used for finding environment list
/   r1: counter for scanning argument list
/   r2: nargs - number of arguments to the program
/   r3: character pointer, for finding length of each argument string
/   r4: pointer to dummy expression frame marker
/   r5: pointer to command line argument list

.text
.globl  start
start:
#ifdef NOFP
/ Use software floating point--?-whm
	sys	signal; 4; fptrap
	setd
#else
/ Enable floating point traps, double precision.
	ldfps	$3200
#endif NOFP

/ Set _environ to point to list of environment variables.

	mov	sp,r0		/ r0 <- pointer to nargs
	tst	(r0)+		/ r0 <- pointer to arg0
1:
	tst	(r0)+		/ Look for end of command line arguments
	bne	1b

	cmp	r0,2(sp)	/ Make sure there is an environment
	blo	1f		/   If not, point to the null pointer
	tst	-(r0)		/   at the end of the arguments
1:
	mov	r0,_environ

/ Create a dummy expression frame.

	clr	-(sp)		/ old r4
	mov	sp,r4
	clr	-(sp)		/ old r3
#ifdef INT
	mov	$flab,-(sp)	/ failure label
#endif INT
#ifdef CMP
	mov	$9f,-(sp)	/ failure label
#endif CMP

/ Initialize memory.
#ifdef INT
	mov	6(r4),-(sp)	/ pass first command-line arg to init
	clr	-(sp)		/   (name of file to interpret)
        jsr     pc,_init
	tst	(sp)+
#endif INT
#ifdef CMP
	clr	-(sp)
        jsr     pc,_init
#endif CMP
/ Prepare to invoke procedure main, which should be first global variable.
#ifdef INT
	mov	_globals,r0
	cmp	$D_PROC,(r0)	/ make sure procedure main exists
#endif INT
#ifdef CMP
	cmp	$D_PROC,_globals / make sure procedure main exists
#endif CMP
	bne	nomain
#ifdef INT
	mov	2(r0),-(sp)	/ push variable "main" on stack
	mov	(r0),-(sp)
#endif INT
#ifdef CMP
	mov	$_globals,-(sp)	/ push variable "main" on stack
	mov	$D_VAR,-(sp)
#endif CMP
/ Build a list from the command line arguments.

	clr	-(sp)		/ push &null for result from llist()
	clr	-(sp)
        mov     2(r4),r1        / r1 <- nargs
        mov     r4,r5
#ifdef INT
	add	$8.,r5
#endif INT
#ifdef CMP
	add	$6,r5
#endif CMP
	dec	r1		/ don't count argument 0 (command name)
#ifdef INT
	dec	r1		/   or argument 1 (icon program name)
#endif INT
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

.globl	_c_exit
_c_exit:
	mov	r5,-(sp)
	mov	sp,r5
	tst	_monres			/ is monitoring on?
	beq	1f
	clr	-(sp)
	jsr	pc,_monitor		/   yes, turn it off
	tst	(sp)+
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

.globl	_tended			/ tended descriptors
.globl	_etended		/ end of tended descriptors

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
_environ: .=.+2
#endif PDP11
