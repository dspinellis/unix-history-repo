#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)00case.h 4.1 10/10/80";
#
# Berkeley Pascal Assembler Kernel
#
	.globl	_interpret	#normal interpreter entry point
#
# register definitions
#
# registers R0 - R6 are for scratch use
#
	.set	lino, r11
	.set	lc, r10
	.set	dp, r9
	.set	loop, r8
	.set	buf, r7
#
# Global constants
#
	.set	R2,0x004	#register fields in call mask
	.set	R3,0x008
	.set	R4,0x010
	.set	R5,0x020
	.set	R6,0x040
	.set	R7,0x080
	.set	R8,0x100
	.set	R9,0x200
	.set	R10,0x400
	.set	R11,0x800
	.set	SIGINT,2	#interrupt signal
	.set	SIGFPE,8	#arithmetic exception signal
	.set	SIGSEGV,11	#segmentation violation
	.set	MASK,6		#offset of save mask in call stack
	.set	FP,12		#offset of "fp" in call stack
	.set	PC,16		#offset of "pc" in call stack
	.set	REGS,20		#beginning of saved registers in call stack
	.set	O_DATE,0345	#DATE opcode number
	.set	O_TIME,0346	#TIME opcode number
	.set	tempsize,-1024	#maximum required temporary stack space
#
# program variables
#
	.globl	_display	#runtime display
	.globl	_file		#current file name
	.globl	_fchain		#head of active file chain
	.globl	_llimit		#max number of output lines
	.globl	_stcnt		#number of stmts executed
	.globl	_stlim		#max number of stmts to exec
	.globl	_nodump		#1 => no postmortum dump
	.globl	_perrno		#interpreter error number
	.globl	_profcnts	#PX profile execution counts
	.globl	_pxpbuf		#ptr to pxp buffer
	.globl	_pxpsize	#size of pxp buffer
	.globl	_argc		#number of passed args
	.globl	_argv		#values of passed args
	.globl	__iob		#base of I/O buffer block
	.globl	__sobuf		#standard output buffer
#
# system subroutines
#
	.globl	_signal
	.globl	_time
	.globl	_times
#
# system math routines
#
	.globl	_atan
	.globl	_cos
	.globl	_exp
	.globl	_log
	.globl	_sin
	.globl	_sqrt
	.globl	_srand
	.globl	_rand
#
# pascal specific subroutines
#
	.globl	_error		#error message routine
	.globl	_palloc		#heap allocator
	.globl	_pfree
	.globl	_cttot		#set constructor
	.globl	_inct		#set inclusion
	.globl	_pdattim	#getting date, time info
	.globl	_perror		#process pxp errors
#
# initializing the interpreter
#
_interpret:
	.word	0xffc		#register save mask
	moval	iloop,r8
	tstl	8(ap)		#check for profiling
	beql	l0050
	moval	ploop,r8	#set profiling request
l0050:
	callg	*4(ap),l0051	#set ap to base of program
	ret
l0051:
	.word	0
	bispsw	$0xe0		#enable overflow traps
	movl	ap,r10		#program start address
	moval	_display,r9
	moval	-4(sp),(r9)
	pushal	stderr		#set up global file variables
	movl	sp,stderr+FLEV
	movl	_llimit,stderr+LLIMIT
	calls	$0,_unit	#init active file
	pushal	stdout
	movl	sp,stdout+FLEV
	movl	_llimit,stdout+LLIMIT
	pushal	stdin
	movl	sp,stdin+FLEV
	moval	stdin,_fchain
	jmp	(r8)		#begin interpreter
#
# main interpreter loop
# the instruction 'jmp	(loop)'
# transfers here
#
ploop:
	movzbl	(r10),r0
	incl	_profcnts[r0]
iloop:
	caseb	(r10)+,$0,$255
optab:
