# @(#)crt0.s	4.1 (Berkeley) 12/21/80
# C runtime startoff
#		TAHOE 3/83

	.set	exit,1
.globl	_exit
.globl	start
.globl	_main
.globl	_environ

#
#	C language startup routine

start:
	.word	0x0000
	movab	-8(sp),sp
	movl	8(sp),(sp)  #  argc
	movab	12(sp),r0
	movl	r0,4(sp)  #  argv
	jmp	L3
L1:
	addl2	$4,r0
L3:
	tstl	(r0)  #  null args term ?
	jneq	L1
	addl2	$4,r0
	cmpl	r0,*4(sp)  #  end of 'env' or 'argv' ?
	jlss	L2
	subl2	$4,r0 	# envp's are in list
L2:
	movl	r0,8(sp)  #  env
	movl	r0,_environ  #  indir is 0 if no env ; not 0 if env
	callf	$4*3+4,_main
	pushl	r0
	callf	$4*1+4,_exit
	kcall	$exit
#
	.data
_environ:	.space	4
