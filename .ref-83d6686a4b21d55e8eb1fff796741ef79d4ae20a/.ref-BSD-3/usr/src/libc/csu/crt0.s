# C runtime startoff

	.set	exit,1
.globl	_exit
.globl	start
.globl	_main
.globl	_environ

#
#	C language startup routine

start:
	.word	0x0000
	subl2	$8,sp
	movl	8(sp),(sp)  #  argc
	movab	12(sp),r0
	movl	r0,4(sp)  #  argv
L1:
	tstl	(r0)+  #  null args term ?
	bneq	L1
	cmpl	r0,*4(sp)  #  end of 'env' or 'argv' ?
	blss	L2
	tstl	-(r0)  # envp's are in list
L2:
	movl	r0,8(sp)  #  env
	movl	r0,_environ  #  indir is 0 if no env ; not 0 if env
	calls	$3,_main
	pushl	r0
	calls	$1,_exit
	chmk	$exit
#
	.data
_environ:	.space	4
