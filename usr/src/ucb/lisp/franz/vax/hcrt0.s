# C runtime startoff
# $Header: /na/franz/franz/vax/RCS/hcrt0.s,v 1.1 83/03/27 18:40:07 jkf Exp $

	.set	exit,1
.globl	_exit
.globl	start
.globl	hstart
.globl	_main
.globl	_environ

#
#	C language startup routine

hstart:	
	.word	0x0000
	movl	$1,r1
	jbr	L0
start:
	.word	0x0000
	clrl	r1
L0:
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
	movl	r0,r10  #  indir is 0 if no env ; not 0 if env
	tstl	r1
	beql	L3
	calls	$0,_rlc
L3:
	movl	r10,_environ
	calls	$3,_main
	pushl	r0
	calls	$1,_exit
	chmk	$exit
#
	.data
_environ:	.space	4
