	.data
	.text
LL0:	.align	1
	.globl	_Kfrexpd
	.data
	.align	2
L18:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.data
	.align	2
L21:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.data
	.align	2
L22:	.long	0x41000000, 0x00000000 # .double 2
	.text
	.data
	.align	2
L25:	.long	0x40000000, 0x00000000 # .double 0.5
	.text
	.data
	.align	2
L28:	.long	0x40000000, 0x00000000 # .double 0.5
	.text
	.data
	.align	2
L29:	.long	0x41000000, 0x00000000 # .double 2
	.text
	.set	L12,0x0
	.data
	.text
_Kfrexpd:
	.word	L12
	subl3	$60,fp,sp
	clrl	-60(fp)
	clrl	-56(fp)
	tstl	4(fp)
	jgeq	L16
	lnd	4(fp)
	std	4(fp)
	movl	$1,-56(fp)
L16:	cmpd2	4(fp),L18
	jleq	L17
L19:	cmpd2	4(fp),L21
	jleq	L23
	addl2	$1,-60(fp)
	pushl	16(fp)		# hfs
	pushl	L22+4		# ldd	L22		# 2.0
	pushl	L22
	pushl	8(fp)
	pushl	4(fp)		# acc
	callf	$24,_Kdivd
	ldd	r0
	std	4(fp)
	jbr	L19
L17:	cmpd2	4(fp),L25
	jlss	L26
	jbr	L23
L2000001:
	subl2	$1,-60(fp)
	pushl	16(fp)		# hfs
	ldd	L22		# 2.0
	pushd
	ldd	4(fp)		# acc
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	4(fp)
L26:	cmpd2	4(fp),L28
	jlss	L2000001
L23:	movl	-60(fp),*12(fp)
	tstl	-56(fp)
	jeql	L30
	lnd	4(fp)
	std	4(fp)
L30:	movl	8(fp),r1
	movl	4(fp),r0
	ret
