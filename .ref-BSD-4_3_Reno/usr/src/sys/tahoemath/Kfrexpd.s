/*	Kfrexpd.s	1.3	86/01/05	*/

#include "../tahoe/SYS.h"

	.text
ENTRY(Kfrexpd, 0)
	subl3	$60,fp,sp
	clrl	-60(fp)
	clrl	-56(fp)
	tstl	4(fp)
	jgeq	1f
	lnd	4(fp)
	std	4(fp)
	movl	$1,-56(fp)
1:
	cmpd2	4(fp),one
	jleq	1f
2:
	cmpd2	4(fp),one
	jleq	3f
	addl2	$1,-60(fp)
	pushl	16(fp)		# hfs
	pushl	two+4		# ldd	2.0
	pushl	two
	pushl	8(fp)
	pushl	4(fp)		# acc
	callf	$24,_Kdivd
	ldd	r0
	std	4(fp)
	jbr	2b
1:	cmpd2	4(fp),half
	jlss	2f
	jbr	3f
0:
	subl2	$1,-60(fp)
	pushl	16(fp)		# hfs
	ldd	two; pushd	# 2.0
	ldd	4(fp); pushd	# acc
	callf	$24,_Kmuld
	ldd	r0
	std	4(fp)
2:
	cmpd2	4(fp),half
	jlss	0b
3:	movl	-60(fp),*12(fp)
	tstl	-56(fp)
	jeql	1f
	lnd	4(fp)
	std	4(fp)
1:
	movl	8(fp),r1
	movl	4(fp),r0
	ret
	.data
	.align 2
one:	.long	0x40800000, 0x00000000 # .double 1
two:	.long	0x41000000, 0x00000000 # .double 2
half:	.long	0x40000000, 0x00000000 # .double 0.5
