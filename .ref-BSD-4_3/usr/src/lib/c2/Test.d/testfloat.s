LL0:
	.data
	.comm	_g,4
	.comm	_h,8
	.text
	.align	1
	.globl	_main
_main:
	.word	L14
	jbr 	L16
L17:
	mulg3	_g,_g,r0
	cvtgh	r0,r0
	cvthg	r0,-4(fp)
	subg3	_g,_g,r0
	cvtgh	r0,r0
	cvtgh	_g,r2
	addh3	r2,r0,r2
	cvthg	r2,-4(fp)
	divg3	_g,_g,r0
	cvtgh	r0,r0
	cvthg	r0,-4(fp)
	mulh3	_h,_h,r0
	movh	r0,-12(fp)
	subh3	_h,_h,r0
	addh2	_h,r0
	movh	r0,-12(fp)
	divh3	_h,_h,r0
	movh	r0,-12(fp)
	ret
	.set	L14,0x0
L16:
	subl2	$12,sp
	jbr 	L17
	.data
