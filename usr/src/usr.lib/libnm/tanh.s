# double tanh(arg)
# double arg
# method: tanh(arg)=sinh(arg)/cosh(arg); if |arg|<0.5 sinh(9 is computed from
# a polynomial approx otherwise from exp().
# Only 1 call to exp() is made.
# J F Jarvis, August 17, 1978
.globl _tanh
.globl _exp
.text
.align 1
_tanh:
	.word	0x07c0
	bispsw	$0xe0
	clrl	r10	# sign bit
	movd	4(ap),r6
	jgeq	t1
	movw	$0x8000,r10
	xorw2	r10,r6	# co|arg|
t1:	cmpd	r6,$0d2.0e+1
	jleq	t2
	movd	$0d1.0e+0,r0
	xorw2	r10,r0
	ret
t2:	movd	r6,-(sp)
	calls	$2,_exp
	movd	r0,r8
	divd3	r8,$0d1.0e+0,-(sp)	# exp(-|arg|)
	cmpd	r6,$0d0.5e+0
	jgeq	t3
	muld3	r6,r6,r0	# |arg|<0.5, sinh() from poly approx
	polyd	r0,$5,pcoef
	muld2	r6,r0
	muld2	$0d2.0e+0,r0	
	jbr	t4
t3:	subd3	(sp),r8,r0
t4:				# r0,1 = 2*sinh(|arg|)
	addd3	(sp),r8,r2	# r2,3 = cosh(|arg|);
	divd2	r2,r0	# r0,1 = tanh(|arg|)
	xorw2	r10,r0	# r0,1 = tanh(arg)
	ret
.data
.align	2
pcoef:
	.double 0d0.251726188251e-7
	.double 0d0.275569807356154e-5
	.double 0d0.1984127027907999e-3
	.double 0d0.833333333307759961e-2
	.double 0d0.16666666666667212324e+0
	.double 0d0.99999999999999998116e+0
