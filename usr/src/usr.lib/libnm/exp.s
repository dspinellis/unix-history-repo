# double exp(arg)
# double arg; 0<= arg< 88
# double exp10(arg)
# method: range reduction to [-0.5,0.5], Hart&Chenet EXPB 1067
# J F Jarvis, August 5, 1978
.set	ERANGE,34
.globl	_exp
.globl	_exp10
.globl	_errno
.text
.align	1
_exp10:
	.word	0x07c0
	bispsw	$0xe0
	muld3	4(ap),loge10,r0
	jbr	argtst
.align	1
_exp:
	.word	0x07c0
	bispsw	$0xe0
	movd	4(ap),r0
argtst:
	jnequ	smlarg
	movd	$0d1.0e+0,r0
	ret
smlarg:
	cmpd	r0,minarg
	jgeq	lrgarg
	mnegd	huge,r0
	ret
lrgarg:
	cmpd	r0,maxarg
	jleq	range
	movl	$ERANGE,_errno
	movd	huge,r0
	ret
range:
	emodd	log2e,log2ex,r0,r10,r6	# r10=int(arg), r6=frac(arg)
	tstd	r0
	jgtr	l1
	addd2	$0d0.5e+0,r6
	subw2	$1,r10
	jbr	l2
l1:	subd2	$0d0.5e+0,r6
l2:			# Hart&Cheney EXPB 1067
	muld3	r6,r6,r8	# range [-.5,.5] D=18.1
	polyd	r8,$2,pcoef
	muld2	r0,r6
	polyd	r8,$2,qcoef
	subd3	r6,r0,r2
	addd2	r6,r0
	divd2	r2,r0
	muld2	sqrt2,r0
	extzv $7,$8,r0,r2
	addl2	r2,r10
	insv	r10,$7,$8,r0	# load correct exponent
	ret
.data
.align	2
pcoef:
	.double	0d0.23093347753750233624e-1
	.double	0d0.20202065651286927227886e+2
	.double	0d0.15139067990543389159e+4
qcoef:
	.double	0d0.1e+1
	.double	0d0.23318421142748162379e+3
	.double	0d0.43682116627275584985e+4
minarg:
	.double	0d-88.028e+0
maxarg:
	.double	0d88.028e+0
huge:	.double	0d1.7e+38
loge10:
	.double	0d2.30258509299404568401e+0
sqrt2:
	.double	0d1.41421356237309504880e+0
log2e:
	.double	0d1.44269504088896340735e+0
log2ex:
	.byte	0xbb
