# double log(arg) ; natural logarithm
# double log10(arg); base 10 log
# double arg
# if(arg<=0){ errno=EDOM; return(-1.7e+38); }
# nat. log computed from Hart&Cheney LOGE 2706 D=22.1
# J F Jarvis August 3, 1978
.set	EDOM,33
.globl	_log
.globl	_log10
.globl	_errno
.text
.align	1
_log10:
	.word	0x0
	bispsw	$0xe0
	movd	4(ap),-(sp)
	calls	$2,_log
	muld2	log10e,r0
	ret
.align	1
_log:
	.word	0x07c0
	bispsw	$0xe0
	movd	4(ap),r0
	jgtr	range
	movl	$EDOM,_errno
	movd	$0d-1.7e+38,r0	# machine dept max neg
	ret
range:
	extzv	$7,$8,r0,r10	# r10 = exp(arg)+128
	insv	$128,$7,$8,r0	# r0,r1: 0.5<=frac(arg)<1.0
	cmpd	r0,sqrt2d2
	jgeq	comp
	insv	$129,$7,$8,r0	# frac *= 2
	decl	r10				# exp -= 1
comp:
	subl2	$128,r10	# signed exp for scaled arg
	subd3	$0d1.0e+0,r0,r6	# Hart&cheney LOGE 2706
	addd2	$0d1.0e+0,r0
	divd2	r0,r6	# r6,r7= (frac-1)/(frac+1)
	muld3	r6,r6,r8
	polyd	r8,$3,pcoef
	muld2	r0,r6
	polyd	r8,$4,qcoef
	divd3	r0,r6,r0
	cvtld	r10,r2
	muld2	log2,r2	# r2,r3= loge(2**exp)
	addd2	r2,r0
	ret
.data
.align	2
pcoef:
	.double	0d-0.24550691103445385056e+2
	.double	0d0.23616053565907671809e+3
	.double	0d-0.54904361859132995001e+3
	.double	0d0.35621151669903912407e+3
qcoef:
	.double	0d0.10e+1
	.double	0d-0.35526251110400238735e+2
	.double	0d0.19375591463035879517e+3
	.double	0d-0.33389039541217149928e+3
	.double 0d0.17810575834951956204e+3
log10e:
	.double	0d0.43429448190325182765e+0
sqrt2d2:
	.double	0d0.70710678118654752440e+0
log2:
	.double	0d0.69314718055994530941e+0
