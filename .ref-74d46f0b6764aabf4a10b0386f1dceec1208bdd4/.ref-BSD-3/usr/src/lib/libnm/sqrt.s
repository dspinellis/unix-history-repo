# double sqrt(arg)
# double arg
# if(arg<0.0) { _errno=EDOM; return(0.0) }
# J. F. Jarvis August 2, 1978
.set	EDOM,98
.text
.align	1
.globl	_sqrt
.globl	_errno
_sqrt:
	.word	0x0c0
	movd	4(ap),r4
	jgtr	range
	jeql	retz
	movl	$EDOM,_errno
retz:
	clrd	r0
	ret
#
range:
	extzv	$7,$8,r4,r6	# r6 is exponent of arg
	insv	$128,$7,$8,r4	# r2: 0.5<=fraction(arg)<1.0
	incl	r6
	clrl	r7
	ediv	$2,r6,r6,r7	# r6=(exp+1)/2; r7=(exp+1)%2
	addb2	$64,r6	# r6 is correct exponent for result
	polyf	r4,$4,pcoef	# init estimate of sqrt(frac)
						# Hart&Cheney SQRT 0132 D=5.1
	divd3	r0,r4,r2	# Newtons method, 2 iterations
	addd2	r2,r0
	muld2	$0d0.5e+0,r0
	divd3	r0,r4,r2	# Hart&Cheney 6.1.7
	addd2	r2,r0	#d=21 at exit
	muld2	hc[r7],r0	# *sqrt(2) requ for even org exp.
	insv	r6,$7,$8,r0	# insert correct exp.
	ret
.data
.align	3
pcoef:
	.float 0f-0.1214683825e+0
	.float 0f0.5010420763e+0
	.float 0f-0.9093210498e+0
	.float 0f0.1300669049e+1
	.float 0f0.2290699453e+0
hc:		.double 0d0.35355339059327376220e+0	# sqrt(2)/4
		.double 0d0.5e+0
