# double atan(arg1); -pi/2 < atan < pi/2
#double atan2(arg1,arg2); -pi< atan2<pi
#double arg1,arg2
#method: range reduction to [sqrt(2)-1,sqrt(2)+1]
# followed by Hart&Cheney ARCTN 5076 D=17.55
# J. F. Jarvis August 8, 1978
.globl	_atan
.globl	_atan2
.text
.align	1
_atan:
	.word	0x03c0
	movd	4(ap),r0
	jgtr	a1
	mnegd	r0,r0	# atan(arg1), arg1<0
	bsbb	satan
	mnegd	r0,r0
	ret
a1:	bsbb	satan	# atan(arg1), arg1>=0
	ret
#
.align	1
_atan2:
	.word	0x03c0
	movd	4(ap),r0	# atan(arg1/arg2)
	movd	12(ap),r2
	addd3	r0,r2,r4
	cmpd	r0,r4
	jneq	b1
	tstd	r0
	jgeq	b2
	mnegd	pio2,r0
	ret
b2:	movd	pio2,r0
	ret
#
b1:	tstd	r2
	jgeq	b3
	divd2	r2,r0
	jleq	b4
	bsbb	satan	# arg1<0, arg2<0
	subd2	pi,r0
	ret
b4:	mnegd	r0,r0	# arg1>0, arg2<0
	bsbb	satan
	subd3	r0,pi,r0
	ret
#
b3:	divd2	r2,r0
	jleq	b5
	bsbb	satan	# arg1>0, arg2>0
	ret
b5:	mnegd	r0,r0	# arg1<0, arg2>0
	bsbb	satan
	mnegd	r0,r0
	ret
#
.globl	satan
satan:	# range reduction on positive arg(r0)
	cmpd	r0,sq2m1
	jgeq	c1
	bsbb	xatan
	rsb
c1:	cmpd	r0,sq2p1
	jleq	c2
	divd3	r0,$0d1.0e+0,r0
	bsbb	xatan
	subd3	r0,pio2,r0
	rsb
c2:	addd3	$0d1.0e+0,r0,r2
	subd2	$0d1.0e+0,r0
	divd2	r2,r0
	bsbb	xatan
	addd2	pio4,r0
	rsb
#
xatan:	# compute arctan(r0) for:sqrt(2)-1<r0<sqrt(2)+1
# Hart&Cheney ARCTN 5076 is evaluated
	movd	r0,r8
	muld3	r0,r0,r6
	polyd	r6,$4,pcoef
	muld2	r0,r8
	polyd	r6,$4,qcoef
	divd3	r0,r8,r0
	rsb
.data
.align	3
pcoef:
	.double 0d0.1589740288482307048e+0
	.double 0d0.66605790170092626575e+1
	.double 0d0.40969264832102256374e+2
	.double 0d0.77477687719204208616e+2
	.double 0d0.44541340059290680319e+2
qcoef:
	.double 0d1.0e+0
	.double 0d0.15503977551421987525e+2
	.double 0d0.62835930511032376833e+2
	.double 0d0.92324801072300974840e+2
	.double 0d0.44541340059290680444e+2
pio4: .double  0d0.78539816339744830961e+0
pio2: .double  0d1.57079632679489661923e+0
sq2p1: .double 0d2.41421356237309504880e+0
sq2m1: .double 0d0.41421356237309504880e+0
pi: .double 0d3.14159265358979323846e+0
