# 
# Copyright (c) 1985 Regents of the University of California.
# 
# Use and reproduction of this software are granted  in  accordance  with
# the terms and conditions specified in  the  Berkeley  Software  License
# Agreement (in particular, this entails acknowledgement of the programs'
# source, and inclusion of this notice) with the additional understanding
# that  all  recipients  should regard themselves as participants  in  an
# ongoing  research  project and hence should  feel  obligated  to report
# their  experiences (good or bad) with these elementary function  codes,
# using "sendbug 4bsd-bugs@BERKELEY", to the authors.
#

# @(#)atan2.s	1.2 (Berkeley) 8/21/85

# ATAN2(Y,X)
# RETURN ARG (X+iY)
# VAX D FORMAT (56 BITS PRECISION)
# CODED IN VAX ASSEMBLY LANGUAGE BY K.C. NG, 4/16/85; 
#
#	
# Method :
#	1. Reduce y to positive by atan2(y,x)=-atan2(-y,x).
#	2. Reduce x to positive by (if x and y are unexceptional): 
#		ARG (x+iy) = arctan(y/x)   	   ... if x > 0,
#		ARG (x+iy) = pi - arctan[y/(-x)]   ... if x < 0,
#	3. According to the integer k=4t+0.25 truncated , t=y/x, the argument 
#	   is further reduced to one of the following intervals and the 
#	   arctangent of y/x is evaluated by the corresponding formula:
#
#          [0,7/16]	   atan(y/x) = t - t^3*(a1+t^2*(a2+...(a10+t^2*a11)...)
#	   [7/16,11/16]    atan(y/x) = atan(1/2) + atan( (y-x/2)/(x+y/2) )
#	   [11/16.19/16]   atan(y/x) = atan( 1 ) + atan( (y-x)/(x+y) )
#	   [19/16,39/16]   atan(y/x) = atan(3/2) + atan( (y-1.5x)/(x+1.5y) )
#	   [39/16,INF]     atan(y/x) = atan(INF) + atan( -x/y )
#
# Special cases:
# Notations: atan2(y,x) == ARG (x+iy) == ARG(x,y).
#
#	ARG( NAN , (anything) ) is NaN;
#	ARG( (anything), NaN ) is NaN;
#	ARG(+(anything but NaN), +-0) is +-0  ;
#	ARG(-(anything but NaN), +-0) is +-PI ;
#	ARG( 0, +-(anything but 0 and NaN) ) is +-PI/2;
#	ARG( +INF,+-(anything but INF and NaN) ) is +-0 ;
#	ARG( -INF,+-(anything but INF and NaN) ) is +-PI;
#	ARG( +INF,+-INF ) is +-PI/4 ;
#	ARG( -INF,+-INF ) is +-3PI/4;
#	ARG( (anything but,0,NaN, and INF),+-INF ) is +-PI/2;
#
# Accuracy:
#	atan2(y,x) returns the exact ARG(x+iy) nearly rounded. 
#	
	.text
	.align 1
	.globl	_atan2
_atan2 :
	.word	0x0ff4
	movq	4(ap),r2		# r2 = y
	movq	12(ap),r4		# r4 = x
	bicw3	$0x7f,r2,r0
	bicw3	$0x7f,r4,r1
	cmpw	r0,$0x8000		# y is the reserved operand
	jeql	resop
	cmpw	r1,$0x8000		# x is the reserved operand
	jeql	resop
	subl2	$8,sp
	bicw3	$0x7fff,r2,-4(fp)	# copy y sign bit to -4(fp)
	bicw3	$0x7fff,r4,-8(fp)	# copy x sign bit to -8(fp)
	cmpd	r4,$0x4080		# x = 1.0 ?	
	bneq	xnot1
	movq	r2,r0
	bicw2	$0x8000,r0		# t = |y|
	movq	r0,r2			# y = |y|
	brb	begin
xnot1:
	bicw3	$0x807f,r2,r11		# yexp
	jeql	yeq0			# if y=0 goto yeq0
	bicw3	$0x807f,r4,r10		# xexp
	jeql	pio2			# if x=0 goto pio2
	subw2	r10,r11			# k = yexp - xexp
	cmpw	r11,$0x2000		# k >= 64 (exp) ?
	jgeq	pio2			# atan2 = +-pi/2
	divd3	r4,r2,r0		# t = y/x  never overflow
	bicw2	$0x8000,r0		# t > 0
	bicw2	$0xff80,r2		# clear the exponent of y
	bicw2	$0xff80,r4		# clear the exponent of x
	bisw2	$0x4080,r2		# normalize y to [1,2)
	bisw2	$0x4080,r4		# normalize x to [1,2)
	subw2	r11,r4			# scale x so that yexp-xexp=k
begin:
	cmpw	r0,$0x411c		# t : 39/16
	jgeq	L50
	addl3	$0x180,r0,r10		# 8*t
	cvtrfl	r10,r10			# [8*t] rounded to int
	ashl	$-1,r10,r10		# [8*t]/2
	casel	r10,$0,$4
L1:	
	.word	L20-L1
	.word	L20-L1
	.word	L30-L1
	.word	L40-L1
	.word	L40-L1
L10:	
	movq	$0xb4d9940f985e407b,r6	# Hi=.98279372324732906796d0
	movq	$0x21b1879a3bc2a2fc,r8	# Lo=-.17092002525602665777d-17
	subd3	r4,r2,r0		# y-x
	addw2	$0x80,r0		# 2(y-x)
	subd2	r4,r0			# 2(y-x)-x
	addw2	$0x80,r4		# 2x	
	movq	r2,r10
	addw2	$0x80,r10		# 2y
	addd2	r10,r2			# 3y
	addd2	r4,r2			# 3y+2x
	divd2	r2,r0			# (2y-3x)/(2x+3y)
	brw	L60
L20:	
	cmpw	r0,$0x3280		# t : 2**(-28)
	jlss	L80
	clrq	r6			# Hi=r6=0, Lo=r8=0
	clrq	r8
	brw	L60
L30:	
	movq	$0xda7b2b0d63383fed,r6	# Hi=.46364760900080611433d0
	movq	$0xf0ea17b2bf912295,r8	# Lo=.10147340032515978826d-17
	movq	r2,r0
	addw2	$0x80,r0		# 2y
	subd2	r4,r0			# 2y-x
	addw2	$0x80,r4		# 2x
	addd2	r2,r4			# 2x+y
	divd2	r4,r0 			# (2y-x)/(2x+y)
	brb	L60
L50:	
	movq	$0x68c2a2210fda40c9,r6	# Hi=1.5707963267948966135d1
	movq	$0x06e0145c26332326,r8	# Lo=.22517417741562176079d-17
	cmpw	r0,$0x5100		# y : 2**57
	bgeq	L90
	divd3	r2,r4,r0
	bisw2	$0x8000,r0 		# -x/y
	brb	L60
L40:	
	movq	$0x68c2a2210fda4049,r6	# Hi=.78539816339744830676d0
	movq	$0x06e0145c263322a6,r8	# Lo=.11258708870781088040d-17
	subd3	r4,r2,r0		# y-x
	addd2	r4,r2			# y+x
	divd2	r2,r0			# (y-x)/(y+x)
L60:	
	movq	r0,r10
	muld2	r0,r0
	polyd	r0,$12,ptable
	muld2	r10,r0
	subd2	r0,r8
	addd3	r8,r10,r0
	addd2	r6,r0
L80:	
	movw	-8(fp),r2
	bneq	pim
	bisw2	-4(fp),r0		# return sign(y)*r0
	ret
L90:					# x >= 2**25 
	movq	r6,r0
	brb	L80
pim:
	subd3	r0,$0x68c2a2210fda4149,r0	# pi-t
	bisw2	-4(fp),r0
	ret
yeq0:
	movw	-8(fp),r2		
	beql	zero			# if sign(x)=1 return pi
	movq	$0x68c2a2210fda4149,r0	# pi=3.1415926535897932270d1
	ret
zero:
	clrq	r0			# return 0
	ret
pio2:
	movq	$0x68c2a2210fda40c9,r0	# pi/2=1.5707963267948966135d1
	bisw2	-4(fp),r0		# return sign(y)*pi/2
	ret
resop:
	movq	$0x8000,r0		# propagate the reserved operand
	ret
	.align 2
ptable:
	.quad	0xb50f5ce96e7abd60
	.quad	0x51e44a42c1073e02
	.quad	0x3487e3289643be35
	.quad	0xdb62066dffba3e54
	.quad	0xcf8e2d5199abbe70
	.quad	0x26f39cb884883e88
	.quad	0x135117d18998be9d
	.quad	0x602ce9742e883eba
	.quad	0xa35ad0be8e38bee3
	.quad	0xffac922249243f12
	.quad	0x7f14ccccccccbf4c
	.quad	0xaa8faaaaaaaa3faa
	.quad	0x0000000000000000
