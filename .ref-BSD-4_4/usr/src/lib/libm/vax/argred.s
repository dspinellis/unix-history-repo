# Copyright (c) 1985, 1993
#	The Regents of the University of California.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#	This product includes software developed by the University of
#	California, Berkeley and its contributors.
# 4. Neither the name of the University nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
#	@(#)argred.s	8.1 (Berkeley) 6/4/93
#
	.data
	.align	2
_sccsid:
.asciz	"@(#)argred.s	1.1 (Berkeley) 8/21/85; 8.1 (ucb.elefunt) 6/4/93"

#  libm$argred implements Bob Corbett's argument reduction and
#  libm$sincos implements Peter Tang's double precision sin/cos.
#  
#  Note: The two entry points libm$argred and libm$sincos are meant
#        to be used only by _sin, _cos and _tan.
#
# method: true range reduction to [-pi/4,pi/4], P. Tang  &  B. Corbett
# S. McDonald, April 4,  1985
#
	.globl	libm$argred
	.globl	libm$sincos
	.text
	.align	1

libm$argred:
#
#  Compare the argument with the largest possible that can
#  be reduced by table lookup.  r3 := |x|  will be used in  table_lookup .
#
	movd	r0,r3
	bgeq	abs1
	mnegd	r3,r3
abs1:
	cmpd	r3,$0d+4.55530934770520019583e+01
	blss	small_arg
	jsb	trigred
	rsb
small_arg:
	jsb	table_lookup
	rsb
#
#  At this point,
#	   r0  contains the quadrant number, 0, 1, 2, or 3;
#	r2/r1  contains the reduced argument as a D-format number;
#  	   r3  contains a F-format extension to the reduced argument;
#          r4  contains a  0 or 1  corresponding to a  sin or cos  entry.
#
libm$sincos:
#
#  Compensate for a cosine entry by adding one to the quadrant number.
#
	addl2	r4,r0
#
#  Polyd clobbers  r5-r0 ;  save  X  in  r7/r6 .
#  This can be avoided by rewriting  trigred .
#
	movd	r1,r6
#
#  Likewise, save  alpha  in  r8 .
#  This can be avoided by rewriting  trigred .
#
	movf	r3,r8
#
#  Odd or even quadrant?  cosine if odd, sine otherwise.
#  Save  floor(quadrant/2) in  r9  ; it determines the final sign.
#
	rotl	$-1,r0,r9
	blss	cosine
sine:
	muld2	r1,r1		# Xsq = X * X
	cmpw	$0x2480,r1	# [zl] Xsq > 2^-56?
	blss	1f		# [zl] yes, go ahead and do polyd
	clrq	r1		# [zl] work around 11/780 FPA polyd bug
1:
	polyd	r1,$7,sin_coef	# Q = P(Xsq) , of deg 7
	mulf3	$0f3.0,r8,r4	# beta = 3 * alpha
	mulf2	r0,r4		# beta = Q * beta
	addf2	r8,r4		# beta = alpha + beta
	muld2	r6,r0		# S(X) = X * Q
#	cvtfd	r4,r4		... r5 = 0 after a polyd.
	addd2	r4,r0		# S(X) = beta + S(X)
	addd2	r6,r0		# S(X) = X + S(X)
	brb	done
cosine:
	muld2	r6,r6		# Xsq = X * X
	beql	zero_arg
	mulf2	r1,r8		# beta = X * alpha
	polyd	r6,$7,cos_coef	# Q = P'(Xsq) , of deg 7
	subd3	r0,r8,r0	# beta = beta - Q
	subw2	$0x80,r6	# Xsq = Xsq / 2
	addd2	r0,r6		# Xsq = Xsq + beta
zero_arg:
	subd3	r6,$0d1.0,r0	# C(X) = 1 - Xsq
done:
	blbc	r9,even
	mnegd	r0,r0
even:
	rsb

.data
.align	2

sin_coef:
	.double	0d-7.53080332264191085773e-13	# s7 = 2^-29 -1.a7f2504ffc49f8..
	.double	0d+1.60573519267703489121e-10	# s6 = 2^-21  1.611adaede473c8..
	.double	0d-2.50520965150706067211e-08	# s5 = 2^-1a -1.ae644921ed8382..
	.double	0d+2.75573191800593885716e-06	# s4 = 2^-13  1.71de3a4b884278..
	.double	0d-1.98412698411850507950e-04	# s3 = 2^-0d -1.a01a01a0125e7d..
	.double	0d+8.33333333333325688985e-03	# s2 = 2^-07  1.11111111110e50
	.double	0d-1.66666666666666664354e-01	# s1 = 2^-03 -1.55555555555554
	.double	0d+0.00000000000000000000e+00	# s0 = 0

cos_coef:
	.double	0d-1.13006966202629430300e-11	# s7 = 2^-25 -1.8D9BA04D1374BE..
	.double	0d+2.08746646574796004700e-09	# s6 = 2^-1D  1.1EE632650350BA..
	.double	0d-2.75573073031284417300e-07	# s5 = 2^-16 -1.27E4F31411719E..
	.double	0d+2.48015872682668025200e-05	# s4 = 2^-10  1.A01A0196B902E8..
	.double	0d-1.38888888888464709200e-03	# s3 = 2^-0A -1.6C16C16C11FACE..
	.double	0d+4.16666666666664761400e-02	# s2 = 2^-05  1.5555555555539E
	.double	0d+0.00000000000000000000e+00	# s1 = 0
	.double	0d+0.00000000000000000000e+00	# s0 = 0

#
#  Multiples of  pi/2  expressed as the sum of three doubles,
#
#  trailing:	n * pi/2 ,  n = 0, 1, 2, ..., 29
#			trailing[n] ,
#
#  middle:	n * pi/2 ,  n = 0, 1, 2, ..., 29
#			middle[n]   ,
#
#  leading:	n * pi/2 ,  n = 0, 1, 2, ..., 29
#			leading[n]  ,
#
#	where
#		leading[n]  := (n * pi/2)  rounded,
#		middle[n]   := (n * pi/2  -  leading[n])  rounded,
#		trailing[n] := (( n * pi/2 - leading[n]) - middle[n])  rounded .

trailing:
	.double	0d+0.00000000000000000000e+00	#  0 * pi/2  trailing
	.double	0d+4.33590506506189049611e-35	#  1 * pi/2  trailing
	.double	0d+8.67181013012378099223e-35	#  2 * pi/2  trailing
	.double	0d+1.30077151951856714215e-34	#  3 * pi/2  trailing
	.double	0d+1.73436202602475619845e-34	#  4 * pi/2  trailing
	.double	0d-1.68390735624352669192e-34	#  5 * pi/2  trailing
	.double	0d+2.60154303903713428430e-34	#  6 * pi/2  trailing
	.double	0d-8.16726343231148352150e-35	#  7 * pi/2  trailing
	.double	0d+3.46872405204951239689e-34	#  8 * pi/2  trailing
	.double	0d+3.90231455855570147991e-34	#  9 * pi/2  trailing
	.double	0d-3.36781471248705338384e-34	# 10 * pi/2  trailing
	.double	0d-1.06379439835298071785e-33	# 11 * pi/2  trailing
	.double	0d+5.20308607807426856861e-34	# 12 * pi/2  trailing
	.double	0d+5.63667658458045770509e-34	# 13 * pi/2  trailing
	.double	0d-1.63345268646229670430e-34	# 14 * pi/2  trailing
	.double	0d-1.19986217995610764801e-34	# 15 * pi/2  trailing
	.double	0d+6.93744810409902479378e-34	# 16 * pi/2  trailing
	.double	0d-8.03640094449267300110e-34	# 17 * pi/2  trailing
	.double	0d+7.80462911711140295982e-34	# 18 * pi/2  trailing
	.double	0d-7.16921993148029483506e-34	# 19 * pi/2  trailing
	.double	0d-6.73562942497410676769e-34	# 20 * pi/2  trailing
	.double	0d-6.30203891846791677593e-34	# 21 * pi/2  trailing
	.double	0d-2.12758879670596143570e-33	# 22 * pi/2  trailing
	.double	0d+2.53800212047402350390e-33	# 23 * pi/2  trailing
	.double	0d+1.04061721561485371372e-33	# 24 * pi/2  trailing
	.double	0d+6.11729905311472319056e-32	# 25 * pi/2  trailing
	.double	0d+1.12733531691609154102e-33	# 26 * pi/2  trailing
	.double	0d-3.70049587943078297272e-34	# 27 * pi/2  trailing
	.double	0d-3.26690537292459340860e-34	# 28 * pi/2  trailing
	.double	0d-1.14812616507957271361e-34	# 29 * pi/2  trailing

middle:
	.double	0d+0.00000000000000000000e+00	#  0 * pi/2  middle
	.double	0d+5.72118872610983179676e-18	#  1 * pi/2  middle
	.double	0d+1.14423774522196635935e-17	#  2 * pi/2  middle
	.double	0d-3.83475850529283316309e-17	#  3 * pi/2  middle
	.double	0d+2.28847549044393271871e-17	#  4 * pi/2  middle
	.double	0d-2.69052076007086676522e-17	#  5 * pi/2  middle
	.double	0d-7.66951701058566632618e-17	#  6 * pi/2  middle
	.double	0d-1.54628301484890040587e-17	#  7 * pi/2  middle
	.double	0d+4.57695098088786543741e-17	#  8 * pi/2  middle
	.double	0d+1.07001849766246313192e-16	#  9 * pi/2  middle
	.double	0d-5.38104152014173353044e-17	# 10 * pi/2  middle
	.double	0d-2.14622680169080983801e-16	# 11 * pi/2  middle
	.double	0d-1.53390340211713326524e-16	# 12 * pi/2  middle
	.double	0d-9.21580002543456677056e-17	# 13 * pi/2  middle
	.double	0d-3.09256602969780081173e-17	# 14 * pi/2  middle
	.double	0d+3.03066796603896507006e-17	# 15 * pi/2  middle
	.double	0d+9.15390196177573087482e-17	# 16 * pi/2  middle
	.double	0d+1.52771359575124969107e-16	# 17 * pi/2  middle
	.double	0d+2.14003699532492626384e-16	# 18 * pi/2  middle
	.double	0d-1.68853170360202329427e-16	# 19 * pi/2  middle
	.double	0d-1.07620830402834670609e-16	# 20 * pi/2  middle
	.double	0d+3.97700719404595604379e-16	# 21 * pi/2  middle
	.double	0d-4.29245360338161967602e-16	# 22 * pi/2  middle
	.double	0d-3.68013020380794313406e-16	# 23 * pi/2  middle
	.double	0d-3.06780680423426653047e-16	# 24 * pi/2  middle
	.double	0d-2.45548340466059054318e-16	# 25 * pi/2  middle
	.double	0d-1.84316000508691335411e-16	# 26 * pi/2  middle
	.double	0d-1.23083660551323675053e-16	# 27 * pi/2  middle
	.double	0d-6.18513205939560162346e-17	# 28 * pi/2  middle
	.double	0d-6.18980636588357585202e-19	# 29 * pi/2  middle

leading:
	.double	0d+0.00000000000000000000e+00	#  0 * pi/2  leading
	.double	0d+1.57079632679489661351e+00	#  1 * pi/2  leading
	.double	0d+3.14159265358979322702e+00	#  2 * pi/2  leading
	.double	0d+4.71238898038468989604e+00	#  3 * pi/2  leading
	.double	0d+6.28318530717958645404e+00	#  4 * pi/2  leading
	.double	0d+7.85398163397448312306e+00	#  5 * pi/2  leading
	.double	0d+9.42477796076937979208e+00	#  6 * pi/2  leading
	.double	0d+1.09955742875642763501e+01	#  7 * pi/2  leading
	.double	0d+1.25663706143591729081e+01	#  8 * pi/2  leading
	.double	0d+1.41371669411540694661e+01	#  9 * pi/2  leading
	.double	0d+1.57079632679489662461e+01	# 10 * pi/2  leading
	.double	0d+1.72787595947438630262e+01	# 11 * pi/2  leading
	.double	0d+1.88495559215387595842e+01	# 12 * pi/2  leading
	.double	0d+2.04203522483336561422e+01	# 13 * pi/2  leading
	.double	0d+2.19911485751285527002e+01	# 14 * pi/2  leading
	.double	0d+2.35619449019234492582e+01	# 15 * pi/2  leading
	.double	0d+2.51327412287183458162e+01	# 16 * pi/2  leading
	.double	0d+2.67035375555132423742e+01	# 17 * pi/2  leading
	.double	0d+2.82743338823081389322e+01	# 18 * pi/2  leading
	.double	0d+2.98451302091030359342e+01	# 19 * pi/2  leading
	.double	0d+3.14159265358979324922e+01	# 20 * pi/2  leading
	.double	0d+3.29867228626928286062e+01	# 21 * pi/2  leading
	.double	0d+3.45575191894877260523e+01	# 22 * pi/2  leading
	.double	0d+3.61283155162826226103e+01	# 23 * pi/2  leading
	.double	0d+3.76991118430775191683e+01	# 24 * pi/2  leading
	.double	0d+3.92699081698724157263e+01	# 25 * pi/2  leading
	.double	0d+4.08407044966673122843e+01	# 26 * pi/2  leading
	.double	0d+4.24115008234622088423e+01	# 27 * pi/2  leading
	.double	0d+4.39822971502571054003e+01	# 28 * pi/2  leading
	.double	0d+4.55530934770520019583e+01	# 29 * pi/2  leading

twoOverPi:
	.double	0d+6.36619772367581343076e-01
	.text
	.align	1

table_lookup:
	muld3	r3,twoOverPi,r0
	cvtrdl	r0,r0			# n = nearest int to ((2/pi)*|x|) rnded
	mull3	$8,r0,r5
	subd2	leading(r5),r3		# p = (|x| - leading n*pi/2) exactly
	subd3	middle(r5),r3,r1	# q = (p - middle  n*pi/2) rounded
	subd2	r1,r3			# r = (p - q)
	subd2	middle(r5),r3		# r =  r - middle  n*pi/2
	subd2	trailing(r5),r3		# r =  r - trailing n*pi/2  rounded
#
#  If the original argument was negative,
#  negate the reduce argument and
#  adjust the octant/quadrant number.
#
	tstw	4(ap)
	bgeq	abs2
	mnegf	r1,r1
	mnegf	r3,r3
#	subb3	r0,$8,r0	...used for  pi/4  reduction -S.McD
	subb3	r0,$4,r0
abs2:
#
#  Clear all unneeded octant/quadrant bits.
#
#	bicb2	$0xf8,r0	...used for  pi/4  reduction -S.McD
	bicb2	$0xfc,r0
	rsb
#
#						p.0
	.text
	.align	2
#
# Only 256 (actually 225) bits of 2/pi are needed for VAX double
# precision; this was determined by enumerating all the nearest
# machine integer multiples of pi/2 using continued fractions.
# (8a8d3673775b7ff7 required the most bits.)		-S.McD
#
	.long	0
	.long	0
	.long	0xaef1586d
	.long	0x9458eaf7
	.long	0x10e4107f
	.long	0xd8a5664f
	.long	0x4d377036
	.long	0x09d5f47d
	.long	0x91054a7f
	.long	0xbe60db93
bits2opi:
	.long	0x00000028
	.long	0
#
#  Note: wherever you see the word `octant', read `quadrant'.
#  Currently this code is set up for  pi/2  argument reduction.
#  By uncommenting/commenting the appropriate lines, it will
#  also serve as a  pi/4  argument reduction code.
#  

#						p.1
#  Trigred  preforms argument reduction
#  for the trigonometric functions.  It
#  takes one input argument, a D-format
#  number in  r1/r0 .  The magnitude of
#  the input argument must be greater
#  than or equal to  1/2 .  Trigred produces
#  three results:  the number of the octant
#  occupied by the argument, the reduced 
#  argument, and an extension of the
#  reduced argument.  The octant number is 
#  returned in  r0 .  The reduced argument 
#  is returned as a D-format number in 
#  r2/r1 .  An 8 bit extension of the 
#  reduced argument is returned as an 
#  F-format number in r3.
#						p.2
trigred:
#
#  Save the sign of the input argument.
#
	movw	r0,-(sp)
#
#  Extract the exponent field.
#
	extzv	$7,$7,r0,r2
#
#  Convert the fraction part of the input
#  argument into a quadword integer.
#
	bicw2	$0xff80,r0
	bisb2	$0x80,r0	# -S.McD
	rotl	$16,r0,r0
	rotl	$16,r1,r1
#
#  If  r1  is negative, add  1  to  r0 .  This
#  adjustment is made so that the two's
#  complement multiplications done later
#  will produce unsigned results.
#
	bgeq	posmid
	incl	r0
posmid:
#						p.3
#
#  Set  r3  to the address of the first quadword
#  used to obtain the needed portion of  2/pi .
#  The address is longword aligned to ensure
#  efficient access.
#
	ashl	$-3,r2,r3
	bicb2	$3,r3
	subl3	r3,$bits2opi,r3
#
#  Set  r2  to the size of the shift needed to 
#  obtain the correct portion of  2/pi .
#
	bicb2	$0xe0,r2
#						p.4
#
#  Move the needed  128  bits of  2/pi  into
#  r11 - r8 .  Adjust the numbers to allow
#  for unsigned multiplication.
#
	ashq	r2,(r3),r10

	subl2	$4,r3
	ashq	r2,(r3),r9
	bgeq	signoff1
	incl	r11
signoff1:
	subl2	$4,r3
	ashq	r2,(r3),r8
	bgeq	signoff2
	incl	r10
signoff2:
	subl2	$4,r3
	ashq	r2,(r3),r7
	bgeq	signoff3
	incl	r9
signoff3:
#						p.5
#
#  Multiply the contents of  r0/r1  by the 
#  slice of  2/pi  in  r11 - r8 .
#
	emul	r0,r8,$0,r4
	emul	r0,r9,r5,r5
	emul	r0,r10,r6,r6

	emul	r1,r8,$0,r7
	emul	r1,r9,r8,r8
	emul	r1,r10,r9,r9
	emul	r1,r11,r10,r10

	addl2	r4,r8
	adwc	r5,r9
	adwc	r6,r10
#						p.6
#
#  If there are more than five leading zeros
#  after the first two quotient bits or if there
#  are more than five leading ones after the first
#  two quotient bits, generate more fraction bits.
#  Otherwise, branch to code to produce the result.
#
	bicl3	$0xc1ffffff,r10,r4
	beql	more1
	cmpl	$0x3e000000,r4
	bneq	result
more1:
#						p.7
#
#  generate another  32  result bits.
#
	subl2	$4,r3
	ashq	r2,(r3),r5
	bgeq	signoff4

	emul	r1,r6,$0,r4
	addl2	r1,r5
	emul	r0,r6,r5,r5
	addl2	r0,r6
	brb	addbits1

signoff4:
	emul	r1,r6,$0,r4
	emul	r0,r6,r5,r5

addbits1:
	addl2	r5,r7
	adwc	r6,r8
	adwc	$0,r9
	adwc	$0,r10
#						p.8
#
#  Check for massive cancellation.
#
	bicl3	$0xc0000000,r10,r6
#	bneq	more2			-S.McD  Test was backwards
	beql	more2
	cmpl	$0x3fffffff,r6
	bneq	result
more2:
#						p.9
#
#  If massive cancellation has occurred,
#  generate another  24  result bits.
#  Testing has shown there will always be 
#  enough bits after this point.
#
	subl2	$4,r3
	ashq	r2,(r3),r5
	bgeq	signoff5

	emul	r0,r6,r4,r5
	addl2	r0,r6
	brb	addbits2

signoff5:
	emul	r0,r6,r4,r5

addbits2:
	addl2	r6,r7
	adwc	$0,r8
	adwc	$0,r9
	adwc	$0,r10
#						p.10
#
#  The following code produces the reduced
#  argument from the product bits contained
#  in  r10 - r7 .
#
result:
#
#  Extract the octant number from  r10 .
#
#	extzv	$29,$3,r10,r0	...used for  pi/4  reduction -S.McD
	extzv	$30,$2,r10,r0
#
#  Clear the octant bits in  r10 .
#
#	bicl2	$0xe0000000,r10	...used for  pi/4  reduction -S.McD
	bicl2	$0xc0000000,r10
#
#  Zero the sign flag.
#
	clrl	r5
#						p.11
#
#  Check to see if the fraction is greater than
#  or equal to one-half.  If it is, add one 
#  to the octant number, set the sign flag
#  on, and replace the fraction with  1 minus
#  the fraction.
#
#	bitl	$0x10000000,r10		...used for  pi/4  reduction -S.McD
	bitl	$0x20000000,r10
	beql	small
	incl	r0
	incl	r5
#	subl3	r10,$0x1fffffff,r10	...used for  pi/4  reduction -S.McD
	subl3	r10,$0x3fffffff,r10
	mcoml	r9,r9
	mcoml	r8,r8
	mcoml	r7,r7
small:
#						p.12
#
##  Test whether the first  29  bits of the ...used for  pi/4  reduction -S.McD
#  Test whether the first  30  bits of the 
#  fraction are zero.
#
	tstl	r10
	beql	tiny
#
#  Find the position of the first one bit in  r10 .
#
	cvtld	r10,r1
	extzv	$7,$7,r1,r1
#
#  Compute the size of the shift needed.
#
	subl3	r1,$32,r6
#
#  Shift up the high order  64  bits of the
#  product.
#
	ashq	r6,r9,r10
	ashq	r6,r8,r9
	brb	mult
#						p.13
#
#  Test to see if the sign bit of  r9  is on.
#
tiny:
	tstl	r9
	bgeq	tinier
#
#  If it is, shift the product bits up  32  bits.
#
	movl	$32,r6
	movq	r8,r10
	tstl	r10
	brb	mult
#						p.14
#
#  Test whether  r9  is zero.  It is probably
#  impossible for both  r10  and  r9  to be
#  zero, but until proven to be so, the test
#  must be made.
#
tinier:
	beql	zero
#
#  Find the position of the first one bit in  r9 .
#
	cvtld	r9,r1
	extzv	$7,$7,r1,r1
#
#  Compute the size of the shift needed.
#
	subl3	r1,$32,r1
	addl3	$32,r1,r6
#
#  Shift up the high order  64  bits of the
#  product.
#
	ashq	r1,r8,r10
	ashq	r1,r7,r9
	brb	mult
#						p.15
#
#  The following code sets the reduced
#  argument to zero.
#
zero:
	clrl	r1
	clrl	r2
	clrl	r3
	brw	return
#						p.16
#
#  At this point,  r0  contains the octant number,
#  r6  indicates the number of bits the fraction
#  has been shifted,  r5  indicates the sign of
#  the fraction,  r11/r10  contain the high order
#  64  bits of the fraction, and the condition
#  codes indicate where the sign bit of  r10
#  is on.  The following code multiplies the
#  fraction by  pi/2 .
#
mult:
#
#  Save  r11/r10  in  r4/r1 .		-S.McD
	movl	r11,r4
	movl	r10,r1
#
#  If the sign bit of  r10  is on, add  1  to  r11 .
#
	bgeq	signoff6
	incl	r11
signoff6:
#						p.17
#
#  Move  pi/2  into  r3/r2 .
#
	movq	$0xc90fdaa22168c235,r2
#
#  Multiply the fraction by the portion of  pi/2
#  in  r2 .
#
	emul	r2,r10,$0,r7
	emul	r2,r11,r8,r7
#
#  Multiply the fraction by the portion of  pi/2 
#  in  r3 .
	emul	r3,r10,$0,r9
	emul	r3,r11,r10,r10
#
#  Add the product bits together.
#
	addl2	r7,r9
	adwc	r8,r10
	adwc	$0,r11
#
#  Compensate for not sign extending  r8  above.-S.McD
#
	tstl	r8
	bgeq	signoff6a
	decl	r11
signoff6a:
#
#  Compensate for  r11/r10  being unsigned.	-S.McD
#
	addl2	r2,r10
	adwc	r3,r11
#
#  Compensate for  r3/r2  being unsigned.	-S.McD
#
	addl2	r1,r10
	adwc	r4,r11
#						p.18
#
#  If the sign bit of  r11  is zero, shift the
#  product bits up one bit and increment  r6 .
#
	blss	signon
	incl	r6
	ashq	$1,r10,r10
	tstl	r9
	bgeq	signoff7
	incl	r10
signoff7:
signon:
#						p.19
#
#  Shift the  56  most significant product
#  bits into  r9/r8 .  The sign extension
#  will be handled later.
#
	ashq	$-8,r10,r8
#
#  Convert the low order  8  bits of  r10
#  into an F-format number.
#
	cvtbf	r10,r3
#
#  If the result of the conversion was
#  negative, add  1  to  r9/r8 .
#
	bgeq	chop
	incl	r8
	adwc	$0,r9
#
#  If  r9  is now zero, branch to special
#  code to handle that possibility.
#
	beql	carryout
chop:
#						p.20
#
#  Convert the number in  r9/r8  into
#  D-format number in  r2/r1 .
#
	rotl	$16,r8,r2
	rotl	$16,r9,r1
#
#  Set the exponent field to the appropriate
#  value.  Note that the extra bits created by
#  sign extension are now eliminated.
#
	subw3	r6,$131,r6
	insv	r6,$7,$9,r1
#
#  Set the exponent field of the F-format
#  number in  r3  to the appropriate value.
#
	tstf	r3
	beql	return
#	extzv	$7,$8,r3,r4	-S.McD
	extzv	$7,$7,r3,r4
	addw2	r4,r6
#	subw2	$217,r6		-S.McD
	subw2	$64,r6
	insv	r6,$7,$8,r3
	brb	return
#						p.21
#
#  The following code generates the appropriate 
#  result for the unlikely possibility that
#  rounding the number in  r9/r8  resulted in 
#  a carry out.
#
carryout:
	clrl	r1
	clrl	r2
	subw3	r6,$132,r6
	insv	r6,$7,$9,r1
	tstf	r3
	beql	return
	extzv	$7,$8,r3,r4
	addw2	r4,r6
	subw2	$218,r6
	insv	r6,$7,$8,r3
#						p.22
#
#  The following code makes an needed
#  adjustments to the signs of the 
#  results or to the octant number, and
#  then returns.
#
return:
#
#  Test if the fraction was greater than or 
#  equal to  1/2 .  If so, negate the reduced
#  argument.
#
	blbc	r5,signoff8
	mnegf	r1,r1
	mnegf	r3,r3
signoff8:
#						p.23
#
#  If the original argument was negative,
#  negate the reduce argument and
#  adjust the octant number.
#
	tstw	(sp)+
	bgeq	signoff9
	mnegf	r1,r1
	mnegf	r3,r3
#	subb3	r0,$8,r0	...used for  pi/4  reduction -S.McD
	subb3	r0,$4,r0
signoff9:
#
#  Clear all unneeded octant bits.
#
#	bicb2	$0xf8,r0	...used for  pi/4  reduction -S.McD
	bicb2	$0xfc,r0
#
#  Return.
#
	rsb
