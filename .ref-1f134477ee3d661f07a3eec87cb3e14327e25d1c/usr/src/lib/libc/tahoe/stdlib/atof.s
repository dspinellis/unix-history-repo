/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)atof.s	5.3 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 *	atof: convert ascii to floating
 *
 *	C usage:
 *
 *		double atof (s)
 *		char *s;
 *
 *	Register usage:
 *
 *		r0-1:	value being developed
 *		r2:	first section: pointer to the next character
 *			second section: binary exponent
 *		r3:	flags
 *		r4:	first section: the current character
 *			second section: scratch
 *		r5:	the decimal exponent
 *		r6-7:	scratch
 */
	.set	msign,0		# mantissa has negative sign
	.set	esign,1		# exponent has negative sign
	.set	decpt,2		# decimal point encountered

ENTRY(atof, R6|R7)
/*
 *	Initialization
 */
	clrl	r3		# All flags start out false
	movl	4(fp),r2	# Address the first character
	clrl	r5		# Clear starting exponent
/*
 *	Skip leading white space
 */
sk0:	movzbl	(r2),r4		# Fetch the next (first) character
	incl	r2
	cmpb	$' ,r4		# Is it blank?
	beql	sk0		#   ...yes
	cmpb	r4,$8		# 8 is lowest of white-space group
	blss	sk1		# Jump if char too low to be white space
	cmpb	r4,$13		# 13 is highest of white-space group
	bleq	sk0		# Jump if character is white space
sk1:
/*
 *	Check for a sign
 */
	cmpb	$'+,r4		# Positive sign?
	beql	cs1		#   ... yes
	cmpb	$'-,r4		# Negative sign?
	bneq	cs2		#   ... no
	orb2	$1<msign,r3	# Indicate a negative mantissa
cs1:	movzbl	(r2),r4		# Skip the character
	incl	r2
cs2:
/*
 *	Accumulate digits, keeping track of the exponent
 */
	clrl	r1
	clrl	r0		# Clear the accumulator
ad0:	cmpb	r4,$'0		# Do we have a digit?
	blss	ad4		#   ... no, too small
	cmpb	r4,$'9
	bgtr	ad4		#   ... no, too large
/*
 *	We got a digit.  Accumulate it
 */
	cmpl	r0,$214748364	# Would this digit cause overflow?
	bgeq	ad1		#   ... yes
/*
 *	Multiply (r0,r1) by 10.  This is done by developing
 *	(r0,r1)*2 in (r6,r7), shifting (r0,r1) left three bits,
 *	and adding the two quadwords.
 */
	shlq	$1,r0,r6	# (r6,r7)=(r0,r1)*2
	shlq	$3,r0,r0	# (r0,r1)=(r0,r1)*8
	addl2	r7,r1		# Add low halves
	adwc	r6,r0		# Add high halves
/*
 *	Add in the digit
 */
	subl2	$'0,r4		# Get the digit value
	addl2	r4,r1		# Add it into the accumulator
	adwc	$0,r0		# Possible carry into high half
	brb	ad2		# Join common code
/*
 *	Here when the digit won't fit in the accumulator
 */
ad1:	incl	r5		# Ignore the digit, bump exponent
/*
 *	If we have seen a decimal point, decrease the exponent by 1
 */
ad2:	bbc	$decpt,r3,ad3	# Jump if decimal point not seen
	decl	r5		# Decrease exponent
ad3:
/*
 *	Fetch the next character, back for more
 */
	movzbl	(r2),r4		# Fetch
	incl	r2
	brb	ad0		# Try again
/*
 *	Not a digit.  Could it be a decimal point?
 */
ad4:	cmpb	r4,$'.		# If it's not a decimal point, either it's
	bneq	ad5		#   the end of the number or the start of
				#   the exponent.
	bbs	$decpt,r3,ad5
	orb2	$1<decpt,r3	# If it IS a decimal point, we record that
	brb	ad3		#   we've seen one, and keep collecting
				#   digits if it is the first one.
			
/*
 *	Check for an exponent
 */
ad5:	clrl	r6		# Initialize the exponent accumulator

	cmpb	r4,$'e		# We allow both lower case e
	beql	ex1		#   ... and ...
	cmpb	r4,$'E		#   upper-case E
	bneq	ex7
/*
 *	Does the exponent have a sign?
 */
ex1:	movzbl	(r2),r4		# Get next character
	incl	r2
	cmpb	r4,$'+		# Positive sign?
	beql	ex2		#   ... yes ...
	cmpb	r4,$'-		# Negative sign?
	bneq	ex3		#   ... no ...
	orb2	$1<esign,r3	# Indicate exponent is negative
ex2:	movzbl	(r2),r4		# Grab the next character
	incl	r2
/*
 *	Accumulate exponent digits in r6
 */
ex3:	cmpb	r4,$'0		# A digit is within the range
	blss	ex4		# '0' through
	cmpb	r4,$'9		# '9',
	bgtr	ex4		# inclusive.
	cmpl	r6,$214748364	# Exponent outrageously large already?
	bgeq	ex2		#   ... yes
	moval	(r6)[r6],r6	# r6 *= 5
	movaw	-'0(r4)[r6],r6	# r6 = r6 * 2 + r4 - '0'
	brb	ex2		# Go 'round again
ex4:
/*
 *	Now get the final exponent and force it within a reasonable
 *	range so our scaling loops don't take forever for values
 *	that will ultimately cause overflow or underflow anyway.
 *	A tight check on over/underflow will be done by ldexp.
 */
	bbc	$esign,r3,ex5	# Jump if exponent not negative
	mnegl	r6,r6		# If sign, negate exponent
ex5:	addl2	r6,r5		# Add given exponent to calculated exponent
	cmpl	r5,$-100	# Absurdly small?
	bgtr	ex6		#   ... no
	movl	$-100,r5	#   ... yes, force within limit
ex6:	cmpl	r5,$100		# Absurdly large?
	blss	ex7		#   ... no
	movl	$100,r5		#   ... yes, force within bounds
ex7:
/*
 *	Our number has now been reduced to a mantissa and an exponent.
 *	The mantissa is a 63-bit positive binary integer in r0,r1,
 *	and the exponent is a signed power of 10 in r5.  The msign
 *	bit in r3 will be on if the mantissa should ultimately be
 *	considered negative.
 *
 *	We now have to convert it to a standard format floating point
 *	number.  This will be done by accumulating a binary exponent
 *	in r2, as we progressively get r5 closer to zero.
 *
 *	Don't bother scaling if the mantissa is zero
 */
	tstl	r1
	bneq	1f
	tstl	r0		# Mantissa zero?
	jeql	exit		#   ... yes

1:	clrl	r2		# Initialize binary exponent
	tstl	r5		# Which way to scale?
	bleq	sd0		# Scale down if decimal exponent <= 0
/*
 *	Scale up by "multiplying" r0,r1 by 10 as many times as necessary,
 *	as follows:
 *
 *	Step 1: Shift r0,r1 right as necessary to ensure that no
 *	overflow can occur when multiplying.
 */
su0:	cmpl	r0,$429496729	# Compare high word to (2**31)/5
	blss	su1		# Jump out if guaranteed safe
	shrq	$1,r0,r0	# Else shift right one bit
	incl	r2		#    bump exponent to compensate
	brb	su0		#    and go back to test again.
/*
 *	Step 2: Multiply r0,r1 by 5, by appropriate shifting and
 *	double-precision addition
 */
su1:	shlq	$2,r0,r6	# (r6,r7) := (r0,r1) * 4
	addl2	r7,r1		# Add low-order halves
	adwc	r6,r0		#   and high-order halves
/*
 *	Step 3: Increment the binary exponent to take care of the final
 *	factor of 2, and go back if we still need to scale more.
 */
	incl	r2		# Increment the exponent
	decl	r5		# ...sobgtr r5,su0
	bgtr	su0		#    and back for more (maybe)

	brb	cm0		# Merge to build final value

/*
 *	Scale down.  We must "divide" r0,r1 by 10 as many times
 *	as needed, as follows:
 *
 *	Step 0: Right now, the condition codes reflect the state
 *	of r5.  If it's zero, we are done.
 */
sd0:	beql	cm0		# If finished, build final number
/*
 *	Step 1: Shift r0,r1 left until the high-order bit (not counting
 *	the sign bit) is nonzero, so that the division will preserve
 *	as much precision as possible.
 */
	tstl	r0		# Is the entire high-order half zero?
	bneq	sd2		#   ...no, go shift one bit at a time
	shlq	$30,r0,r0	#   ...yes, shift left 30,
	subl2	$30,r2		#   decrement the exponent to compensate,
				#   and now it's known to be safe to shift
				#   at least once more.
sd1:	shlq	$1,r0,r0	# Shift (r0,r1) left one, and
	decl	r2		#   decrement the exponent to compensate
sd2:	bbc	$30,r0,sd1	# If the high-order bit is off, go shift
/*
 *	Step 2: Divide the high-order part of (r0,r1) by 5,
 *	giving a quotient in r1 and a remainder in r7.
 */
sd3:	movl	r0,r7		# Copy the high-order part
	clrl	r6		# Zero-extend to 64 bits
	ediv	$5,r6,r0,r6	# Divide (cannot overflow)
/*
 *	Step 3: Divide the low-order part of (r0,r1) by 5,
 *	using the remainder from step 2 for rounding.
 *	Note that the result of this computation is unsigned,
 *	so we have to allow for the fact that an ordinary division
 *	by 5 could overflow.  We make allowance by dividing by 10,
 *	multiplying the quotient by 2, and using the remainder
 *	to adjust the modified quotient.
 */
	addl3	$2,r1,r7	# Dividend is low part of (r0,r1) plus
	adwc	$0,r6		#  2 for rounding plus
				#  (2**32) * previous remainder
	ediv	$10,r6,r1,r7	# r1 := quotient, r7 := remainder.
	addl2	r1,r1		# Make r1 result of dividing by 5
	cmpl	r7,$5		# If remainder is 5 or greater,
	blss	sd4		#   increment the adjustted quotient.
	incl	r1
/*
 *	Step 4: Increment the decimal exponent, decrement the binary
 *	exponent (to make the division by 5 into a division by 10),
 *	and back for another iteration.
 */
sd4:	decl	r2		# Binary exponent
	aoblss	$0,r5,sd2
/*
 *	We now have the following:
 *
 *	r0:	high-order half of a 64-bit integer
 *	r1:	load-order half of the same 64-bit integer
 *	r2:	a binary exponent
 *
 *	Our final result is the integer represented by (r0,r1)
 *	multiplied by 2 to the power contained in r2.
 *	We will transform (r0,r1) into a floating-point value,
 *	set the sign appropriately, and let ldexp do the
 *	rest of the work.
 *
 *	Step 1: if the high-order bit (excluding the sign) of
 *	the high-order half (r0) is 1, then we have 63 bits of
 *	fraction, too many to convert easily.  However, we also
 *	know we won't need them all, so we will just throw the
 *	low-order bit away (and adjust the exponent appropriately).
 */
cm0:	bbc	$30,r0,cm1	# jump if no adjustment needed
	shrq	$1,r0,r0	# lose the low-order bit
	incl	r2		# increase the exponent to compensate
/*
 *	Step 2: split the 62-bit number in (r0,r1) into two
 *	31-bit positive quantities
 */
cm1:	shlq	$1,r0,r0	# put the high-order bits in r0
				#   and a 0 in the bottom of r1
	shrl	$1,r1,r1	# right-justify the bits in r1
				#   moving 0 into the sign bit.
/*
 *	Step 3: convert both halves to floating point
 */
	cvld	r1
	std	r6		# low-order part in r6-r7
	cvld	r0
	std	r0		# high-order part in r0-r1
/*
 *	Step 4: multiply the high order part by 2**31 and combine them
 */
	ldd	two31
	muld	r0		# multiply
	addd	r6		# combine
/*
 *	Step 5: if appropriate, negate the floating value
 */
	bbc	$msign,r3,cm2	# Jump if mantissa not signed
	negd			# If negative, make it so
/*
 *	Step 6: call ldexp to complete the job
 */
cm2:	pushl	r2		# Put exponent in parameter list
	pushd			#    and also mantissa
	calls	$3,_ldexp	# go combine them

exit:
	ret

	.align	2
two31:	.long	0x50000000	# (=2147483648) 2 ** 31 in floating-point
	.long	0		# so atof doesn't have to convert it
