# @(#)ldexp.s	4.1 (Berkeley) 12/21/80
#
#	double ldexp (value, exp)
#		double value;
#		int exp;
#
#	Ldexp returns value*2**exp, if that result is in range.
#	If underflow occurs, it returns zero.  If overflow occurs,
#	it returns a value of appropriate sign and largest
#	possible magnitude.  In case of either overflow or underflow,
#	the external int "errno" is set to ERANGE.  Note that errno is
#	not modified if no error occurs, so if you intend to test it
#	after you use ldexp, you had better set it to something
#	other than ERANGE first (zero is a reasonable value to use).
#
#	Constants
#
	.set	erange,34	# error number for range error

	.data
	.globl	_errno		# error flag

huge:	.word	0x7fff		# The largest number that can
	.word	0xffff		#   be represented in a long floating
	.word	0xffff		#   number.  This is given in hex in order
	.word	0xffff		#   to avoid floating conversions
#
#	Entry point
#
	.text
	.globl	_ldexp
_ldexp:	.word	0x0000		# We use r2, but do not save it

	movd	4(ap),r0	# Fetch "value"

	extzv	$7,$8,r0,r2	# r2 := biased exponent
	jeql	ld1		# If it's zero, we're done

	addl2	12(ap),r2	# r2 := new biased exponent
	jleq	under		# if it's <= 0, we have an underflow
	cmpl	r2,$256		# Otherwise check if it's too big
	jgeq	over		# jump if overflow
#
#	Construct the result and return
#
	insv	r2,$7,$8,r0	# Put the exponent back in the result
ld1:	ret
#
#	Underflow
#
under:	clrd	r0		# Result is zero
	jbr	err		# Join general error code
#
#	Overflow
#
over:	movd	huge,r0		# Largest possible floating magnitude
	jbc	$15,4(ap),err	# Jump if argument was positive
	mnegd	r0,r0		# If arg < 0, make result negative

err:	movl	$erange,_errno	# Indicate range error
	ret
