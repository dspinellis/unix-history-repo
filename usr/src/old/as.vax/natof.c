/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)natof.c 4.3 %G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "as.h"

Bignum bigatof(str, radix)
	reg	char	*str;		/* r11 */
		int	radix;		/* TYPF ... TYPH */
{
		int	msign;
		int	esign;
		int	decpt;
	reg	chptr	temp;		/* r10 */
	reg	u_int	quotient;	/* r9 */	/* must be here */
	reg	u_int	remainder;	/* r8 */	/* must be here */
	reg	chptr	acc;
	reg	int	dividend;	/* for doing division */
	reg	u_int	i;	
		short	*sptr;		/* for doing division */
		int	ch;
		int	dexponent;	/* decimal exponent */
		int	bexponent;	/* binary exponent */
		Bignum	Acc;
		Bignum	Temp;
	static	Bignum	znumber;
		Ovf	ovf;
		u_int	j;
	extern	int	errno;
		u_int	ediv();

#ifdef lint
	quotient = 0;
	remainder = 0;
#endif lint
	msign = 0;
	esign = 0;
	decpt = 0;
	dexponent = 0;
	Acc = znumber;
	Acc.num_tag = radix;
	acc = CH_FIELD(Acc);
	temp = CH_FIELD(Temp);

	do{
		ch = *str++;
	} while(isspace(ch));

	switch(ch){
	case '-':
		msign = -1;
		/* FALLTHROUGH */
	case '+':
		ch = *str++;
		break;
	}
  dofract:
	for(; isdigit(ch); ch = *str++){
		assert(((acc[HOC] & SIGNBIT) == 0), "Negative HOC");
		if (acc[HOC] < MAXINT_10){
			ovf = numshift(3, temp, acc);
			ovf |= numshift(1, acc, acc);
			ovf |= numaddv(acc, temp, acc);
			ovf |= numaddd(acc, acc, ch - '0');
			assert(ovf == 0, "Overflow building mantissa");
		} else {
			/*
			 *	Then, the number is too large anyway
			 */
			dexponent++;
		}
		if (decpt)
			dexponent--;
	}
	switch(ch){
	case '.':
		if (decpt == 0){
			decpt++;
			ch = *str++;
			goto dofract;
		}
		break;
	/*
	 *	only 'e' and 'E' are recognized by atof()
	 */
	case 'e':
	case 'E':
	/*
	 *	we include the remainder for compatability with as formats
	 *	in as, the radix actual paramater agrees with the character
	 *	we expect; consequently, no checking is done.
	 */
	case 'd':
	case 'D':
	case 'g':
	case 'G':
	case 'h':
	case 'H':
		j = 0;
		ch = *str++;
		esign = 0;
		switch(ch){
		case '-':
			esign = 1;
			/* FALLTHROUGH */
		case '+':
			ch = *str++;
		}
		for(; isdigit(ch); ch = *str++){
			if (j < MAXINT_10){
				j *= 10;
				j += ch - '0';
			} else {
				/*
				 *	outrageously large exponent
				 */
				 /*VOID*/
			}
		}
		if (esign)
			dexponent -= j;
		else
			dexponent += j;
		/*
		 *	There should be a range check on dexponent here
		 */
	}
	/*
	 *	The number has now been reduced to a mantissa
	 *	and an exponent.
	 *	The mantissa is an n bit number (to the precision
	 *	of the extended words) in the acc.
	 *	The exponent is a signed power of 10 in dexponent.
	 *	msign is on if the resulting number will eventually
	 *	be negative.
	 *
	 *	We now must convert the number to standard format floating
	 *	number, which will be done by accumulating
	 *	a binary exponent in bexponent, as we gradually
	 *	drive dexponent towards zero, one count at a time.
	 */
	if (isclear(acc)){
		return(Acc);
	}
	bexponent = 0;

	/*
	 *	Scale the number down.
	 *	We must divide acc by 10 as many times as needed.
	 */
	for (; dexponent < 0; dexponent++){
		/*
		 *	Align the number so that the most significant
		 *	bits are aligned in the most significant
		 *	bits of the accumulator, adjusting the
		 *	binary exponent as we shift.
		 *	The goal is to get the high order bit (NOT the
		 *	sign bit) set.
		 */
		assert(((acc[HOC] & SIGNBIT) == 0), "Negative HOC");
		ovf = 0;

		for (j = 5; j >= 1; --j){
			i = 1 << (j - 1); 		/* 16, 8, 4, 2, 1 */
			quotient = ONES(i);
			quotient <<= (CH_BITS - 1) - i;
			while((acc[HOC] & quotient) == 0){
				ovf |= numshift((int)i, acc, acc);
				bexponent -= i;
			}
		}
		/*
		 *	Add 2 to the accumulator to effect rounding,
		 *	and get set up to divide by 5.
		 */
		ovf = numaddd(acc, acc, 2);
		assert(ovf == 0, "Carry out of left rounding up by 2");
		/*
		 *	Divide the high order chunks by 5;
		 *	The last chunk will be divided by 10,
		 *	(to see what the remainder is, also to effect rounding)
		 *	and then multipiled by 2 to effect division by 5.
		 */
		remainder = 0;
#if DEBUGNATOF
		printf("Dividing: ");
		bignumprint(Acc);
		printf("\n");
#endif DEBUGNATOF
		sptr = (short *)acc;
		for (i = (CH_N * 2 - 1); i >= 1; --i){
			/*
			 *	Divide (remainder:16).(acc[i]:16)
			 *	by 5, putting the quotient back
			 *	into acc[i]:16, and save the remainder
			 *	for the next iteration.
			 */
			dividend = (remainder << 16) | (sptr[i] & ONES(16));
			assert(dividend >= 0, "dividend < 0");
			quotient = dividend / 5;
			remainder = dividend - (quotient * 5);
			sptr[i] = quotient;
			remainder = remainder;
		}
		/*
		 *	Divide the lowest order chunk by 10,
		 *	saving the remainder to decide how to round.
		 *	Then, multiply by 2, making it look as
		 *	if we divided by 10.
		 *	This multiply fills in a 0 on the least sig bit.
		 */
		dividend = (remainder << 16) | (sptr[0] & ONES(16));
		assert(dividend >= 0, "dividend < 0");
		quotient = dividend / 10;
		remainder = dividend - (quotient * 10);
		sptr[0] = quotient + quotient;

		if (remainder >= 5)
			ovf = numaddd(acc, acc, 1);
		/*
		 *	Now, divide by 2, effecting division by 10,
		 *	merely by adjusting the binary exponent.
		 */
		bexponent--;
	}
	/*
	 *	Scale the number up by multiplying by 10 as 
	 *	many times as necessary
	 */
	for (; dexponent > 0; dexponent--){
		/*
		 *	Compare high word to (2**31)/5,
		 *	and scale accordingly
		 */
		while ( ((unsigned)acc[HOC]) > MAXINT_5){
			(void)numshift(-1, acc, acc);
			bexponent++;
		}
		/*
		 *	multiply the mantissa by 5,
		 *	and scale the binary exponent by 2
		 */
		ovf = numshift(2, temp, acc);
		ovf |= numaddv(acc, acc, temp);
		assert(ovf == 0, "Scaling * 10 of manitissa");
		bexponent++;
	}
	/*
	 *	We now have:
	 *	a CH_N chunk length binary integer, right
	 *		justified (in native format).
	 *	a binary exponent.
	 *
	 *	Now, we treat this large integer as an octa word
	 *	number, and unpack it into standard unpacked
	 *	format.  That unpacking will give us yet
	 *	another binary exponent, which we adjust with
	 *	the accumulated binary exponent.
	 */
	Acc.num_tag = TYPO;
#if DEBUGNATOF
	printf("Octal number: ");
	bignumprint(Acc);
	printf("\n");
#endif DEBUGNATOF
	Acc = bignumunpack(Acc, &ovf);

	if (ovf)
		errno = ERANGE;
#if DEBUGNATOF
	printf("Unpacked octal number: ");
	bignumprint(Acc);
	printf("bexponent == %d\n", bexponent);
#endif DEBUGNATOF
	Acc.num_exponent += bexponent;
	assert(Acc.num_sign == 0, "unpacked integer is < 0");
	Acc.num_sign = msign;
	/*
	 *	We now pack the number back into a radix format number.
	 *	This checks for overflow, underflow,
	 *	and rounds by 1/2 ulp.
	 */
	ovf = 0;
	Acc = bignumpack(Acc, radix, &ovf);
	if (ovf)
		errno = ERANGE;
#if DEBUGNATOF
	printf("packed number: ");
	bignumprint(Acc);
	printf("\n");
#endif DEBUGNATOF
	return(Acc);
}
#if 0
/*
 *	Unfortunately, one can't use the ediv instruction to do
 *	division on numbers with > 64 bits.
 *	This is because ediv returns signed quantities;
 *	if the quotient is an unsigned number > 2^31,
 *	ediv sets integer overflow.
 */
unsigned int ediv(high, low, divisor, qp, i)
	register	unsigned int	high;		/* r11 */
	register	unsigned int	low;		/* r10 */
	register	unsigned int	divisor;	/* r9 */
			unsigned int	*qp;
{
	register	unsigned int	remainder;	/* r8 */
	register	unsigned int	quotient;	/* r7 */

	asm("ediv r9, r10, r7, r8	# Divide.  q->r7, r->r8 (discarded)");
	*qp = quotient;
	return(remainder);
}
#endif 0
