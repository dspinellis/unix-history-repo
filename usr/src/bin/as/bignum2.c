/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bignum2.c	5.1 (Berkeley) 4/30/85";
#endif not lint

#include <stdio.h>
#include "as.h"
Bignum	Znumber;			/* zero reference */
#define	MINEXP		-32768		/* never generate; reserved for id 0 */

Bignum intconvert(number, convto)
	Bignum	number;
	int	convto;
{
	reg	int	i;
	if (number.num_tag == convto)
		return(number);
	if (ty_nbyte[number.num_tag] > ty_nbyte[convto] && (passno == 2)){
		yywarning("Conversion between %s and %s looses significance",
			ty_string[number.num_tag],
			ty_string[convto]);
	}
	for (i = ty_nbyte[convto]; i < ty_nbyte[TYPO]; i++)
		number.num_uchar[i] = 0;
	return(number);
}

#define	CONV(src, dst)	(((src) << TYPLG) + (dst))

Bignum floatconvert(number, convto)
	Bignum	number;
	int	convto;
{
	reg	u_int	*bp;		/* r11 */
		int	loss = 0;
		int	gain = 0;
		int	mixs = 0;
		Ovf	ovf;

	if (number.num_tag == convto)
		return(number);
	bp = &number.num_uint[0];
#ifdef lint
	*bp = *bp;
#endif lint

	switch(CONV(number.num_tag, convto)){

	case CONV(TYPF, TYPD):	asm("cvtfd (r11), (r11)"); break;
	case CONV(TYPF, TYPG):	mixs++; break;
	case CONV(TYPF, TYPH):	mixs++; break;

	case CONV(TYPD, TYPF):	asm("cvtdf (r11), (r11)"); break;
	case CONV(TYPD, TYPG):	mixs++; break;
	case CONV(TYPD, TYPH):	mixs++; break;

	case CONV(TYPG, TYPF):	mixs++; break;
	case CONV(TYPG, TYPD):	mixs++; break;
	case CONV(TYPG, TYPH):	mixs++; break;

	case CONV(TYPH, TYPF):	mixs++; break;
	case CONV(TYPH, TYPD):	mixs++; break;
	case CONV(TYPH, TYPG):	mixs++; break;
	default:	panic("Bad floating point conversion?");
	}
	if ((gain || mixs || loss) && (passno == 2)){
		yywarning("Converting from %s to %s: %s ",
			ty_string[number.num_tag],
			ty_string[convto],
			gain ? "gains significance" :
			(loss ? "looses significance" : "mixs exponent formats")
		);
	}
	if (mixs){
		number = bignumconvert(number, convto, &ovf);
		if (ovf && passno == 2){
			yywarning("Floating conversion over/underflowed\n");
		}
	} else {
		number.num_tag = convto;
	}
	return(number);
}

/* 
 *	Convert a big number between various representations
 */
Bignum bignumconvert(number, toconv, ovfp)
	Bignum	number;
	int	toconv;
	Ovf	*ovfp;
{
	int	tag;

	*ovfp = 0;
	tag = number.num_tag;
	if (tag == toconv)
		return(number);
	if (tag == TYPUNPACKED){
		return(bignumpack(number, toconv, ovfp));
	}
	if (toconv == TYPUNPACKED){
		return(bignumunpack(number, ovfp));
	}
	return(bignumpack(bignumunpack(number, ovfp), toconv, ovfp));
}

Bignum bignumunpack(Packed, ovfp)
	Bignum	Packed;
		Ovf	*ovfp;
{
		Bignum	Mantissa;
		Bignum	Enumber;
	reg	int	i;
		int	j;
		int	k;
	reg	struct	ty_bigdesc	*p;
	reg	chptr	packed;
	reg	chptr	mantissa;
	reg	chptr	enumber;
		u_short	exponent;
		int	sign;
		int	mask;

	p = &ty_bigdesc[Packed.num_tag];

	*ovfp = 0;
	Mantissa = Znumber;
	sign = 0;
	exponent = 0;
	mantissa = CH_FIELD(Mantissa);
	enumber = CH_FIELD(Enumber);
	packed = CH_FIELD(Packed);

	if (isclear(packed)){
		Mantissa.num_tag = TYPUNPACKED;
		Mantissa.num_exponent = MINEXP;
		return(Mantissa);
	}
	/*
	 *	map the packed number into the mantissa, using
	 *	the unpacking map
	 */
	mapnumber(mantissa, packed, 16, p->b_upmmap);
	/*
	 *	perform the mantissa shifting.
	 *	This may appear to overflow; all that is lost
	 *	is low order bits of the exponent.
	 */
	(void)numshift(p->b_mlshift, mantissa, mantissa);
	/*
	 *	handle sign and exponent
	 */
	switch(Packed.num_tag){
	case TYPB:
	case TYPW:
	case TYPL:
	case TYPO:
	case TYPQ:
		sign = 0;
		exponent = p->b_eexcess;
		if (mantissa[HOC] & SIGNBIT){
			sign = -1;
			*ovfp |= numnegate(mantissa, mantissa);
		}
		/*
		 *	Normalize the packed by left shifting,
		 *	adjusting the exponent as we go.
		 *	Do a binary weighted left shift for some speed.
		 */
		k = 0;
		for (j = 4; j >= 0; --j){
			i = 1 << j;	/* 16, 8, 4, 2, 1 */
			while(1){
				if (k >= p->b_msigbits)
					break;
				mask = ONES(i) << (CH_BITS - i);
				if (mantissa[HOC] & mask)
					break;
				(void)numshift(i, mantissa, mantissa);
				k += i;
				exponent -= i;
			}
		}
		assert(mantissa[HOC] & SIGNBIT, "integer <<ing");
		/*
		 *	now, kick the most significant bit off the top
		 */
		(void)numshift(1, mantissa, mantissa);
		break;
	default:
		/*
		 *	map the exponent into the local area.
		 */
		Enumber = Znumber;
		mapnumber(enumber, packed, 2, p->b_upemap);
		/*
		 *	Extract the exponent, and get rid
		 *	of the sign bit
		 */
		exponent = Enumber.num_ushort[0] & ONES(15);
		/*
		 *	shift the exponent, and get rid of high order
		 *	trash
		 */
		exponent >>= p->b_ershift;
		exponent &= ONES(p->b_esigbits);
		/*
		 *	un excess the exponent
		 */
		exponent -= p->b_eexcess;
		/*
		 *	extract and extend the sign bit
		 */
		sign = (Enumber.num_ushort[0] & ~ONES(15)) ? -1 : 0;
	}
	/*
	 *	Assemble the pieces, and return the number
	 */
	Mantissa.num_tag = TYPUNPACKED;
	Mantissa.num_sign = sign;
	Mantissa.num_exponent = exponent;
	return(Mantissa);
}

Bignum bignumpack(Unpacked, toconv, ovfp)
	Bignum	Unpacked;
	int	toconv;
	Ovf	*ovfp;
{
	Bignum	Back;
	Bignum	Enumber;
	Bignum	Temp;

		short	exponent;
		char	sign;
	reg	struct	ty_bigdesc	*p;
	reg	chptr	back;
	reg	chptr	enumber;
	reg	chptr	temp;
	reg	chptr	unpacked;

		int	i,j;

	if (Unpacked.num_tag != TYPUNPACKED)
		panic("Argument to bignumpack is not unpacked");

	*ovfp = 0;
	Back = Znumber;
	Temp = Znumber;
	Back.num_tag = toconv;

	back = CH_FIELD(Back);
	temp = CH_FIELD(Temp);
	enumber = CH_FIELD(Enumber);
	unpacked = CH_FIELD(Unpacked);
	p = &ty_bigdesc[toconv];

	exponent = Unpacked.num_exponent;
	sign = Unpacked.num_sign;
	if (exponent == MINEXP)
		return(Back);	/* identically zero */

	switch(toconv){
	case TYPB:
	case TYPW:
	case TYPL:
	case TYPQ:
	case TYPO:
		/*
		 *	Put back in the assumed high order fraction
		 *	bit that is always a 1.
		 */
		(void)numshift(-1, temp, unpacked);
		temp[HOC] |= SIGNBIT;
		if (exponent > p->b_eexcess){
			/*
			 *	Construct the largest positive integer
			 */
			(void)numclear(temp);
			(void)num1comp(temp, temp);
			temp[HOC] &= ~SIGNBIT;
			sign = sign;
			*ovfp |= OVF_OVERFLOW;
		} else
		if (exponent <= 0){
			/*
			 *	chop the temp; underflow to integer 0
			 */
			(void)numclear(temp);
			sign = 0;
			*ovfp |= OVF_UNDERFLOW;
		} else {
			/*
			 *	denormalize the temp.
			 *	This will again chop, by shifting
			 *	bits off the right end into oblivion.
			 */
			for (j = 4; j >= 0; --j){
				i = 1 << j;	/* 16, 8, 4, 2, 1 */
				while(exponent + i <= p->b_eexcess){
					numshift(-i, temp, temp);
					exponent += i;
				}
			}
		}
		/*
		 *	negate the temp if the sign is set
		 */
		if (sign)
			*ovfp |= numnegate(temp, temp);
		/*
		 *	Stuff the temp number into the return area
		 */
		mapnumber(back, temp, 16, p->b_pmmap);
		return(Back);
	default:
		/*
		 *	Shift the mantissa to the right, filling in zeroes on
		 *	the left.  This aligns the least significant bit
		 *	on the bottom of a byte, something that upround
		 *	will use.
		 *	Put the result into a temporary.
		 *	Even though the shift may be zero, there
		 *	is still a copy involved here.
		 */
		(void)numshift(-(p->b_mlshift), temp, unpacked);
		/*
		 *	Perform the rounding by adding in 0.5 ulp's
		 */
		exponent = upround(&Temp, p, exponent);
		/*
		 *	Do a range check on the exponent, in preparation
		 *	to stuffing it in.
		 */
		if ((short)(exponent + p->b_eexcess) == 0){
			/*
			 *	Sorry, no gradual underflow on the
			 *	VAX.  Chop this beasty totally to zero
			 */
			goto zeroret;
		} else
		if ((short)(exponent + p->b_eexcess) < 0){
			/*
			 *	True underflow will happen;
			 *	Chop everything to positive zero
			 */
		  zeroret:
			(void)numclear(temp);
			exponent = 0;
			sign = 0;	/* avoid reserved operand! */
			*ovfp |= OVF_UNDERFLOW;
		} else
		if ((unsigned)(exponent + p->b_eexcess)
		    >= (unsigned)(1 << p->b_esigbits)){
			/*
			 *	Construct the largest magnitude possible
			 *	floating point unpacked: 0.{1}111111111
			 */
			(void)numclear(temp);
			(void)num1comp(temp, temp);
			exponent = ONES(p->b_esigbits);
			sign = sign;
			*ovfp |= OVF_OVERFLOW;
		} else {
			/*
			 *	The exponent will fit.
			 *	Bias it up, and the common code will stuff it.
			 */
			exponent += p->b_eexcess;
		}
		exponent <<= p->b_ershift;
		/*
		 *	mask out trash for the sign, and put in the sign.
		 */
		exponent &= ONES(15);
		if (sign)
			exponent |= ~ONES(15);
		Enumber.num_ushort[0] = exponent;
		/*
		 *	Map the unpacked exponent into the value going back
		 */
		mapnumber(back, enumber, 2, p->b_pemap);
		/*
		 *	Stuff the unpacked mantissa into the return area
		 */
		mapnumber(back, temp, 16, p->b_pmmap);
		return(Back);
	}
	/*NOTREACHED*/
}

mapnumber(chp1, chp2, nbytes, themap)
		chptr	chp1, chp2;
		int	nbytes;
		char	*themap;
{
	reg	int	i;
	reg	u_char	*p1, *p2;

	p1 = (u_char *)chp1;
	p2 = (u_char *)chp2;
	for (i = 0; i < nbytes; i++){
		switch(themap[i]){
		case NOTAKE:
			break;
		default:
			p1[themap[i]] |= p2[i];
			break;
		}
	}
}

#define	UPSHIFT	2
/*
 *	round in 1/2 ulp in the number, possibly modifying
 *	the binary exponent if there was total carry out.
 *	Return the modified exponent
 */
int upround(numberp, p, exponent)
	reg	Bignum	*numberp;
	reg	struct	ty_bigdesc	*p;
		int	exponent;
{
	reg	u_char	*bytep;
		int	nbytes;
		int	byteindex;
		int	hofractionbit;
		int	ovffractionbit;
	reg	int	ovfbitindex;
	reg	chptr	number;
	static	Bignum	ulp;

	/*
	 *	Find out the byte index of the byte containing the ulp
	 */
	number = CH_FIELD(numberp[0]);
	bytep = numberp->num_uchar;

	nbytes = (p->b_msigbits - 1) + p->b_mlshift;
	assert((nbytes % 8) == 0, "mantissa sig bits");
	nbytes /= 8;
	byteindex = 15 - nbytes;
	assert(byteindex >= 0, "ulp in outer space");
	/*
	 *	Shift the number to the right by two places,
	 *	so that we can do full arithmetic without overflowing
	 *	to the left.
	 */
	numshift(-UPSHIFT, number, number);
	/*
	 *	Construct the missing high order fraction bit
	 */
	ovfbitindex = 8 - (p->b_mlshift + UPSHIFT);
	assert(ovfbitindex >= 0, "Shifted byte 15 into byte 14");
	hofractionbit = (0x01 << ovfbitindex);
	ovffractionbit = (0x02 << ovfbitindex);
	bytep[15] |= hofractionbit;
	/*
	 *	construct the unit in the last place, and it
	 *	to the fraction
	 */
	ulp.num_uchar[byteindex] |= (0x80 >> UPSHIFT);
	numaddv(number, number, CH_FIELD(ulp));
	ulp.num_uchar[byteindex] &= ~(0x80 >> UPSHIFT);
	/*
	 *	Check if there was an overflow,
	 *	and adjust by shifting.
	 *	Also, bring the number back into canonical
	 *	unpacked form by left shifting by two to undeo
	 *	what we did before.
	 */
	if (bytep[15] & ovffractionbit){
		exponent += 1;
		numshift(UPSHIFT - 1, number, number);
	} else {
		numshift(UPSHIFT, number, number);
	}
	/*
	 *	Clear off trash in the unused bits of the high
	 *	order byte of the number
	 */
	bytep[15] &= ONES(8 - p->b_mlshift);
	return(exponent);
}
#ifdef DEBUG
bignumprint(number)
	Bignum	number;
{
	printf("Bignum: %s (exp: %d, sign: %d) 0x%08x%08x%08x%08x",
		ty_string[number.num_tag],
		number.num_exponent,
		number.num_sign,
		number.num_uint[3],
		number.num_uint[2],
		number.num_uint[1],
		number.num_uint[0]);
	switch(number.num_tag){
	case TYPB:
	case TYPW:
	case TYPL:
	case TYPQ:
	case TYPO:
	case TYPUNPACKED:
		break;
	case TYPF:
		printf(" == %10.8e", number.num_num.numFf_float.Ff_value);
		break;
	case TYPD:
		printf(" == %20.17e", number.num_num.numFd_float.Fd_value);
		break;
	case TYPG:
	case TYPH:
		break;
	}
}

numprintovf(ovf)
	Ovf	ovf;
{
	int	i;
	static struct	ovftab{
		Ovf	which;
		char	*print;
	} ovftab[] = {
		OVF_POSOVF,	"posovf",
		OVF_MAXINT,	"maxint",
		OVF_ADDV,	"addv",
		OVF_LSHIFT,	"lshift",
		OVF_F,		"F float",
		OVF_D,		"D float",
		OVF_G,		"G float",
		OVF_H,		"H float",
		OVF_OVERFLOW,	"cvt overflow",
		OVF_UNDERFLOW,	"cvt underflow",
		0,		0
	};
	for(i = 0; ovftab[i].which; i++){
		if (ovf & ovftab[i].which)
			printf("Overflow(%s) ", ovftab[i].print);
	}
}
#endif DEBUG
