/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)bignum1.c 4.3 %G%";
#endif not lint

#include <errno.h>
#include <stdio.h>
#include "as.h"

/*
 *	Construct a floating point number
 */
Bignum as_atof(numbuf, radix, ovfp)
	char	*numbuf;
	int	radix;
	Ovf	*ovfp;
{
	Bignum	number;
	extern	int	errno;
	double	atof();

	number = Znumber;
	errno = 0;
	switch(radix){
	case TYPF:
	case TYPD:
		number.num_tag = TYPD;
		*ovfp = 0;
		number.num_num.numFd_float.Fd_value = atof(numbuf);
		break;
	case TYPG:
	case TYPH:
		number = bigatof(numbuf, radix);
		break;
	}
	if (errno == ERANGE && passno == 2){
		yywarning("Floating conversion over/underflowed\n");
	}
	return(number);
}

/*
 *	Construct an integer.
 */

Bignum as_atoi(ccp, radix, ovfp)
	reg	char	*ccp;		/* character cp */
		int	radix;
		Ovf	*ovfp;
{
	reg	chptr	bcp;
		chptr	tcp;
	reg	int	i;
		int	val;
		Bignum	n_n;
		Bignum	t_n;
		int	sign;
		Ovf	ovf;

	ovf = 0;
	sign = 0;
	for (; *ccp; ccp++){
		switch(*ccp){
		case '0':
		case '+':	continue;
		case '-':	sign ^= 1;
				continue;
		}
		break;
	}

	n_n = Znumber;
	t_n = Znumber;
	bcp = CH_FIELD(n_n); (void)numclear(bcp);
	tcp = CH_FIELD(t_n); (void)numclear(tcp);
	for (; *ccp; ccp++){
		switch(*ccp){
		case '8': case '9':
			if (radix < 10)
				goto done;
			/*FALLTHROUGH*/
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':
			val = *ccp - '0';
			break;
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
			if (radix < 16)
				goto done;
			val = *ccp - 'A' + 10;
			break;
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
			if (radix < 16)
				goto done;
			val = *ccp - 'a' + 10;
			break;
		default:
			goto done;
		}
		switch(radix){
		case 8:
			ovf |= numshift(3, bcp, bcp);
			break;
		case 16:
			ovf |= numshift(4, bcp, bcp);
			break;
		case 10:
			ovf |= numshift(1, tcp, bcp);
			ovf |= numshift(3, bcp, bcp);
			ovf |= numaddv(bcp, tcp, bcp);
			break;
		}
		ovf |= numaddd(bcp, bcp, val);
	}
  done: ;
	ovf |= posovf(bcp);
	if (sign){
		if (ovf & OVF_MAXINT) {
			ovf &= ~(OVF_MAXINT | OVF_POSOVF);
		} else {
			ovf |= numnegate(bcp, bcp);
		}
	}
	/*
	 *	find the highest set unit of the number
	 */
	val = sign ? -1 : 0;
	for (i = 0; i < CH_N; i++){
		if (bcp[i] == val)
			break;
	}
	{
		static u_char tagtab[4][16] = {
		{	TYPB,
			TYPW,
			TYPL, TYPL,
			TYPQ, TYPQ, TYPQ, TYPQ,
			TYPO, TYPO, TYPO, TYPO, TYPO, TYPO, TYPO, TYPO},
		{	TYPW,
			TYPL,
			TYPQ, TYPQ,
			TYPO, TYPO, TYPO, TYPO},
		{   0   },
		{	TYPL,
			TYPQ,
			TYPO, TYPO }
		};
		/*
		 *	i indexes to the null chunk; make it point to the
		 *	last non null chunk
		 */
		i -= 1;
		if (i < 0)
			i = 0;
		n_n.num_tag = tagtab[HOC][i];
		assert(n_n.num_tag != 0, "Botch width computation");
	}
	*ovfp = ovf;
	return(n_n);
}

Ovf posovf(src)
	reg	chptr	src;
{
	reg	int	i;
	Ovf	overflow = 0;

	if (src[HOC] & SIGNBIT)
		overflow = OVF_POSOVF;
	if (src[HOC] == SIGNBIT){
		for (i = HOC - 1; i >= 0; --i){
			if (src[i] != 0)
				return(overflow);
		}
		overflow |= OVF_MAXINT;
	}
	return(overflow);
}

/*
 *	check if the number is clear
 */
int isclear(dst)
	reg	chptr	dst;
{
	return(!isunequal(dst, CH_FIELD(Znumber)));
}

int isunequal(src1, src2)
	reg	chptr	src1, src2;
{
	reg	int	i;

	i = CH_N;
	do{
		if (*src1++ != *src2++)
			return(i);
	}while(--i);
	return(0);
}

Ovf numclear(dst)
	reg	chptr	dst;
{
	reg	int	i;
	i = CH_N;
	do{
		*dst++ = 0;
	}while(--i);
	return(0);
}

Ovf numshift(n, dst, src)
		int	n;
	reg	chptr	dst, src;
{
	reg	int	i;
	reg	u_int	carryi, carryo;
	reg	u_int	mask;
	reg	u_int	value;

	i = CH_N;
	if (n == 0){
		do{
			*dst++ = *src++;
		} while(--i);
		return(0);
	}

	carryi = 0;
	mask = ONES(n);

	if (n > 0){
		do{
			value = *src++;
			carryo = (value >> (CH_BITS - n)) & mask; 
			value <<= n;
			value &= ~mask;
			*dst++ = value | carryi;
			carryi = carryo;
		} while (--i);
		return(carryi ? OVF_LSHIFT : 0);
	} else {
		n = -n;
		src += CH_N;
		dst += CH_N;
		do{
			value = *--src;
			carryo = value & mask; 
			value >>= n;
			value &= ONES(CH_BITS - n);
			*--dst = value | carryi;
			carryi = carryo << (CH_BITS - n);
		} while (--i);
		return(carryi ? OVF_LSHIFT : 0);
	}
}

Ovf numaddd(dst, src1, val)
	chptr	dst, src1;
	int	val;
{
	static	Bignum	work;

	work.num_uchar[0] = val;
	return (numaddv(dst, src1, CH_FIELD(work)));
}

Ovf numaddv(dst, src1, src2)
	reg	chptr	dst, src1, src2;
{
	reg	int	i;
	reg	int	carry;
	reg	u_int	A,B,value;

	carry = 0;
	i = CH_N;
	do{
		A = *src1++;
		B = *src2++;
		value = A + B + carry;
		*dst++ = value;
		carry = 0;
		if (value < A || value < B)
			carry = 1;
	} while (--i);
	return(carry ? OVF_ADDV : 0);
}

Ovf numnegate(dst, src)
	chptr	dst, src;
{
	Ovf	ovf;

	ovf = num1comp(dst, src) ;
	ovf |= numaddd(dst, dst, 1);
	return(ovf);
}

Ovf num1comp(dst, src)
	reg	chptr	dst, src;
{
	reg	int	i;
	i = CH_N;
	do{
		*dst++ = ~ *src++;
	}while (--i);
	return(0);
}

/*
 *	Determine if floating point numbers are
 *	capable of being represented as a one byte immediate literal constant
 *	If it is, then stuff the value into *valuep.
 *	argtype is how the instruction will interpret the number.
 */
int slitflt(number, argtype, valuep)
	Bignum	number;		/* number presented */
	int	argtype;	/* what the instruction expects */
	int	*valuep;
{
#define	EXPPREC 3
#define	MANTPREC 3

		int	mask;
	reg	int	i;
		Bignum	unpacked;
		Ovf	ovf;

	*valuep = 0;
	if (!ty_float[argtype])
		return(0);
	unpacked = bignumunpack(number, &ovf);
	assert(ovf == 0, "overflow in unpacking floating #!?");
	if (unpacked.num_sign)
		return(0);
	if (unpacked.num_exponent < 0)
		return(0);
	if (unpacked.num_exponent > ONES(EXPPREC))
		return(0);
	for (i = 0; i < HOC; i++){
		if (CH_FIELD(unpacked)[i])
			return(0);
	}
	if ((CH_FIELD(unpacked)[HOC]) & ONES(CH_BITS - MANTPREC))
		return(0);
	*valuep = (unpacked.num_exponent & ONES(EXPPREC)) << MANTPREC;
	mask = (CH_FIELD(unpacked)[HOC]) >> (CH_BITS - MANTPREC);
	mask &= ONES(MANTPREC);
	*valuep |= mask;
	*valuep &= ONES(MANTPREC + EXPPREC);
	return(1);
}

#ifndef STANDALONE
/*
 *	Output a big number to txtfil
 *	Called only when passno == 2
 *
 *	The conversion specifies the width of the number to be written out.
 *	The width is supplied from either an initialized data directive
 *	(for example .float, .double), or from the operand size
 *	defined by an operator.
 *	If the number is of type quad or octal,
 *	we just write it out; this allows one to specify bit
 *	patterns for floating point numbers.
 *	If the number is one of the floating types and the conversion
 *	is not the same type, then we complain, but do the conversion anyway.
 *	The conversion is strict.
 */
bignumwrite(number, toconv)
	Bignum	number;
	int	toconv;		/* one of TYP[QO FDGH] */
{
	reg	u_int	*bp;
#ifdef VMS
		int	nints;
	reg	int	i;
#endif VMS

	if (passno != 2)
		return;

	bp = &number.num_uint[0];
	switch(number.num_tag){
	case TYPB:
	case TYPW:
	case TYPL:
	case TYPQ:
	case TYPO:
		number = intconvert(number, toconv);
		break;
	default:
		number = floatconvert(number, toconv);
		break;
	}
#ifdef UNIX
	bwrite((char *)bp, ty_nbyte[toconv], txtfil);
#endif UNIX
#ifdef VMS
	/*
	 *	rrh did not check this code when the new floating point
	 *	numbers were put into the assembler, as he didn't
	 *	have access to a vms system.
	 */
	nints = ty_nbyte[toconv] / 4;
	for (i = 0; i < nints; i++){
		puchar(vms_obj_ptr,-4);
		pulong(vms_obj_ptr, bp[i]);
	}
	if((vms_obj_ptr-sobuf) > 400) {
		write(objfil,sobuf,vms_obj_ptr-sobuf);
		vms_obj_ptr = sobuf + 1;
	}
	return;
#endif VMS
}
#endif STANDALONE
