/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)bignum1.c 4.4 6/30/83";
#endif not lint

#include <errno.h>
#include <stdio.h>
#include "as.h"

Bignum	Znumber;

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
		static u_char tagtab[4][8] = {
		{	TYPB,
			TYPW,
			TYPL, TYPL,
			TYPQ, TYPQ, TYPQ, TYPQ },
		{	TYPW,
			TYPL,
			TYPQ, TYPQ },
		{   0   },
		{	TYPL,
			TYPQ }
		};
		/*
		 *	i indexes to the null chunk; make it point to the
		 *	last non null chunk
		 */
		i -= 1;
		if (i < 0)
			i = 0;
		n_n.num_tag = tagtab[HOC][i];
		assert(n_n.num_tag != 0, " Botch width computation");
	}
	*ovfp = ovf;
	return(n_n);
}

Bignum as_atof (numbuf, radix)
	char *numbuf;
{
	double atof ();
	Bignum number;

	number = Znumber;
	number.num_tag = radix;
	switch (radix)
	{
	   case TYPD:
		number.num_num.numFd_float.Fd_value = atof (numbuf);
		break;
	   case TYPF:
		number.num_num.numFf_float.Ff_value = atof (numbuf);
		break;
	}

	return (number);
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

	work.num_uchar[3] = val;
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

bignumprint(number)
	Bignum	number;		/* number presented */
{
	switch (num_type)
	{
		case TYPQ:
			printf ("val[msd] = 0x%x, val[lsd] = 0x%x.",
			number.num_num.numIq_int.Iq_ulong[1],
			number.num_num.numIq_int.Iq_ulong[0]);
			break;
		case TYPF:
			printf ("value %20.17f",
			number.num_num.numFf_float.Ff_value);
			break;
		case TYPD:
			printf ("value %20.17f",
			number.num_num.numFd_float.Fd_value);
			break;
		default:
			break;
	}
}

