/*
 * integer to ascii conversion
 */

#include "fio.h"

#define digit(c)	( (c > 9) ? (c - 10 + 'a') : c + '0')

char *icvt(value,ndigit,sign) long value; int *ndigit,*sign;
{
	static char buf[MAXINTLENGTH+1];
	register int i;
	long kludge, rem, mask = 0x7fffffff;
	int one = 1;
	char c;

	if (value == 0)
	{	*sign=0;
		*ndigit=one;
		buf[MAXINTLENGTH]='0';
		return(&buf[MAXINTLENGTH]);
	}
	else if (signit)	/* signed ? */
	{
		if (value > 0) *sign = 0;
		else
		{	value = -value;
			*sign = 1;
		}
		c = (int)(value % radix);
		value /= radix;
	}
	else			/* unsigned */
	{	*sign = 0;
		if (value < 0)
		{	/* ALL THIS IS TO SIMULATE UNSIGNED MOD & DIV */
			kludge = mask - (radix - one);
			value &= mask;
			rem = (kludge % radix) + (value % radix);
			value = (kludge / radix) + (value / radix)
				 + (rem / radix) + one;
			c = (int)(rem % radix);
		}
		else
		{
			c = (int)(value % radix);
			value /= radix;
		}
	}
	*(buf+MAXINTLENGTH) = digit(c);
	for(i=MAXINTLENGTH-one; value!=0; i--)
	{
		c = (int)(value % radix);
		*(buf+i) = digit(c);
		value /= radix;
	}
	*ndigit = MAXINTLENGTH - i;
	return(&buf[i+one]);
}
