/*
char id_fmtlib[] = "@(#)fmtlib.c	1.2";
 *
 * integer to ascii conversion
 */

#include "fio.h"

char	_digit[] = "0123456789abcdefghijklmnopqrstuvwxyz";

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
	else if (signit)	/* signed */
	{
		*sign = (value < 0);
		c = (int)(value % radix);
		value /= radix;
		if (*sign)
		{	value = -value;
			c = -c;
		}
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
	*(buf+MAXINTLENGTH) = _digit[c];
	for(i=MAXINTLENGTH-one; value!=0; i--)
	{
		c = (int)(value % radix);
		*(buf+i) = _digit[c];
		value /= radix;
	}
	*ndigit = MAXINTLENGTH - i;
	return(&buf[i+one]);
}
