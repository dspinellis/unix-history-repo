/*
char id_fmtlib[] = "@(#)fmtlib.c	1.3";
 *
 * integer to ascii conversion
 *
 * This code has been rearranged to produce optimized runtime code.
 */

#include "fio.h"

static char	_digit[] = "0123456789abcdefghijklmnopqrstuvwxyz";
static char	_icv_buf[MAXINTLENGTH+1];
#define		_mask	0x7fffffff

char *
icvt(value, ndigit, sign)
long	value;
int	*ndigit;
int	*sign;
{
	register long	val = value;
	register long	rad = radix;
	register char	*b = &_icv_buf[MAXINTLENGTH];
	register char	*d = _digit;
	register long	tmp1;
	register int	tmp2;
	long	rem;
	long	kludge;

	if (val == 0)
	{
		*--b = '0';
		*sign = 0;
		*ndigit = 1;
		return(b);
	}

	if (signit && (*sign = (val < 0)))	/* signed conversion */
	{
		/*
		 * It is necessary to do the first divide
		 * before the absolute value, for the case -2^31
		 *
		 * This is actually what is being done...
		 * tmp1 = (int)(val % rad);
		 * val /= rad;
		 * val = -val
		 * *--b = d[-tmp1];
		 */
		tmp1 = val / rad;
		*--b = d[(tmp1 * rad) - val];
		val = -tmp1;
	}
	else				/* unsigned conversion */
	{
		*sign = 0;
		if (val < 0)
		{	/* ALL THIS IS TO SIMULATE UNSIGNED LONG MOD & DIV */
			kludge = _mask - (rad - 1);
			val &= _mask;
			/*
			 * This is really what's being done...
			 * rem = (kludge % rad) + (val % rad);
			 * val = (kludge / rad) + (val / rad) + (rem / rad) + 1;
			 * *--b = d[rem % rad];
			 */
			tmp1 = kludge / rad;
			tmp2 = val / rad;
			rem = (kludge - (tmp1 * rad)) + (val - (tmp2 * rad));
			val = ++tmp1 + tmp2;
			tmp1 = rem / rad;
			val += tmp1;
			*--b = d[rem - (tmp1 * rad)];
		}
	}

	while (val)
	{
		/*
		 * This is really what's being done ...
		 * *--b = d[val % rad];
		 * val /= rad;
		 */
		tmp1 = val / rad;
		*--b = d[val - (tmp1 * rad)];
		val = tmp1;
	}

	*ndigit = (&_icv_buf[MAXINTLENGTH] - b);
	return(b);
}
